# get_zoom_cloud_recordings-v2.R

library(tidyverse)
library(httr)
library(jsonlite)
library(lubridate)

# Zoom API Configuration
zoom_api_base <- "https://api.zoom.us/v2"

account_id <- Sys.getenv("zoom_account_id") 
client_id <- Sys.getenv("zoom_client_id") 
client_secret <- Sys.getenv("zoom_client_secret")

# Function to get OAuth access token
get_access_token <- function(account_id, client_id, client_secret) {
  
  # OAuth token endpoint
  token_url <- "https://zoom.us/oauth/token"
  
  # Prepare the request
  response <- POST(
    url = token_url,
    body = list(
      grant_type = "account_credentials",
      account_id = account_id
    ),
    authenticate(client_id, client_secret),
    encode = "form"
  )
  
  # Check if request was successful
  if (status_code(response) != 200) {
    stop("Failed to get access token. Status: ", status_code(response), 
         "\nResponse: ", content(response, "text"))
  }
  
  # Extract access token
  token_data <- content(response, "parsed")
  return(token_data$access_token)
}

# Function to get all cloud recordings for entire account (using Reports API)
get_zoom_recordings <- function(account_id, client_id, client_secret, from_date = NULL, to_date = NULL) {
  
  # Get fresh access token
  access_token <- get_access_token(account_id, client_id, client_secret)
  
  # Set default dates if not provided (last 30 days)
  if (is.null(from_date)) {
    from_date <- Sys.Date() - 30
  }
  if (is.null(to_date)) {
    to_date <- Sys.Date()
  }
  
  # Convert dates to proper format
  from_date <- as.character(from_date)
  to_date <- as.character(to_date)
  
  # Initialize variables for pagination
  all_recordings <- list()
  next_page_token <- ""
  page_count <- 1
  
  repeat {
    cat("Fetching page", page_count, "...\n")
    
    # Try the Reports API endpoint first (often has different scope requirements)
    url <- paste0(zoom_api_base, "/report/cloud_recording")
    
    # Set up query parameters
    query_params <- list(
      from = from_date,
      to = to_date,
      page_size = 300  # Maximum allowed
    )
    
    # Add page token if not first page
    if (next_page_token != "") {
      query_params$next_page_token <- next_page_token
    }
    
    # Make API request
    response <- GET(
      url = url,
      query = query_params,
      add_headers(
        "Authorization" = paste("Bearer", access_token),
        "Content-Type" = "application/json"
      )
    )
    
    # Check if request was successful
    if (status_code(response) != 200) {
      stop("API request failed with status: ", status_code(response), 
           "\nResponse: ", content(response, "text"))
    }
    
    # Parse JSON response
    response_data <- content(response, "parsed")
    
    # Debug: Print response structure for first page
    if (page_count == 1) {
      cat("API Response structure:\n")
      cat("- from:", response_data$from, "\n")
      cat("- to:", response_data$to, "\n")
      cat("- page_count:", response_data$page_count, "\n")
      cat("- total_records:", response_data$total_records, "\n")
      cat("- cloud_recording_meetings found:", length(response_data$cloud_recording_meetings), "\n")
    }
    
    # Add recordings from this page (note: different field name for reports API)
    if (length(response_data$cloud_recording_meetings) > 0) {
      all_recordings <- append(all_recordings, response_data$cloud_recording_meetings)
    }
    
    # Check if there are more pages
    if (is.null(response_data$next_page_token) || 
        response_data$next_page_token == "") {
      break
    }
    
    next_page_token <- response_data$next_page_token
    page_count <- page_count + 1
  }
  
  cat("Retrieved", length(all_recordings), "recordings total.\n")
  return(all_recordings)
}

# Function to flatten the nested recording data
flatten_recordings <- function(recordings_list) {
  
  # Extract main meeting info and recording files
  recordings_df <- map_dfr(recordings_list, function(meeting) {
    
    # Extract main meeting fields
    main_fields <- meeting[!names(meeting) %in% c("recording_files")]
    
    # Convert NULL values to NA
    main_fields <- map(main_fields, ~ if(is.null(.)) NA else .)
    
    # Extract recording files
    if (!is.null(meeting$recording_files) && length(meeting$recording_files) > 0) {
      
      # Create a row for each recording file
      map_dfr(meeting$recording_files, function(file) {
        
        # Combine meeting info with file info
        combined <- c(main_fields, file)
        
        # Convert to data frame row
        as_tibble(combined)
      })
      
    } else {
      # If no recording files, just return meeting info
      as_tibble(main_fields)
    }
  })
  
  # Clean up data types
  recordings_df <- recordings_df %>%
    mutate(
      # Convert date/time fields
      across(matches("start_time|end_time"), ~ as_datetime(., format = "%Y-%m-%dT%H:%M:%SZ")),
      across(matches("recording_start|recording_end"), ~ as_datetime(., format = "%Y-%m-%dT%H:%M:%SZ")),
      
      # Convert numeric fields
      across(matches("duration|file_size"), ~ as.numeric(.)),
      across(matches("participant_count"), ~ as.integer(.)),
      
      # Convert logical fields
      across(matches("password_protected"), ~ as.logical(.))
    )
  
  return(recordings_df)
}

# Main execution
main <- function() {
  
  # Check if credentials are set
  if (account_id == "YOUR_ACCOUNT_ID_HERE" || 
      client_id == "YOUR_CLIENT_ID_HERE" || 
      client_secret == "YOUR_CLIENT_SECRET_HERE") {
    stop("Please set your Zoom OAuth credentials in the variables above")
  }
  
  # Get recordings (adjust date range as needed)
  cat("Downloading Zoom cloud recordings...\n")
  cat("Date range:", Sys.Date() - 90, "to", Sys.Date(), "\n")
  
  recordings_raw <- get_zoom_recordings(
    account_id = account_id,
    client_id = client_id,
    client_secret = client_secret,
    from_date = Sys.Date() - 90,  # Last 90 days
    to_date = Sys.Date()
  )
  
  # Handle empty results
  if (length(recordings_raw) == 0) {
    cat("No recordings found in the specified date range.\n")
    cat("Try extending the date range or check if you have recordings in your account.\n")
    return(tibble())
  }
  
  # Flatten the data
  cat("Processing recording data...\n")
  recordings_df <- flatten_recordings(recordings_raw)
  
  # Display summary
  cat("\nRecording Summary:\n")
  cat("Total recordings:", nrow(recordings_df), "\n")
  
  if (nrow(recordings_df) > 0) {
    cat("Date range:", min(recordings_df$start_time, na.rm = TRUE), "to", 
        max(recordings_df$start_time, na.rm = TRUE), "\n")
    
    # Show column names
    cat("\nAvailable columns:\n")
    print(names(recordings_df))
    
    # Show first few rows (only if we have the expected columns)
    if (all(c("topic", "start_time", "duration") %in% names(recordings_df))) {
      cat("\nFirst 5 recordings:\n")
      print(recordings_df %>% 
              select(any_of(c("topic", "start_time", "duration", "file_type", "file_size"))) %>% 
              head(5))
    } else {
      cat("\nFirst few rows:\n")
      print(head(recordings_df, 5))
    }
    
    # Save to CSV
    output_file <- paste0("zoom_recordings_", Sys.Date(), ".csv")
    write_csv(recordings_df, output_file)
    cat("\nData saved to:", output_file, "\n")
  } else {
    cat("No data to save.\n")
  }
  
  return(recordings_df)
}

# Run the script
recordings_data <- main()

# Example analysis (only if we have data)
if (exists("recordings_data") && nrow(recordings_data) > 0) {
  cat("\nExample Analysis:\n")
  
  # Recordings by file type
  if ("file_type" %in% names(recordings_data)) {
    recordings_data %>%
      count(file_type, sort = TRUE) %>%
      print()
  }
  
  # Total storage used
  if (all(c("file_size", "duration") %in% names(recordings_data))) {
    recordings_data %>%
      summarise(
        total_files = n(),
        total_size_gb = sum(file_size, na.rm = TRUE) / (1024^3),
        avg_duration_min = mean(duration, na.rm = TRUE)
      ) %>%
      print()
  }
} else {
  cat("\nNo data available for analysis.\n")
  cat("Possible issues:\n")
  cat("1. No recordings in the specified date range\n")
  cat("2. Check your Zoom app permissions (need 'recording:read:admin' scope)\n")
  cat("3. Verify your account has cloud recordings enabled\n")
}