# get a lsit of Zoom recordings

library(tidyverse)
library(httr)
library(jsonlite)
library(lubridate)
library(jose)
library(curl)
library(RCurl)


claim <- jwt_claim(
  exp = as.numeric(Sys.time() + 3600),
  iss = Sys.getenv("zoom_api_key")
)

jwt <- jwt_encode_hmac(
  claim,
  secret = charToRaw(Sys.getenv("zoom_api_secret"))
)

# Zoom API Configuration
zoom_api_base <- "https://api.zoom.us/v2"
jwt_token <- jwt # 


# Function to get all cloud recordings
get_zoom_recordings <- function(jwt_token, from_date = NULL, to_date = NULL) {
  
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
    
    # Build URL with parameters
    url <- paste0(zoom_api_base, "/users/me/recordings")
    
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
        "Authorization" = paste("Bearer", jwt_token),
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
    
    # Add recordings from this page
    if (length(response_data$meetings) > 0) {
      all_recordings <- append(all_recordings, response_data$meetings)
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
  
  # Check if JWT token is set
  if (jwt_token == "YOUR_JWT_TOKEN_HERE") {
    stop("Please set your JWT token in the jwt_token variable")
  }
  
  # Get recordings (adjust date range as needed)
  cat("Downloading Zoom cloud recordings...\n")
  recordings_raw <- get_zoom_recordings(
    jwt_token = jwt_token,
    from_date = Sys.Date() - 90,  # Last 90 days
    to_date = Sys.Date()
  )
  
  # Flatten the data
  cat("Processing recording data...\n")
  recordings_df <- flatten_recordings(recordings_raw)
  
  # Display summary
  cat("\nRecording Summary:\n")
  cat("Total recordings:", nrow(recordings_df), "\n")
  cat("Date range:", min(recordings_df$start_time, na.rm = TRUE), "to", 
      max(recordings_df$start_time, na.rm = TRUE), "\n")
  
  # Show column names
  cat("\nAvailable columns:\n")
  print(names(recordings_df))
  
  # Show first few rows
  cat("\nFirst 5 recordings:\n")
  print(recordings_df %>% 
          select(topic, start_time, duration, file_type, file_size) %>% 
          head(5))
  
  # Save to CSV
  output_file <- paste0("zoom_recordings_", Sys.Date(), ".csv")
  write_csv(recordings_df, output_file)
  cat("\nData saved to:", output_file, "\n")
  
  return(recordings_df)
}

# Run the script
recordings_data <- main()

# Example analysis
cat("\nExample Analysis:\n")

# Recordings by file type
recordings_data %>%
  count(file_type, sort = TRUE) %>%
  print()

# Total storage used
recordings_data %>%
  summarise(
    total_files = n(),
    total_size_gb = sum(file_size, na.rm = TRUE) / (1024^3),
    avg_duration_min = mean(duration, na.rm = TRUE)
  ) %>%
  print()