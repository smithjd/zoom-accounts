# Load required libraries
library(httr2)
library(jsonlite)
library(dplyr)
library(openssl)
library(purrr)
library(tidyverse)

# Zoom API credentials
# zoom_api_key <- Sys.getenv("zoom_client_id")
# zoom_api_secret <- Sys.getenv("zoom_client_secret")
# account_id <- Sys.getenv("zoom_account_id")

client_id <- Sys.getenv("zoom_client_id")
client_secret <- Sys.getenv("zoom_client_secret")
account_id <- Sys.getenv("zoom_account_id")

# Zoom OAuth endpoints
auth_endpoint <- "https://zoom.us/oauth/authorize"
token_endpoint <- "https://zoom.us/oauth/token"

# Function to get OAuth token
get_oauth_token <- function(client_id, client_secret) {
  req <- request(token_endpoint) %>%
    req_method("POST") %>%
    req_headers("Content-Type" = "application/x-www-form-urlencoded") %>%
    req_body_form(
      grant_type = "account_credentials",
      account_id = account_id,
      client_id = client_id,
      client_secret = client_secret
    )
  
  resp <- req_perform(req)
  token_data <- resp %>% resp_body_json()
  return(token_data$access_token)
}

# Get the OAuth token
oauth_token <- get_oauth_token(client_id, client_secret)

# Create a base request with OAuth authentication
base_req <- request("https://api.zoom.us/v2") %>%
  req_headers(
    Authorization = paste("Bearer", oauth_token),
    "Content-Type" = "application/json"
  )

# Function to get all users (including pagination)
get_all_users <- function(req) {
  all_users <- list()
  page_number <- 1
  page_size <- 300
  
  repeat {
    users_req <- req %>%
      req_url_path_append("users") %>%
      req_url_query(status = "active", page_size = page_size, page_number = page_number)
    
    resp <- req_perform(users_req)
    users_data <- resp %>% resp_body_json()
    
    all_users <- c(all_users, users_data$users)
    
    if (users_data$page_number * users_data$page_size >= users_data$total_records) {
      break
    }
    
    page_number <- page_number + 1
  }
  
  return(all_users)
}

user_list <- get_all_users(base_req)

# Function to get meetings for a user
get_user_meetings <- function(req, user_id, start_date, end_date) {
  meetings_req <- req %>%
    req_url_path_append("users", user_id, "meetings") %>%
    req_url_query(type = "past", page_size = 300, from = start_date, to = end_date)
  
  resp <- req_perform(meetings_req)
  meetings_data <- resp %>% resp_body_json()
  return(meetings_data$meetings)
}

# get a data frame with the id from the user_list 
users <- tibble(id = map(user_list, "id"),
                display_name = map(user_list, "display_name"),
                email = map(user_list, "email"),
                type = map(user_list, "type"),
                timezone = map(user_list, "timezone"),
                dept = map(user_list, "dept"),
                last_login_time = map(user_list, "last_login_time")
                ) |> 
  unnest(cols = c(id, display_name, email, type, timezone, dept, last_login_time)) |>
  mutate(last_login_time = lubridate::ymd_hms(last_login_time))



user_meeting_list <- get_user_meetings(base_req, "kwocPxT1SKigKQYDRw9rzg", "2024-01-01", "2024-08-31")
user_meeting_list <- get_user_meetings(base_req, users, "2024-01-01", "2024-08-31")

# Main function to generate the report
generate_zoom_usage_report <- function(start_date, end_date) {
  users <- get_all_users(base_req)
  
  user_meetings <- map_df(users, function(user) {
    meetings <- get_user_meetings(base_req, user$id, start_date, end_date)
    meetings_df <- if (length(meetings) > 0) {
      map_df(meetings, ~ as_tibble(.x))
    } else {
      tibble()
    }
    
    meetings_summary <- meetings_df %>%
      summarise(
        total_meetings = n(),
        total_duration = sum(duration, na.rm = TRUE),
        avg_participants = mean(participants, na.rm = TRUE)
      )
    
    tibble(
      host_id = user$id,
      host_email = user$email,
      total_meetings = meetings_summary$total_meetings,
      total_duration = if ("duration" %in% names(meetings_df)) meetings_summary$total_duration else NA_real_,
      avg_participants = meetings_summary$avg_participants
    )
  })
  
  # Ensure all columns exist and replace NaN with NA
  user_meetings <- user_meetings %>%
    replace_na(list(total_meetings = 0, total_duration = NA_real_, avg_participants = NA_real_)) %>%
    mutate(across(where(is.numeric), ~ifelse(is.nan(.), NA_real_, .)))
  
  return(user_meetings)
}

# Generate the report for a specific date range
start_date <- "2024-01-01"
end_date <- "2024-08-31"
usage_report <- generate_zoom_usage_report(start_date, end_date)

# Display the report
print(usage_report)

# Optionally, save the report to a CSV file
write.csv(usage_report, "zoom_usage_report.csv", row.names = FALSE)

#####

# Function to get meetings for a user with debugging
# Load required libraries
library(httr)
library(jsonlite)

# Function to get meetings for a user with debugging
get_user_meetings <- function(oauth_token, user_id, start_date, end_date) {
  base_url <- "https://api.zoom.us/v2"
  endpoint <- paste0("/users/", user_id, "/meetings")
  
  # Construct the full URL
  url <- modify_url(base_url, path = endpoint)
  
  # Set up query parameters
  query_params <- list(
    type = "scheduled",
    page_size = 300,
    from = start_date,
    to = end_date
  )
  
  # Print the full URL for debugging
  print(paste("Request URL:", url))
  
  # Make the API call
  response <- GET(
    url,
    query = query_params,
    add_headers(
      Authorization = paste("Bearer", oauth_token),
      "Content-Type" = "application/json"
    )
  )
  
  # Print response status for debugging
  print(paste("Response status:", status_code(response)))
  
  # Check for errors
  if (http_error(response)) {
    print(paste("Error: ", http_status(response)$message))
    print("Response content:")
    print(content(response, "text"))
    return(NULL)
  }
  
  # Parse the JSON response
  meetings_data <- content(response, "parsed")
  
  # Print the raw response body for debugging
  print("Raw response body:")
  print(str(meetings_data))
  
  if (length(meetings_data$meetings) == 0) {
    print("No meetings found in the response.")
  }
  
  return(meetings_data$meetings)
}

# Test the function
oauth_token <- get_oauth_token(client_id, client_secret)
user_meeting_list <- get_user_meetings(oauth_token, "john.smith@shambhalaglobal.org", "2024-01-01", "2024-08-31")
print(paste("Number of meetings returned:", length(user_meeting_list)))

########

# Load required libraries
library(httr)
library(jsonlite)

# Function to get meetings for a user with improved error handling
get_user_meetings <- function(oauth_token, user_id, start_date, end_date) {
  base_url <- "https://api.zoom.us/v2"
  endpoint <- paste0("/users/", user_id, "/meetings")
  
  # Construct the full URL
  url <- modify_url(base_url, path = endpoint)
  
  # Set up query parameters
  query_params <- list(
    type = "scheduled",
    page_size = 300,
    from = start_date,
    to = end_date
  )
  
  # Print the full URL for debugging
  print(paste("Request URL:", url))
  
  # Make the API call
  response <- GET(
    url,
    query = query_params,
    add_headers(
      Authorization = paste("Bearer", oauth_token),
      "Content-Type" = "application/json"
    )
  )
  
  # Print response status for debugging
  print(paste("Response status:", status_code(response)))
  
  # Check for errors
  if (http_error(response)) {
    error_content <- content(response, "text", encoding = "UTF-8")
    print(paste("Error: ", http_status(response)$message))
    print("Response content:")
    print(error_content)
    
    # Try to parse error content as JSON
    tryCatch({
      error_json <- fromJSON(error_content)
      if (!is.null(error_json$message)) {
        print(paste("Error message:", error_json$message))
      }
    }, error = function(e) {
      print("Could not parse error response as JSON")
    })
    
    return(NULL)
  }
  
  # Parse the JSON response
  meetings_data <- content(response, "parsed")
  
  # Print the raw response body for debugging
  print("Raw response body:")
  print(str(meetings_data))
  
  if (length(meetings_data$meetings) == 0) {
    print("No meetings found in the response.")
  }
  
  return(meetings_data$meetings)
}

# Function to check user existence
check_user <- function(oauth_token, user_id) {
  url <- modify_url("https://api.zoom.us/v2", path = paste0("/users/", user_id))
  
  response <- GET(
    url,
    add_headers(
      Authorization = paste("Bearer", oauth_token),
      "Content-Type" = "application/json"
    )
  )
  
  print(paste("User check status:", status_code(response)))
  
  if (status_code(response) == 200) {
    user_data <- content(response, "parsed")
    print(paste("User found:", user_data$email))
    return(TRUE)
  } else {
    print("User not found or error in user check")
    return(FALSE)
  }
}

# Test the functions
oauth_token <- get_oauth_token(client_id, client_secret)

# First, check if the user exists
user_exists <- check_user(oauth_token, "kwocPxT1SKigKQYDRw9rzg")

if (user_exists) {
  user_meeting_list <- get_user_meetings(oauth_token, "kwocPxT1SKigKQYDRw9rzg", "2024-01-01", "2024-08-31")
  print(paste("Number of meetings returned:", length(user_meeting_list)))
} else {
  print("Please check the user ID and try again.")
}