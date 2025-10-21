# Claude 2

library(httr2)
library(jsonlite)
library(lubridate)
library(dplyr)

# Function to get Zoom API token (you'll need to implement this based on your auth method)

client_id <- Sys.getenv("zoom_client_id")
client_secret <- Sys.getenv("zoom_client_secret")
account_id <- Sys.getenv("zoom_account_id")

# Zoom OAuth endpoints
auth_endpoint <- "https://zoom.us/oauth/authorize"
token_endpoint <- "https://zoom.us/oauth/token"

# Function to get OAuth token
get_zoom_token <- function(client_id, client_secret) {
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


# Function to get all users
get_zoom_users <- function(token) {
  response <- request("https://api.zoom.us/v2/users") %>%
    req_headers(Authorization = paste("Bearer", token)) %>%
    req_url_path_append(user_id) %>%
    req_headers(Authorization = paste("Bearer", token)) %>%
    # req_params(
    #   from = start_date,
    #   to = end_date,
    #   page_size = 300  # Adjust as needed
    # ) %>%
    req_perform()

  users <- response %>% resp_body_json()
  return(users$users)
}

users <- get_zoom_users(token)

users <- tibble(
  id = map(user_list, "id"),
  display_name = map(user_list, "display_name"),
  email = map(user_list, "email"),
  type = map(user_list, "type"),
  timezone = map(user_list, "timezone"),
  dept = map(user_list, "dept"),
  last_login_time = map(user_list, "last_login_time")
) |>
  unnest(
    cols = c(id, display_name, email, type, timezone, dept, last_login_time)
  ) |>
  mutate(last_login_time = lubridate::ymd_hms(last_login_time))

# so far so good

# Function to get meeting counts for a user
get_user_meeting_count <- function(token, user_id) {
  # get_user_meeting_count <- function(token, user_id, start_date, end_date) {
  response <- request("https://api.zoom.us/v2/report/users/") %>%
    # response <- request("https://api.zoom.us/v2/report/users/{user_id}/meetings") %>%
    req_url_path_append(user_id) %>%
    req_url_path_append("meetings") %>%
    req_headers(Authorization = paste("Bearer", token)) %>%
    # req_options(
    #   from = start_date,
    #   to = end_date,
    #   page_size = 300  # Adjust as needed
    # ) %>%
    req_perform()

  meetings <- response %>% resp_body_json()
  return(list(user_id = user_id, count = length(meetings$meetings)))
}
start_date <- floor_date(Sys.Date(), "month")
end_date <- Sys.Date()

one_user <- get_user_meeting_count(
  oauth_token,
  user_id = "emx8MtweQdq9zkH1p7trlA"
)
# one_user <- get_user_meeting_count(oauth_token, user_id = "emx8MtweQdq9zkH1p7trlA", start_date, end_date)

# Main function
get_all_user_meeting_counts <- function() {
  token <- get_zoom_token()
  # users <- get_zoom_users(token)

  end_date <- Sys.Date()
  start_date <- end_date - months(1)

  results <- lapply(users_list$id, function(user_id) {
    get_user_meeting_count(token, user_id, start_date, end_date)
  })

  results_df <- do.call(rbind, lapply(results, as.data.frame))

  return(
    results_df %>% left_join(as.data.frame(users), by = c("user_id" = "id"))
  )
}

# Run the main function
meeting_counts <- get_all_user_meeting_counts()
print(meeting_counts)
