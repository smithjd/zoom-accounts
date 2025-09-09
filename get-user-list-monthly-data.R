#!/usr/lib64/R/bin/Rscript
#' ---
#' output: github_document
#' date: "1/18/2018"
#' ---
#' 
#' WARNING!! : renamed start_time to start_date_time on April 29, 2020!!
#' 

report_month <- "2023-06-15"

#
# API documentation: https://zoom.github.io/api/#introduction
# number of accounts: https://marketplace.zoom.us/docs/api-reference/zoom-api/accounts/accounts
# recordings: https://marketplace.zoom.us/docs/api-reference/zoom-api/cloud-recording/recordingslist
# plan storage usage documentation: https://marketplace.zoom.us/docs/api-reference/zoom-api/billing/getplanusage
# details on specific meetings: https://marketplace.zoom.us/docs/api-reference/zoom-api/meetings/meetings
# example: https://api.zoom.us/v2/accounts/{accountId}/plans/usage
# API playground: https://developer.zoom.us/playground/
#
# The gmail authorization can be downloaded here: https://console.cloud.google.com/apis/credentials?project=so-emails
# 
# Get meeting data for all meetings held in the previous complete month
#
# library(googlesheets4)
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lubridate))
library(openssl)
library(jose)
suppressPackageStartupMessages(library(jsonlite))
library(curl)
library(RCurl)
library(here)
library(httpuv)
library(zoomr)
report_month <- as_date(report_month)


start_date <- floor_date(report_month, "month")
# end_date <- rollback(report_month)
end_date <- ceiling_date(report_month, "month") - 1

claim <- jwt_claim(
  exp = as.numeric(Sys.time() + 3600),
  iss = Sys.getenv("zoom_client_id")
)

jwt <- jwt_encode_hmac(
  claim,
  secret = charToRaw(Sys.getenv("zoom_client_secret"))
)

get_user_list <- function() {
  # Get all the meetings a user has scheduled
  
  response <- curl_fetch_memory(
    paste0(
      "https://api.zoom.us/v2/users",
      "?page_size=60&page_number=1&access_token=",
      jwt
    )
  )
  # Assume that there are less than 60 users.
  
  response$status_code
  response_content <- fromJSON(rawToChar(response$content))
  users <- response_content$users
  # users$host_email <- URLdecode(email_id)
  users
}

zoom_user_list <- get_user_list() 

zoom_user_list <- zoom_user_list %>% 
  filter(type == 2)

str(zoom_user_list)


zoom_user_list <- zoom_user_list %>% 
  mutate(url_email_account = curlEscape(email))

meeting_type_lookup <- c(
  "1" = "Instant",
  "2" = "Scheduled",
  "3" = "Recurring, no fixed time",
  "8" = "Recurring, fixed time"
)


get_past_meetings <- function(email_id, from_date, to_date) {
  # Get all the meetings a user has scheduled

  # curl https://api.zoom.us/v2/report/users/{userId}/meetings?from=string&to=string

  response <- curl_fetch_memory(
    paste0(
      "https://api.zoom.us/v2/report/users/",
      email_id, "/meetings?from=", from_date, "&to=", to_date,
      "&page_size=60&page_number=1&access_token=",
      jwt
    )
  )
  # Assume that nobody has more than 60 meetings in the time period.

  response$status_code
  response_content <- fromJSON(rawToChar(response$content))
  meetings <- response_content$meetings
  meetings$host_email <- URLdecode(email_id)
  meetings
}

account_list <- zoom_user_list$url_email_account

past_meetings <- account_list %>%  map(~get_past_meetings(.x,
                         from_date = start_date,
                         to_date = end_date))

past_meetings <- bind_rows(past_meetings)
# replace object with a very different one of the same name


#  The start and end date times are in UTC time.
past_meetings <- past_meetings %>%
  mutate(
    start_time = ymd_hms(start_time),
    end_time = ymd_hms(end_time),
    start_time_only = hms::as_hms(format(start_time, "%H:%M:%S")),
    end_time_only = hms::as_hms(format(end_time, "%H:%M:%S")),
    type = meeting_type_lookup[type],
    weekday = wday(start_time, label = TRUE, abbr = TRUE)
  ) %>%
  select(start_time, end_time, id, weekday, topic, host_email, 
         dept, duration, participants_count, start_time_only, end_time_only, uuid) %>%
  arrange(start_time)

str(past_meetings)


write_rds(zoom_user_list, here("data", "zoom_user_list.Rds"))
write_rds(past_meetings, here("data", paste0("meetings_", year(report_month), 
                                             "-", month(report_month),  ".Rds")))
write_rds(past_meetings, here("data", "past_meetings.Rds"))

report_month

# ss <- gs4_create(
#   'Google sheet name',
#   sheets = list(sheet_name = local_df_name))
# 
# gs4_browse(ss)
# 
# write_sheet(another_df, ss = ss) # to add another sheet
