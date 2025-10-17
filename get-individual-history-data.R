#!/usr/lib64/R/bin/Rscript
#' ---
#' output: github_document
#' date: "1/18/2018"
#' ---
#
# API documentation: https://zoom.github.io/api/#introduction
# API playground: https://developer.zoom.us/playground/
#
# The gmail authorization can be downloaded here: https://console.cloud.google.com/apis/credentials?project=so-emails
#
# Get history of one individual
#
# Check out recordings with this API end point: https://marketplace.zoom.us/docs/api-reference/zoom-api/cloud-recording/recordingslist

# Parameters:

account_email <- "cynthiamackay@mac.com"
# account_email <- "dixie@shambhalaonline.org"
month_start <- "2019-01-01"
month_end <- "2019-02-01"
# month_end <- "2019-12-31"

# Libraries
source("~/Rfunctions/setup_my_google.R")
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lubridate))
library(openssl)
library(jose)
suppressPackageStartupMessages(library(jsonlite))
library(curl)
library(RCurl)
library(googlesheets4)
library(here)
library(httpuv)

start_date <- floor_date(rollback(today()), "month")
end_date <- rollback(today())

claim <- jwt_claim(
  exp = as.numeric(Sys.time() + 3600),
  iss = Sys.getenv("zoom_api_key")
)

jwt <- jwt_encode_hmac(
  claim,
  secret = charToRaw(Sys.getenv("zoom_api_secret"))
)

account_email <- curlEscape(account_email)

get_past_meetings <- function(month, email_id) {
  # Get all the meetings a user has scheduled

  # curl https://api.zoom.us/v2/report/users/{userId}/meetings?from=string&to=string

  from_date <- as.Date.character(rollback(as.Date(month), roll_to_first = TRUE))
  to_date <- as.Date.character(ceiling_date(as.Date(month), "month") - 1)

  paste0(
    "https://api.zoom.us/v2/report/users/",
    email_id,
    "/meetings?from=",
    from_date,
    "&to=",
    to_date,
    "&page_size=60&page_number=1&access_token=",
    jwt
  )

  response <- curl_fetch_memory(
    paste0(
      "https://api.zoom.us/v2/report/users/",
      email_id,
      "/meetings?from=",
      from_date,
      "&to=",
      to_date,
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


date_list <- seq.Date(as.Date(month_start), as.Date(month_end), by = "month")

past_meetings <- date_list %>% map(~ get_past_meetings(.x, account_email))

past_meetings <- bind_rows(past_meetings)
# replace object with a very different one of the same name

past_meetings <- past_meetings %>%
  mutate(
    start_time = ymd_hms(start_time),
    end_time = ymd_hms(end_time),
    type = meeting_type_lookup[type],
    weekday = wday(start_time, label = TRUE, abbr = TRUE)
  ) %>%
  select(
    start_time,
    end_time,
    weekday,
    topic,
    host_email,
    duration,
    participants_count
  ) %>%
  arrange(start_time)

str(past_meetings)
# past_meetings

# write_rds(zoom_user_list, here("data", "zoom_user_list.Rds"))
write_rds(past_meetings, here("data", "past_meetings.Rds"))
