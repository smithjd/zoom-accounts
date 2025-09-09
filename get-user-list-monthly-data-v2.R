#!/usr/lib64/R/bin/Rscript
#' ---
#' output: github_document
#' date: "1/18/2018"
#' ---
#' 
#' WARNING!! : renamed start_time to start_date_time on April 29, 2020!!
#' 

report_month <- "2024-05-15"

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
library(googlesheets4)
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lubridate))
library(zoomr)
library(gt)

user_list <- get_account_users(
  account_id = Sys.getenv("zoom_account_id"),
  client_id = Sys.getenv("zoom_client_id"), 
  client_secret = Sys.getenv("zoom_client_secret")) |> 
  mutate(users_last_login_time = ymd_hms(users_last_login_time),
         users_created_at = ymd_hms(users_created_at)
         )
glimpse(user_list)

#users_type
# 1 = basic
# 2 = licensed

user_list |> 
  # filter(users_type == 2, users_last_login_time < ymd_hms("2024-06-30T00:00:00")) |>
  filter(str_detect(users_email, "onlin")) |> 
  arrange(users_last_login_time) |> 
  # filter(users_last_login_time < ymd_hms("2022-06-30T00:00:00")) |> 
  select(users_display_name, users_email, users_last_login_time) |> 
  gt()


# names(user_list) <- str_replace_all(names(user_list), "_", " ")
# user_list <- user_list |> 
#   filter(`users type` == "2") 
#   ss <- gs4_create(
#     paste0('statistics as of ', Sys.Date()),
#     sheets = list('Zoom accounts' = user_list))
#   






  # gs4_browse(ss)
# webinar_details <- list_webinars(
#   # user_id = "kwocPxT1SKigKQYDRw9rzg",
#   # user_id = "r-ckQBbnT-CcOaMlOAIuOQ",
#   # user_id = "9083993948",
#   user_id = "kwocPxT1SKigKQYDRw9rzg",
#   account_id = Sys.getenv("zoom_account_id"),
#   client_id = Sys.getenv("zoom_client_id"),
#   client_secret = Sys.getenv("zoom_client_secret"))
# 
