# get_zoom_cloud_recording_data_chromote.R
# Instructions: 


library(tidyverse)
library(chromote)

# Create a new Chrome session
b <- ChromoteSession$new()

b <- ChromoteSession$new()
b$view()
b$go_to("https://us06web.zoom.us/recording/management")
b$Page$bringToFront()

cookies <- b$Network$getCookies()
str(cookies)
saveRDS(cookies, "cookies.rds")


b$screenshot()
b$close()

