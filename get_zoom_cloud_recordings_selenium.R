# get_zoom_cloud_recordings_selenium.R
# Sys.setenv(JAVA_HOME = "/opt/homebrew/opt/openjdk@11")

# library(wdman)
# selenium(retcommand = TRUE)

library(RSelenium)

# Start RSelenium directly (this will download ChromeDriver automatically)
rD <- rsDriver(browser = "chrome", 
               port = 4545L, 
               chromever = "latest")

# If that works, you'll get the remote driver
remDr <- rD$client

# Test with a simple navigation
remDr$navigate("https://www.google.com")

# Close when done testing
remDr$close()
rD$server$stop()