# This file is to get user data
# It reads the handles from the twitter file and requests twitter for all the user information
# The resultant data frane is staored and exported as a csv file

#### Install and Load Libs necessary to access twitter ----

if (!requireNamespace("twitteR")) {
  install.packages("twitteR")
}
library(twitteR)

if (!requireNamespace("ROAuth")) {
  install.packages("ROAuth")
}
library(ROAuth)

if (!requireNamespace("httr")) {
  install.packages("httr")
}
library(httr)

# Set API Keys
auth_tw <- function() {
  api_key <- "nwfZNhjGEgkR72YHVYrPUZnX9"
  api_secret <- "Z4vVmEs6JqyiFe8BvTaTBAN3sQShVCbjMj3mcpwcqtCKGhuYhQ"
  access_token <-
    "111242412-bJYOp0m8i8M5YqXI4n2CKpYJEv2sPc0FYiUuQ7Vg"
  access_token_secret <-
    "zzuihiZgfJGWJOkm4GVBOlUdcsPrvwYNN1Oij4Mpo3lmV"
  setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)
}
auth_tw()

#### Execution ----

read.csv("csv_files/tweets_2011-18.csv") -> csv_ds

usernames <-
  csv_ds$handle #name has strange characters. Better option is to
# search using the handle variable. Only need to remove the @ char in handle
as.character(usernames) -> usernames
gsub("@", "", usernames) -> usernames
unique(usernames) -> usernames

# Creating structure of data frame for storing user metadata
twListToDF(lookupUsers(usernames)) -> user_metadata

