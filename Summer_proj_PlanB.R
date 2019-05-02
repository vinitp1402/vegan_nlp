# Once the tweets are collected using the web scraper, this function shall do the basic data cleaning on tweets

#### Install and Load Libs necessary to access twitter ----
    # This is comment for Logic B
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

#### Function Declaration ----

    # function to remove special characters
    removeSpecialChars <- function(x) {
      gsub("[^a-zA-Z0-9 ]", " ", x)
    }
    
    # function to remove hyperlinks
    removeUrl <- function(sentence) {
      gsub("(http[^ ]*)", "", sentence)
    }
    
    library(stringr) # for str_count
    
    # function to count urls
    countUrl <- function(sentence) {
      str_count(sentence, "http[^ ]*")
    }
    
    # function to count hastags
    countHashtag <- function(sentence) {
      str_count(sentence, "#[^ ]*")
    }
    
    # function to get user information
    get_user_data <- function(csv_ds) {
      # Get more information about the users
      usernames <-
        csv_ds$handle #name has strange characters. Better option is to
      # search using the handle variable. Only need to remove the @ char in handle
      as.character(usernames) -> usernames
      gsub("@", "", usernames) -> usernames
      unique(usernames) -> usernames
      
      # Creating structure of data frame for storing user metadata
      twListToDF(lookupUsers("vinitp1402")) -> user_metadata
      user_metadata[-1, ] -> user_metadata
      
      for (j in 1:length(usernames)) {
        user <- usernames[j]
        
        print(paste0("Processing ", j, " of ", length(usernames), ":", user))
        # Get data for each user and store in a temp data frame
        twListToDF(lookupUsers(user)) -> new_df
        # Append the new data to the user_metadata data frame
        rbind(user_metadata, new_df) -> user_metadata
        
      }
      print("Done extracting all user data...")
      
      return(user_metadata)
    }
    
    # function for finding top words associated with the highest sentiment
    
    find_freq_words <- function(x, y) {
      # x = tidy data frame
      # y = sentiment
      inner_join(x, lex_nrc, by = c("term" = "word")) %>%
        filter(sentiment == y) %>%
        count(term, sort = TRUE) %>%
        top_n(n = 5) %>%
        arrange(desc(n)) -> tempp_tibble
      
      unlist(tempp_tibble$term) %>%
        paste(collapse = " ")
      
    }
    
    
#### Execution

# Read source files ----
    setwd("C:/Users/vinit/Summer Project (Vegan)/src_files_proj")
    read.csv("csv_files/all_tweets_3.csv", stringsAsFactors = FALSE) -> tweets_ds
    read.csv("csv_files/user_ds_cleaned.v.3.0.csv", stringsAsFactors = FALSE) -> user_ds
    
# library required to perform join operation to join user data with tweets data
    if (!requireNamespace('sqldf')) {
      install.packages('sqldf')
    }
    library('sqldf')
  
    # Remove the '@' in handle variable to perform a join with user data df
    # tweets_ds$handle <- gsub("@", "", tweets_ds$handle) # no need as I removed the '@' symbol during the scraping process
   
    # Remove useless columns
    user_ds$url <- NULL
    user_ds$name <- NULL
    user_ds$protected <- NULL
    user_ds$country_main_filter <- NULL
    user_ds$id <- NULL
    user_ds$listedCount <- NULL
    user_ds$followRequestSent <- NULL
    user_ds$profileImageUrl <- NULL
    
    # Rename columns to more meaningful names
    names(user_ds)[names(user_ds)=="Description"] <- "user_description"
    names(user_ds)[names(user_ds)=="created"] <- "account_created"
    names(user_ds)[names(user_ds)=="screenName"] <- "handle"
    names(user_ds)[names(user_ds)=="statusesCount"] <- "total_status_counts"
    names(user_ds)[names(user_ds)=="followersCount"] <- "total_followers"
    names(user_ds)[names(user_ds)=="favoritesCount"] <- "total_favorites"
    names(user_ds)[names(user_ds)=="friendsCount"] <- "total_friends"
    
    # Export this to csv (for client presentation)
    write.csv(user_ds, file = "user_ds_cleaned.v.3.0.csv", row.names = FALSE)

# Cleaning of data before processing the content ----
    tweets_ds$handle <- gsub("@", "", tweets_ds$handle)
    
    # for counting hastags, add a new column
    tweets_ds$no_of_hashtags <- 0
        for (i in 1:length(tweets_ds$content)) {
      tweets_ds$no_of_hashtags[i] <- countHashtag(tweets_ds$content[i])
    }
    
    # for counting urls, add a new column
    tweets_ds$no_of_urls <- 0
        for (i in 1:length(tweets_ds$content)) {
      tweets_ds$no_of_urls[i] <- countUrl(tweets_ds$content[i])
    }
    
    # remove urls from tweets
    tweets_ds$content <- sapply(tweets_ds$content, removeUrl)
    
    # remove special characters
    tweets_ds$content <- sapply(tweets_ds$content, removeSpecialChars)
    
    # convert all tweets to lower case
    tweets_ds$content <- sapply(tweets_ds$content, tolower)

    # joining the  two data frames    
    merge(tweets_ds, user_ds, by = "handle") -> tweets_ds

#### Sentiment Analysis ----

    if (!requireNamespace('tm')) {
      # for Corpus() function
      install.packages('tm')
    }
    library('tm')
    
    # Make a corpus
    tweets_ds_au <- subset(tweets_ds, country_final == "Australia")
    aus_corpus <- Corpus(VectorSource(tweets_ds_au$content))
    
    # Convert to DTM
    aus_dtm <- DocumentTermMatrix(aus_corpus)
    
    # prepare a tidy data frame
    # tidy data frame lists every word with their frequency of occurence for every line
    library(tidytext)
    tidy(aus_dtm) -> aus_tidy_dtm
    
    # Get the three lexicons
    get_sentiments("nrc") -> lex_nrc
    # get_sentiments("bing") -> lex_bing
    # get_sentiments("afinn") -> lex_afinn

    library(dplyr)

# arranging the sentiments
inner_join(aus_tidy_dtm, lex_nrc, by = c("term" = "word")) %>%
  # group_by(document) %>%
  count(sentiment, sort = TRUE) -> nrc_sentiments_aus

#### Find top keywords associated with each sentiment and store in dataframe
    # Add a new column top words
    nrc_sentiments_aus$top_words <- c("")
    for (i in 1:length(nrc_sentiments_aus$sentiment)) {
      find_freq_words(aus_tidy_dtm, nrc_sentiments_aus$sentiment[i]) -> nrc_sentiments_aus$top_words[i]
    }

# Export files necessary to explain client (only for 2019-02-05) ----
        write.csv(nrc_sentiments_aus, file = "sentiments_australia.csv", row.names = FALSE)
    
    
     
       
