#### Install and Load Libs necessary to access twitter ----
# Thhis is comment for LOgic B
    if(!requireNamespace("twitteR")){
      install.packages("twitteR")
      library(twitteR)
    } else
    library(twitteR)
    
    if(!requireNamespace("ROAuth")){
      install.packages("ROAuth")
    }
    library(ROAuth)
    
    if(!requireNamespace("httr")){
      install.packages("httr")
    }
    library(httr)

# Set API Keys
    auth_tw <- function(){
    api_key <- "nwfZNhjGEgkR72YHVYrPUZnX9"
    api_secret <- "Z4vVmEs6JqyiFe8BvTaTBAN3sQShVCbjMj3mcpwcqtCKGhuYhQ"
    access_token <- "111242412-bJYOp0m8i8M5YqXI4n2CKpYJEv2sPc0FYiUuQ7Vg"
    access_token_secret <- "zzuihiZgfJGWJOkm4GVBOlUdcsPrvwYNN1Oij4Mpo3lmV"
    setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)
    }
    auth_tw()

#### Function Declaration ----   
    
    # function to remove special characters
    removeSpecialChars <- function(x)
      gsub("[^a-zA-Z0-9 ]", " ", x)
    
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
      user_metadata[-1,] -> user_metadata
      
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
    
    find_freq_words <- function(x) {
      # x = tidy data frame
      inner_join(x, lex_nrc, by = c("term" = "word")) %>%
        filter(sentiment == x) %>%
        count(term, sort = TRUE) %>%
        top_n(n = 5) %>%
        arrange(desc(n)) -> tempp_tibble
      
      unlist(tempp_tibble$term) %>%
        paste(collapse = " ")
      
    }

# Execution
    
    # Read source files 
    setwd("C:/Users/vinit/Summer Project (Vegan)/csv_files")
        read.csv("2011.csv") -> ds1
  
    # Collect user's data      
    get_user_data(ds1) -> user_ds1

# Cleaning of data before processing the content ----
        ds1$handle <- gsub("@", "", ds1$handle)
        
        # counting hastags
        # first add a new column
        ds1$no_of_hashtags <- 0
        
        for (i in 1:length(ds1$content)) {
          ds1$no_of_hashtags[i] <- countHashtag(ds1$content[i])
        }
        
        # counting urls
        # first add a new column
        ds1$no_of_urls <- 0
        
        for (i in 1:length(ds1$content)) {
          ds1$no_of_urls[i] <- countUrl(ds1$content[i])
        }
        
        # remove urls
        ds1$content <- sapply(ds1$content, removeUrl)
        
        # remove special characters
        ds1$content <- sapply(ds1$content, removeSpecialChars)
        
        # convert all tweets to lower case
        ds1$content <- sapply(ds1$content, tolower)
    
#### Sentiment Analysis ----

  if(!requireNamespace('tm')){ # for Corpus() function
    install.packages('tm')
  }
    library('tm')
        
# Make a corpus
    aus_corpus <- Corpus(VectorSource(ds1$content))

# Convert to DTM
    aus_dtm <- DocumentTermMatrix(aus_corpus)

# prepare a tidy data frame
# tidy data frame lists eveyr word with their frequency of occurence for every line
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

# Find top keywords associated with each sentiment and store in dataframe
  # Add a new column top words
    nrc_sentiments_aus$top_words <- c("")
    for (i in 1:length(nrc_sentiments_aus$sentiment)) {
      
       find_freq_words(nrc_sentiments_aus$sentiment[i]) -> nrc_sentiments_aus$top_words[i]
    }

inner_join(aus_tidy_dtm, lex_nrc, by = c("term" = "word")) %>%
  # group_by(document) %>%
  filter(sentiment == "positive") %>%
  count(term) %>%
  top_n(n = 5, wt = n) %>%
  arrange(desc(n))

inner_join(aus_tidy_dtm, lex_bing, by = c("term" = "word")) %>%
  # group_by(document) %>%
  filter(sentiment == "positive") %>%
  count(term) %>%
  top_n(n = 10, wt = n) %>%
  arrange(desc(n))

library(qdap)
polarity(ds1$content, ds1$city)

















get_user_data <- function(csv_ds) {

  # Get more information about the users
  usernames <- ds1$handle 
  # search using the handle variable. Only need to remove the @ char in handle
  as.character(usernames) -> usernames
  gsub("@", "", usernames) -> usernames
  unique(usernames) -> usernames
  
  
  # Creating structure of data frame for storing user metadata
  twListToDF(lookupUsers("vinitp1402")) -> user_metadata
  user_metadata[-1,] -> user_metadata
  
  for (j in 1:length(usernames)) {
    user <- usernames[j]

    print(paste0("Processing ",j, " out of ", length(usernames), " : ", user))
    
     twListToDF(lookupUsers(user)) -> new_df
     rbind(user_metadata, new_df) -> user_metadata
}
}