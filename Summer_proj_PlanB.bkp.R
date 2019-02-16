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
    
    # function to scrape tweets and related metadata
    twitter_scraper <- function(search_url){
      library(RSelenium)
      library(rvest)
      #start RSelenium
      
      remoteDriver() -> remDr
      rD <- rsDriver(port=4444L, browser="chrome")
      
      
      remDr =rD[["client"]]
      # Open the browser
      # remDr$open()
      # Navigate to the oage you want. This is equivalent to entering url in browser and hitting enter
      remDr$navigate(search_url)
      
      # # Log into twitter. However, not necessary to login to scrape data as on 2019-02-07
      # 
      # mailid<-remDr$findElement(using = 'css',  "[class = 'text-input email-input js-signin-email']")
      # mailid$sendKeysToElement(list("myemail@gmail.com"))
      # 
      # # Enter password
      # 
      # password<-remDr$findElement(using = 'css', ".LoginForm-password .text-input")
      # password$sendKeysToElement(list("password"))
      # 
      # # Click Enter
      # 
      # login <- remDr$findElement(using = 'css',".js-submit")
      # login$clickElement()
      
      
      #scroll down 5 times, waiting for the page to load at each time
      # for(i in 1:100){      
      #   remDr$executeScript(paste("scroll(0,",i*10000,");"))
      #   Sys.sleep(3)    
      # }
      
      
      date <- "1 Jan 2019"
      i <- 1
      while (date != '1 Jan 2011') {
        remDr$executeScript(paste("scroll(0,",i*10000,");"))
        Sys.sleep(3)    
        page_source <- remDr$getPageSource()
        
        read_html(page_source[[1]]) %>%
          html_nodes(".js-short-timestamp") %>%
          html_text() %>%
          # Convert to character
          as.character() -> dates
        dates[length(dates)] -> date
        i <- i + 1
        
        print(paste0(date, "\n"))
      }
      page_source <- remDr$getPageSource()
      
      # Scrape the content
      read_html(page_source[[1]]) %>%
        html_nodes(".tweet-text") %>%
        html_text() %>%
        # Convert to character
        as.character() -> content
      
      # Scrape date of tweet
      read_html(page_source[[1]]) %>%
        html_nodes(".js-short-timestamp") %>%
        html_text() %>%
        # Convert to character
        as.character() -> tweet_date
      
      # Scrape handle
      read_html(page_source[[1]]) %>%
        html_nodes(".show-popup-with-id") %>%
        html_text() %>%
        # Convert to character
        as.character() -> handle
      
      # Scrape username
      read_html(page_source[[1]]) %>%
        html_nodes(".show-popup-with-id") %>%
        html_text() %>%
        # Convert to character
        as.character() -> user_name
      
      # Scrape number of replies
      read_html(page_source[[1]]) %>%
        html_nodes(xpath='//*[@class="ProfileTweet-actionButton js-actionButton js-actionReply"]/span/span') %>%
        html_text() %>%
        # Convert to character
        as.character() -> tweet_replies
      
      # Scrape number of retweets
      read_html(page_source[[1]]) %>%
        html_nodes(xpath='//*[@class="ProfileTweet-actionButton  js-actionButton js-actionRetweet"]/span/span') %>%
        html_text() %>%
        # Convert to character
        as.character() -> tweet_retweets
      
      # Scrape number of favourites
      read_html(page_source[[1]]) %>%
        html_nodes(xpath='//*[@class="ProfileTweet-actionButton js-actionButton js-actionFavorite"]/span/span') %>%
        html_text() %>%
        # Convert to character
        as.character() -> tweet_favourites
      
      # Close the server
      remDr$close()
      rD$server$stop()
      # Combine the columns to form a data frame
      cbind(handle, content, tweet_date, tweet_replies, tweet_retweets, tweet_favourites) -> scraped_df
      
      return(scraped_df)
    }

    
# Prepare the search url strings
    
    search_tweets_all <- "https://twitter.com/search?l=en&q=%23vegan%20since%3A2011-01-01%20until%3A2018-12-31&src=typd"
    
    twitter_scraper(search_tweets_all) -> tweets_all
    
#### Execution

# Read source files ----
    setwd("C:/Users/vinit/Summer Project (Vegan)/src_files_proj")
    read.csv("csv_files/2011.csv") -> "tweets_2011" #processed
    read.csv("csv_files/2012.csv") -> "tweets_2012"
    read.csv("csv_files/2013.csv") -> "tweets_2013" #processed
    read.csv("csv_files/2013.csv") -> "tweets_2014"
    read.csv("csv_files/2013.csv") -> "tweets_2015"
    read.csv("csv_files/2013.csv") -> "tweets_2016"
    read.csv("csv_files/2013.csv") -> "tweets_2017"
    read.csv("csv_files/2013.csv") -> "tweets_2018"

# Combine all the source files using the rbind function
    rbind(
      tweets_2011,
      tweets_2012,
      tweets_2013,
      tweets_2014,
      tweets_2015,
      tweets_2016,
      tweets_2017,
      tweets_2018
    ) -> tweets_master_df

# Collect user's data
    get_user_data(tweets_2011) -> user_2011_df
    get_user_data(tweets_2013) -> user_2013_df

# Merge user data with tweets data ----
    if (!requireNamespace('sqldf')) {
      install.packages('sqldf')
    }
    library('sqldf')
  
    # Remove the '@' in handle variable to perform a join with user data df
    tweets_2011$handle <- gsub("@", "", tweets_2011$handle)
    user_2011_df$handle <- rownames(user_2011_df)
    merge(tweets_2011, user_2011_df, by = "handle") -> tweets_2011
    
    # Export this to csv (for client presentation)
    write.csv(tweets_2011, file = "tweets_2011_master_table.csv", row.names = FALSE)

# Cleaning of data before processing the content ----

# Get database of all world cities
if (!requireNamespace("maps")) {
  install.packages("maps")
}
library("maps")
# use the dataset world.cities for cleaning location variable of user_ds

    tweets_2011$handle <- gsub("@", "", tweets_2011$handle)
    
    # counting hastags
    # first add a new column
    tweets_2011$no_of_hashtags <- 0
    
    for (i in 1:length(tweets_2011$content)) {
      tweets_2011$no_of_hashtags[i] <- countHashtag(tweets_2011$content[i])
    }
    
    # counting urls
    # first add a new column
    tweets_2011$no_of_urls <- 0
    
    for (i in 1:length(tweets_2011$content)) {
      tweets_2011$no_of_urls[i] <- countUrl(tweets_2011$content[i])
    }
    
    # remove urls
    tweets_2011$content <- sapply(tweets_2011$content, removeUrl)
    
    # remove special characters
    tweets_2011$content <- sapply(tweets_2011$content, removeSpecialChars)
    
    # convert all tweets to lower case
    tweets_2011$content <- sapply(tweets_2011$content, tolower)

#### Sentiment Analysis ----

    if (!requireNamespace('tm')) {
      # for Corpus() function
      install.packages('tm')
    }
    library('tm')
    
    # Make a corpus
    aus_corpus <- Corpus(VectorSource(tweets_2011$content))
    
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

#### Find top keywords associated with each sentiment and store in dataframe
    # Add a new column top words
    nrc_sentiments_aus$top_words <- c("")
    for (i in 1:length(nrc_sentiments_aus$sentiment)) {
      find_freq_words(aus_tidy_dtm, nrc_sentiments_aus$sentiment[i]) -> nrc_sentiments_aus$top_words[i]
    }


# Export files necessary to explain client (only for 2019-02-05) ----
        write.csv(nrc_sentiments_aus, file = "sentiments_australia.csv", row.names = FALSE)
        
        # Remove the unwanted columns before exporting to csv
        tweets_2011$web.scraper.order <- NULL
        tweets_2011$web.scraper.start.url <- NULL
        tweets_2011$name.x <- NULL
        tweets_2011$unix_timestamp <- NULL
        tweets_2011$url.x <- NULL
        tweets_2011$url.y <- NULL
        tweets_2011$name.y <- NULL
        tweets_2011$profileImageUrl <- NULL
        
        # Rename columns to more meaningful names
        names(tweets_2011)[names(tweets_2011)=="description"] <- "user_description"
        names(tweets_2011)[names(tweets_2011)=="created"] <- "account_created"
        names(tweets_2011)[names(tweets_2011)=="replies"] <- "tweet_replies"
        names(tweets_2011)[names(tweets_2011)=="retweets"] <- "tweet_retweets"
        names(tweets_2011)[names(tweets_2011)=="favorites"] <- "tweets_favorites"
        names(tweets_2011)[names(tweets_2011)=="statusesCount"] <- "total_status_counts"
        names(tweets_2011)[names(tweets_2011)=="followersCount"] <- "total_followers"
        names(tweets_2011)[names(tweets_2011)=="favoritesCount"] <- "total_favorites"
        names(tweets_2011)[names(tweets_2011)=="friendsCount"] <- "total_friends"
        
        write.csv(tweets_2011, file = "tweets_2011_master_table.csv", row.names = FALSE) 
