scroll <- function(search_url) {
  
  since_scroll <- since_date
  until_scroll <- until_date
  
  while (date != since_str + months(1)){
  
  # Scroll to end of page and wait
  remDr$executeScript(paste("scroll(0,",i*10000,");"))
  Sys.sleep(3)    
  page_source <- remDr$getPageSource()
  
  # Check if the page is loaded until the date put in search query
  read_html(page_source[[1]]) %>%
    html_nodes(".js-short-timestamp") %>%
    html_text() %>%
    # Convert to character
    as.character() -> dates
  dates[length(dates)] -> date
  i <- i + 1
  
  print(paste0(date, "\n"))

  }
}

scraper <- function(){
  # call scraper function to scrape tweets and return temp_df  
  
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
    html_nodes(xpath = '//*[@class="ProfileTweet-actionButton js-actionButton js-actionReply"]/span/span') %>%
    html_text() %>%
    # Convert to character
    as.character() -> tweet_replies
  
  # Scrape number of retweets
  read_html(page_source[[1]]) %>%
    html_nodes(xpath = '//*[@class="ProfileTweet-actionButton  js-actionButton js-actionRetweet"]/span/span') %>%
    html_text() %>%
    # Convert to character
    as.character() -> tweet_retweets
  
  # Scrape number of favourites
  read_html(page_source[[1]]) %>%
    html_nodes(xpath = '//*[@class="ProfileTweet-actionButton js-actionButton js-actionFavorite"]/span/span') %>%
    html_text() %>%
    # Convert to character
    as.character() -> tweet_favourites
  
  cbind(
    handle,
    username,
    content,
    tweet_date,
    tweet_replies,
    tweet_retweets,
    tweet_favourites
  ) -> temp_df
  
  return(temp_df)
  
}


main_loop <- function(hashtag, since, until){
  
  
  continue_flag <- TRUE
  while(continue_flag){
    
    since_date <- as.Date(since)
    until_date <- as.Date(until)
    
    # Prepare the dates that can be used in the search url
    since_str <- format(since, format = "%d %b %Y")
    until_str <- format(since + months(3), format = "%d %b %Y")
    
    # Prepare search string
    search_url <- paste0("https://twitter.com/search?l=en&q=%23", hashtag, "%20since%3A", 
                         since_date, "%20until%3A", until_date, "&src=typd")
    
    scroll(search_url)
    scrape()
    
    # Append temp_df to scraped_df
  }
  
}