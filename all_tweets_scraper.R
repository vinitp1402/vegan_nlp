# This file is to collect tweets from twitter
# The technique used is web scraping using the libarary RSelenium and rvest


# The scroll function shall keep scrolling until no further content is loaded
# This shall work for any website that has no pagination and infinte scrolling
scroll <- function(start_date_url, end_date_url) {
  # Keep scrolling down page, loading new content each time.
  last_height = 0 #
  repeat {
    remDr$executeScript("window.scrollTo(0,document.body.scrollHeight);")
    Sys.sleep(3) #delay by 3sec to give chance to load.
    
    # Updated if statement which breaks if we can't scroll further
    new_height = remDr$executeScript("return document.body.scrollHeight")
    if (unlist(last_height) == unlist(new_height)) {
      print("scroll:: Reached end of page")
      break
    } else {
      last_height = new_height
      print(
        paste0(
          "scroll:: Scrolling down. Start date:",
          start_date_url,
          " End date: ",
          end_date_url
        )
      )
    }
  }
  
  
}

# The scraper function does the task of scraping the elements from the web page
# This function needs to be redefined for every web site as the html contents would be different for each website
# This one is designed for twitter
scraper <- function(page_source) {
  # call scraper function to scrape tweets and return temp_df
  
  print("scrape:: Started scraping.")
  
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
    # html_nodes("username u-dir u-textTruncate") %>%
    # html_nodes(xpath = '//*[@class="username u-dir u-textTruncate"]/b') %>%
    html_nodes(xpath = '//*[@class="FullNameGroup"]//following-sibling::span/b') %>%
    html_text() %>%
    # Convert to character
    as.character() -> handle
  
  # Scrape username
  read_html(page_source[[1]]) %>%
    # html_nodes(".show-popup-with-id") %>%
    html_nodes(xpath = '//*[@class="FullNameGroup"]/strong') %>%
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
    user_name,
    content,
    tweet_date,
    tweet_replies,
    tweet_retweets,
    tweet_favourites
  ) -> temp_df
  
  print(paste0("scrape:: Completed scraping. Total tweets: ", dim(temp_df)[1]))
  
  return(temp_df)
  
}


#### Start Execution here ----
library(RSelenium)
library(rvest)
library(lubridate)
rD = rsDriver(browser = "chrome")
remDr = rD[["client"]]
remDr$close()

# Change values for different type of search
# Date format needs to be yyyy-mm-dd
hashtag = "vegan"
since = "2011-01-01"
until = "2018-12-31"

start_date_url <- as.Date(since)
# Calculating last day of the month
end_date_url <- as.Date(since) + months(1) - 1
continue_flag <- TRUE

# Create an empty df that shall store scraped data
data.frame(
  handle = character(),
  username = character(),
  content = character(),
  tweet_date = character(),
  tweet_replies = integer(),
  tweet_retweets = integer(),
  tweet_favourites = integer()
) -> scraped_df

while (continue_flag) {
  # end_date_url <- as.Date(until)
  
  # Prepare the dates that can be used in the search url
  # since_str <- format(since, format = "%d %b %Y")
  until_str <-
    format(start_date_url + months(1) - 1, format = "%d %b %Y")
  
  # Prepare search string
  search_url <-
    paste0(
      "https://twitter.com/search?l=en&q=%23",
      hashtag,
      "%20since%3A",
      start_date_url,
      "%20until%3A",
      end_date_url,
      "&src=typd"
    )
  
  print(paste0("main:: search url: ", search_url))
  
  # Open the browser
  remDr$open()
  
  #  This line is equivalent to the action of entering the url and hitting enter
  remDr$navigate(search_url)
  
  # Scroll to the end of the page using the scroll function defined above
  scroll(start_date_url, end_date_url)
  
  # Once reached the end, capture the web page
  page_source <- remDr$getPageSource()
  
  # Scrape the required html elements using the scraper function defined above
  scraper(page_source) -> temp_df
  
  # Append the scraped tweets to previously scraped tweets
  rbind(scraped_df, temp_df) -> scraped_df
  
  # Close the browser
  remDr$close()
  
  # Write the output file
  # Incase loop brekas, you always have the data scraped till this step
  write.csv(scraped_df, file = "sracped_df.csv", row.names = FALSE)
  
  if (end_date_url == until) {
    continue_flag <- FALSE
    print("main loop:: until date reached")
    print(paste0("Total tweets collected: ", dim(scraped_df)[1]))
  } else{
    # increment the start and end dates to make while loop work
    start_date_url <- start_date_url + months(1)
    end_date_url <- start_date_url + months(1) - 1
    
    print(
      paste0(
        "main:: Continuing scraping. New Start date:",
        start_date_url,
        "New End date:",
        end_date_url
      )
    )
  }
  
  
}
