# This file is for cleaning the user data
# Specifically cleaning the location column
# Needs to be run only once

library(maps)
library(stringr)

read.csv('user_metadata_en_version_2.1.csv') -> user_ds

# Set the pattern to look for
state.initials <- ", TX|, OH|, CA|, GA|, NY|, OR|, NM|, LA|, VA|, PA|, FL|, IA|, AK|, IN|, MA|, MI|, MD|, MN|, WI|, IL|, CO|, NC|, NJ|, AL|, WA|, ME|, AZ|, TN|, NE|, MT|, MS|, ND|, MO|, ID|, UT|, KY|, CT|, OK|, NV|, WY|, SC|, WV|, NH|, AR|, RI|, DE|, HI|, KS|, VT|, SD|, DC"

# Change to character as factors cannot be changed
as.character(user_ds$country_main_filter) -> user_ds$country_main_filter

# Search for pattern and assign country_main_filter as USA
for(i in 1:length(user_ds$country_main_filter)) {
  if (str_detect(user_ds$country_main_filter[i], state.initials)) {
    print(user_ds$country_main_filter[i])
    user_ds$country_final[i] <- "USA"
    user_ds$country_main_filter[i] <- ""
  }
}



str_detect(user_ds$country_main_filter[1], state.initials)


library(maps)

unique(us.cities$name)

state.initials <- character(length = 0L)
for (i in unique(us.states)) {
  #print(paste0(state.initials, ", ", i, "|"))  -> state.initials
  #print(i)
   print(paste0(state.initials, "|", i)) -> state.initials
  # print(state.initials)
  
  
}

# Read in the US states full names
read.csv('US_states.csv', header =  FALSE, as.is = FALSE) -> us.states
head(us.states)

us_state_vec <- "Wyoming|Wisconsin|West Virginia|Washington|Virginia|Vermont|Utah|Texas|Tennessee|South Dakota|South Carolina|Rhode Island|Pennsylvania|Oregon|Oklahoma|Ohio|North Dakota|North Carolina|New York|New Mexico|New Jersey|New Hampshire|Nevada|Nebraska|Montana|Missouri|Mississippi|Minnesota|Michigan|Massachusetts|Maryland|Maine|Louisiana|Kentucky|Kansas|Iowa|Indiana|Illinois|Idaho|Hawaii|Georgia|Florida|Delaware|Connecticut|Colorado|California|Arkansas|Arizona|Alaska|Alabama"
# for (i in us.states[,1]) {
#   print(paste0(i)) -> tempf
#   paste0(tempf, "|", us_state_vec) -> us_state_vec
# }

# If any of the country_main_filter contains US state name, set that country_final as USA
for (i in 1:length(user_ds$country_main_filter)) {
  if (str_detect(user_ds$country_main_filter[i],us_state_vec)) {
    user_ds$country_final[i] <- "USA"
    user_ds$country_main_filter[i] <- "null"
    }
}

write.csv(user_ds, file = "user_data_version_2.2.csv", row.names = FALSE)


# Cleaning for Australian cities
read.csv("worldcities.csv", stringsAsFactors = FALSE) -> world.cities

world.cities[world.cities$country == "Australia",] -> aus.cities
world.cities[world.cities$country == "Australia",] -> aus.cities
world.cities[world.cities$country == "United Kingdom",] -> uk.cities

for (i in 1:length(user_ds$country_main_filter)) {
  if (str_detect(user_ds$country_main_filter[i], paste(unique(uk.cities$admin_name), collapse = "|"))
      #&& str_detect(user_ds$country_main_filter[i], paste(unique(uk.cities$city_ascii), collapse = "|"))
      ) {
    print(user_ds$country_main_filter[i])
     # user_ds$country_final[i] <- "Australia"
     # user_ds$country_main_filter[i] <- "null"
  }
  
}





