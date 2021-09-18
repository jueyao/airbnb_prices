library("ggplot2")
library("dplyr")
library("tidyr")
library("maps")

airbnb_df <- read.csv("data/airbnb.csv", stringsAsFactors = FALSE)
rent_df <- read.csv("data/housing_rent_price.csv", stringsAsFactors = FALSE)
zipcodes_df <- read.csv("data/uszips.csv", stringsAsFactors = FALSE)

zipcodes_df$zip <- as.character(zipcodes_df$zip)
zipcodes_df <- rename(zipcodes_df, zipcode = zip) %>%
  select(zipcode,
         city,
         state_id,
         state_name)
zipcodes_df$location <- paste(zipcodes_df$city, zipcodes_df$state_id)

# cleaning airbnb dataset
airbnb_df <- airbnb_df %>%
  mutate(price = exp(log_price)) %>%            #change log price to price
  filter(room_type == "Entire home/apt") %>%    #select listings for whole unit only
  select(price,                                 #select relevant features
         property_type,
         first_review,
         bedrooms,
         zipcode,
         latitude,
         longitude) %>%
  inner_join(zipcodes_df, by = "zipcode") %>%   #join with zipcodes dataset for info about location
  filter(first_review != "")                    #filters out listings without dates
airbnb_df$first_review <- as.Date(airbnb_df$first_review, format = "%m/%d/%y")


######################
### Subsection 2.2 ###
######################
# Pick most populous cities in U.S.
top_30_cities <- rent_df %>%
  arrange(Population.Rank) %>%
  mutate(location = paste(City, State)) %>%
  head(30)

# Reformats MonthName.YYYY to YYYY-MM
reformat_date <- function(date) {
  year = substr(date, nchar(date) - 3, nchar(date))
  month = substr(date, 1, nchar(date) - 5)
  month_number <- as.character(match(month, month.name))
  if (nchar(month_number) < 2) {
    month_number <- paste("0", month_number, sep = "")
  }
  paste(year, "-", month_number, sep = "")
}


# Reformats XYYYY.MM to YYYY-MM
reformat_date2 <- function(date) {
  year <- substr(date, 2, nchar(date) - 3)
  month <- substr(date, nchar(date) - 1, nchar(date))
  return(paste0(year, "-", month))
}

# reformats YYYY-M to YYYY-MM
double_digit_month <- function(date) {
  year = substr(date, 1, 4)
  month = substr(date, 6, nchar(date))
  if (nchar(month) < 2) {
    month <- paste("0", month, sep = "")
  }
  paste(year, "-", month, sep = "")
}


# Select and rename rent_df columns
rent_df <- rent_df %>%
  mutate(location = paste(City, State)) %>%
  select(location, November.2010:January.2017)
col_names <- colnames(rent_df)
new_col_names <- c("location", sapply(col_names[-1], reformat_date, USE.NAMES = FALSE))
colnames(rent_df) <- new_col_names

# Select major cities present in both datasets, prep for joining
airbnb_median_prices <- as.data.frame(
  airbnb_df %>%
    mutate(date = substr(first_review, 1, 7), na.rm = TRUE) %>%
    group_by(date, location) %>%
    summarize(airbnb_nightly_median_price = median(price)) %>%
    filter(is.element(location, top_30_cities$location)) %>%
    mutate(airbnb_monthly_median_price = airbnb_nightly_median_price * 30)
)

# Count of Airbnb listings in major cities
airbnb_listings_count <- as.data.frame(
  airbnb_df %>%
    mutate(date = substr(first_review, 1, 7), na.rm = TRUE) %>%
    group_by(date, location) %>%
    summarize(airbnb_monthly_count = n()) %>%
    filter(is.element(location, top_30_cities$location))
)


# Median rent prices for U.S. cities		# Median rent prices for U.S. cities
median_rent_all_cities <- rent_df %>%	
  mutate(location = substr(location, nchar(location) - 1, nchar(location)))

  # Yearly average of median rent prices for all states		  group_by(location) %>%
median_rent_all_states <- as.data.frame(
  aggregate(median_rent_all_cities[, -1], 		
    list(median_rent_all_cities$location), 		
    mean,		
    na.rm = TRUE) %>%		
  rename(location = Group.1) %>%		
  gather(key = date,		
     value = median_monthly_rent,		
     -location) %>%		
  mutate(year = substr(date, 1, 4)) %>%		
  filter(year != "2010" & year != "2011") %>%		
  group_by(location, year) %>%		
  summarize(avg_median_rent = mean(median_monthly_rent))		
)


# Filter rent by major cities
major_cities_rent_df <- rent_df %>%
  filter(is.element(location, airbnb_median_prices$location)) %>%
  gather(key = date,
         value = median_rent,
         -location,
         na.rm = TRUE)

rent_airbnb_count_df <- major_cities_rent_df %>%
  inner_join(airbnb_listings_count, by = c("location", "date")) %>%
  mutate(date = substr(date, 1, 4))

# Joining datasets
airbnb_vs_rent <- inner_join(airbnb_median_prices, major_cities_rent_df, by = c("date", "location")) 

summary_stats_all_years <- as.data.frame(
  airbnb_vs_rent %>%
    mutate(year = substr(date, 1, 4)) %>%
    group_by(location, year) %>%
    summarize(annual_median_airbnb_per_night = median(airbnb_nightly_median_price),
              annual_median_airbnb_per_month = median(airbnb_monthly_median_price),
              annual_monthly_rent = median(median_rent))
)

# mutated for SHINY app
mean_median_airbnb_vs_rent <- as.data.frame(
  airbnb_vs_rent %>% 
    mutate(year = substr(airbnb_vs_rent$date, 1, 4), 
           month = substr(airbnb_vs_rent$date, 6, 7)) %>% 
    mutate(month = as.numeric(month)) %>%
    group_by(year, month) %>%
    summarize(monthly_average_median_rent = mean(median_rent),
              monthly_average_median_airbnb = mean(airbnb_monthly_median_price)) %>%
    mutate(date = paste(year, "-", as.character(month), sep = ""))
)

mean_median_airbnb_vs_rent$date <- sapply(mean_median_airbnb_vs_rent$date, 
                                          double_digit_month)

# yearly average median airbnb by state		
median_airbnb_all_states <- as.data.frame(		
  airbnb_df %>%		
    mutate(date = substr(first_review, 1, 7), na.rm = TRUE) %>%		
    group_by(date, location) %>%
  summarize(airbnb_nightly_median_price = median(price)) %>%
  mutate(airbnb_monthly_median_price = airbnb_nightly_median_price * 30)		
)
