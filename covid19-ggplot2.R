# 1. Loading Data and Checking Contents

# Loading tidyverse, magrittr (For pipes) and lubridate (For date ops.):
  
library("tidyverse")
library("magrittr")
library("lubridate")

# Downloading confirmed cases data from JHU repo and loading into R:
  

time_series_raw_confirmed <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

# Checking out the dimensions:
  
dim(time_series_raw_confirmed)

# Checking out the first few rows and columns:
  
as_tibble(time_series_raw_confirmed)


# Dataframe has 266 rows (One row for each country/state) and 106 columns (As of 04/05/2020, one column for each day, starting on January 22).

# 2. Cleaning Data

# Writing a function (There is a more complete and comprehensive paper written by Zhao Yancheng, inspired by / used some of the code in the paper while writing the function. Yanchang Zhao, COVID-19 Data Analysis with R – Worldwide. RDataMining.com, 2020. URL: http://www.rdatamining.com/docs/Coronavirus-data-analysis-world.pdf] to pivot the data into longer format and summarize by country (I might work on same data in the future or on deaths and recoveries, so, better to write a function):
  
tidydatafunction <- function(data) {
  data %<>% select(everything(), -'Province.State', -'Lat', -'Long') %>% rename(country = Country.Region) # Removing unnecessary columns and renaming one
  data %<>% pivot_longer(-country, names_to = "date", values_to = "count") # Pivoting data into longer format
  data %<>% mutate(date = date %>% substr(2,8)%>% mdy()) # Removing the "x"s and converting to dates
  data %<>% group_by(country, date) %>% summarise(accumulated_cases = sum(count, na.rm = TRUE)) %>% as.data.frame() # Group by country and summarise on case data
  return(data)
}

# Cleaning previously loaded data into new tidy dataset:

time_series_tidy_confirmed <- time_series_raw_confirmed %>% tidydatafunction()

# Checking out new, tidy dataset:

as_tibble(time_series_tidy_confirmed) %>%
  arrange(desc(accumulated_cases)) # Sorting desc. to see top numbers

# Adding daily cases as calculated field from accumulated cases and saving as a new data frame:

time_series_tidy_confirmed_with_daily <- time_series_tidy_confirmed %>%
  group_by(country) %>% # Group by country
  arrange(country, date) %>% # Sort asc. by country and then date
  mutate(daily_cases = c(0,diff(accumulated_cases))) # Calculate daily cases
time_series_tidy_confirmed_with_daily %>%
  arrange(desc(accumulated_cases)) # Sorting desc. to see top numbers

# Filtering Turkey, calculating daily cases from accumulated cases and saving as a new data frame:

turkey_time_series_tidy_confirmed_with_daily <- time_series_tidy_confirmed %>%
  filter(country == "Turkey") %>% # Filtering Turkey
  arrange(date) %>% # Sort asc. by date
  mutate(daily_cases = c(0,diff(accumulated_cases))) # Calculate daily cases
head(turkey_time_series_tidy_confirmed_with_daily %>%
       arrange(desc(accumulated_cases))) # Sorting desc. to see top numbers

# Listing top 9 countries in terms of total cases (Will come in handy later)

top_nine_countries <- time_series_tidy_confirmed_with_daily %>%
  filter(date == max(date)) %>% # For the most recent day in data
  group_by(country) %>% # Group by country
  summarise(total_case = sum(accumulated_cases, na.rm = TRUE)) %>% # Summarise on accumulated cases
  top_n(9, total_case) %>% # Select top 9 countries by total cases
  arrange(desc(total_case)) %>% # Sort desc. to see in proper order
  select(country) # Select only country names
top_nine_countries

#3. Plotting Some Graphs

# Let's see how Turkey is doing, in terms of new cases.

# Plotting daily case trend for Turkey, black line and dots respresent actual case numbers and blue line is regression line:

ggplot(data = turkey_time_series_tidy_confirmed_with_daily, mapping = aes(x = date, y = daily_cases)) + # Passed on data, mapping and variables to plot, wrote explicitly on purpose
  geom_line() + # Added line
  geom_point() + # Added points
  geom_smooth(se = FALSE) + # Added regression line, locally fitted with loess method 
  labs(y = "Daily Cases", x = "Date", title = "Daily Covid19 Cases in Turkey")  # Axis labels and main title added

# It seems that Turkey has reached the peak and now is on a downwards trend.

# Let's see how top 9 countries (In terms of accumulated cases) are doing.

# Plotting daily case trend for top 9 countries, black line and dots respresent actual case numbers and blue line is regression line:

ggplot(data = filter(time_series_tidy_confirmed_with_daily, country == as_vector(top_nine_countries)), mapping = aes(x = date, y = daily_cases)) + # Passed on data, mapping and variables to plot, wrote explicitly on purpose
  geom_line() + # Added line
  geom_point() + # Added points
  geom_smooth(se = FALSE) + # Added regression line, locally fitted with loess method 
  labs(y = "Daily Cases", x = "Date", title = "Daily Covid19 Cases in Top 9 Countries") + # Axis labels and main title added
  facet_wrap(country~., nrow = 3, scales = "free") 


# And situation about accumulated cases is as following:
  
# Plotting accumulated case trend top 9 countries, black line and dots respresent actual case numbers and blue line is regression line:

options(scipen = 999999) # To see numbers as is, not in scientific format 
ggplot(data = filter(time_series_tidy_confirmed_with_daily, country == as_vector(top_nine_countries)), mapping = aes(x = date, y = accumulated_cases)) + # Passed on data, mapping and variables to plot, wrote explicitly on purpose
  geom_line() + # Added line
  geom_point() + # Added points
  geom_smooth(se = FALSE) + # Added regression line, locally fitted with loess method 
  labs(y = "Accumulated Cases", x = "Date", title = "Accumulated Covid19 Cases in Top 9 Countries") + # Axis labels and main title added
  facet_grid(country~., scales = "free_y")
