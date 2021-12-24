### Data Cleaning Codebook
## EDA Long Form
# Anna Wagman

#load packages
library(tidyverse)
library(janitor)
library(skimr)
library(ggplot2)
library(RColorBrewer)
library(lubridate)
library(cowplot)
library(viridis)
library(plyr)
library(purrr)
library(dbplyr)
library(ggmap)


#load data
hulu_clean <- read_csv("data/processed/hulu_titles.csv")
disney_clean <- read_csv("data/processed/disney_plus_titles.csv")
amazon_clean <- read_csv("data/processed/amazon_prime_titles.csv")

## Visualization 1: Total TV Shows and Movies
#calculate total movies + tv shows on each service
#Hulu Movies + TV show
Hulu_MoviesvTV <- count(hulu_clean, vars = "type")

#Hulu: total titles 
Hulu_Total <- sum(Hulu_MoviesvTV$freq)
Hulu_Total

#Disney+ Movies + TV show 
Disney_MoviesvTV <- count(disney_clean, vars = "type")

#Disney+: total titles
Disney_Total <- sum(Disney_MoviesvTV$freq)
Disney_Total

#AmazonPrime Movies + TV shows
Amazon_MoviesvTV <- count(amazon_clean, vars = "type")

#AmazonPrime: total titles
Amazon_Total <- sum(Amazon_MoviesvTV$freq)
Amazon_Total

#joint tibble with each service and the number of titles on each
TOTAL_COMPARISON <- tibble(
  service = c("Hulu", "Disney+", "Amazon Prime"), 
  total = c(Hulu_Total, Disney_Total, Amazon_Total)
)


## Visualization 2: Movies vs TV Shows on each service
#calculate individual Movies and TV Shows per service
#Hulu Movies:
Hulu_Movies <- count(hulu_clean, vars = "type")
Hulu_Movies_n <- as.numeric(Hulu_Movies[1, 2]) #extract first 2 columns
Hulu_Movies_n

#Disney+ Movies:
Disney_Movies <- count(disney_clean, vars = "type")
Disney_Movies_n <- as.numeric(Disney_Movies[1, 2]) #extract first 2 columns
Disney_Movies_n

#AmazonPrime Movies
Amazon_Movies <- count(amazon_clean, vars = "type")
Amazon_Movies_n <- as.numeric(Amazon_Movies[1, 2]) #extract first 2 columns
Amazon_Movies_n


##create tibble of total movies on each
Total_Movies <- tibble(
  service = c("Hulu", "Disney+", "Amazon Prime"), 
  total = c(Hulu_Movies_n, Disney_Movies_n, Amazon_Movies_n) 
)


### Visualization 3: Movie Duration distribution
#parse duration to a number in minutes

#convert duration to minutes for each service

#hulu movie minutes
hulu_movie_length <- hulu_clean %>%
  select(type, duration, title) %>%
  filter(type == "Movie") %>% #filter by only movies
  drop_na() %>%
  #parse duration into an int:
  mutate(hulu_minutes = parse_number(duration)) #new var hulu_minutes


#amazon movie minutes
amazon_movie_length <- amazon_clean %>%
  select(type, duration, title) %>%
  filter(type == "Movie") %>% #filter by only movies
  mutate(amazon_minutes = parse_number(duration)) #parse duration into an int

#disney movie minutes
disney_movie_length <- disney_clean %>%
  select(type, duration, title) %>% 
  filter(type == "Movie") %>% #only movies
  drop_na() %>%
  mutate(disney_minutes = parse_number(duration)) #parse duration into an int



## 3b: Average (mean) of Movies 

#calculate the mean duration each of a movie on each in minutes

#hulu average movie length (min)
mean_hulu_movies <- mean(hulu_movie_length$hulu_minutes)

#amazon average movie length (min)
mean_amazon_movies <- mean(amazon_movie_length$amazon_minutes)

#disney average movie length (min)
mean_disney_movies <- mean(disney_movie_length$disney_minutes)

#create joint tibble of the mean_service for all three
Movie_Averages <- tibble(
  service = c("Hulu", "Disney+", "Amazon Prime"), 
  total = c(mean_hulu_movies, mean_disney_movies, mean_amazon_movies)
)

### Visualization 4: Titles Available on Multiple services
#select all titles per service
hulu_titles <- hulu_clean %>%
  select(title)

amazon_titles <- amazon_clean %>%
  select(title)

disney_titles <- disney_clean %>%
  select(title)

# use inner_join between each combination of 2 of the services
# inner_join will join by "title" and only return titles in both

#hulu and amazon titles compared
hulu_and_amazon <- inner_join(hulu_titles, amazon_titles) 

#hulu and disney
disney_and_hulu <- inner_join(hulu_titles, disney_titles) 

#amazon and disney
amazon_and_disney <- inner_join(amazon_titles, disney_titles) 

#overlap between all three services
total_total_overlap <- inner_join(hulu_and_amazon, disney_titles)


## Visualization 5: Worldwide Streaming
# clean data
amazon_countries <- read_csv("processed/amazon_prime_titles.csv")  %>%
  select(country) %>% ## select the column var = "country"
  mutate(country = str_split(country,', ')) %>% ##read in multiple countries separated by commas  
  unnest(country) %>%
  group_by(country) %>% ##group by country name
  count() %>% ##count how many titles in each country
  arrange(desc(freq)) %>% ##order highest to lowest
  remove_empty() %>%
  head(20) ##return highest 20

#remove the first row which is country = NA...
amazon_countries_removeNA <- amazon_countries[-1, ]


disney_countries <- read_csv("processed/disney_plus_titles.csv") %>%
  select(country) %>%
  mutate(country = str_split(country, ', ')) %>% ##read in multiple countries separated by commas 
  unnest(country) %>%
  group_by(country) %>% ##group by country
  count() %>% ##count titles per country
  arrange(desc(freq)) %>%
  head(20) ##select highest 20

#remove column country = NA
disney_removeNA <- disney_countries[-2, ]


## Visualization 6: Longest runnning TV Shows on each:
#Convert duration given by "# Season" where # is an integer and sort in descending order

# extract type = TV Show
# extract number from "2 Seasons"
# arrange descending, select top 10

##hulu tv seasons
hulu_seasons <- read_csv("data/processed/hulu_titles.csv") %>%
  filter(type == "TV Show") %>%
  select(title, duration) %>%
  mutate(duration = extract_numeric(duration)) %>% ##parse duration = "# Season" -> int; use extract_numeric() function
  arrange(desc(duration)) %>% ## arrange high -> low
  head(10) 

##disney+ tv seasons
disney_seasons <- read_csv("data/processed/disney_plus_titles.csv") %>%
  filter(type == "TV Show") %>%
  select(title, duration) %>%
  mutate(duration = extract_numeric(duration)) %>% ##parse duration = "# Season" -> int
  arrange(desc(duration)) %>%
  head(10)

##amazon tv seasons
amazon_seasons <- read_csv("data/processed/amazon_prime_titles.csv") %>%
  filter(type == "TV Show") %>%
  select(title, duration) %>%
  mutate(duration = extract_numeric(duration)) %>% ##parse duration = "# Season" -> int
  arrange(desc(duration)) %>%
  head(10)