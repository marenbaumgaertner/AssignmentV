
# Load the required packeges
rm(list = ls())

if (!require("jsonlite")) install.packages("jsonlite")
if (!require("httr")) install.packages("httr")
if (!require("rlist")) install.packages("rlist")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("naniar")) install.packages("naniar")
if (!require("devtools"))install.packages("devtools")

library(jsonlite);library(httr);library(rlist);library(tidyverse);library(naniar);library(devtools)

# Exercise 3

# set access to apikey
source("apikey.R")

# Set country specific information
code = "DE"
country = "Germany"
coordinates <- matrix(c(47.271679, 55.0846, 5.866944, 15.043611), byrow = TRUE, nrow = 2)

# get api response
venue_res <- GET(url = 'https://app.ticketmaster.com/discovery/v2/venues?',
                 query = list(apikey = key,
                              countryCode = code,
                              locale = "*")) 
status_code(venue_res)

# extract content from response
venue_content <- content(venue_res, as = "text", encoding = "UTF-8")

# build a dataframe of the content
venue_df_p1 <- data.frame(fromJSON(venue_content, flatten = TRUE)[["_embedded"]][["venues"]]) %>%
  # select colums
  select("name", "city.name", "postalCode", "address.line1", "url", "location.longitude", "location.latitude")
glimpse(venue_df_p1)

# Exercise 4

# get size of page, total number of elements and available pages in server from the json data
venue_json <- fromJSON(venue_content, flatten = TRUE)

n <- as.numeric(venue_json[["page"]][["totalElements"]])

# Set size to the maximum size of 200
size = 200

# Compute maximum number of pages
maxpage <- floor(n/size)

# Number of entries on the last incomplete page:
remainder <- n-size*floor(n/size)
print(remainder)

# initiate a dataframe in the correct dimensions to speed up the loop
venue_data <- data.frame(
  name  = character(n),
  city   = character(n),
  postalCode = character(n),
  address   = character(n),
  url = character(n),
  longitude = character(n),
  latitude = character(n),
  stringsAsFactors = FALSE)

# loop over pages
for (i in 1:(maxpage)) {
  res_venue <- GET(url = "https://app.ticketmaster.com/discovery/v2/venues/?",
                   query = list(apikey = key,
                                countryCode = code, 
                                locale = "*",
                                # start at page 0 as current page number counted from 0
                                page   = i-1,
                                size = 200))
  
  venue_content <- content(res_venue, as = "text", encoding = "UTF-8")
  
  # parse content to json 
  venue_json <- fromJSON(venue_content, flatten = TRUE)[["_embedded"]][["venues"]]
  
  # Replace column by "NA" if it does not exists on each page
  venue_json$name[is.null(venue_json$name)] <- NA
  venue_json$city.name[is.null(venue_json$city.name)] <- NA
  venue_json$postalCode[is.null(venue_json$postalCode)] <- NA
  venue_json$address.line1[is.null(venue_json$address.line1)] <- NA
  venue_json$url[is.null(venue_json$url)] <- NA
  venue_json$location.longitude[is.null(venue_json$location.longitude)] <- NA
  venue_json$location.latitude[is.null(venue_json$location.latitude)] <- NA
  
  venue_data[(size * i - (size-1)):(size * i),] <- data.frame(venue_json) %>%
    #select colums
    select("name", "city.name", "postalCode", "address.line1", "url", "location.longitude", "location.latitude")
  
  # pause in loop
  Sys.sleep(0.5)
}

# Last Page
res_venue <- GET(url = "https://app.ticketmaster.com/discovery/v2/venues/?",
                 query = list(apikey = key,
                              countryCode = code, 
                              locale = "*",
                              # start at page 0 as current page number counted from 0
                              page   = i,
                              size = remainder))

venue_content <- content(res_venue, as = "text", encoding = "UTF-8")

# parse content to json 
venue_json <- fromJSON(venue_content, flatten = TRUE)[["_embedded"]][["venues"]]

# Replace column by "NA" if it does not exists on each page
venue_json$name[is.null(venue_json$name)] <- NA
venue_json$city.name[is.null(venue_json$city.name)] <- NA
venue_json$postalCode[is.null(venue_json$postalCode)] <- NA
venue_json$address.line1[is.null(venue_json$address.line1)] <- NA
venue_json$url[is.null(venue_json$url)] <- NA
venue_json$location.longitude[is.null(venue_json$location.longitude)] <- NA
venue_json$location.latitude[is.null(venue_json$location.latitude)] <- NA

last_page <- data.frame(venue_json) %>%
  #select colums
  select("name", "city.name", "postalCode", "address.line1", "url", "location.longitude", "location.latitude")

venue_data[(size * (i+1) - (size-1)):n,] <- last_page


# Exercise 5
plot_data <- venue_data

# Put coordinates into the correct class
sapply(plot_data, class)
plot_data[c("longitude", "latitude")] <- sapply(plot_data[c("longitude", "latitude")],as.numeric)

# drop rows with coordinates outside the given range
plot_data <-
  subset(plot_data, latitude > coordinates[1,1] & latitude < coordinates[1,2])
plot_data <-
  subset(plot_data, longitude > coordinates[2,1] & longitude < coordinates[2,2])

# plot venues in a map of Germany
ggplot() +
  geom_polygon(
    aes(x = long, y = lat, group = group), 
    data = map_data("world", region = country),
    fill = "#F9CCC3",color = "black",size = 0.1) +
  theme_void() + 
  coord_quickmap() +
  ggtitle(paste("Event locations across", country))+
  labs(caption = "Source: ticketmaster.com") +
  theme(title = element_text(size=8, face='bold'),
        plot.caption = element_text(face = "italic"))+
  geom_point(aes(x = longitude, y = latitude),
             data = plot_data,
             color = "#F32167",
             alpha = 1,
             size = 1,
             shape = 18)

# Exercise 6
#**************************LUXEMBOURG***********************************
# Repeat Ex 2. to Ex. 5 for 

# Set country specific information
code <- "LU"
country <- "Luxembourg"
coordinates <- matrix(cbind(49.447778, 50.182944, 5.733333, 6.533333), byrow = TRUE, nrow = 2)

# Repeat Exercise 3
# get api response
venue_res <- GET(url = 'https://app.ticketmaster.com/discovery/v2/venues?',
                 query = list(apikey = key,
                              countryCode = code,
                              locale = "*")) 
status_code(venue_res)

# extract content from response
venue_content <- content(venue_res, as = "text", encoding = "UTF-8")

# build a dataframe of the content
venue_df_p1 <- data.frame(fromJSON(venue_content, flatten = TRUE)[["_embedded"]][["venues"]]) %>%
  # select colums
  select("name", "city.name", "postalCode", "address.line1", "url", "location.longitude", "location.latitude")
tibble(venue_df_p1)

# Repeat Exercise 4

# get size of page, total number of elements and available pages in server from the data
venue_json <- fromJSON(venue_content, flatten = TRUE)

n <- as.numeric(venue_json[["page"]][["totalElements"]])

venue_data <- data.frame(
  name  = character(n),
  city   = character(n),
  postalCode = character(n),
  address   = character(n),
  url = character(n),
  longitude = character(n),
  latitude = character(n),
  stringsAsFactors = FALSE)

if (n > 200){
  # Set size to the maximum size of 200
  size = 200
  
  # Compute maximum number of pages
  maxpage <- floor(n/size)
  
  # Number of entries on the last incomplete page:
  remainder <- n-size*floor(n/size)
  print(remainder)
  
  
  for (i in 1:(maxpage)) {
    res_venue <- GET(url = "https://app.ticketmaster.com/discovery/v2/venues/?",
                     query = list(apikey = key,
                                  countryCode = code, 
                                  locale = "*",
                                  # start at page 0 as current page number counted from 0
                                  page   = i-1,
                                  size = 200))
    
    venue_content <- content(res_venue, as = "text", encoding = "UTF-8")
    
    # parse content to json 
    venue_json <- fromJSON(venue_content, flatten = TRUE)[["_embedded"]][["venues"]]
    
    # Replace column by "NA" if it does not exists on each page
    venue_json$name[is.null(venue_json$name)] <- NA
    venue_json$city.name[is.null(venue_json$city.name)] <- NA
    venue_json$postalCode[is.null(venue_json$postalCode)] <- NA
    venue_json$address.line1[is.null(venue_json$address.line1)] <- NA
    venue_json$url[is.null(venue_json$url)] <- NA
    venue_json$location.longitude[is.null(venue_json$location.longitude)] <- NA
    venue_json$location.latitude[is.null(venue_json$location.latitude)] <- NA
    
    
    venue_data[(size * i - (size-1)):(size * i),] <- data.frame(venue_json) %>%
      #select colums
      select("name", "city.name", "postalCode", "address.line1", "url", "location.longitude", "location.latitude")
    
    # pause in loop
    Sys.sleep(0.5)
  }
  
  # Last Page
  res_venue <- GET(url = "https://app.ticketmaster.com/discovery/v2/venues/?",
                   query = list(apikey = key,
                                countryCode = code, 
                                locale = "*",
                                page   = i,
                                size = remainder))
  
  venue_content <- content(res_venue, as = "text", encoding = "UTF-8")
  
  # parse content to json 
  venue_json <- fromJSON(venue_content, flatten = TRUE)[["_embedded"]][["venues"]]
  
  # Replace column by "NA" if it does not exists on each page
  venue_json$name[is.null(venue_json$name)] <- NA
  venue_json$city.name[is.null(venue_json$city.name)] <- NA
  venue_json$postalCode[is.null(venue_json$postalCode)] <- NA
  venue_json$address.line1[is.null(venue_json$address.line1)] <- NA
  venue_json$url[is.null(venue_json$url)] <- NA
  venue_json$location.longitude[is.null(venue_json$location.longitude)] <- NA
  venue_json$location.latitude[is.null(venue_json$location.latitude)] <- NA
  
  last_page <- data.frame(venue_json) %>%
    #select colums
    select("name", "city.name", "postalCode", "address.line1", "url", "location.longitude", "location.latitude")
  
  venue_data[(size * (i+1) - (size-1)):n,] <- last_page
} else {
  
  res_venue <- GET(url = "https://app.ticketmaster.com/discovery/v2/venues/?",
                   query = list(apikey = key,
                                countryCode = code, 
                                locale = "*",
                                page   = 0,
                                size = n))
  
  venue_content <- content(res_venue, as = "text", encoding = "UTF-8")
  
  # parse content to json 
  venue_json <- fromJSON(venue_content, flatten = TRUE)[["_embedded"]][["venues"]]
  
  # Replace column by "NA" if it does not exists on each page
  venue_json$name[is.null(venue_json$name)] <- NA
  venue_json$city.name[is.null(venue_json$city.name)] <- NA
  venue_json$postalCode[is.null(venue_json$postalCode)] <- NA
  venue_json$address.line1[is.null(venue_json$address.line1)] <- NA
  venue_json$url[is.null(venue_json$url)] <- NA
  venue_json$location.longitude[is.null(venue_json$location.longitude)] <- NA
  venue_json$location.latitude[is.null(venue_json$location.latitude)] <- NA
  
  venue_data[1:n,] <- data.frame(venue_json) %>%
    #select colums
    select("name", "city.name", "postalCode", "address.line1", "url", "location.longitude", "location.latitude")
}

# Repeat Exercise 5

# Put coordinates into the correct class
plot_data <- venue_data

sapply(plot_data, class)
plot_data[c("longitude", "latitude")] <- sapply(plot_data[c("longitude", "latitude")],as.numeric)


# drop rows with coordinates outside the given range
plot_data <-
  subset(plot_data, latitude > coordinates[1,1] & latitude < coordinates[1,2])
plot_data <-
  subset(plot_data, longitude > coordinates[2,1] & longitude < coordinates[2,2])

# plot venues in a map of Luxembourg
ggplot() +
  geom_polygon(
    aes(x = long, y = lat, group = group), 
    data = map_data("world", region = country),
    fill = "#F9CCC3",color = "black",size = 0.1) +
  theme_void() + 
  coord_quickmap() +
  ggtitle(paste("Event locations across", country))+
  labs(caption = "Source: ticketmaster.com") +
  theme(title = element_text(size=8, face='bold'),
        plot.caption = element_text(face = "italic"))+
  geom_point(aes(x = longitude, y = latitude),
             data = plot_data,
             color = "#F32167",
             alpha = 1,
             size = 1,
             shape = 18)
