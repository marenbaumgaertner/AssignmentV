
# Load the required packeges
rm(list = ls())

if (!require("jsonlite")) install.packages("jsonlite")
if (!require("httr")) install.packages("httr")
if (!require("rlist")) install.packages("rlist")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("naniar")) install.packages("naniar")
if (!require("devtools"))install.packages("devtools")

library(jsonlite);library(httr);library(rlist);library(tidyverse);library(naniar);library(devtools)

# Ex 3

# set access to apikey
source("apikey.R")

# get api response
venue_res <- GET(url = 'https://app.ticketmaster.com/discovery/v2/venues?',
                 query = list(apikey = key,
                              countryCode = "DE",
                              locale = "*")) 
status_code(venue_res)

# extract content from response
venue_content <- content(venue_res, as = "text", encoding = "UTF-8")

# build a dataframe of the content
venue_df_p1 <- data.frame(fromJSON(venue_content, flatten = TRUE)[["_embedded"]][["venues"]]) %>%
  # select colums
  select("name", "city.name", "postalCode", "address.line1", "url", "location.longitude", "location.latitude")


# Ex 4

# get size of page, total number of elements and available pages in server from the json data
venue_json <- fromJSON(venue_content, flatten = TRUE)

n <- as.numeric(venue_json[[3]][["totalElements"]])
maxpage <- as.numeric(venue_json[[3]][["totalPages"]])
size <- as.numeric(venue_json[[3]][["size"]])


# Number of entries on the last incomplete page:
remainder <- n-size*floor(n/size)
print(remainder)

venue_data <- data.frame(
  name  = character(n),
  city   = character(n),
  postalCode = character(n),
  address   = character(n),
  url = character(n),
  longitude = double(n),
  latitude = double(n),
  stringsAsFactors = FALSE)



for (i in 1:maxpage) {
  res_venue <- GET(url = "https://app.ticketmaster.com/discovery/v2/venues/?",
                   query = list(apikey = key,
                                countryCode = "DE", 
                                locale = "*",
                                # start at page 0 as current page number counted from 0
                                page   = i-1))
  
  venue_content <- content(res_venue, as = "text", encoding = "UTF-8")
  
  venue_data[(size * i - (size-1)):(size * i),] <- data.frame(fromJSON(venue_content, flatten = TRUE)[[1]][[1]]) %>%
    #select colums
    select("name", "city.name", "postalCode", "address.line1", "url", "location.longitude", "location.latitude")
  
  # pause in loop
  Sys.sleep(0.5)
}
# loop stopped at i=15


# Ex. 5

# create subset of first observations to plot a map 
plot_data <- venue_data[1:280,]

# create numeric longitude and latitude
sapply(map_data, class)
plot_data[c("longitude", "latitude")] <- sapply(plot_data[c("longitude", "latitude")],as.numeric)

# drop rows with coordinates outside the given range
plot_data[which(plot_data$longitude %in% 5.866944:15.043611) & (plot_data$latitude %in% 47.271679:55.0846),]

# plot venues in a map of Germany
ggplot() +
  geom_polygon(
    aes(x = long, y = lat, group = group), 
    data = map_data("world", region = "Germany"),
    fill = "grey90",color = "black") +
  geom_point(aes(x = longitude, y = latitude),
             data = plot_data,
             color = "red",
             alpha = 1,
             size = 1,
             shape = 18)
  theme_void() +
  labs(title = "Event locations across Germany", caption = "Source: ticketmaster.com") +
  theme(title = element_text(size=8, face='bold'),
        plot.caption = element_text(face = "italic"))
