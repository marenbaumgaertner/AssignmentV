
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
german_venue <- GET(url = "https://app.ticketmaster.com/discovery/v2/venues?",
                    query = list(apikey = key,
                                 countryCode = "DE")) 
status_code(german_venue)

# extract content from response
german_venue_content <- content(german_venue, as = "text")

# build an r-dataframe 
venue_data = data.frame(fromJSON(german_venue_content)[[1]][[1]]) %>%
  #select colums
  select("name", "city", "postalCode", "address", "url", "location")


