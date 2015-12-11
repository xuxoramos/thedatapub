# Clean environment
rm(list = ls())

# Meetup.com API key: 253275a5c18147e74c5b1e2a6708
meetupurl <-
  "https://api.meetup.com/2/profiles?&sign=true&photo-host=public&group_urlname=thedatapub&page=100&key=253275a5c18147e74c5b1e2a6708"

# Libraries
library(RCurl)
library(jsonlite)
library(dplyr)
library(tidyr)

# Get Meetup.com list of attendees.
# Result from jsonlite call is a list, from which only
# the 1st element is the actual dataframe. We extract that.
attendeesraw <-
  fromJSON(
    getURL(meetupurl), simplifyVector = T, flatten = T, simplifyDataFrame = T
  )[[1]]

# Build a new dataframe with just names and answers.
# Be aware that we cannot unlist and keep the same number of elements in the resulting vector
# because NULLs are eliminated. Instead, we should convert them to NA.
# Also, be aware that some scores have commas in them, so we're removing them from the start.
# Finally, we're converting each score from string to a numeric value.
attendees <- data.frame(
  name = attendeesraw$name,
  statistics = unlist(sapply(
    attendeesraw$answers,
    FUN = function(x) {
      elem <- x$answer[1]
      as.numeric(ifelse(is.null(elem), NA, gsub(',','', substr(elem, 1, 3))))
    }
  )),
  computerscience = unlist(sapply(
    attendeesraw$answers,
    FUN = function(x) {
      elem <- x$answer[2]
      as.numeric(ifelse(is.null(elem), NA, gsub(',','', substr(elem, 1, 3))))
    }
  )),
  softwaredevelopment = unlist(sapply(
    attendeesraw$answers,
    FUN = function(x) {
      elem <- x$answer[3]
      as.numeric(ifelse(is.null(elem), NA, gsub(',','', substr(elem, 1, 3))))
    }
  )),
  dataviz = unlist(sapply(
    attendeesraw$answers,
    FUN = function(x) {
      elem <- x$answer[4]
      as.numeric(ifelse(is.null(elem), NA, gsub(',','', substr(elem, 1, 3))))
    }
  )),
  business = unlist(sapply(
    attendeesraw$answers,
    FUN = function(x) {
      elem <- x$answer[5]
      as.numeric(ifelse(is.null(elem), NA, gsub(',','', substr(elem, 1, 3))))
    }
  )),
  stringsAsFactors = F
)

# Create another dataframe in long format.
attendeeslong <-
  gather(attendees, key = profileattribute, value = score,-name)


#Format plots
par(mfrow = c(3,4))
plot(attendees$statistics, attendees$computerscience)
plot(attendees$statistics, attendees$softwaredevelopment)
plot(attendees$statistics, attendees$dataviz)
plot(attendees$statistics, attendees$business)
plot(attendees$computerscience, attendees$softwaredevelopment)
plot(attendees$computerscience, attendees$dataviz)
plot(attendees$computerscience, attendees$business)
plot(attendees$softwaredevelopment, attendees$dataviz)
plot(attendees$softwaredevelopment, attendees$business)
plot(attendees$dataviz, attendees$business)

corrplot.mixed(rho, lower = "ellipse", upper = 'number', tl.pos = 'd')
