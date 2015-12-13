# Clean environment
rm(list = ls())

# Meetup.com API key: 253275a5c18147e74c5b1e2a6708
profileapiurl <-
  "https://api.meetup.com/2/profiles?&sign=true&photo-host=public&group_urlname=thedatapub&page=100&key=253275a5c18147e74c5b1e2a6708"

memberapiurl <- 
  "https://api.meetup.com/2/members?&sign=true&group_urlname=thedatapub&&key=253275a5c18147e74c5b1e2a6708"

# Libraries
library(RCurl)
library(jsonlite)
library(dplyr)
library(tidyr)
library(corrplot)

# Get Meetup.com list of attendees.
# Result from jsonlite call is a list, from which only
# the 1st element is the actual dataframe. We extract that.
attendeesraw <-
  fromJSON(
    getURL(profileapiurl), simplifyVector = T, flatten = T, simplifyDataFrame = T
  )[[1]]

genderraw <- fromJSON(
  getURL(memberapiurl), simplifyVector = T, flatten = T, simplifyDataFrame = T
)

saveRDS(attendeesraw, 'attendees.RDS')

# Build a new dataframe with just names and answers.
# Be aware that we cannot unlist and keep the same number of elements in the 
# resulting vector because NULLs are eliminated. Instead, we should convert
# them to NA.
# Also, be aware that some scores have commas in them, so we're removing them
# from the start.
# Finally, we're converting each score from string to a numeric value.
attendees <- data.frame(
  name = attendeesraw$name,
  stats = unlist(sapply(
    attendeesraw$answers,
    FUN = function(x) {
      elem <- x$answer[1]
      as.numeric(ifelse(is.null(elem), NA, gsub(',','', substr(elem, 1, 3))))
    }
  )),
  cs = unlist(sapply(
    attendeesraw$answers,
    FUN = function(x) {
      elem <- x$answer[2]
      as.numeric(ifelse(is.null(elem), NA, gsub(',','', substr(elem, 1, 3))))
    }
  )),
  swdev = unlist(sapply(
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
  biz = unlist(sapply(
    attendeesraw$answers,
    FUN = function(x) {
      elem <- x$answer[5]
      as.numeric(ifelse(is.null(elem), NA, gsub(',','', substr(elem, 1, 3))))
    }
  )),
  stringsAsFactors = F # Critical if yo don't want factors to screw up your transformation
)

# impute NA values with mean
attendees <- attendees %>% mutate(
                stats = ifelse(is.na(stats), mean(stats, na.rm = T), stats),
                cs = ifelse(is.na(cs), mean(cs, na.rm = T), cs),
                swdev = ifelse(is.na(swdev), mean(swdev, na.rm = T), swdev),
                dataviz = ifelse(is.na(dataviz), mean(dataviz, na.rm = T), dataviz),
                biz = ifelse(is.na(biz), mean(biz, na.rm = T), biz)
          )

# Attendee names as row names
rownames(attendees) <- attendees$name

# Drop name column
attendees <- attendees %>% select(-name)

# Calculate avg for each attendee
attendees <- mutate(attendees, avgscore = rowMeans(attendees[-1], na.rm = T))

# Create another dataframe in long format.
attendeeslong <-
  gather(attendees, key = profileattribute, value = score)


#Format plots
rho <- cor(select(attendees, -avgscore), use = 'pairwise.complete.obs')
corrplot.mixed(rho, lower = "ellipse", upper = 'number', tl.pos = 'd')
