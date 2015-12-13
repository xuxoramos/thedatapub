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
library(gender)

# Get Meetup.com list of attendees.
# Load it from file if present, otherwise go get it from meetup.com API
if (file.exists('./attendees.RDS')) {
  attendeesraw <- readRDS('./attendees.RDS')
} else {
  # Result from jsonlite call is a list, from which only
  # the 1st element is the actual dataframe. We extract that.
  attendeesraw <-
    fromJSON(
      getURL(meetupurl), simplifyVector = T, flatten = T, simplifyDataFrame = T
    )[[1]]
  saveRDS(attendeesraw, './attendees.RDS')
}

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

# Calculate avg for each attendee
attendees <- mutate(attendees, avgscore = rowMeans(attendees[-1], na.rm = T))

# Create another dataframe in long format.
attendeeslong <-
  gather(attendees, key = profileattribute, value = score, c(-name))

# Extract 1st name only for gender estimation
attendees <- mutate(attendees, firstname = sapply(strsplit(attendees$name, " "), `[`, 1))

# Estimate gender using gender package, and using genderize.io API.
# Since this is a costly operation, we save it to a file and if not present,
# hit the API again.
if (file.exists('./namegenders.RDS')) {
    gendr <- readRDS('./namegenders.RDS')
} else {
  gendr <- gender(attendees$firstname, method = 'genderize')  
  saveRDS(gendr, './namegenders.RDS')
}
attendees$gendr = gendr$gender
attendees <- attendees %>% mutate(gendr = ifelse(is.na(gendr), 'NA', gendr))

# Gender count
gen <- data.frame(genero = c('mujeres', 'hombres', 'Sin respuesta'), 
                  count = c(nrow(filter(attendees, gendr == 'female')),
                            nrow(filter(attendees, gendr == 'male')),
                            nrow(filter(attendees, gendr == 'NA'))
                  )
)

# Create 3rd dataframe with mean by discipline
meandiscipline <- data.frame(discipline = c('stats','cs','swdev','dataviz', 'biz'), 
                             meanscore = c(mean(attendees$stats),
                                          mean(attendees$cs),
                                          mean(attendees$swdev),
                                          mean(attendees$dataviz),
                                          mean(attendees$biz)))
