# Clean environment
rm(list = ls())

# Meetup.com API key: 253275a5c18147e74c5b1e2a6708
meetupurl <-
  "https://api.meetup.com/2/profiles?&sign=true&photo-host=public&group_urlname=thedatapub&key=253275a5c18147e74c5b1e2a6708"

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
      ifelse(is.null(elem), NA, gsub(',','', substr(elem, 1, 3)))
    }
  )),
  cs = unlist(sapply(
    attendeesraw$answers,
    FUN = function(x) {
      elem <- x$answer[2]
      ifelse(is.null(elem), NA, gsub(',','', substr(elem, 1, 3)))
    }
  )),
  swdev = unlist(sapply(
    attendeesraw$answers,
    FUN = function(x) {
      elem <- x$answer[3]
      ifelse(is.null(elem), NA, gsub(',','', substr(elem, 1, 3)))
    }
  )),
  dataviz = unlist(sapply(
    attendeesraw$answers,
    FUN = function(x) {
      elem <- x$answer[4]
      ifelse(is.null(elem), NA, gsub(',','', substr(elem, 1, 3)))
    }
  )),
  biz = unlist(sapply(
    attendeesraw$answers,
    FUN = function(x) {
      elem <- x$answer[5]
      ifelse(is.null(elem), NA, gsub(',','', substr(elem, 1, 3)))
    }
  )),
  stringsAsFactors = F # Critical if yo don't want factors to screw up your transformation
)

answerlist <- sapply(select(attendees, -name), as.numeric, simplify = F)
answerlist <- lapply(answerlist, function(x) {ifelse(x > 10, 10, x)})
attendeesclean <- attendees %>% mutate(stats = answerlist[[1]],
                                       cs = answerlist[[2]],
                                       swdev = answerlist[[3]],
                                       dataviz = answerlist[[4]],
                                       biz = answerlist[[5]])


# impute NA values with mean
attendeesclean <- attendeesclean %>% mutate(
  stats = ifelse(is.na(stats), mean(stats, na.rm = T), stats),
  cs = ifelse(is.na(cs), mean(cs, na.rm = T), cs),
  swdev = ifelse(is.na(swdev), mean(swdev, na.rm = T), swdev),
  dataviz = ifelse(is.na(dataviz), mean(dataviz, na.rm = T), dataviz),
  biz = ifelse(is.na(biz), mean(biz, na.rm = T), biz)
)

# Calculate avg for each attendee
attendeesclean <- mutate(attendeesclean, avgscore = rowMeans(attendeesclean[-1], na.rm = T))

# Create another dataframe in long format.
attendeeslong <-
  gather(attendeesclean, key = profileattribute, value = score, c(-name))

# Extract 1st name only for gender estimation
attendeesclean <- mutate(attendeesclean, firstname = sapply(strsplit(attendeesclean$name, " "), `[`, 1))

# Estimate gender using gender package, and using genderize.io API.
# Since this is a costly operation, we save it to a file and if not present,
# hit the API again.
if (file.exists('./namegenders.RDS')) {
    gendr <- readRDS('./namegenders.RDS')
} else {
  gendr <- gender(attendeesclean$firstname, method = 'genderize')  
  saveRDS(gendr, './namegenders.RDS')
}
attendeesclean$gendr = gendr$gender
attendeesclean <- attendeesclean %>% mutate(gendr = ifelse(is.na(gendr), 'NA', gendr))

# Gender count
gen <- data.frame(genero = c('mujeres', 'hombres', 'Sin respuesta'), 
                  count = c(nrow(filter(attendeesclean, gendr == 'female')),
                            nrow(filter(attendeesclean, gendr == 'male')),
                            nrow(filter(attendeesclean, gendr == 'NA'))
                  )
)

# Create 3rd dataframe with mean by discipline
meandiscipline <- data.frame(discipline = c('stats','cs','swdev','dataviz', 'biz'), 
                             meanscore = c(mean(attendeesclean$stats),
                                          mean(attendeesclean$cs),
                                          mean(attendeesclean$swdev),
                                          mean(attendeesclean$dataviz),
                                          mean(attendeesclean$biz)))
