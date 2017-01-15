# Load a few libraries and set working directory:
rm(list=ls())
setwd("~/Stats/HOFmodel/")
lu <- function(x) length(unique(x))
su <- function(x) sort(unique(x))
count.na <- function(x) sum(is.na(x))

# load libraries:
library(rvest)

# Raw data from previous years:
# This file was created manually by cutting and posting a bunch of
# baseball-reference pages together, including the projected 2015 ballot,
# which was pasted in August 2014 (and contained several players who did
# not eventually show up on the 2015 ballot).
data <- read.csv("data/HOFraw.csv", stringsAsFactors = FALSE, as.is = TRUE)

# Read in the 2015 ballot results to add to the raw data:
new <- read_html(x = "http://www.baseball-reference.com/awards/hof_2015.shtml")

# Get number of ballots:
num.ballots <- new %>% 
  html_nodes("div.table_heading_text") %>% 
  html_text() %>%
  strsplit(split = " ") %>% 
  unlist() %>% 
  head(1) %>%
  as.numeric()
  
# manipulate it a bit to get it into a data frame:
new.tab <- new %>% html_nodes("table") %>% html_table(fill = TRUE)
new.tab <- new.tab[[1]]
categories <- as.character(new.tab[1, ])
new.tab <- new.tab[-1, ]
names(new.tab) <- categories

# compute number of ballots:
n.new <- dim(new.tab)[1]
new.tab <- data.frame(Year = rep(2015, n.new), 
                      new.tab, 
                      NumBallots = rep(num.ballots, n.new))

names(new.tab) <- names(data)

# replace old 2015 data with updated 2015 data:
data <- data[data[, "Year"] != 2015, ]
data <- rbind(new.tab, data)

# Get rid of the "X" prefix in the name field indicating a player's last ballot
data[, "Name"] <- gsub("X-", "", data[, "Name"])


# Add in the 2016 ballot:

# Read in the 2015 ballot results to add to the raw data:
new <- read_html(x = "http://www.baseball-reference.com/awards/hof_2016.shtml")

# manipulate it a bit to get it into a data frame:
new.tab <- new %>% html_nodes("table") %>% html_table(fill = TRUE)
new.tab <- new.tab[[1]]
categories <- as.character(new.tab[1, ])
new.tab <- new.tab[-1, ]
names(new.tab) <- categories

# compute number of ballots:
n.new <- dim(new.tab)[1]
new.tab <- data.frame(Year = rep(2016, n.new), 
                      new.tab[, 1:3], 
                      Votes = rep(NA, n.new), 
                      new.tab[, 4:dim(new.tab)[2]], 
                      NumBallots = rep(450, n.new))
# 450 comes from the HOF ballot tracker estimate
# Much fewer because the list of BBWAA voters has been "purged" to exclude
# those who don't follow baseball anymore (i.e. aren't qualified)

names(new.tab) <- names(data)


# Append 2016 ballot to the data:
data <- rbind(new.tab, data)
rownames(data) <- 1:dim(data)[1]

# force to correct class:
integer.cols <- c(1, 2, 5,  7, 8, 9, 14, 15, 16, 17, 18, 19, 20, 21, 26, 27, 
                  28, 30, 32, 33, 34, 36, 37, 38, 39, 41)
character.cols <- c(3, 4, 6, 40)
numeric.cols <- c(10, 11, 12, 13, 22, 23, 24, 25, 29, 31, 35)
for (i in integer.cols) data[, i] <- as.integer(data[, i])
for (i in character.cols) data[, i] <- as.character(data[, i])
for (i in numeric.cols) data[, i] <- as.numeric(data[, i])



### Compute position from 'Position Summary':
pos <- substr(gsub("[\\*\\/]", "", data[, "PosSummary"]), 1, 1)
pos[pos == "D"] <- "10"
pos[pos == "O"] <- "11"
Position <- c("P", "C", "1B", "2B", "3B", "SS", "LF", "CF", "RF", "DH", "OF")
Position <- Position[as.numeric(pos)]

# Append the main position to the data.frame:
data <- cbind(data, Position)

# Discard Warren Spahn's weird 1958 vote (cast while he was currently playing)
data <- data[-which(data[, "Name"] == "Warren Spahn" & data[, "Year"] == 1958), ]

# Select only those whose first ballot appearance was in 1967 or after:
included.players <- data[data$YoB == "1st" & data$Year > 1966, "Name"]
data <- data[data$Name %in% included.players, ]

# Look at year of election and number of years on ballot:
data$YoB <- as.integer(gsub("[a-z]+", "", data$YoB))

# Compute voting percentage, 'p', and re-order some of the columns:
p <- data[, "Votes"]/data[, "NumBallots"]
data <- data.frame(data[, c("Name", "Year", "Votes", "NumBallots")], 
                   p, 
                   data[, c("YoB", "PosSummary", "Position")], 
                   data[, c(7:39)])

# hof for reference:
#hof <- read.csv("HOFregression.csv", as.is = TRUE)

## 1 ##  Read in all-star data from 1933 - 2013:
allstar <- read.csv("data/allstars.csv", as.is = TRUE)
allstar[, "Year"] <- as.numeric(gsub(" \\(.\\)", "", allstar[, "Year"]))

# Just unique name + year:
as <- unique(allstar[, c("Name", "Year")])

# look at it in a data.frame:
df <- data.frame(table(as[, 1]))
df <- df[order(df[, 2], decreasing=TRUE), ]

# Include the number of years a player was in the league:
num.years <- data[match(df[, 1], data[, "Name"]), "Yrs"]
df <- data.frame(Name = as.character(df[, 1]), 
                 ASGames = df[, 2], 
                 Years = num.years)
# we'll only have num.years for those who appeared on HOF ballot since 1967

# Create all-star variable for the main data.frame, 'data':
m <- match(data[, "Name"], df[, 1])
all.star <- df[m, 2]
all.star[is.na(all.star)] <- 0

# Fix Ken Griffey Sr. and Jr. since their all-star appearances are combined
all.star[which(data[, "Name"] == "Ken Griffey")[1]] <- 13   # junior
all.star[which(data[, "Name"] == "Ken Griffey")[2]] <- 3   # senior
# correct # of all-star appearances gathered manually from baseball-reference

# append it to the data:
data <- cbind(data, all.star)

## 2 ## Mitchell Report:
mitch <- read.csv("data/mitchell_report.csv", as.is=TRUE)
mitch[, 1] <- gsub(",", "", mitch[, 1])
mitchell.report <- as.numeric(data[, "Name"] %in% mitch[, 1])

## 3 ## Suspended by MLB: (downloaded December 2013)
suspended <- read.csv("data/suspended.csv", as.is=TRUE)
suspended <- as.numeric(data[, "Name"] %in% suspended[, 1])
# Just Rafael Palmeiro (although others will eventually show up on ballot: 
# Mike Cameron, Manny Ramirez, Bartolo Colon, Melky Cabrera, Ryan Braun, Nelson Cruz, A-Rod, Miguel Tejada)

# OK, let's lump mitchell report and suspended together:
drugs <- mitchell.report + suspended
data <- cbind(data, drugs)

## 3 ## Rookie of the Year through 2013:
roy <- read.csv("data/roy_raw.csv", as.is=TRUE)
roy[, 2] <- gsub(", Jr.", "", roy[, 2])  # fix sandy alomar and cal ripken
rookie <- numeric(dim(data)[1])
for (i in 1:dim(data)[1]) rookie[i] <- sum(roy[, 2] == data[i, "Name"])
data <- cbind(data, rookie)

## 4 ##  Gold Gloves through 2013:
gg <- read.csv("data/goldglove2_raw.csv", as.is = TRUE, header = FALSE)
ny <- diff(which(!is.na(as.numeric(gg[, 1]))))
dgg <- data.frame(gg[is.na(as.numeric(gg[, 1])), 1:4], 
                  year = c(rep(rep(2013:1958, 2), ny-1), rep(1957, 9)))

# Get rid of the ", Jr." occurrences for Cal Ripken and Sandy Alomar:
dgg[, 2] <- gsub(", Jr.", "", dgg[, 2])

gold <- numeric(dim(data)[1])
for (i in 1:dim(data)[1]) gold[i] <- sum(dgg[, 2] == data[i, "Name"])

# Fix Griffey Sr. who has zero gold gloves:
gold[which(data[, "Name"] == "Ken Griffey")[2]] <- 0
data <- cbind(data, gold.gloves = gold)


## 5 ##  MVP through 2013:
mvp <- read.csv("data/mvp_raw.csv", as.is = TRUE)
mvp[, 2] <- gsub("\\*", "", mvp[, 2])
mvp[, 2] <- gsub("Cal Ripken Jr.", "Cal Ripken", mvp[, 2])
mvp[, 2] <- gsub("Ken Griffey Jr.", "Ken Griffey", mvp[, 2])

mvp.freq <- numeric(dim(data)[1])
for (i in 1:dim(data)[1]) mvp.freq[i] <- sum(mvp[, 2] == data[i, "Name"])

# set Griffey Sr. to zero
mvp.freq[which(data[, "Name"] == "Ken Griffey")[2]] <- 0 

# append to the data:
data <- cbind(data, mvp = mvp.freq)

## 6 ##  Cy Young through 2013:
cy <- read.csv("data/cyyoung_raw.csv", as.is=TRUE)
cy[, 2] <- gsub(" \\*", "", cy[, 2])
cy.young <- numeric(dim(data)[1])
for (i in 1:dim(data)[1]) cy.young[i] <- sum(cy[, 2] == data[i, "Name"])
data <- cbind(data, cy.young)

## 8 ## One Team for Whole Career (downloaded through 2011?):
one <- read.csv("data/oneteam_raw.csv", as.is=TRUE)
oneteam <- as.numeric(data[, "Name"] %in% one[, 1])
data <- cbind(data, oneteam)

# Remove Pete Rose, and change Kirby Puckett to one team:
data <- data[data[, "Name"] != "Pete Rose", ]
data[data[, "Name"] == "Kirby Puckett", "oneteam"] <- 1

# Fix the positions of the "OF" players (just 3 of them), looked up manually:
data[data[, "Name"] %in% c("Augie Galan", "Stan Musial"), "Position"] <- "LF"
data[data[, "Name"] == "Elmer Valo", "Position"] <- "RF"

# Set a few NA values in the batting fields for pitchers to zero instead of NA:
data[is.na(data[, "BA"]), c("BA", "OBP", "SLG", "OPS", "OPS.Plus")] <- 0





# Write the data to a .csv file:
write.csv(data, file="HOFregression_updated_20151228.csv", 
          quote = FALSE, row.names = FALSE)
#data <- read.csv("HOFregression_updated.csv", as.is=TRUE)






################################################################################

# Make predictions for the 2017 Ballot
# Voting will be announced on January 18, 2017.
# We'll start by reading in the file from 2016 and adding to it

# Load a few libraries and set working directory:
rm(list = ls())
setwd("~/Stats/HOFmodel/")
lu <- function(x) length(unique(x))
su <- function(x) sort(unique(x))
count.na <- function(x) sum(is.na(x))

# load libraries:
library(rvest)

# Raw data from previous years:
original_raw_data <- read.csv("data/HOFraw.csv", stringsAsFactors = FALSE, as.is = TRUE)
data <- read.csv(file = "HOFregression_updated_20151228.csv", 
                 stringsAsFactors = FALSE, as.is = TRUE)

# Read in the 2016 ballot results to add to the raw data:
new <- read_html(x = "http://www.baseball-reference.com/awards/hof_2016.shtml")

# Get number of ballots:
num.ballots <- new %>% 
  html_nodes("div.table_heading_text") %>% 
  html_text() %>%
  strsplit(split = " ") %>% 
  unlist() %>% 
  head(1) %>%
  as.numeric()

# manipulate it a bit to get it into a data frame:
new.tab <- new %>% html_nodes("table") %>% html_table(fill = TRUE)
new.tab <- new.tab[[1]]
categories <- as.character(new.tab[1, ])
new.tab <- new.tab[-1, ]
names(new.tab) <- categories

# compute number of ballots:
n.new <- dim(new.tab)[1]
new.tab <- data.frame(Year = rep(2015, n.new), 
                      new.tab, 
                      NumBallots = rep(num.ballots, n.new))

#names(new.tab) <- names(data)
# Fix two column names:
names(new.tab)[names(new.tab) == "OPS."] <- "OPS.Plus"
names(new.tab)[names(new.tab) == "ERA."] <- "ERA.Plus"

# remove the "X-" in the recent results table:
new.tab$Name <- gsub("X-", "", new.tab$Name)

# re-order the previous year of data in decreasing order of vote proportion
m <- match(new.tab$Name, data$Name)
data[data$Year == 2016, ] <- data[data$Year == 2016, ][m, ]

# replace three columns of old pre-voting data updated voting results
data[data$Year == 2016, "Votes"] <- as.integer(new.tab$Votes)
data[data$Year == 2016, "NumBallots"] <- num.ballots
data[data$Year == 2016, "p"] <- as.integer(new.tab$Votes)/num.ballots


# Add in the upcoming (not yet voted on) ballot:

# Read in the 2015 ballot results to add to the raw data:
new <- read_html(x = "http://www.baseball-reference.com/awards/hof_2017.shtml")

# manipulate it a bit to get it into a data frame:
new.tab <- new %>% html_nodes("table") %>% html_table(fill = TRUE)
new.tab <- new.tab[[1]]
categories <- as.character(new.tab[1, ])
new.tab <- new.tab[-1, ]
names(new.tab) <- categories

# compute number of ballots:
n.new <- dim(new.tab)[1]
new.tab <- data.frame(Year = rep(2017, n.new), 
                      new.tab[, 1:3], 
                      Votes = rep(NA, n.new), 
                      new.tab[, 4:dim(new.tab)[2]], 
                      NumBallots = rep(435, n.new))
# 435 comes from the HOF ballot tracker estimate
# http://www.bbhoftracker.com/
# Much fewer because the list of BBWAA voters has been "purged" to exclude
# those who don't follow baseball anymore (i.e. aren't qualified)

colnames <- readLines("col_names_raw_data.txt")
names(new.tab) <- colnames

# change percentage to p:
names(new.tab)[names(new.tab) == "X.vote"] <- "p"

# remove Rank
new.tab <- new.tab[, -which(names(new.tab) == "Rk")]


# Add Position to new.tab:
new.tab$Position <- NA
new.tab$p <- NA
new.tab$YoB <- as.numeric(substr(new.tab$YoB, 1, nchar(new.tab$YoB) - 2))

# re-order the columns:
new.tab <- new.tab[, match(names(data)[1:41], names(new.tab))]

# set the classes of the columns:
for (i in 1:41) {
  class(new.tab[, i]) <- class(data[, i])
}

# coerce Votes to integer
new.tab$Votes <- as.integer(new.tab$Votes)
data$Votes <- as.integer(data$Votes)

### Compute position from 'Position Summary':
pos <- substr(gsub("[\\*\\/]", "", new.tab[, "PosSummary"]), 1, 1)
pos[pos == "D"] <- "10"
pos[pos == "O"] <- "11"
Position <- c("P", "C", "1B", "2B", "3B", "SS", "LF", "CF", "RF", "DH", "OF")
Position <- Position[as.numeric(pos)]

# Append the main position to the data.frame:
new.tab$Position <- Position




## 1 ##  Read in all-star data from 1933 - 2013:
allstar <- read.csv("data/allstars.csv", as.is = TRUE)
allstar[, "Year"] <- as.numeric(gsub(" \\(.\\)", "", allstar[, "Year"]))

# Just unique name + year:
as <- unique(allstar[, c("Name", "Year")])

# look at it in a data.frame:
df <- data.frame(table(as[, 1]))
df <- df[order(df[, 2], decreasing=TRUE), ]

# Include the number of years a player was in the league:
num.years <- data[match(df[, 1], new.tab[, "Name"]), "Yrs"]
df <- data.frame(Name = as.character(df[, 1]), 
                 ASGames = df[, 2], 
                 Years = num.years)
# we'll only have num.years for those who appeared on HOF ballot since 1967

# Create all-star variable for the main data.frame, 'data':
m <- match(new.tab[, "Name"], df[, 1])
all.star <- df[m, 2]
all.star[is.na(all.star)] <- 0

# append it to the data:
new.tab <- cbind(new.tab, all.star)

## 2 ## Mitchell Report:
mitch <- read.csv("data/mitchell_report.csv", as.is=TRUE)
mitch[, 1] <- gsub(",", "", mitch[, 1])
mitchell.report <- as.numeric(new.tab[, "Name"] %in% mitch[, 1])

## 3 ## Suspended by MLB: (downloaded December 2013)
suspended <- read.csv("data/suspended.csv", as.is = TRUE)
suspended <- as.numeric(data[, "Name"] %in% suspended[, 1])
# Just Rafael Palmeiro (although others will eventually show up on ballot: 
# Mike Cameron, Manny Ramirez, Bartolo Colon, Melky Cabrera, Ryan Braun, Nelson Cruz, A-Rod, Miguel Tejada)


# Get data from:
# https://en.wikipedia.org/wiki/List_of_Major_League_Baseball_players_suspended_for_performance-enhancing_drugs
# cut and paste the table into excel
# save as .csv
# manually remove "GG", and "MVP" and stuff from column 1
suspended2 <- read.csv("data/suspended-2017-01-14.csv", as.is = TRUE)
suspended2$Player <- iconv(suspended2$Player, "ASCII")
suspended2 <- as.numeric(new.tab[, "Name"] %in% suspended2$Player)

# OK, let's lump mitchell report and suspended together:
drugs <- pmin(mitchell.report + suspended2, 1)
new.tab <- cbind(new.tab, drugs)

## 3 ## Rookie of the Year through 2013:
roy <- read.csv("data/roy_raw.csv", as.is = TRUE)
roy[, 2] <- gsub(", Jr.", "", roy[, 2])  # fix sandy alomar and cal ripken
rookie <- numeric(dim(new.tab)[1])
for (i in 1:dim(new.tab)[1]) rookie[i] <- sum(roy[, 2] == new.tab[i, "Name"])
new.tab <- cbind(new.tab, rookie)

## 4 ##  Gold Gloves through 2013:
gg <- read.csv("data/goldglove2_raw.csv", as.is = TRUE, header = FALSE)
ny <- diff(which(!is.na(as.numeric(gg[, 1]))))
dgg <- data.frame(gg[is.na(as.numeric(gg[, 1])), 1:4], 
                  year = c(rep(rep(2013:1958, 2), ny-1), rep(1957, 9)))

# Get rid of the ", Jr." occurrences for Cal Ripken and Sandy Alomar:
dgg[, 2] <- gsub(", Jr.", "", dgg[, 2])

gold <- numeric(dim(new.tab)[1])
for (i in 1:dim(new.tab)[1]) gold[i] <- sum(dgg[, 2] == new.tab[i, "Name"])

new.tab <- cbind(new.tab, gold.gloves = gold)


## 5 ##  MVP through 2013:
mvp <- read.csv("data/mvp_raw.csv", as.is = TRUE)
mvp[, 2] <- gsub("\\*", "", mvp[, 2])
mvp[, 2] <- gsub("Cal Ripken Jr.", "Cal Ripken", mvp[, 2])
mvp[, 2] <- gsub("Ken Griffey Jr.", "Ken Griffey", mvp[, 2])

mvp.freq <- numeric(dim(new.tab)[1])
for (i in 1:dim(new.tab)[1]) mvp.freq[i] <- sum(mvp[, 2] == new.tab[i, "Name"])

# append to the data:
new.tab <- cbind(new.tab, mvp = mvp.freq)

## 6 ##  Cy Young through 2013:
cy <- read.csv("data/cyyoung_raw.csv", as.is=TRUE)
cy[, 2] <- gsub(" \\*", "", cy[, 2])
cy.young <- numeric(dim(new.tab)[1])
for (i in 1:dim(new.tab)[1]) cy.young[i] <- sum(cy[, 2] == new.tab[i, "Name"])
new.tab <- cbind(new.tab, cy.young)

## 8 ## One Team for Whole Career (downloaded through 2011?):
one <- read.csv("data/oneteam_raw.csv", as.is = TRUE)
oneteam <- as.numeric(new.tab[, "Name"] %in% one[, 1])

# manually add Jorge Posada and Jason Varitek to the one team list.
# This is pretty subjective, obviously; the previous threshold was 2,000 career
# games and they both fall short
oneteam[new.tab$Name == "Jorge Posada"] <- 1
oneteam[new.tab$Name == "Jason Varitek"] <- 1
new.tab <- cbind(new.tab, oneteam)





# Write the data to a .csv file:
data <- rbind(new.tab, data)
rownames(data) <- 1:dim(data)[1]

write.csv(data, file="HOFregression_updated_20170114.csv", 
          quote = FALSE, row.names = FALSE)
#data <- read.csv("HOFregression_updated.csv", as.is=TRUE)

### EOF
