# Load a few libraries and set working directory:
rm(list=ls())
setwd("~/Stats/HOFmodel/")
lu <- function(x) length(unique(x))
su <- function(x) sort(unique(x))
count.na <- function(x) sum(is.na(x))

# Read in all the HOF raw data (from the table on baseball-reference.com):
data <- read.csv("HOFraw.csv", stringsAsFactors=FALSE, as.is=TRUE)

# Compute position from 'Position Summary':
pos <- substr(gsub("[\\*\\/]", "", data[, "PosSummary"]), 1, 1)
pos[pos == "D"] <- "10"
pos[pos == "O"] <- "11"
Position <- c("P", "C", "1B", "2B", "3B", "SS", "LF", "CF", "RF", "DH", "OF")[as.numeric(pos)]

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
data <- data.frame(data[, c("Name", "Year", "Votes", "NumBallots")], p, data[, c("YoB", "PosSummary", "Position")], data[, c(7:39)])

# hof for reference:
hof <- read.csv("HOFregression.csv", as.is=TRUE)

## 1 ##  Read in all-star data from 1933 - 2013:
allstar <- read.csv("allstars.csv", as.is=TRUE)
allstar[, "Year"] <- as.numeric(gsub(" \\(.\\)", "", allstar[, "Year"]))

# Just unique name + year:
as <- unique(allstar[, c("Name", "Year")])

# look at it in a data.frame:
df <- data.frame(table(as[, 1]))
df <- df[order(df[, 2], decreasing=TRUE), ]

# Include the number of years a player was in the league:
num.years <- data[match(df[, 1], data[, "Name"]), "Yrs"]
df <- data.frame(Name=as.character(df[, 1]), ASGames=df[,2], Years=num.years)
# we'll only have num.years for those who appeared on HOF ballot since 1967

# Create all-star variable for the main data.frame, 'data':
m <- match(data[, "Name"], df[, 1])
all.star <- df[m, 2]
all.star[is.na(all.star)] <- 0

# Fix Ken Griffey Sr., since the all-star data also includes his son Ken Griffey Jr.:
all.star[which(data[, "Name"] == "Ken Griffey")] <- 3  # looked up the 3 all-star appearances manually from baseball-reference

# append it to the data:
data <- cbind(data, all.star)

## 2 ## Mitchell Report:
mitch <- read.csv("mitchell_report.csv", as.is=TRUE)
mitch[, 1] <- gsub(",", "", mitch[, 1])
mitchell.report <- as.numeric(data[, "Name"] %in% mitch[, 1])

## 3 ## Suspended by MLB: (downloaded December 2013)
suspended <- read.csv("suspended.csv", as.is=TRUE)
suspended <- as.numeric(data[, "Name"] %in% suspended[, 1])
# Just Rafael Palmeiro (although others will eventually show up on ballot: 
# Mike Cameron, Manny Ramirez, Bartolo Colon, Melky Cabrera, Ryan Braun, Nelson Cruz, A-Rod, Miguel Tejada)

# OK, let's lump mitchell report and suspended together:
drugs <- mitchell.report + suspended
data <- cbind(data, drugs)

## 3 ## Rookie of the Year through 2013:
roy <- read.csv("roy_raw.csv", as.is=TRUE)
roy[, 2] <- gsub(", Jr.", "", roy[, 2])  # fix sandy alomar and cal ripken
rookie <- numeric(dim(data)[1])
for (i in 1:dim(data)[1]) rookie[i] <- sum(roy[, 2] == data[i, "Name"])
data <- cbind(data, rookie)

## 4 ##  Gold Gloves through 2013:
gg <- read.csv("goldglove2_raw.csv", as.is=TRUE, header=FALSE)
ny <- diff(which(!is.na(as.numeric(gg[, 1]))))
dgg <- data.frame(gg[is.na(as.numeric(gg[, 1])), 1:4], year=c(rep(rep(2013:1958, 2), ny-1), rep(1957, 9)))

# Get rid of the ", Jr." occurrences for Cal Ripken and Sandy Alomar:
dgg[, 2] <- gsub(", Jr.", "", dgg[, 2])

gold <- numeric(dim(data)[1])
for (i in 1:dim(data)[1]) gold[i] <- sum(dgg[, 2] == data[i, "Name"])

# Fix Griffey Sr. who has zero gold gloves:
gold[data[, "Name"] == "Ken Griffey"] <- 0
data <- cbind(data, gold.gloves=gold)


## 5 ##  MVP through 2013:
mvp <- read.csv("mvp_raw.csv", as.is=TRUE)
mvp[, 2] <- gsub("\\*", "", mvp[, 2])
mvp[, 2] <- gsub("Cal Ripken Jr.", "Cal Ripken", mvp[, 2])

mvp.freq <- numeric(dim(data)[1])
for (i in 1:dim(data)[1]) mvp.freq[i] <- sum(mvp[, 2] == data[i, "Name"])

# append to the data:
data <- cbind(data, mvp=mvp.freq)

## 6 ##  Cy Young through 2013:
cy <- read.csv("cyyoung_raw.csv", as.is=TRUE)
cy[, 2] <- gsub(" \\*", "", cy[, 2])
cy.young <- numeric(dim(data)[1])
for (i in 1:dim(data)[1]) cy.young[i] <- sum(cy[, 2] == data[i, "Name"])
data <- cbind(data, cy.young)

## 8 ## One Team for Whole Career (downloaded through 2011?):
one <- read.csv("oneteam_raw.csv", as.is=TRUE)
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
write.csv(data, file="HOFregression_updated.csv", quote=FALSE, row.names=FALSE)
#data <- read.csv("HOFregression_updated.csv", as.is=TRUE)






### EOF


