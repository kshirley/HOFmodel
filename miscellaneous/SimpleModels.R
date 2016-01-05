# Load a few libraries and set working directory:
rm(list=ls())
setwd("~/Stats/HOFmodel/")
library(randomForest)
lu <- function(x) length(unique(x))
su <- function(x) sort(unique(x))
count.na <- function(x) sum(is.na(x))

# Read in the data:
HOFdat <- read.csv("HOFvotingdata.csv", stringsAsFactors=FALSE, as.is=TRUE)

# Discard Warren Spahn's weird 1958 vote (cast while he was currently playing)
HOFdat <- HOFdat[-2249,]

# Select players for the analysis:
included.players <- HOFdat[HOFdat$YoB == "1st" & HOFdat$Year > 1966, "Name"]

# Select only those whose first ballot appearance was in 1967 or after:
election_dat <- HOFdat[HOFdat$Name %in% included.players, ]

# Look at year of election and number of years on ballot:
election_dat$YoB <- as.integer(gsub("[a-z]+", "", election_dat$YoB))

# For pitchers, I'll enter 0 for NA batting stats:
election_dat[is.na(election_dat[, "BA"]), c("BA", "OBP", "SLG", "OPS", "OPS.Plus")] <- 0

# Take Pete Rose out of the data:
election_dat <- election_dat[election_dat[, "Name"] != "Pete Rose", ]

# Replace the new WAR-based stats with old versions, based on regressions:
sel.newguys <- election_dat[, "Year"] == 2014 & election_dat[, "YoB"] == 1
for (i in 10:12) {
  new.stat <- election_dat[1:17, i]
  old.stat <- election_dat[c(37:49, 51:54), i]
  print(cor(new.stat, old.stat))
  f <- lm(old.stat ~ new.stat)
  p <- coef(f)[1] + coef(f)[2]*election_dat[sel.newguys, i]
  election_dat[sel.newguys, i] <- round(p, 1)
}

# Replace the WAR-based things for repeat ballot people in 2014 also:
election_dat[1:17, 10:13] <- election_dat[c(37:49, 51:54), 10:13]

# 654 unique players, and at least 40 variables that are player level stats (not player-year level stats):
lu(election_dat[, 3])
dim(unique(election_dat[, c(3, 7:42, 44:46)]))

# start a new data.frame called data with one row per player:
data <- unique(election_dat[, c(3, 7:42, 44:46)])
rownames(data) <- 1:dim(data)[1]

# create a position matrix, with "P" as the baseline position:
pos.names <- sort(unique(data[,"position"]))[order(table(data[, "position"]), decreasing=TRUE)]
lp <- length(pos.names)
pos.mat <- matrix(0, nrow(data), lp - 1)
for (i in 1:10) pos.mat[, i] <- as.numeric(data[, "position"] == pos.names[i + 1])
colnames(pos.mat) <- pos.names[2:lp]

# Add pos.mat to the data:
data <- cbind(data, pos.mat[, 1:10])

# logical values for pitchers and batters:
pitchers <- data[, "position"] == "P"
batters <- !pitchers

## 1 ##  Read in all-star data:
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

# Create all-star variable for the main data.frame, 'data':
m <- match(data[, "Name"], df[, 1])
asp <- df[m, 2]/df[m, 3]
asp[is.na(asp)] <- 0  # assume NA values are those with zero all-star appearances (which is true)
as.total <- df[m, 2]
as.total[is.na(as.total)] <- 0

# Fix Ken Griffey, since he is matched to his father:
asp[which(data[, "Name"] == "Ken Griffey")] <- 3/19  # looked up the 3 all-star appearances manually from baseball reference

## 2 ## Mitchell Report:
mitch <- read.csv("mitchell_report.csv", as.is=TRUE)
mitch[, 1] <- gsub(",", "", mitch[, 1])
mitchell.report <- as.numeric(data[, "Name"] %in% mitch[, 1])

## 3 ## Suspended by MLB:
suspended <- read.csv("suspended.csv", as.is=TRUE)
suspended <- as.numeric(data[, "Name"] %in% suspended[, 1])
# Just Rafael Palmeiro (although others will eventually show up on ballot: 
# Mike Cameron, Manny Ramirez, Bartolo Colon, Melky Cabrera, Ryan Braun, Nelson Cruz, A-Rod, Miguel Tejada)

# OK, let's lump mitchell report and suspended together:
drugs <- mitchell.report + suspended

## 4 ##  Gold Gloves:
gg <- read.csv("goldglove2_raw.csv", as.is=TRUE, header=FALSE)
ny <- diff(which(!is.na(as.numeric(gg[, 1]))))
dgg <- data.frame(gg[is.na(as.numeric(gg[, 1])), 1:4], year=c(rep(rep(2013:1958, 2), ny-1), rep(1957, 9)))

# Get rid of the ", Jr." occurrences for Cal Ripken and Sandy Alomar:
dgg[, 2] <- gsub(", Jr.", "", dgg[, 2])

gold <- numeric(dim(data)[1])
for (i in 1:dim(data)[1]) gold[i] <- sum(dgg[, 2] == data[i, "Name"])

# Fix Griffey Sr. who has zero gold gloves:
gold[data[, "Name"] == "Ken Griffey"] <- 0

## 5 ##  MVP:
mvp <- read.csv("mvp_raw.csv", as.is=TRUE)
mvp[, 2] <- gsub("\\*", "", mvp[, 2])
mvp[, 2] <- gsub("Cal Ripken Jr.", "Cal Ripken", mvp[, 2])

mvp.freq <- numeric(dim(data)[1])
for (i in 1:dim(data)[1]) mvp.freq[i] <- sum(mvp[, 2] == data[i, "Name"])

## 6 ##  Cy Young:
cy <- read.csv("cyyoung_raw.csv", as.is=TRUE)
cy[, 2] <- gsub(" \\*", "", cy[, 2])
cy.young <- numeric(dim(data)[1])
for (i in 1:dim(data)[1]) cy.young[i] <- sum(cy[, 2] == data[i, "Name"])

## 7 ## Rookie of the Year:
roy <- read.csv("roy_raw.csv", as.is=TRUE)
roy[, 2] <- gsub(", Jr.", "", roy[, 2])  # fix sandy alomar and cal ripken
rookie <- numeric(dim(data)[1])
for (i in 1:dim(data)[1]) rookie[i] <- sum(roy[, 2] == data[i, "Name"])

## 8 ## One Team for Whole Career:
one <- read.csv("oneteam_raw.csv", as.is=TRUE)
oneteam <- as.numeric(data[, "Name"] %in% one[, 1])

# Now, create categories for these guys:
gold1 <- as.numeric(gold == 1)
gold2 <- as.numeric(gold > 1 & gold < 6)
gold3 <- as.numeric(gold >= 6)

mvp1 <- as.numeric(mvp.freq == 1)
mvp2 <- as.numeric(mvp.freq > 1)

cy1 <- as.numeric(cy.young == 1)
cy2 <- as.numeric(cy.young > 1)

### Now, append these to the data.frame:
data <- cbind(data, AS.total=as.total, AS.percent=asp, drugs=drugs, gold1, gold2, gold3, mvp1, mvp2, cy1, cy2, rookie, oneteam)


m <- match(data[, "Name"], election_dat[election_dat[, "YoB"] == 1, "Name"])
elect <- election_dat[election_dat[, "YoB"] == 1, c("Year", "Votes", "NumBallots")][m, ]


# Now 'data' contains election data for first-time-on-ballot, too:
data <- cbind(data, elect)

# order the data in a sensible way:
data <- data[order(data[, "Year"], data[, "Votes"], decreasing=TRUE), ]
rownames(data) <- 1:dim(data)[1]
write.csv(data, "firstballot.csv", row.names=FALSE, quote=FALSE)




### Load a few libraries and set working directory:
rm(list=ls())
setwd("~/Stats/HOFmodel/")
library(randomForest)
lu <- function(x) length(unique(x))
su <- function(x) sort(unique(x))
count.na <- function(x) sum(is.na(x))

# Read in the first ballot data:
data <- read.csv("firstballot.csv", as.is=TRUE)
n <- dim(data)[1]

# Set for batters and pitchers:
batters <- data[, "position"] != "P"
pitchers <- !batters

# set up batting and pitching statistics:
bs <- colnames(data)[c(4, 9:19, 21)]
ps <- colnames(data)[c(4, 22:34)]

# set training to all elections before 2014:
train <- data[, "Year"] < 2014


# For batters:
y <- data[train & batters, "Votes"]/data[train & batters, "NumBallots"]

# baseline stats:
#X.mat <- as.matrix(data[train & batters, c(4, 9:19, 21)])

# milestones:
hr400 <- as.numeric(data[train & batters, "HR"] > 400)
hr500 <- as.numeric(data[train & batters, "HR"] > 500)
h3000 <- as.numeric(data[train & batters, "H"] > 3000)
era <- as.numeric(data[train & batters, "Year"] > 2004)


# Try different predictors:
X.mat <- as.matrix(data[train & batters, c("AS.percent", "drugs", "OPS")])
X.mat <- as.matrix(data[train & batters, c("WAR", "drugs")])
X.mat <- as.matrix(data[train & batters, c("WAR", "drugs", "AS.percent", "Yrs")])
X.mat <- as.matrix(data[train & batters, c("WAR", "drugs", "AS.percent", "Yrs", "SB")])

# Baseline + AS.percent + drugs + mvp
X.mat <- as.matrix(data[train & batters, c(colnames(data)[c(4, 9:19, 21)], "AS.percent", "drugs", "mvp1", "mvp2", "gold1", "gold2", "gold3", "rookie", "oneteam")])
X.mat <- cbind(X.mat, hr500, h3000, hr500.era=hr500*era)


# Scale the inputs, keeping the means and sds:
x.mean <- apply(X.mat, 2, mean)
x.sd <- apply(X.mat, 2, sd)
X.scale <- X.mat
for (i in 1:dim(X.mat)[2]) X.scale[, i] <- (X.mat[, i] - x.mean[i])/x.sd[i]

# fit regression model for first-time on ballot:
fit <- glm(y ~ X.scale, weights=data[train & batters, "NumBallots"], family=binomial(link = "logit"))

# residual plot:
plot(fit$fitted.values, y - fit$fitted.values, type="n", xlim=c(0, 1.2))
abline(0, -1, lty=2)
abline(1, -1, lty=2)
abline(h=0, col=gray(0.7))
text(fit$fitted.values, y - fit$fitted.values, data[train & batters, "Name"], cex=0.6)


# rmse:
sd(fit$fitted.values - y)  # 0.1347 for baseline
sd(fit$fitted.values - y)  # 0.1499 for All-star + drugs + OPS, full interactions
sd(fit$fitted.values - y)  # 0.1382 for WAR + drugs, full interactions
sd(fit$fitted.values - y)  # 0.0985 for WAR + drugs + AS.percent + Yrs, full interactions
sd(fit$fitted.values - y)  # 0.0981 for WAR + drugs + AS.percent + Yrs + SB, full interactions
sd(fit$fitted.values - y)  # 0.0791 for baseline + drugs + AS.percent + mvp
sd(fit$fitted.values - y)  # 0.0707 for baseline + drugs + AS.percent + mvp + milestones
sd(fit$fitted.values - y)  # 0.0695 for baseline + drugs + AS.percent + mvp + milestones*drugs
sd(fit$fitted.values - y)  # 0.0657 for baseline + drugs + AS.percent + mvp + gg + milestones*drugs
sd(fit$fitted.values - y)  # 0.0619 for baseline + drugs + AS.percent + mvp + gg + milestones


# Use variable names and use interactions:
#w <- data[train & batters, "NumBallots"]
#fit <- glm(y ~ AS.percent*drugs*OPS, data=data.frame(X.scale), weights=w, family=binomial(link = "logit"))
#fit <- glm(y ~ WAR*drugs, data=data.frame(X.scale), weights=w, family=binomial(link = "logit"))
#fit <- glm(y ~ WAR*drugs*AS.percent*Yrs, data=data.frame(X.scale), weights=w, family=binomial(link = "logit"))
#fit <- glm(y ~ WAR*drugs*AS.percent*Yrs + SB, data=data.frame(X.scale), weights=w, family=binomial(link = "logit"))






### Now try pitchers first ballots:

# For pitchers
y <- data[train & pitchers, "Votes"]/data[train & pitchers, "NumBallots"]

# milestones:
k3000 <- as.numeric(data[train & pitchers, "SO"] > 3000)
w300 <- as.numeric(data[train & pitchers, "W"] > 300)
#era <- as.numeric(data[train & pitchers, "Year"] > 2004)

# Try different statistics
X.mat <- as.matrix(data[train & pitchers, c(colnames(data)[c(4, 22:34)])])
X.mat <- as.matrix(data[train & pitchers, c(colnames(data)[c(4, 22:34)], "AS.percent", "drugs")])
X.mat <- as.matrix(data[train & pitchers, c(colnames(data)[c(4, 22:34)], "AS.percent", "drugs", "mvp1", "cy1", "cy2")])


#X.mat <- as.matrix(data[train & pitchers, c("AS.percent", "drugs", "OPS")])
#X.mat <- as.matrix(data[train & pitchers, c("WAR", "drugs")])
#X.mat <- as.matrix(data[train & pitchers, c("WAR", "drugs", "AS.percent", "Yrs")])
#X.mat <- as.matrix(data[train & pitchers, c("WAR", "drugs", "AS.percent", "Yrs", "SB")])
#X.mat <- as.matrix(data[train & batters, c(colnames(data)[c(4, 9:19, 21)], "AS.percent", "drugs", "mvp1", "mvp2", "gold1", "gold2", "gold3", "rookie", "oneteam")])


X.mat <- cbind(X.mat, k3000, w300)
X.mat <- cbind(X.mat, k3000, w300, X.mat[, "IP"]*X.mat[, "ERA"], X.mat[, "SO"]/X.mat[, "IP"])


# Scale the inputs, keeping the means and sds:
x.mean <- apply(X.mat, 2, mean)
x.sd <- apply(X.mat, 2, sd)
X.scale <- X.mat
for (i in 1:dim(X.mat)[2]) X.scale[, i] <- (X.mat[, i] - x.mean[i])/x.sd[i]

# fit regression model for first-time on ballot:
fit <- glm(y ~ X.scale, weights=data[train & pitchers, "NumBallots"], family=binomial(link = "logit"))

# rmse:
sd(fit$fitted.values - y)  # 0.0655 for baseline
sd(fit$fitted.values - y)  # 0.0490 for baseline + AS.percent + drugs
sd(fit$fitted.values - y)  # 0.0402 for baseline + AS.percent + drugs + mvp + cy
sd(fit$fitted.values - y)  # 0.0394 for baseline + AS.percent + drugs + mvp + cy + milestones
sd(fit$fitted.values - y)  # 0.0392 for baseline + AS.percent + drugs + mvp + cy + milestones + ERA*IP
sd(fit$fitted.values - y)  # 0.0396 for baseline + AS.percent + drugs + mvp + cy + milestones + ERA*IP + SO/IP


# residual plot:
plot(fit$fitted.values, y - fit$fitted.values, type="n", xlim=c(0, 1.2))
abline(0, -1, lty=2)
abline(1, -1, lty=2)
abline(h=0, col=gray(0.7))
text(fit$fitted.values, y - fit$fitted.values, data[train & pitchers, "Name"], cex=0.6)


# OK, so batters are around 6% in-sample for first ballot, and pitchers are around 4%









### try a random forest:
fit <- randomForest(x = data[train & batters, c(4, 9:19, 21, 51:58)], y = y, ntree=2000, mtry=10)

# residual plot:
plot(fit$predicted, y - fit$predicted, type="n", xlim=c(0, 1.2), las=1)
text(fit$predicted, y - fit$predicted, data[train & batters, "Name"], cex=0.6)

# rmse:
sd(fit$predicted - y)  # 0.1002















### Look at individual variables for batters:
pdf(file="fig_univariate.pdf", height=6, width=10.5)
for (i in c(2:21, 51:52)) {
  plot(data[batters & train, i], y, type="n")
  text(data[batters & train, i], y, paste(data[batters & train, "Name"], data[batters & train, "drugs"], sep="-"))
  title(main=colnames(data)[i])
}
dev.off()






### Try using all stats and computing distance matrix between all players:
X.mat <- as.matrix(data[train & batters, c(4, 9:19, 21, 51:58, 61:62)])
nx <- dim(X.mat)[1]

d <- matrix(0, nx, nx)
for (i in 1:nx) {
  for (j in 1:(i - 1)) {
  	d[i, j] <- sqrt(sum((X.mat[i, ] - X.mat[j,])^2))
  	d[j, i] <- d[i, j]
  }
}

m <- cmdscale(d = d, k = 2)

library(RColorBrewer)
col.vec <- brewer.pal(10, name="Paired")
col <- col.vec[floor(y*10) + 1]

pdf(file="fig_mdscale_batters.pdf", width=16, height=16)
plot(m[, 1], m[, 2], type="n")
text(m[, 1], m[, 2], data[batters & train, "Name"], cex=0.6, col=col)
legend("topright", inset=0.01, pch=20, col=col.vec, legend=paste("< ", seq(10, 100, 10), "%"))
dev.off()



















