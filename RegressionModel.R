# Load a few libraries and set working directory:
rm(list=ls())
setwd("~/Git/HOFmodel/")
lu <- function(x) length(unique(x))
su <- function(x) sort(unique(x))
count.na <- function(x) sum(is.na(x))
logit <- function(x) log(x/(1-x))
expit <- function(x) exp(x)/(1+exp(x))

# Load a library to do Bayesian generalized linear models:
library(arm)

# New data:
#data <- read.csv(file="HOFregression_updated.csv", as.is=TRUE)
data <- read.csv(file="HOFregression.csv", as.is=TRUE)
n <- dim(data)[1]

# get number of unique players:
np <- lu(data[, "Name"])

# Get number of ballots each year:
nb <- aggregate(data[, "NumBallots"], list(year=data[, "Year"]), median)[, 2]
# I've assumed 571 for 2015, the same number as 2014

# Create variables for previous year's vote, for 1 up to 14 previous years:
prev <- matrix(0, n, 14)
for (i in 1:n) {
  if (data[i, "YoB"] > 1) {
    sel <- data[, "Name"] == data[i, "Name"] & data[, "YoB"] < data[i, "YoB"]
    prev[i, 1:sum(sel)] <- data[sel, "p"][order(data[sel, "Year"], decreasing=TRUE)]
  }
}
colnames(prev) <- paste0("prev", 1:14)
data <- cbind(data, prev)

# Create "All-Star Appearances per year":
AllStarpy <- data[, "all.star"]/data[, "Yrs"]
data <- cbind(data, AllStarpy)

# Add indicators for 8 batting positions (DH is the baseline)
for (i in c("C", "1B", "2B", "3B", "SS", "LF", "CF", "RF")) assign(paste0("pos", i), as.numeric(data[, "Position"] == i))
data <- cbind(data, posC, pos1B, pos2B, pos3B, posSS, posLF, posCF, posRF)

# Add in previous year's vote squared
prev1.squared <- data[, "prev1"]^2
data <- cbind(data, prev1.squared)

# Add the mean vote percentage of the top-k (k = 1, 2, 3, 4, 5) first-year ballot players in each year:
# Idea is that this will provide a variable to account for 'crowded' ballots:
ny <- lu(data[, "Year"])
first.ballot.crowd <- matrix(NA, ny, 5)
for (i in 1:ny) {
  for (k in 1:5) {
  	sel <- data[, "Year"] == su(data[, "Year"])[i] & data[, "YoB"] == 1
  	if (i == 7) {  # special case for 1973, when Roberto Clemente was a special election:
  	  first.ballot.crowd[i, k] <- mean(sort(data[sel & data[, "Name"] != "Roberto Clemente", "p"], decreasing=TRUE)[1:k], na.rm=TRUE)
  	} else {
  	  first.ballot.crowd[i, k] <- mean(sort(data[sel, "p"], decreasing=TRUE)[1:k], na.rm=TRUE)  	  
  	}
  }
}
rownames(first.ballot.crowd) <- 1967:max(data[, "Year"])

fb <- matrix(NA, n, 5)
for (k in 1:5) fb[, k] <- first.ballot.crowd[(1:ny)[data[, "Year"] - 1966], k]
colnames(fb) <- paste0("top", 1:5)

# Append these to the data:
data <- cbind(data, fb)

# Add the mean vote percentage of the top-5 returning ballot players in each year:
return.ballot.crowd <- numeric(ny)
for (i in 1:ny) {
  sel <- data[, "Year"] == su(data[, "Year"])[i] - 1 & data[, "YoB"] > 1 & data[, "YoB"] < 15
  if (sum(sel) > 0) {
    return.ballot.crowd[i] <- mean(sort(data[sel, "p"], decreasing=TRUE)[1:5], na.rm=TRUE)
  }
}
rb <- return.ballot.crowd[(1:ny)[data[, "Year"] - 1966]]

# Append these to the data:
data <- cbind(data, rb)

# add a few career milestones that are thought to impact HOF voting:
hr500 <- as.numeric(data[, "HR"] >= 500)
h3000 <- as.numeric(data[, "H"] >= 3000)
w300 <- as.numeric(data[, "W"] >= 300)
k3000 <- as.numeric(data[, "SO"] >= 3000)
data <- cbind(data, hr500, h3000, w300, k3000)

# Create indicator variables for 2nd ballot and 15th ballot (players are thought to get an extra bump 
# in these two situations)
ballot2 <- as.numeric(data[, "YoB"] == 2)
ballotfinal <- as.numeric(data[, "YoB"] == 15)
data <- cbind(data, ballot2, ballotfinal)

ballot2.x.prev1 <- data[, "ballot2"]*data[, "prev1"]  # Interaction between 2nd year and 1st year percentage
data <- cbind(data, ballot2.x.prev1)

# Try removing Bonds and Clemens first year:
#data <- data[-which(data[, "Name"] %in% c("Barry Bonds", "Roger Clemens") & data[, "Year"] == 2013), ]
#n <- dim(data)[1]

# Set up 'type' as a categorical variable for different types of player-years:
type <- numeric(n)

# Type 1 = First-ballot batters:
type[data[, "Position"] != "P" & data[, "YoB"] == 1] <- 1

# Type 2 = First-ballot pitchers:
type[data[, "Position"] == "P" & data[, "YoB"] == 1] <- 2

# Type 3 = 2nd or more time on ballot:
type[data[, "YoB"] > 1] <- 3

# total number of different types (this changed a few times as I tried different models)
nt <- lu(type)

# Set up list to hold names of variables to include in the regression model for each type:
var.names <- as.list(rep(NA, nt))



# R0: Baseline model: Just use main career statistics for batters and pitchers, and position for batters:
var.names[[1]] <- c("Yrs", "G", "AB", "R", "H", "HR", "RBI", "SB", "BB", "BA", "OBP", "SLG",
                    "posC", "pos1B", "pos2B", "pos3B", "posSS", "posLF", "posCF", "posRF")
var.names[[2]] <- c("Yrs", "W", "L", "ERA", "WHIP", "G.1", "GS", "SV", "IP", "H.1", "HR.1", "BB.1", "SO")
var.names[[3]] <- c("prev1")  # for returning players, just use the previous year's voting percentage as the sole predictor


# R1 with Drugs, all-stars and gold gloves:
var.names[[1]] <- c("Yrs", "G", "AB", "R", "H", "HR", "RBI", "SB", "BB", "BA", "OBP", "SLG",
                    "posC", "pos1B", "pos2B", "pos3B", "posSS", "posLF", "posCF", "posRF", "drugs", "AllStarpy", 
                    "gold.gloves")
var.names[[2]] <- c("Yrs", "W", "L", "ERA", "WHIP", "G.1", "GS", "SV", "IP", "H.1", "HR.1", "BB.1", "SO", "drugs", 
                    "AllStarpy", "gold.gloves")
var.names[[3]] <- c("prev1")


# R6: Add in 1-team and milestones for batters, milestones for pitchers,
# and add top-3 'crowded ballot' and quadratic term for returning-ballot players:
var.names[[1]] <- c("Yrs", "G", "AB", "R", "H", "HR", "RBI", "SB", "BB", "BA", "OBP", "SLG",
                    "posC", "pos1B", "pos2B", "pos3B", "posSS", "posLF", "posCF", "posRF", "drugs", "AllStarpy", 
                    "gold.gloves", "rookie", "mvp", "oneteam", "hr500", "h3000")
var.names[[2]] <- c("Yrs", "W", "L", "ERA", "WHIP", "G.1", "GS", "SV", "IP", "H.1", "HR.1", "BB.1", "SO", "drugs", 
                    "AllStarpy", "gold.gloves", "rookie", "mvp", "cy.young", "w300", "k3000")
var.names[[3]] <- c("prev1", "prev1.squared", "top3", "rb", "ballot2", "ballotfinal", "ballot2.x.prev1")




### Set up a vector to store all predictions:
pred <- rep(NA, n)
pred.mat <- matrix(NA, n, 5)
qbounds <- function(x) quantile(x, c(0.025, 0.250, 0.500, 0.750, 0.975))

coef <- as.list(rep(NA, nt))
lt <- length(1997:max(data[, "Year"]))
for (j in 1:nt){
  coef[[j]] <- matrix(NA, lt, length(var.names[[j]]) + 1)
  colnames(coef[[j]]) <- c("Intercept", var.names[[j]])
  rownames(coef[[j]]) <- 1997:max(data[, "Year"])
}

in.samp <- matrix(NA, lt, nt)

# Loop through years and positions (batter vs. pitcher):
for (t in 1997:max(data[, "Year"])) {
  print(t)
  for (j in 1:nt) {

    # Set up the design matrix for this type of prediction:
    if (j %in% 1:2) sel <- type == j & data[, "Year"] < t
    if (j > 2) sel <- type == j & data[, "Year"] < t & data[, "prev1"] >= 0.05
    X.mat <- as.matrix(data[sel, var.names[[j]]])

    # Scale the inputs, keeping the means and sds:
    x.mean <- apply(X.mat, 2, mean)
    x.sd <- apply(X.mat, 2, sd)
    X.scale <- X.mat
    for (i in 1:dim(X.mat)[2]) {
      if (x.sd[i] != 0) X.scale[, i] <- (X.mat[, i] - x.mean[i])/x.sd[i]
    }
    
    # Fit the model using weak priors:
    fit <- bayesglm(data[sel, "p"] ~ X.scale, weights=data[sel, "NumBallots"], family=binomial(link = "logit"), 
                    prior.mean=0, prior.scale=2.5)
    in.samp[t - 1996, j] <- sd(fit$fitted.values - data[sel, "p"])
    
    # Store the coefficients:
    coef[[j]][t - 1996, ] <- coef(fit)

    # predict this type for the year of interest:
    sel.test <- type == j & data[, "Year"] == t

    if (sum(sel.test) > 0) {
      X.mat <- as.matrix(data[sel.test, var.names[[j]]])
      if (t > 2013 & j == 3 & "top3" %in% var.names[[j]]) {
      	X.mat[, "top3"] <- mean(sort(pred[data[, "Year"] == t & type %in% 1:2], decreasing=TRUE)[1:3], na.rm=TRUE)
      }
      X.scale <- X.mat
      for (i in 1:dim(X.mat)[2]) {
        if (x.sd[i] != 0) X.scale[, i] <- (X.mat[, i] - x.mean[i])/x.sd[i]
      }
      beta <- mvrnorm(1000, mu=coef(fit), Sigma=summary(fit)$cov.scaled)
      pred[sel.test] <- expit(coef(fit)[1] + X.scale %*% matrix(coef(fit)[-1], ncol=1))
      pred.sim <- expit(beta[,1] + X.scale %*% t(beta[, -1]))
      votes.sim <- matrix(rbinom(sum(sel.test)*1000, size=nb[t - 1966], prob=pred.sim), sum(sel.test), 1000)/nb[t - 1966]
      pred.mat[sel.test, ] <- t(apply(votes.sim, 1, qbounds))
    }
  }
}

# For M1, M2, and M3, replace regression-based predictions for returning players with their prevoius year's values:
#pred[type == 3] <- data[type == 3, "prev1"]

# Look at overall rmse:
sel.pred <- data[, "Year"] > 1996 & data[, "Year"] < 2014
rmse <- sqrt(mean((pred[sel.pred] - data[sel.pred, "p"])^2))

# Break it down by type:
rmse.vec <- numeric(nt)
sel.vec <- as.list(rep(NA, nt))
for (j in 1:3) sel.vec[[j]] <- sel.pred & type == j
for (i in 1:nt) rmse.vec[i] <- sqrt(mean((pred[sel.vec[[i]]] - data[sel.vec[[i]], "p"])^2))

# Compute out-of-sample rmse by year and type:
out.samp <- matrix(NA, lt, 3)
n.out <- matrix(NA, lt, 3)
for (i in 1:lt) {
  for (j in 1:3) {
  	sel <- data[, "Year"] == i + 1996 & type == j
  	out.samp[i, j] <- sd(pred[sel] - data[sel, "p"])
  	n.out[i, j] <- sum(sel)
  }
}

# Plot in-sample vs. out-of-sample rmse:
par(mfrow=c(1, 3))
for (j in 1:3) {
  plot(1997:max(data[, "Year"]), in.samp[, j], type="l", ylim=range(c(in.samp[, j], out.samp[-lt, j])), 
       las=1, ylab="RMSE", xlab="Year")
  lines(1997:(max(data[, "Year"]) - 1), out.samp[-lt, j], lty=2)
}

# Compute residuals:
resids <- data[sel.pred, "p"] - pred[sel.pred]

# Look at residuals with nametags:
sel.big <- abs(resids) > 0.1  # select big residuals to display names
xl <- "Predicted Percentage"
pdf(file="fig_residuals_with2014.pdf", width=9, height=6)
par(mfrow=c(1, 1))
plot(pred[sel.pred], resids, type="n", las=1, ylab="Actual Vote % - Predicted Vote %", yaxt="n", xlim=c(0, 1.1), xlab=xl, xaxt="n")
axis(2, at=seq(-1, 1, 0.2), labels=paste(seq(-100, 100, 20), "%", sep=""), las=1)
axis(1, at=seq(0, 1, 0.2), labels=paste0(seq(0, 100, 20), "%"))
abline(h=seq(-1, 1, 0.1), col=gray(0.8))
text(pred[sel.pred][sel.big], resids[sel.big], paste(data[sel.pred, "Name"], 
     data[sel.pred, "Year"], sep="-")[sel.big], cex=0.6, col=as.numeric(data[sel.pred, "YoB"][sel.big] == 1) + 1)
points(pred[sel.pred][!sel.big], resids[!sel.big], col=as.numeric(data[sel.pred, "YoB"][!sel.big] == 1) + 1)
legend("topright", inset=0.01, col=c(1, 2), pch=19, legend=c("Returning Player", "First Ballot"))
dev.off()


# compute coverage of interval estimates:
cover50 <- numeric(nt)
cover95 <- numeric(nt)
for (j in 1:nt) {
  s <- sel.pred & type == j
  cover50[j] <- sum(data[s, "p"] > pred.mat[s, 2] & data[s, "p"] < pred.mat[s, 4])/sum(s)
  cover95[j] <- sum(data[s, "p"] > pred.mat[s, 1] & data[s, "p"] < pred.mat[s, 5])/sum(s)
}

# 1997 batters example:
sel1997 <- data[, "Year"] == 1997 & type == 1
d1997 <- data.frame(Name=data[sel1997, "Name"], Prediction=round(pred[sel1997], 3)*100, Actual=round(data[sel1997, "p"], 3)*100)

# 1997 batters rmse:
sqrt(mean((d1997[, 2] - d1997[, 3])^2))


# 2014 results:
sel2014 <- data[, "Year"] == 2014
d2014 <- data.frame(Name=data[sel2014, "Name"], Previous=round(data[sel2014, "prev1"], 3)*100, 
                    Predicted=round(pred[sel2014], 3)*100)
d2014 <- d2014[order(d2014[, "Predicted"], decreasing=TRUE), ]
rownames(d2014) <- 1:dim(d2014)[1]

# Just first-time ballots:
first2014 <- d2014[d2014[, 2] == 0, ]
rownames(first2014) <- 1:dim(first2014)[1]


# 2014 results:
sel2015 <- data[, "Year"] == 2015
d2015 <- data.frame(Name=data[sel2015, "Name"], Previous=round(data[sel2015, "prev1"], 3)*100, 
                    Predicted=round(pred[sel2015], 3)*100)
d2015 <- d2015[order(d2015[, "Predicted"], decreasing=TRUE), ]
rownames(d2015) <- 1:dim(d2015)[1]

                    

# Top and bottom residuals:
top <- data.frame(data[sel.pred, c("Year", "Name", "p")], Predicted=pred[sel.pred], 
                  Residual=resids)[order(resids, decreasing=TRUE), ][1:5,]

bottom <- data.frame(data[sel.pred, c("Year", "Name", "p")], Predicted=pred[sel.pred], 
                     Residual=resids)[order(resids, decreasing=FALSE), ][1:5,]

rownames(top) <- 1:5
rownames(bottom) <- 1:5
top[, 4:5] <- round(top[, 4:5], 3)*100
bottom[, 4:5] <- round(bottom[, 4:5], 3)*100
top[, 3] <- round(top[, 3], 3)*100
bottom[, 3] <- round(bottom[, 3], 3)*100
colnames(top)[3] <- "Actual"
colnames(bottom)[3] <- "Actual"

# Plot of effect of 'drugs' over time for batters and pitchers:
pdf(file="fig_drugs_time.pdf", width=7, height=4)
plot(1996:2013, coef[[1]][, "drugs"], las=1, xlab="Year", ylab="Estimated Effect", type="n")
abline(h = 0, lty=1, col=gray(0.5))
title(main="Effect of 'drugs' variable over time")
points(1996:2013, coef[[1]][, "drugs"], pch=19)
lines(1996:2013, coef[[1]][, "drugs"])
points(1996:2013, coef[[2]][, "drugs"], pch=19, col=2)
lines(1996:2013, coef[[2]][, "drugs"], col=2)
legend("bottomleft", inset=0.03, col=c(1, 2), lty=1, legend=c("Batters", "Pitchers"), pch=19)
dev.off()






### Create data.frame to create the table for the website:
web <- cbind(data[, c("Name", "Position", "YoB")], round(data[, "prev1"], 3)*100, 
             round(pred, 3)*100, round(pred.mat[, c(2, 4, 1, 5)], 3)*100)
colnames(web) <- c("Name", "Position", "Year.On.Ballot", "2013 Vote", "2014 Prediction", 
                   "Lower 50%", "Upper 50%", "Lower 95%", "Upper 95%")
web <- web[data[, "Year"]  == 2014, ]
web <- web[order(web[, "2014 Prediction"], decreasing=TRUE), ]
rownames(web) <- 1:36
web <- cbind(Rank=1:36, web)

# create intervals:
int1 <- paste("(", sprintf("%2.1f", web[, 7]), ", ", sprintf("%2.1f", web[, 8]), ")", sep="")
int2 <- paste("(", sprintf("%2.1f", web[, 9]), ", ", sprintf("%2.1f", web[, 10]), ")", sep="")

# gather into a data.frame:
web.table <- data.frame(web[, 1:6], int1, int2)
web.table[web.table[, 5] == 0, 5] <- ""
web.table <- web.table[, -1]
colnames(web.table)[3:7] <- c("Year on Ballot", "2013 Result", "2014 Prediction", "50% Interval", "95% Interval")

# print to screen, and then copy to index.html:
library(xtable)
print(xtable(web.table, align=c("r", "l", "r", "r", "r", "r", "r", "r")), type="html")



# Make a plot of the intervals:
nw <- dim(web.table)[1] # number of players on 2014 ballot:

# colors for type 1 (first ballot batters), type 2 (first ballot pitchers), and type 3 (returning)
col.name <- ifelse(web[, 3] == "P" & web[, 4] == 1, "blue", ifelse(web[, 3] != "P" & web[, 4] == 1, "orange", "black"))


### Create the plot:
pred.vec <- data[data[, "Year"] == 2014, "p"][match(web[, 2], data[data[, "Year"] == 2014, "Name"])]*100
pt <- ifelse(pred.vec > web[, 10], web[, 10], ifelse(pred.vec < web[, 9], web[, 9], pred.vec))

pdf(file="fig_2014_intervals_updated.pdf", height=6, width=8)
par(mar=c(5, 1, 1, 1), mfrow=c(1, 1))
plot(web.table[, 5], nw:1, xlim=c(-10, 100), bty="n", xaxt="n", yaxt="n", xlab="Predicted 2014 Voting Percentage", 
     ylab="", type="n")
polygon(c(0, 100, 100, 0), c(0, 0, nw + 1, nw + 1), col=gray(0.85), border=NA)
abline(v=seq(0, 100, by=10), col="white")
lines(c(75, 75), c(10, nw + 1), col=3, lty = 1)
lines(c(5, 5), c(0, nw + 1), col=2, lty = 1)
points(web.table[, 5], nw:1, pch=1, cex=0.6)
axis(1, at=seq(0, 100, 10), labels=paste0(seq(0, 100, 10), "%"), cex=0.6)
for (i in 1:nw) {
  lines(c(web[i, 9], web[i, 10]), nw - c(i - 1, i - 1))
  #lines(c(web[i, 8], web[i, 8]), nw - c(i - 1.3, i - 0.7))
  #lines(c(web[i, 7], web[i, 7]), nw - c(i - 1.3, i - 0.7))
  lines(c(web[i, 9], web[i, 9]), nw - c(i - 1.3, i - 0.7))
  lines(c(web[i, 10], web[i, 10]), nw - c(i - 1.3, i - 0.7))
  lines(c(pred.vec[i], pt[i]), c(nw - c(i - 1, i - 1)), col="purple", lty=2)
}  
text(pmin(web[, 9], pred.vec), nw:1, web[, 2], pos=2, cex=0.6, col=col.name)
leg.text <- c("5% cutoff: Won't appear on future ballots", "75% cutoff: Inducted into HOF", 
              "95% prediction intervals", "Point prediction", "Actual Voting Percentage")
legend(62, 8, pch=c(73, 73, 73, 1, 4), col=c(2, 3, 1, 1, "purple"), legend=leg.text, cex=0.6, title="Symbol Legend")
leg.text2 <- c("First-ballot batter", "First-ballot pitcher", "Returning ballot")
legend(32, 8, pch=c(NA, NA, NA), text.col=c("orange", "blue", "black"), legend=leg.text2, cex=0.6, 
       title="Player Color Legend", title.col="black")
# update the plot:
points(pred.vec, nw:1, pch=4, col="purple", lwd=2)
dev.off()








# Updates after the 2014 results:
rmse.2014 <- sqrt(mean((data[data[, "Year"] == 2014, "p"] - pred[data[, "Year"] == 2014])^2))

# residuals for 2014:
r <- data[data[, "Year"] == 2014, "p"] - pred[data[, "Year"] == 2014]
a <- data[data[, "Year"] == 2014, "p"]

# biggest dropoffs in years 2-15:
sel.drop <- data[, "YoB"] > 1 & data[, "Year"] < 2015
drop <- data[sel.drop, "p"] - data[sel.drop, "prev1"]

drop.table <- cbind(data[sel.drop, c("Name", "Year", "YoB", "p", "prev1")], Drop=round(drop, 3)*100)
drop.table <- drop.table[order(drop.table[, "Drop"]), ]
drop.table[1:10, ]

mm <- match(d2014[, 1], data[data[, "Year"] == 2014, "Name"])
cbind(d2014, Actual=round(a[mm], 3)*100, Residual=round(r[mm], 3)*100)




# Exploratory analysis: make a plot of mean difference from one year to the next based on year on ballot:
vdiff <- matrix(NA, 14, 3)
for (i in 1:14) {
  sel <- data[, "YoB"] == i + 1 & data[, "Year"] != 2014
  vdiff[i, 1] <- sum(sel)
  vdiff[i, 2] <- mean(data[sel, "p"] - data[sel, "prev1"])
  vdiff[i, 3] <- sd(data[sel, "p"] - data[sel, "prev1"])
}

lower <- vdiff[, 2] - 2*vdiff[, 3]/sqrt(vdiff[, 1])
upper <- vdiff[, 2] + 2*vdiff[, 3]/sqrt(vdiff[, 1])

par(mfrow=c(1, 1))
plot(2:15, vdiff[, 2], ylim=range(c(lower, upper)), las=1)
for (i in 1:14) lines(c(i+1, i+1), c(lower[i], upper[i]))
abline(h = 0, lty=2)



# end of file



library(knitr)
knit("HOF_vis_model.Rmd")
# pandoc -s -S -i -t dzslides --mathjax HOF_vis_model.md -o HOF_vis_model.html


