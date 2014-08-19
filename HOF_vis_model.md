% Visualizing and Modeling Baseball Hall of Fame Voting
% Kenny Shirley
% NYC Sports Analytics Meetup, August 19, 2014




# Welcome to AT&T Labs at 33 Thomas Street in NYC!
<img src='figures/33thomas.jpg' height=500>

Built for machines; safe, secure, and habitable by humans.

# About Me
- Long-time (~ 10 years) sports statistics researcher/hobbyist
- Undergrad thesis on the Markov model for baseball
- In statistics grad school, co-authored "Bayesball", a relatively early (Bayesian) model for fielding that used location information about all batted balls.
    - Our method was called SAFE = Spatial Aggregate Fielding Evaluation (Annals of Applied Statistics, 2009)
- I've also dabbled with some basketball and football statistics...

# Today's Outline
<ol class='incremental'>
<li>Introduction: Baseball Hall of Fame voting is awful</li>
<li>But... if you can't beat 'em, join 'em</li>
<li>Part 1: Visualize the data</li>
<li>Part 2: Model the outcome</li>
</ol>
<img src='figures/halloffamebuilding.jpg' width=400>

# Collaborators
Joint work with:

Carlos Scheidegger (University of Arizona)

<p><img src='figures/cscheid.jpg' width='100'></p>

and

Carson Sievert (Iowa State University)

<img src='figures/sievert.png' width='100'>

# Introduction: Baseball Hall of Fame voting is awful

# First, the rules:
<ol class='incremental'>
<li>A player can appear on the ballot after having played for at least 10 years and having been retired for at least 5 years.</li>
<li>A committee chooses who appears on the ballot, and they are... "generous"</li>
</ol>

# Jeff Cirillo
<img src='figures/cirillo.jpg'>

Jeff Cirillo, 3B, 1994 - 2007: .296 BA, 112 HR, 32 WAR, 2 All-Star Teams, 2013 HOF ballot

# Jeromy Burnitz
<img src='figures/burnitz.jpg'>

Jeromy Burnitz, OF, 1993 - 2006: .253 BA, 315 HR, 17.4 WAR, 1 All-Star Team, 2012 HOF ballot

# Dan Plesac
<img src='figures/plesac.jpg'>

Dan Plesac, P, 1986 - 2003: 65 - 71 Win-Loss, 3.64 ERA, 17.2 WAR, 3 All-Star Teams, 2009 HOF ballot

# These are just a few players who have appeared on the ballot
<ol class='incremental'>
<li>... and these are just some of the former Brewers!</li>
<li>None of them received a single HOF vote, thankfully.</li>
<li>Unlike:</li>
<li>

```
##               Name Year  WAR Votes NumBallots
## 1     Jacque Jones 2014 11.5     1        571
## 2      David Segui 2010  7.8     1        539
## 3   Shawon Dunston 2008  9.1     1        543
## 4       Walt Weiss 2006 14.6     1        520
## 5      Randy Myers 2004 14.2     1        506
## 6    Cecil Fielder 2004 14.7     1        506
## 7       Mark Davis 2003  6.8     1        496
## 8     Jim Deshaies 2001 10.2     1        515
## 9  Steve Bedrosian 2001 13.2     1        515
## 10      Ray Knight 1994 10.9     1        456
```
</li>
</ol>

# More Rules:
- About 570 members (as of 2014) of the Baseball Writers Association of America (BBWAA) vote each year in December, with results announced in January
- A player must be selected on at least 75% of the returned ballots to be inducted into the HOF that year.
- In 1967 they started to enforce the rule that if you appear on less than 5% of ballots, you are permanently removed from future ballots.
- A player may appear on a maximum of 15 ballots (i.e. consecutive years) before being permanently removed.
- In a given year, each voter may vote for as few as 0 players, and as many as 10 players (where an average year's ballot contains about 40 players).

# Some Statistics
<ol class='incremental'>
<li>1936 was the first year of Hall of Fame voting</li>
<li>Five players were elected:</li>
<li>

```
##   Year              Name Pos NumBallots Votes Percentage
## 1 1936           Ty Cobb  OF        226   222     98.20%
## 2 1936      Honus Wagner  SS        226   215     95.10%
## 3 1936         Babe Ruth  OF        226   215     95.10%
## 4 1936 Christy Mathewson   P        226   205     90.70%
## 5 1936    Walter Johnson   P        226   189     83.60%
```
</li>
<li>From 1936 - 2014, 1089 unique players have appeared on the ballot</li>
<li>112 have been elected, 47 on their first ballot appearance (*not* Lou Gehrig, Cy Young, Warren Spahn)</li>
<li>Famously, no player has been unanimously elected.</li>
<li>We consider 1967 to be the first year of 'modern' HOF voting (when the 5% rule was established)</li>
</ol>

# Some Problems:

<ol class='incremental' style='list-style-type:none'>
<li>Does it really take 15 years to decide?

</li>
<li>The so-called 'morals' clause, rule 5 out of 9:

> Voting: Voting shall be based upon the player's record, playing ability, integrity, sportsmanship, character, and contributions to the team(s) on which the player played.

> (from http://baseballhall.org/hall-famers/rules-election/BBWAA)

</li>
<li>The voters don't actively cover baseball!

> Q: Does that mean some Hall of Fame voters don’t even cover baseball any more?

> A: Yes. The BBWAA trusts that its voters take their responsibility seriously, and even those honorary members who are no longer covering baseball do their due diligence to produce a thoughtful ballot.

> (from http://bbwaa.com/voting-faq/)

</li>
</ol>

# Some Responses:

- Will Leitch: "Voter Fraud" 11/14/2013, sportsonearth.com

> Stupid things are called out as stupid, and people try to figure out how to correct them. It's encouraging. It's kinda nice. And then there is Baseball Hall of Fame voting.

- Jonathan Mahler: "Kill The HOF Character Clause, For The Sake Of The Writers", 1/10/2013, deadspin.com
- Deadspin offered to buy a vote in 2013, and mainstream journalist Dan Le Batard (Miami Herald) gave his vote to the website (for no money). He was banned for life from voting on future HOF ballots.

# My thoughts:

- In a nutshell: the voting process is a phenomenon unto itself. It is not a yes-or-no question!
- We might as well embrace this, and study it.
- (Hasn't baseball always been famous for embracing the 'human element' of the game?)

# Part 1: Visualize the data

# Getting the data
- Baseball Reference has it all! Fantastic website.
- Career statistics and voting percentages for each player, each year, from 1936-2014.
- We did a bit of web scraping to download and parse each year's ballot.

# A few plots in R
We were really interested in the trajectories of voting percentages of players who had appeared on the ballot multiple times.


```r
data <- read.csv(file="HOFregression_updated.csv", as.is=TRUE)
par(mfrow=c(1, 2))
sel <- data[, "Name"] == "Alan Trammell"
plot(data[sel, "Year"], data[sel, "p"], ylim=c(0, 1), las=1, pch=19, xlab="Year", 
     ylab="Voting Proportion")
lines(data[sel, "Year"], data[sel, "p"])
title(main="Alan Trammell")
abline(h = 0.05, col=2, lwd=2)
abline(h = 0.75, col=3, lwd=2)
sel <- data[, "Name"] == "Bert Blyleven"
plot(data[sel, "Year"], data[sel, "p"], ylim=c(0, 1), las=1, pch=19, xlab="Year", 
     ylab="Voting Proportion")
lines(data[sel, "Year"], data[sel, "p"])
title(main="Bert Blyleven")
abline(h = 0.05, col=2, lwd=2)
abline(h = 0.75, col=3, lwd=2)
```

# A few plots in R
We were really interested in the trajectories of voting percentages of players who had appeared on the ballot multiple times.

<img src="figure/edaplots2.png" title="plot of chunk edaplots2" alt="plot of chunk edaplots2" width="900" />

How did these guys end up with such different voting trajectories?

# We built an interactive plot using D3
- Link: http://cscheid.net/static/mlb-hall-of-fame-voting
- Two main parts of the visualization:
    - Brushable plot of voting % vs. year
    - Linked histograms for each statistical category

# Lots of interesting trivia was uncovered here:
- Lefty Grove vs. Orval Grove
- Runoff Elections
- Dips in voting percentages for most players when a group of strong first-ballot players appears (1999, 2007)
- Edd Roush's 19 ballots
- Minnie Minoso and Jose Rijo's comebacks
- Warren Spahn's early vote
- HR > 400
- OPS > .850

# Part 2: Model the data and make predictions

# The obvious next question is: Can we predict next year's vote?
- Others have worked on this, of course:
    - JAWS: Jay Jaffe's 'Jaffe WAR Scoring System', an average of WAR and 'peak' WAR, normalized for position
    - Bill James has a few systems. Hall of Fame Monitor, for example, as described on baseball-reference.com (leader glossary section):
    <pre style='white-space: pre-wrap'>
[1] For Batting Average, 2.5 points for each season over .300, 5.0 for over .350, 15 for over .400. Seasons are not double-counted. I require 100 games in a season to qualify for this bonus.
[2] For hits, 5 points for each season of 200 or more hits.
[3] 3 points for each season of 100 RBI's and 3 points for each season of 100 runs.
[4] 10 points for 50 home runs, 4 points for 40 HR, and 2 points for 30 HR.
[5] 2 points for 45 doubles and 1 point for 35 doubles.
[6] 8 points for each MVP award and 3 for each AllStar Game, and 1 point for a Rookie of the Year award.
[7] 2 points for a gold glove at C, SS, or 2B, and 1 point for any other gold glove.
[8] 6 points if they were the regular SS or C on a WS winning team, 5 points for 2B or CF, 3 for 3B, 2 for LF or RF, and 1 for 1B. I don't have the OF distribution, so I give 3 points for OF (requires at least 82 games as the position).
...
[19] ...
    </pre>
    - Michael Freiman's paper in JQAS a few years back. Random Forests for 0/1 induction into HOF.
    - Mills and Salaga, 2011 JQAS paper using random forests as well.
- Most of these focus on the yes/no question of induction, rather than predicting voting percentages.

# What predictors should we use?


# A simple model: lay out the equation

# Show the R code to do logistic regression

# Show results of the regression

# Show the residuals

# Model 2 additional variables

# Model 2 residuals

# Model 3 additional variables

# Model 3 residuals

# 2014 results

# 2015 predictions
