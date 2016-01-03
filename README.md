## Baseball Hall of Fame Predictions

This repo contains R code to predict the Baseball Hall of Fame vote percentage that a given player will receive based on his career statistics. It started in 2013 as a visualization project (see [@cscheid](https://github.com/cscheid)'s [mlb-hall-of-fame-voting](https://github.com/cscheid/mlb-hall-of-fame-voting?source=c) repo for an interactive visualization of baseball HOF voting over the years), and since then I've fit a few simple statistical models to try to find out how predictable vote percentages are based solely on a player's statistics.

2016 will be the third year I've produced these predictions. For the 2014 predictions and a writeup of the models, see <a href='http://kshirley.github.io/HOFmodel'>here</a>.

I gave a meetup talk about this project in August of 2014 in NYC (slides <a href="http://www.kennyshirley.com/nycmeetup.html">here</a>), and shared predictions for the 2015 ballot at that time (about 3 months before the ballots were sent out).

Below are the model's predictions for 2016.

To be honest, the results haven't been great -- sometimes the model is off by more than, say, 20 or 30% for a given player in a given year! This historical RMSE (of the predicted proportions) is about 9 or 10%. But the model occasionally produces more accurate predictions than the so-called experts, and I think it could serve as some sort of benchmark for the value of career statistics as predictors of HOF voting, so I keep running it. Perhaps the biggest result is that HOF vote percentages depend on much more than just the player's career statistics.

Comments about the 2016 predictions are below the table.

|Name              | YearOnBallot| Previous| Predicted|
|:-----------------|------------:|--------:|---------:|
|Ken Griffey       |            1|      0.0|      90.5|
|Trevor Hoffman    |            1|      0.0|      76.1|
|Mike Piazza       |            4|     69.9|      68.8|
|Billy Wagner      |            1|      0.0|      63.6|
|Jeff Bagwell      |            6|     55.7|      56.0|
|Tim Raines        |            9|     55.0|      55.3|
|Curt Schilling    |            4|     39.2|      36.6|
|Roger Clemens     |            4|     37.5|      34.6|
|Barry Bonds       |            4|     36.8|      33.7|
|Lee Smith         |           14|     30.2|      26.0|
|Alan Trammell     |           15|     25.1|      22.5|
|Edgar Martinez    |            7|     27.0|      22.4|
|Mike Mussina      |            3|     24.6|      20.0|
|Jeff Kent         |            3|     14.0|      11.2|
|Fred McGriff      |            7|     12.9|      10.5|
|Larry Walker      |            6|     11.8|       9.8|
|Gary Sheffield    |            2|     11.7|       8.8|
|Mark McGwire      |           10|     10.0|       8.7|
|Sammy Sosa        |            4|      6.6|       6.9|
|Nomar Garciaparra |            2|      5.5|       5.6|
|Jim Edmonds       |            1|      0.0|       3.8|
|Mike Sweeney      |            1|      0.0|       3.1|
|Garret Anderson   |            1|      0.0|       2.6|
|Jason Kendall     |            1|      0.0|       1.1|
|Luis Castillo     |            1|      0.0|       1.0|
|Mike Lowell       |            1|      0.0|       0.7|
|Mark Grudzielanek |            1|      0.0|       0.4|
|Brad Ausmus       |            1|      0.0|       0.3|
|Troy Glaus        |            1|      0.0|       0.2|
|Randy Winn        |            1|      0.0|       0.2|
|Mike Hampton      |            1|      0.0|       0.1|
|David Eckstein    |            1|      0.0|       0.1|


<br>
<br>

-  Overall these predictions don't look great, expecially factoring in the early results from about 140 publicly-shared ballots, which have been collected and published by Ryan Thibs on his <a href="http://www.bbhoftracker.com/">BBHOF Tracker</a>.
  - The model has Ken Griffey Jr. getting 90% of the votes, but from early ballots and pulbic opinion, it looks like he'll get close to 100%. He was a great slugger and fielder (and a popular player), and perhaps most importantly, he was never suspected (to my knowledge) of using illegal performance-enhancing drugs (PEDs). The model does, in fact, contain variables to account for these characterstics (Gold gloves for fielding, All-Star games for popularity, and Mitchell Report mentions + MLB suspensions to flag PED users and suspects). But if Griffey Jr. gets close to 100% of the vote, it seems that there is still something else going on that the model hasn't accounted for that gives him an additional boost in voting relative to other 90's sluggers.
-  I think returning candidates Bagwell (6th year), Piazza (4th year), and Mussina (3rd year) will get many more votes than the model predicts. For returning players, the model almost always produces a prediction similar to the previous year, which usually has worked well, historically, but this year looks a bit different so far. (Regarding Mussina: Interestingly, the model overestimated his vote percentage in his first year on the ballot -- 42% predicted vs. 20% actual -- maybe the ballot was simply crowded in 2014 and 2015 and now voters are catching up and giving him their votes).
-  First-ballot relieveres Trevor Hoffman and Billy Wagner have high predictions. If they get significantly fewer votes than predicted, maybe this means that the value of relief pitching statistics is declining.
-  Last, about 100 voters were purged from the ballot this year, bringing down the total number of HOF voters from about 550 to an estimated 450. This might have a *huge* effect on the accuracy of these predictions if the 100 voters who were purged tended to vote differently from the remaining voters. For baseball fans and the HOF, it's probably a good thing, but for statistical models trained on historical data, it's a bummer :)
 


Kenny Shirley<br>
Jan 3, 2016

