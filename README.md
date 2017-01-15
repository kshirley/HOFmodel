## Baseball Hall of Fame Predictions

This repo contains R code to predict the Baseball Hall of Fame vote percentage that a given player will receive based on his career statistics. It started in 2013 as a visualization project (see [@cscheid](https://github.com/cscheid)'s [mlb-hall-of-fame-voting](https://github.com/cscheid/mlb-hall-of-fame-voting?source=c) repo for an interactive visualization of baseball HOF voting over the years), and since then I've fit a few simple statistical models to try to find out how predictable vote percentages are based solely on a player's statistics.

2017 will be the fourth year I've produced these predictions. For the 2014 predictions and a writeup of the models, see <a href='http://kshirley.github.io/HOFmodel'>here</a>.

I gave a meetup talk about this project in August of 2014 in NYC (slides <a href="http://www.kennyshirley.com/nycmeetup.html">here</a>), and shared predictions for the 2015 ballot at that time (about 3 months before the ballots were sent out).

<b>[Updated on Jan 15, 2017]:</b>

Below are the model's predictions for 2017:

|Name              | YearOnBallot| Previous| Predicted|
|:-----------------|------------:|--------:|---------:|
|Manny Ramirez     |            1|      0.0|      92.8|
|Ivan Rodriguez    |            1|      0.0|      88.3|
|Trevor Hoffman    |            2|     67.3|      73.3|
|Jeff Bagwell      |            7|     71.6|      70.9|
|Tim Raines        |           10|     69.8|      69.6|
|Curt Schilling    |            5|     52.3|      53.6|
|Roger Clemens     |            5|     45.2|      45.3|
|Barry Bonds       |            5|     44.3|      44.2|
|Edgar Martinez    |            8|     43.4|      43.1|
|Mike Mussina      |            4|     43.0|      42.6|
|Vladimir Guerrero |            1|      0.0|      36.4|
|Lee Smith         |           15|     34.1|      34.4|
|Fred McGriff      |            8|     20.9|      17.3|
|Jeff Kent         |            4|     16.6|      13.6|
|Larry Walker      |            7|     15.5|      12.7|
|Gary Sheffield    |            3|     11.6|      10.1|
|Billy Wagner      |            2|     10.5|       8.3|
|Sammy Sosa        |            5|      7.0|       7.5|
|Magglio Ordonez   |            1|      0.0|       4.2|
|Jorge Posada      |            1|      0.0|       3.6|
|Edgar Renteria    |            1|      0.0|       3.4|
|Derrek Lee        |            1|      0.0|       1.7|
|Tim Wakefield     |            1|      0.0|       1.3|
|Jason Varitek     |            1|      0.0|       0.5|
|Carlos Guillen    |            1|      0.0|       0.3|
|Orlando Cabrera   |            1|      0.0|       0.3|
|Freddy Sanchez    |            1|      0.0|       0.3|
|Melvin Mora       |            1|      0.0|       0.2|
|J.D. Drew         |            1|      0.0|       0.1|
|Arthur Rhodes     |            1|      0.0|       0.1|
|Matt Stairs       |            1|      0.0|       0.1|
|Mike Cameron      |            1|      0.0|       0.0|
|Casey Blake       |            1|      0.0|       0.0|
|Pat Burrell       |            1|      0.0|       0.0|

<br>

These are pretty far off what we're seeing from Ryan Thibodaux's BB HOF tracker, where
he tallies the votes that are published before the official announcement:

http://www.bbhoftracker.com/

Lots of problems with my model:

- Manny is a shoo-in according to my model (92.8%), but only has about 25% of the ballots
according to the tracker. I have a variable in my model for PED use; but it encodes the same value for players (1) named in the Mitchell report, (2) suspended by MLB once, and (3) suspended by MLB 2 or more times. Manny was suspended twice, and is literally the top image on the wikipedia page for "list of players suspended by MLB for PED use":

https://en.wikipedia.org/wiki/List_of_Major_League_Baseball_players_suspended_for_performance-enhancing_drugs

So maybe I'm not using as fine-grained a variable as necessary for the effect of PED usage.

- Because of the rule change last year, this is Tim Raines' last appearance on the ballot, and so he has over 90% support on early ballots according to the BBHOF tracker. My model has him at about 69%, similar to last year. I need up update my model to allow a boost in the 10th year (it already allows for on in the 15th year on the ballot, which was traditionally the last chance).

- The model looks pretty accurate for Pudge Rodriguez (model: 88%, current BBHOF tracker: 80%), but it's terrible for Vlad Guerrero (model: 36% vs. BBHOF tracker: 74%)! I don't know him well enough to have an opinion on why he's getting so much support (or got so little from the model). The only thing I can see is that he never won a gold glove (which would have boosted him in my model), but I know he was well-known for his arm, which might boost him in the eyes of voters.

- Bonds, Clemens, Edgar Martinez, and Mussina are all getting big increases on BBHOF tracker, where my model has them all as basically the same as they were last year. I think it's partly because of Manny and Pudge: I have a variable for the expected vote from 1st ballot players, and if there are a few 1st ballot players with large predictions, then this pushes down vote predictions for others on the ballot. I obviously overestimated Manny, so maybe a lot of those votes are going instead to Bonds, Clemens, Edgar, and Mussina. Interestingly, maybe Manny will get a wave of support in about 3-5 years, like PED-tarnished Bonds and Clemens. Also Bonds and Clemens seem to be benefitting from the election of Bud Selig by the Veterans Committee (since he presided over the steroid era, many voters seem to be more inclined to vote in players tarnished by steroids allegations). This is pretty tough to model...

- My model is missing Bagwell (my prediction is still around 71%, but BBHOF has him at about 90%). Maybe I need some sort of "pushing him over the threshold" variable. He was close last year (71.6%), and it's rare to get that close and not get elected the following year.

- My model has a "played for only one team in his career" variable, and it's kind of cool that the only first-ballot players to whom this variable applies this year are Jorge Posada and Jason Varitek. I hadn't realized they retired the same year. Catchers for rival teams with somewhat mirror-image careers.

Kenny Shirley<br>
Jan 15, 2017


<b>[Continued text from 2016 predictions]:</b>

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
