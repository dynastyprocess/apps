## About the DynastyProcess Expected Points Model

Expected Points is inspired by many different sources, but the idea is simple: 
> How many fantasy points should a player have scored on the opportunities they were given?

Our hope is to put a new spin on it while drawing from the work of Kevin Cole, Josh Hermsmeyer, Mike Clay and many others in the industry. 

You can read more about Kevin Cole's methodologies here:
https://predictivefootball.com/using-air-yards-to-calculate-expected-fantasy-points/
https://predictivefootball.com/week-2-expected-fantasy-points-rushing-and-receiving/ 

Our models use many of the same components, but with a few new innovations. Here's a brief rundown of features:

- Using MARS models, we searched for interactions and non-linear relationships in our important variables (http://uc-r.github.io/mars). 
- Everyone knows air yards are king in modelling passing/receiving stats. Along with that, we found that down, yardline, game script, middle of the field targets and QB Hits also play a significant role. 
- Two new variables we've included are QB completion percentage and receiver YAC average (both to-date). When we look for good situations we always consider the QB who threw it so why shouldn't our EP model? A pass from Russell Wilson should be expected to be better than the same pass from Sam Darnold. 
- Rushing yards are always hard to predict (esp without tracking data, see the Big Data Bowl) but our model incorporates yardline, direction, QB scrambles, and the ball carrier's YPC to date.

Finally, we use our predictions of the individual components of fantasy scoring (rush/pass/receiving yards, TDs, receptions) to predict the expected fantasy points on each play. We've tried to make as much of the data accessible for you to explore across the three different tabs, but let us know on Twitter if you have any questions on our methodology or data. 

This wouldn't be possible without the fantastic resource that is nflfastR - both for training the model as well as allowing us extensive access to the 2020 NFL season. Thanks to Julia Silge (https://twitter.com/juliasilge) for her informative tidymodel examples and Tom Mock (https://twitter.com/thomas_mock) for his useful reactable and nflfastR plotting resources.

