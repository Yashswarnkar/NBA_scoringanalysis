# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

hello <- function() {
  print("Hello, world!")
}
nba=read.csv("NBA_train.csv")
nba_test=read.csv("NBA_test.csv")
str(nba)
## trying to see relationship between wins and making to playoffs
table(nba$W,nba$Playoffs)
## after 35 wins , chances we see lesser number of teams not making to playoff and
## after 42-43 wins teams generally made it to the playsoff


## trying to see relationship between baskets done and conceded against wins
nba$PTSdiff=nba$PTS-nba$oppPTS
plot(nba$PTSdiff,nba$W)
## i observe a highly linear data with very little ouliers
wins=lm(W~PTSdiff,data=nba)
summary(wins)
pointsreg=lm(PTS~FGA+X2PA+X3PA+FTA+ORB+STL+BLK+TOV+AST,data=nba)
summary(pointsreg)
pointsreg1=lm(PTS~FGA+X2PA+X3PA+FTA+ORB+STL+BLK+AST,data=nba)
summary(pointsreg1)
pointsreg2=lm(PTS~FGA+X2PA+X3PA+FTA+ORB+STL+AST,data=nba)
summary(pointsreg1)
## generally removing those features with maximum p-value , also observed same Rsquared
rss=sum(pointsreg2$residuals^2)
rmse=sqrt(rss/nrow(nba))
rmse
prediction=predict(pointsreg2,newdata =nba_test )
