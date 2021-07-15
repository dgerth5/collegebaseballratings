# install.packages("readxl")
library(readxl)

# data from
df = readxl::read_xlsx("C:\\Users\\david\\OneDrive\\Documents\\college baseball 2019.xlsx")

## clean data ##
library(tidyverse)
library(stringr)

# get rid of @'s
at = as.data.frame(str_split_fixed(df$Column6, "@", 2))

# add home team column
df = cbind(df, home_team = at$V2)
# drop old @ column
df = df[, -6]

# configuring home score/mov and away team/score
df = df %>%
  mutate(home_score = if_else(Column2 == home_team, Column3, Column5),
         away_team = if_else(Column2 == home_team, Column4, Column2),
         away_score = if_else(Column2 == home_team, Column5, Column3),
         h_mov = home_score - away_score,
         h_win = if_else(h_mov > 0, 1, 0))
df = select(df, -(Column1:Column5)) # drop useless columns

## now to split data into train/test ##
## goal is to create two types of models ##
## one is to predict team runs scored based on team strength, opponent strength, and home (yes/no) ##
## the other is to predict home win/loss based on home team and away team ##
## the first requires additional data wrangling ##

# train/test #
require(caTools)

set.seed(5)
ss = sample.split(df$home_team, SplitRatio = 0.7)
train = subset(df, ss == TRUE)
test = subset(df, ss == FALSE)

# now edit, need columns for team_score, team, opponent, home (yes/no)(1,0)#

home.t = train %>%
  select(home_score, home_team, away_team) %>%
  mutate(home = 1)
colnames(home.t)[1] <- "team_score"
colnames(home.t)[2] <- "team"
colnames(home.t)[3] <- "opponent"

away.t = train %>%
  select(away_score, away_team, home_team) %>%
  mutate(home = 0)
colnames(away.t)[1] <- "team_score"
colnames(away.t)[2] <- "team"
colnames(away.t)[3] <- "opponent"

option1.train = rbind(home.t,away.t)

home.te = test %>%
  select(home_score, home_team, away_team) %>%
  mutate(home = 1)
colnames(home.te)[1] <- "team_score"
colnames(home.te)[2] <- "team"
colnames(home.te)[3] <- "opponent"

away.te = test %>%
  select(away_score, away_team, home_team) %>%
  mutate(home = 0)
colnames(away.te)[1] <- "team_score"
colnames(away.te)[2] <- "team"
colnames(away.te)[3] <- "opponent"

option1.test = rbind(home.te,away.te)

## now run mixed models ##
library(fitdistrplus)
library(lme4)

descdist(option1.train$team_score, discrete = TRUE, boot = 1000) # checking how team_Score is distributed
# negative binomial
m0 = glmer.nb(team_score ~ home + (1|team) + (1|opponent), data = option1.train, verbose = TRUE, nAGQ = 0, control=glmerControl(optimizer = "nloptwrap"))

# now option 2
m1 = glmer(h_win ~ 1 + (1|home_team) + (1|away_team), data = test, family = binomial(), verbose = TRUE, nAGQ = 0, control=glmerControl(optimizer = "nloptwrap"))

## tests ##

m0.pred = predict(m0, option1.test, type = "response", allow.new.levels = TRUE)
m1.pred = predict(m1, test, type = "response")

option1.test = cbind(option1.test, pred = m0.pred)
test = cbind(test, pred = m1.pred)
test = test %>%
  mutate(fun = if_else(pred > .5, 1, 0))

option1 = option1.test %>%
  filter(home == 1)
option11 = option1.test %>%
  filter(home == 0)
away_pred = option11[, -c(1:4)]

master = cbind(option1, away_pred, test$h_win)
master = master %>%
  mutate(pred_win = if_else(pred > away_pred, 1, 0))

library(MLmetrics)

LogLoss(test$pred, test$h_win)

Accuracy(master$pred_win, master$`test$h_win`)
Accuracy(test$fun, test$h_win)

# the second model (m1) wins
# correctly chose 72% of the games, which was better than m0 65%.

