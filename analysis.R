#***********************************************************************************
#
# file: analysis.R
# author: Pat Schloss
# date: July 17, 2018
#
# Purpose: This script runs the analysis to validat the 538 ELO model and other
# models for predicting who will win individual baseball games.
#
#***********************************************************************************

library(tidyverse)
library(lubridate)

current_date <- now()

game_data <- read_csv(file="https://projects.fivethirtyeight.com/mlb-api/mlb_elo.csv",
											col_types=cols(date=col_date(),
																		 season=col_integer(),
																		 rating_prob1=col_double(),
																		 rating_prob2=col_double(),
																		 score1=col_integer(),
																		 score2=col_integer())
											) %>%
						filter(date < current_date)


favorite_win_prob <- game_data %>%
	mutate(fav_538_won=ifelse(rating_prob1>rating_prob2, score1 > score2, score2 > score1),
				 fav_538_prob=ifelse(rating_prob1>rating_prob2, rating_prob1, rating_prob2)) %>%
	select(season, date, team1, team2, fav_538_won, fav_538_prob)


