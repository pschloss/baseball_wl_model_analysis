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
																		 rating1_post=col_double(),
																		 rating2_post=col_double(),
																		 score1=col_integer(),
																		 score2=col_integer())
											) %>%
						filter(date < current_date)


