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
library(broom)

current_date <- now()


# Load and format baseball games that have already been played
game_data <- read_csv(file="https://projects.fivethirtyeight.com/mlb-api/mlb_elo.csv",
											col_types=cols(date=col_date(),
																		 season=col_integer(),
																		 rating_prob1=col_double(),
																		 rating_prob2=col_double(),
																		 score1=col_integer(),
																		 score2=col_integer())
											) %>%
						filter(date < current_date)


# Ascertain the favorite and whether they won according to the 538 ELO model
favorite_win_prob <- game_data %>%
	mutate(fav_538_won=ifelse(rating_prob1>rating_prob2, score1 > score2, score2 > score1),
				 fav_538_prob=ifelse(rating_prob1>rating_prob2, rating_prob1, rating_prob2)) %>%
	select(season, date, team1, team2, fav_538_won, fav_538_prob)


overall_win_prob <- mean(favorite_win_prob$fav_538_won)

#Plot the fraction games that the favorite has won over the history of baseball
favorite_win_prob %>%
	group_by(season) %>%
	summarize(fraction_favorite_won = mean(fav_538_won)) %>%
	ggplot(aes(x=season, y=fraction_favorite_won)) +
	geom_hline(aes(yintercept=overall_win_prob), color="lightgray") +
	geom_line() + 
	theme_classic() +
	coord_cartesian(ylim=c(0,1)) +
	labs(x="Season", y="Fraction of games favorite won",
			 title="The 538 model does a better than average job of predicting the winner of baseball games",
			 subtitle=paste0("Since 1871, the favorite has won ",  round(100*overall_win_prob, digits=1),"% of their games"))



# Plot the observed versus expected fraction of games won by the favorite
all_predicted_observed <- favorite_win_prob %>% 
	mutate(fav_538_prob = round(fav_538_prob, digits=2)) %>%
	group_by(fav_538_prob) %>%
	summarize(games = n(),
						wins = sum(fav_538_won),
						observed = wins / games)

binomial_fit_validation <- all_predicted_observed %>%
	mutate(prob = fav_538_prob) %>%
	group_by(fav_538_prob) %>%
	nest() %>%
	mutate(binomial = map(data, function(df)
																	tidy(binom.test(x=as.integer(df$games * df$prob), 
																									n=df$games),
																			 						p=df$prob
																									)
																			 )
												) %>%
	unnest() %>%
	select(fav_538_prob, games, wins, observed, conf.low, conf.high)



binomial_fit_validation %>%
	ggplot(aes(x=fav_538_prob, y=observed)) +
	geom_ribbon(aes(ymin=conf.low, ymax=conf.high), fill="lightgray") +
	geom_abline(aes(intercept=0, slope=1), color="darkgray") +
	geom_point() +
	theme_classic() + 
	coord_cartesian(ylim=c(0,1)) +
	labs(x="Predicted Probability of Winning",
			 y="Observed Probability of Winning",
			 title="The 538 model underpredicts the true ability of the favorite to win",
			 subtitle="All games from 1871 to present")

























