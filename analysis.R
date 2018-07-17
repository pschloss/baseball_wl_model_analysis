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
library("wesanderson")


get_wp <- function(A, B){
	
	if((A == 0 && B == 0) || (A == 1 && B == 1)){
		0.5
	} else {
		A * (1-B) / (A*(1-B) + B*(1-A))
	}
	
}



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
	select(season, date, team1, team2, score1, score2, fav_538_won, fav_538_prob)


win_losses_live <- favorite_win_prob %>%
	mutate(win1=score1>score2,
				 win2=score2>score1,
				 team_win1 = paste(team1, win1, sep="_"),
				 team_win2 = paste(team2, win2, sep="_")) %>%
	gather(one_two, team_win, team_win1, team_win2) %>%
	separate(team_win, into=c("team", "win"), sep="_", convert=TRUE) %>%
	arrange(date) %>%
	group_by(season, team) %>%
	mutate(wins=c(0, na.omit(lag(cumsum(win)))),
				 losses=c(0, na.omit(lag(cumsum(!win)))),
				 avg=c(0, na.omit(wins/(wins+losses)))
				 )  %>% 
	ungroup() %>%
	select(season, team, date, avg)
	

win_losses_season <- favorite_win_prob %>%
	mutate(win1=score1>score2,
				 win2=score2>score1,
				 team_win1 = paste(team1, win1, sep="_"),
				 team_win2 = paste(team2, win2, sep="_")) %>%
	gather(one_two, team_win, team_win1, team_win2) %>%
	separate(team_win, into=c("team", "win"), sep="_", convert=TRUE) %>%
	group_by(team, season) %>%
	summarize(current_avg = mean(win)) %>%
	mutate(prev_avg = c(0.5, na.omit(lag(current_avg)))) %>%
	ungroup()



# join in win_losses_live into favorite_win_prob
favorite_win_prob <- favorite_win_prob %>%
	inner_join(., win_losses_live, by=c("team1"="team", "season", "date")) %>%
	inner_join(., win_losses_live, by=c("team2"="team", "season", "date")) %>%
	mutate(win_prob1=map2_dbl(avg.x, avg.y, get_wp),
				 win_prob2=1-win_prob1,
				 fav_wplive_won=ifelse(win_prob1 > win_prob2, score1 > score2, score2 > score1),
				 fav_wplive_prob=ifelse(win_prob1 > win_prob2, win_prob1, win_prob2)) %>%
	select(season, date, team1, team2, score1, score2, fav_538_won, fav_538_prob, fav_wplive_won, fav_wplive_prob)
	

# join in win_losses_season into favorite_win_prob
favorite_win_prob <- favorite_win_prob %>%
	inner_join(., win_losses_season, by=c("team1"="team", "season")) %>%
	inner_join(., win_losses_season, by=c("team2"="team", "season")) %>%
	mutate(win_prob1=map2_dbl(current_avg.x, current_avg.y, get_wp),
				 win_prob2=1-win_prob1,
				 fav_wpcurrent_won=ifelse(win_prob1 > win_prob2, score1 > score2, score2 > score1),
				 fav_wpcurrent_prob=ifelse(win_prob1 > win_prob2, win_prob1, win_prob2),
				 
				 win_prob1=map2_dbl(prev_avg.x, prev_avg.y, get_wp),
				 win_prob2=1-win_prob1,
				 fav_wpprev_won=ifelse(win_prob1 > win_prob2, score1 > score2, score2 > score1),
				 fav_wpprev_prob=ifelse(win_prob1 > win_prob2, win_prob1, win_prob2)
				 ) %>%
	select(season, date, team1, team2, score1, score2, fav_538_won, fav_538_prob, fav_wplive_won, fav_wplive_prob, fav_wpcurrent_won, fav_wpcurrent_prob, fav_wpprev_won, fav_wpprev_prob)


# make the data frame "tidy"
tidy_win_prob <- favorite_win_prob %>% 
	mutate(fte=paste(fav_538_won, fav_538_prob, sep="_"),
				 wplive=paste(fav_wplive_won, fav_wplive_prob, sep="_"),
				 wpcurrent=paste(fav_wpcurrent_won, fav_wpcurrent_prob, sep="_"),
				 wpprev=paste(fav_wpprev_won, fav_wpprev_prob, sep="_")) %>%
	select(-starts_with("fav")) %>%
	gather(model, won_prob, fte, wplive, wpcurrent, wpprev) %>%
	separate(won_prob, into=c("won", "prob"), sep="_", convert=TRUE)



overall_win_prob <- tidy_win_prob %>%
	group_by(model) %>%
	summarize(mean=mean(won))


# Plot the fraction games that the favorite has won over the history of baseball
tidy_win_prob %>%
	group_by(season, model) %>%
	summarize(fraction_favorite_won = mean(won)) %>%
	ungroup() %>%
	ggplot(aes(x=season, y=fraction_favorite_won, group=model, color=model)) +
	geom_hline(data=overall_win_prob, aes(yintercept=mean, group=model, color=model)) +
	geom_line() + 
	theme_classic() +
	coord_cartesian(ylim=c(0,1)) +
	labs(x="Season", y="Fraction of games favorite won",
			 title="The Winning Percentage model can out perform the 538 ELO model if it uses end of\nseason winning averages") +
	scale_color_manual(name=NULL,
										 breaks=c("fte", "wpcurrent", "wplive", "wpprev"),
										 labels=c("538", "WP Curent", "WP Live", "WP Previous"),
										 values=wes_palette("Darjeeling2"))



# Plot the observed versus expected fraction of games won by the favorite
tidy_win_prob %>% 
	mutate(prob = round(prob, digits=2)) %>%
	group_by(prob, model) %>%
	summarize(games = n(),
						wins = sum(won),
						observed = wins / games) %>%
	ggplot(aes(x=prob,  y=observed, group=model, color=model)) +
		geom_abline(aes(intercept=0, slope=1), color="gray") +
		geom_line() +
		theme_classic() + 
		scale_color_manual(name=NULL,
										 breaks=c("fte", "wpcurrent", "wplive", "wpprev"),
										 labels=c("538", "WP Curent", "WP Live", "WP Previous"),
										 values=wes_palette("Darjeeling2")) +
		labs(x="Predicted Win Probability", y="Observed Win Probability",
				 title="The 538 and WP Current models generate more reliable win probabilities than the\nWP Live or Previous models",
				 subtitle="All data since 1871")






















