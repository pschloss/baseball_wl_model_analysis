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
library(rvest)

get_wp <- function(A, B){

	if((A == 0 && B == 0) || (A == 1 && B == 1)){
		0.5
	} else {
		A * (1-B) / (A*(1-B) + B*(1-A))
	}

}

get_moneyline_prob <- function(x){
	if(x < 100) {
		-x/(-x+100)
	} else {
		100 / (x+100)
	}
}

current_date <- today()


# Load and format baseball games that have already been played
game_data <- read_csv(file="https://projects.fivethirtyeight.com/mlb-api/mlb_elo.csv",
											col_types=cols(date=col_date(),
																		 season=col_integer(),
																		 rating_prob1=col_double(),
																		 rating_prob2=col_double(),
																		 score1=col_integer(),
																		 score2=col_integer())
											) %>%
						filter(date < current_date)	%>%
						group_by(date, team1, team2) %>%
						mutate(game=1:n()) %>%
						ungroup() 


# Ascertain the favorite and whether they won according to the 538 ELO model
favorite_win_prob <- game_data %>%
	mutate(fav_538_won=ifelse(rating_prob1>rating_prob2, score1 > score2, score2 > score1),
				 fav_538_prob=ifelse(rating_prob1>rating_prob2, rating_prob1, rating_prob2),
				 fav_538_team=ifelse(rating_prob1>rating_prob2, team1, team2),
				) %>%
	select(season, game, date, team1, team2, score1, score2, fav_538_won, fav_538_prob, fav_538_team)


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
	select(season, team, date, game, avg)


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
	inner_join(., win_losses_live, by=c("team1"="team", "season", "date", "game")) %>%
	inner_join(., win_losses_live, by=c("team2"="team", "season", "date", "game")) %>%
	mutate(win_prob1=map2_dbl(avg.x, avg.y, get_wp),
				 win_prob2=1-win_prob1,
				 fav_wplive_won=ifelse(win_prob1 > win_prob2, score1 > score2, score2 > score1),
				 fav_wplive_prob=ifelse(win_prob1 > win_prob2, win_prob1, win_prob2),
				 fav_wplive_team=ifelse(win_prob1 > win_prob2, team1, team2)) %>%
	select(season, date, game, team1, team2, score1, score2,
				 fav_538_won, fav_538_prob, fav_538_team,
				 fav_wplive_won, fav_wplive_prob, fav_wplive_team)


# join in win_losses_season into favorite_win_prob
favorite_win_prob <- favorite_win_prob %>%
	inner_join(., win_losses_season, by=c("team1"="team", "season")) %>%
	inner_join(., win_losses_season, by=c("team2"="team", "season")) %>%
	mutate(win_prob1=map2_dbl(current_avg.x, current_avg.y, get_wp),
				 win_prob2=1-win_prob1,
				 fav_wpcurrent_won=ifelse(win_prob1 > win_prob2, score1 > score2, score2 > score1),
				 fav_wpcurrent_prob=ifelse(win_prob1 > win_prob2, win_prob1, win_prob2),
				 fav_wpcurrent_team=ifelse(win_prob1 > win_prob2, team1, team2),
				 
				 win_prob1=map2_dbl(prev_avg.x, prev_avg.y, get_wp),
				 win_prob2=1-win_prob1,
				 fav_wpprev_won=ifelse(win_prob1 > win_prob2, score1 > score2, score2 > score1),
				 fav_wpprev_prob=ifelse(win_prob1 > win_prob2, win_prob1, win_prob2),
				 fav_wpprev_team=ifelse(win_prob1 > win_prob2, team1, team2)) %>%
	select(season, date, game, team1, team2, score1, score2,
				 fav_538_won, fav_538_prob, fav_538_team, 
				 fav_wplive_won, fav_wplive_prob, fav_wplive_team,
				 fav_wpcurrent_won, fav_wpcurrent_prob, fav_wpcurrent_team,
				 fav_wpprev_won, fav_wpprev_prob, fav_wpprev_team)


fwp <- c("ANA", "ARI", "ATL", "BAL", "BOS", "CHC", "CHW", "CIN", "CLE", "COL",
"DET", "FLA", "HOU", "KCR", "LAD", "MIL", "MIN", "NYM", "NYY", "OAK",
"PHI", "PIT", "SDP", "SEA", "SFG", "STL", "TBD", "TEX", "TOR", "WSN")

ml <- c("LAA", "ARI", "ATL", "BAL", "BOS", "CHC", "CWS", "CIN", "CLE", "COL",
"DET", "MIA", "HOU", "KC", "LAD", "MIL", "MIN", "NYM", "NYY", "OAK",
"PHI", "PIT", "SD", "SEA", "SF", "STL", "TB", "TEX", "TOR", "WSH")

name_convert <- tibble(fwp=fwp, ml=ml)

favorite_win_prob <- read_csv("data/money_line.csv") %>%
	drop_na() %>%
	group_by(date, team1, team2) %>%
	mutate(game=1:n()) %>%
	ungroup() %>% 
	mutate(money_prob1=map_dbl(money_line1, get_moneyline_prob),
				 money_prob2=map_dbl(money_line2, get_moneyline_prob),
				 fav_money_won=ifelse(money_prob1 > money_prob2, score1 > score2, score2 > score1),
				 fav_money_prob=ifelse(money_prob1 > money_prob2, money_prob1, money_prob2),
				 fav_money_team=ifelse(money_prob1 > money_prob2, team1, team2)
				) %>%
	inner_join(., name_convert, by=c("team1"="ml")) %>%
	inner_join(., name_convert, by=c("team2"="ml")) %>%
	select(-team1, -team2) %>%
	rename(team1=fwp.x, team2=fwp.y) %>%
	inner_join(favorite_win_prob, .,
						 by=c("date", "team1", "team2", "score1", "score2", "game")) %>%
	select(-money_line1, -money_line2, -money_prob1, -money_prob2)


# make the data frame "tidy"
tidy_win_prob <- favorite_win_prob %>%
	mutate(fte=paste(fav_538_won, fav_538_prob, fav_538_team, sep="_"),
				 wplive=paste(fav_wplive_won, fav_wplive_prob, fav_wplive_team, sep="_"),
				 wpcurrent=paste(fav_wpcurrent_won, fav_wpcurrent_prob, fav_wpcurrent_team, sep="_"),
				 wpprev=paste(fav_wpprev_won, fav_wpprev_prob, fav_wpprev_team, sep="_"),
				 money=paste(fav_money_won, fav_money_prob, fav_money_team, sep="_")
				 ) %>%
	select(-starts_with("fav")) %>%
	gather(model, won_prob, fte, wplive, wpcurrent, wpprev, money) %>%
	separate(won_prob, into=c("won", "prob", "team"), sep="_", convert=TRUE)


write_csv(tidy_win_prob, "data/model_data.csv")


overall_win_prob <- tidy_win_prob %>%
	group_by(model) %>%
	summarize(mean=mean(won)) %>%
	filter(model=="fte" | model == "money" | model == "wpcurrent")


# Plot the fraction games that the favorite has won over the history of baseball
tidy_win_prob %>%
	filter(model=="fte" | model == "money" | model == "wpcurrent") %>%
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
			breaks=c("fte", "money", "wpcurrent"),
			labels=c("538", "Moneyline", "WP Curent"),
			values=wes_palette("Darjeeling2"))



# Plot the observed versus expected fraction of games won by the favorite
tidy_win_prob %>%
	filter(model=="fte" | model == "money" | model == "wpcurrent") %>%
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
										 breaks=c("fte", "money", "wpcurrent"),
										 labels=c("538", "Moneyline", "WP Curent"),
										 values=wes_palette("Darjeeling2")) +
		labs(x="Predicted Win Probability", y="Observed Win Probability",
				 title="The thre models do an excellent job of predicting the true fraction of games that the favorite will win",
				 subtitle="All data since 2009")
