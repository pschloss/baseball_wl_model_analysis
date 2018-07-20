#***********************************************************************************
#
# file: betting_simulation.R
# author: Pat Schloss
# date: July 20, 2018
#
# Purpose: Simulate how much money we'd make or lose based on betting $100 on each
# of the favorites from our models
#
#***********************************************************************************

library(tidyverse)
library(lubridate)
library(broom)
library("wesanderson")

get_payout <- function(moneyline, bet=100){
	
	ifelse(moneyline < 0, -bet * 100/moneyline, moneyline)

}


fwp <- c("ANA", "ARI", "ATL", "BAL", "BOS", "CHC", "CHW", "CIN", "CLE", "COL",
				 "DET", "FLA", "HOU", "KCR", "LAD", "MIL", "MIN", "NYM", "NYY", "OAK",
				 "PHI", "PIT", "SDP", "SEA", "SFG", "STL", "TBD", "TEX", "TOR", "WSN")

ml <- c("LAA", "ARI", "ATL", "BAL", "BOS", "CHC", "CWS", "CIN", "CLE", "COL",
				"DET", "MIA", "HOU", "KC", "LAD", "MIL", "MIN", "NYM", "NYY", "OAK",
				"PHI", "PIT", "SD", "SEA", "SF", "STL", "TB", "TEX", "TOR", "WSH")

name_convert <- tibble(fwp=fwp, ml=ml)


payout <- read_csv("data/money_line.csv") %>%
	drop_na() %>%
	group_by(date, team1, team2) %>%
	mutate(game=1:n()) %>%
	ungroup() %>% 
	mutate(money_prob1=map_dbl(money_line1, get_moneyline_prob),
				 money_prob2=map_dbl(money_line2, get_moneyline_prob),
				 fav_money_won=ifelse(money_prob1 > money_prob2, score1 > score2, score2 > score1),
				 fav_money_prob=ifelse(money_prob1 > money_prob2, money_prob1, money_prob2),
				 money_payout1=get_payout(money_line1),
				 money_payout2=get_payout(money_line2)) %>%
	inner_join(., name_convert, by=c("team1"="ml")) %>%
	inner_join(., name_convert, by=c("team2"="ml")) %>%
	select(-team1, -team2) %>%
	rename(team1=fwp.x, team2=fwp.y) %>%
	mutate(team_payout1=paste(team1, money_payout1, sep="_"),
				 team_payout2=paste(team2, money_payout2, sep="_")) %>%
	gather(one_two, team_payout, team_payout1, team_payout2) %>%
	select(-money_line1, -money_line2, -money_payout1, -money_payout2, -one_two) %>%
	separate(team_payout, into=c("team", "payout"), sep="_", convert=TRUE) %>%
	select(-money_prob1, -money_prob2, -fav_money_won, -fav_money_prob)

daily_winnings <- read_csv("data/model_data.csv") %>%
	inner_join(., payout, by=c("date", "game", "team1", "team2", "score1", "score2", "team")) %>%
	mutate(payout=ifelse(won, payout, -100)) %>% 
	group_by(season, date, model) %>%
	summarize(days_winnings=sum(payout)) %>%
	ungroup() %>%
	group_by(season, model) %>%
	mutate(cumulative_winnings = cumsum(days_winnings)) %>%
	ungroup()

annual_winnings <- daily_winnings %>%
	mutate(year = year(date)) %>%
	group_by(model, year) %>%
	summarize(years_winnings = sum(days_winnings)) %>%
	ungroup()

total_winnings <- annual_winnings %>%
	group_by(model) %>%
	summarize(total_winnings = sum(years_winnings)) %>%
	arrange(desc(total_winnings))


daily_winnings %>%
	filter(model=="fte" | model=="money" |  model=="wpcurrent") %>%
	ggplot(aes(x=date, y=cumulative_winnings, group=model, color=model)) +
		geom_hline(aes(yintercept=0)) +
		geom_line() +
		facet_grid(.~season, scales="free_x") +
		scale_color_manual(name=NULL,
											 breaks=c("fte", "money", "wpcurrent"),
											 labels=c("538", "Moneyline", "WP Curent"),
											 values=wes_palette("Darjeeling2")) +
		labs(x="Date", y="Winnings",
				 title="Don't gamble based on the models' predictions!",
				 subtitle="All data since 2009") +
		theme_classic()


