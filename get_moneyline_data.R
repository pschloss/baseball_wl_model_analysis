library(tidyverse)
library(lubridate)
library(rvest)


parse_table_row <- function(row) {
	
	date <- row %>% html_nodes("div.status-complete") %>% html_attr("rel")
	team_names <- row %>% html_nodes("span.team-name") %>% html_text()
	scores <- row %>% html_nodes("span.total") %>% html_text()
	money_line <- row %>% html_node("div.eventLine-book") %>% html_nodes("div.eventLine-book-value") %>% html_text()
	
	tibble(date =as.Date(str_replace(date, "\\s.*", "")),
				 team1=str_replace(team_names[1], "\\s-.*\\n.*", ""),
				 team2=str_replace(team_names[2], "\\s-.*\\n.*", ""),
				 score1=as.numeric(scores[1]),
				 score2=as.numeric(scores[2]),
				 money_line1=as.numeric(str_replace(money_line[1], "\\+", "")),
				 money_line2=as.numeric(str_replace(money_line[2], "\\+", ""))
	)
	
}


get_moneyline_data <- function(url) {
	
	print(url)
	
	read_html(url) %>% 
		html_nodes("div.holder-complete") %>%
		map_dfr(., parse_table_row)
	
}


html_pages <- favorite_win_prob %>%
	filter(date >= "2009-01-01") %>%
	pull(date) %>%
	unique() %>%
	str_replace_all(., "-", "") %>%
	paste0("https://www.sportsbookreview.com/betting-odds/mlb-baseball/?date=", .)

money_line <- map_dfr(html_pages, get_moneyline_data)

write_csv(path="data/money_line.csv", money_line)
