library(dplyr)
library(rvest)
library(tidyverse)
library(ggplot2)
library(data.table)
setwd("C:/Users/Lenovo/Documents/NBA Parity Project")

# Scrape team records data
for (year in 1976:2023){
  url <- paste0("https://www.basketball-reference.com/leagues/NBA_", year, ".html")

  html <- read_html(url)

  #~~~east~~~

  east_raw <- html %>% html_element("#divs_standings_E") %>% html_table()

  # clean columns
  east_clean <- east_raw
  colnames(east_clean)[1] <- 'team'
  colnames(east_clean)[4] <- 'win_pct'

  # get rid of division headers
  east_clean <- east_clean[!east_clean$team %like% "Division",]

  # finish cleaning columns
  east_clean$win_pct <- as.numeric(east_clean$win_pct)
  east_clean$SRS <- as.numeric(east_clean$SRS)
  east_clean <- east_clean %>% select(team, W, L, win_pct, SRS)



  # clean team names
  east_clean$team <- gsub("[^[:alnum:] ]","",as.character(east_clean$team))

  # add season column
  east_clean$season <- paste0(as.character(year-1),"-",substring(as.character(year),3))

  #~~~west~~~

  west_raw <- html %>% html_element("#divs_standings_W") %>% html_table()

  # clean columns
  west_clean <- west_raw
  colnames(west_clean)[1] <- 'team'
  colnames(west_clean)[4] <- 'win_pct'

  # get rid of division headers
  west_clean <- west_clean[!west_clean$team %like% "Division",]

  # finish cleaning columns
  west_clean$win_pct <- as.numeric(west_clean$win_pct)
  west_clean$SRS <- as.numeric(west_clean$SRS)
  west_clean <- west_clean %>% select(team, W, L, win_pct, SRS)

  # get rid of division headers
  west_clean <- west_clean[!west_clean$team %like% "Division",]

  # clean team names
  west_clean$team <- gsub("[^[:alnum:] ]","",as.character(west_clean$team))

  # add season column
  west_clean$season <- paste0(as.character(year-1),"-",substring(as.character(year),3))

  # stack east and west
  assign(paste0("full_",year), as.data.frame((rbind(east_clean,west_clean))))

  Sys.sleep(4)
}


# stack the years together
dataframe_names <- ls(pattern = "^full_\\d{4}$")
dataframes_list <- mget(dataframe_names)
full <- do.call(rbind, dataframes_list)

#~~~~analysis~~~~~

sd_by_season <- full %>%
  group_by(season) %>%
  summarize(season_sd=sd(win_pct)) %>%
  as.data.frame()

srs_by_season <- full %>%
  group_by(season) %>%
  summarize(season_sd_srs=sd(SRS),
            season_mean_srs=mean(SRS))

full_w_sum_stats <- left_join(full, sd_by_season, by='season')

# Check top 10 seasons by parity (sd)
sd_by_season %>%
  slice_min(season_sd,n = 10) %>%
  rename('parity'=season_sd) %>%
  mutate(rank=row_number()) %>%
  select(rank, season, parity) %>%
  knitr::kable()

# Function to get the short version of each season in case we need it
format_strings <- function(original_vector) {
  # Replace the first two characters with a single apostrophe
  modified_vector <- paste0("'", substr(original_vector, 3, nchar(original_vector)))
  # Insert a single apostrophe at the 6th position of each string
  modified_vector <- paste0(substr(modified_vector, 1, 4), "'", substr(modified_vector, 5, nchar(modified_vector)))
  return(modified_vector)
}

sd_by_season <- mutate(sd_by_season, season_short=format_strings(season))
write.csv(sd_by_season, 'sd_by_season.csv', row.names=FALSE)

# Line plot of parity by season, with three periods highlighted
rects <- data.frame(xstarts=c("1975-76","2001-02","2020-21"), xends=c("1978-79","2006-07","2022-23"))
parity_by_season <-
  ggplot() +
  geom_rect(data=rects, aes(xmin=xstarts, xmax=xends, ymin=-Inf, ymax=Inf), fill= "yellow", alpha=0.4) +
  geom_line(data=sd_by_season, mapping = aes(x=season, y=season_sd, group=1), cex=0.75) +
  geom_point(data=sd_by_season, mapping = aes(x=season, y=season_sd, group=1)) +
  scale_x_discrete(breaks=c('1975-76','1978-79',
                            '2001-02','2006-07',
                            '2020-21','2022-23')) +
  theme_minimal() +
  labs(x="", y="Standard Deviation of Team Win %", title = "The Three Eras of NBA Parity",
       caption="Jonah Wiener-Brodkey . Source: Basketball Reference") +
  theme(axis.text.x = element_text(angle = 55, vjust = 1, hjust=1, size=11, face='bold'),
        plot.title = element_text(size=16),
        plot.caption = element_text(color="#808080", hjust=0),
        axis.line.x = element_line(color="black", size = 0.3),
        axis.line.y = element_line(color="black", size = 0.3))
ggsave(file="parity_by_season.png", plot=parity_by_season, width=8, height=6)


# Count of 50-win teams and 30-win teams by season
pace_50w <- 50/82
pace_30w <- 30/82

n_50_or_30_by_season <-
  full_w_sum_stats %>%
  group_by(season) %>%
  summarize(n_50_plus=sum(win_pct>=pace_50w),
            n_30_less=sum(win_pct<=pace_30w),
            n_50_or_30=sum(n_50_plus,n_30_less),
            n_teams=n_distinct(team),
            n_50_plus_per_team=n_50_plus/n_teams,
            n_30_less_per_team=n_30_less/n_teams,
            n_50_30_per_team=n_50_or_30/n_teams,
            n_50_plus_per_30=round(n_50_plus_per_team*30),
            n_30_less_per_30=round(n_30_less_per_team*30)) %>%
  left_join(sd_by_season, by = "season")

# Table of count of 50+ teams by season
n_50_by_season <- 
  n_50_or_30_by_season %>%
  slice_min(n_50_plus_per_30,n = 10) %>%
  arrange(n_50_plus_per_30) %>%
  mutate(rank=row_number()) %>%
  select(rank, season, n_teams, n_50_plus_per_30)
write.csv(n_50_by_season, "n_50_wins_by_season.csv")

# High volume, high efficiency 3P shooters by season
# Params: GP>=41, 3PA>=5, 3P%>=0.35

for (year in 2014:2023){
  # scrape player data
  url <- paste0("https://www.basketball-reference.com/leagues/NBA_",year,"_per_poss.html")
  html <- read_html(url)
  nba <- html %>% html_element("#per_poss_stats") %>% html_table()
  # clean scraped data
  keep <- c('Player','Tm','G','FGA','3PA','3P%')
  nba <- nba[!duplicated(nba$Player),keep] # get rid of split rows
  nba$G <- as.numeric(nba$G)
  nba$FGA <- as.numeric(nba$FGA)
  nba$`3PA` <- as.numeric(nba$`3PA`)
  nba$`3P%` <- as.numeric(nba$`3P%`)
  nba <- nba %>% filter(G>=41) # limit to players with more than half their games played
  nba <- nba %>% mutate(freq_3P = `3PA`/FGA)
  nba$season <- paste0(as.character(year-1),"-",substring(as.character(year),3))
  assign(paste0("shooting_",year),nba)
  Sys.sleep(4)
}
# stack the years together
dataframe_names <- ls(pattern = "^shooting_\\d{4}$")
dataframes_list <- mget(dataframe_names)
shooting_full <- do.call(rbind, dataframes_list)

# limit players to those fitting params
shooters <- shooting_full %>% filter(`3PA`>=5, `3P%`>=0.35)
shooters_sum <- shooters %>% group_by(season) %>% summarise(n_shooters=n_distinct(Player))
write.csv(shooters_sum, 'shooters_by_season.csv', row.names=FALSE)

# Plot shooter data by season
shooters_by_season <- 
  ggplot(data=shooters_sum, aes(x=season,y=n_shooters)) +
  geom_point() +
  geom_line(aes(group=1)) +
  scale_y_continuous(limits = c(min(shooters_sum$n_shooters)-3,max(shooters_sum$n_shooters)+3),
                     breaks = seq(min(shooters_sum$n_shooters)-3,max(shooters_sum$n_shooters)+3,by=15)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 55, vjust = 1, hjust=1, size=11)) +
  labs(x="", y="",
       title = "Number of High-Volume, High-Efficiency 3-Point Shooters",
       subtitle = "Players shooting 35%+ on 5+ attempts per game (min. 41 games played)",
       caption="Jonah Wiener-Brodkey . Source: Basketball Reference") +
  theme(axis.text.x = element_text(angle = 55, vjust = 1, hjust=1),
        plot.title = element_text(size=16),
        plot.caption = element_text(color="#808080", hjust=0),
        axis.line.x = element_line(color="black", size = 0.3),
        axis.line.y = element_line(color="black", size = 0.3))
ggsave("shooters_by_season_test.png",shooters_by_season,width=8, height=6)
