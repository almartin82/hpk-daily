## ----packages_etc, message=FALSE, error=FALSE, warning=FALSE-------------
library(dplyr)
library(ggplot2)
library(readr)

## ----data----------------------------------------------------------------

hpk_15 <- readr::read_csv(file = '..\\data\\team_by_date_2015.csv', na = "-")
hpk_14 <- readr::read_csv(file = '..\\data\\team_by_date_2014.csv', na = "-")
hpk_13 <- readr::read_csv(file = '..\\data\\team_by_date_2013.csv', na = "-")
hpk_12 <- readr::read_csv(file = '..\\data\\team_by_date_2012.csv', na = "-")
hpk_all <- rbind(hpk_15, hpk_14, hpk_13, hpk_12) %>% as.data.frame()

head(hpk_all)

## ----clean---------------------------------------------------------------

hpk1 <-  hpk_all %>%
  filter(
    !(stat_id == 60 & !is.na(value))
  )

hpk2 <- hpk_all %>%
  filter(
    stat_id == 60 & !is.na(value)
  ) %>%
  rowwise() %>%
  mutate(
    value = eval(parse(text=value)) 
  )
  
hpk_all <- rbind(hpk1, hpk2)
hpk_all$year <- as.numeric(format(hpk_all$date, "%Y"))


## ----labels--------------------------------------------------------------

stat_names = data.frame(
  stat_id = c(60, 7, 13, 16, 23, 4, 50, 28, 32, 42, 26, 27),
  stat_name = c('AVG', 'R', 'RBI', 'SB', 'TB', 'OBP', 'IP', 'W', 'SV', 'K', 'ERA', 'WHIP')
)

hpk_all <- hpk_all %>%
  dplyr::inner_join(
    stat_names
  )

write.csv(hpk_all, '..\\data\\team_by_date_all.csv')


## ----avgs----------------------------------------------------------------
require(Cairo)

stat_dist <- function(df, group_by='stat_name') {
  df <- as.data.frame(df)
  df$grouping_col <- df[,group_by]
  
  ggplot(
    data = df[!is.na(df$value), ],
    aes(
      x = as.numeric(value),
      group = paste0(year, grouping_col),
      color = factor(year)
    )
  ) + 
  geom_density(
    adjust = 2
  ) +
  theme_bw() +
  labs(
    title = unique(df$stat_name)
  )
  
}


all_league <- list()
by_team <- list()

for (i in unique(stat_names$stat_name)) {
  print(i)
  
  this_sub <- hpk_all %>%
    dplyr::filter(
      stat_name == i
    )
  
  p1 <- stat_dist(this_sub)
  p2 <- stat_dist(df = this_sub, group_by = 'team_key')
  
  all_league[[i]] <- p1
  by_team[[i]] <- p2
}

#print all_league
Cairo(
  width = 14, height = 8.5
 ,file = '..\\charts\\all_league.pdf'
 ,type="pdf"
 ,bg = "transparent"
 ,canvas = "white", units = "in"
)
  print(all_league)

dev.off()


#print by_team
Cairo(
  width = 14, height = 8.5
 ,file = '..\\charts\\by_team.pdf'
 ,type="pdf"
 ,bg = "transparent"
 ,canvas = "white", units = "in"
)
  print(by_team)
dev.off()


## ----era_prep------------------------------------------------------------

round_to_any <- function(x, accuracy, f = round) {
  f(x / accuracy) * accuracy
}

ip_df <- hpk_all %>% 
  dplyr::filter(stat_name == 'IP') 
names(ip_df)[2] <- "IP"
ip_df$IP_round <-  round_to_any(x = as.numeric(ip_df$IP), accuracy = 1, f = ceiling)  
#mlb codes 1 out as .1, 2 outs as .2.  replace with 0.333, 0.667
ip_df$IP <- gsub('.1', '.333', ip_df$IP)
ip_df$IP <- gsub('.2', '.667', ip_df$IP)
ip_df$IP <- as.numeric(ip_df$IP)

era_df <- hpk_all %>%
  dplyr::filter(stat_name == 'ERA') %>%
  dplyr::select(
    team_key, date, value
  )
names(era_df)[3] <- 'ERA'
era_df$ERA <- as.numeric(era_df$ERA)

whip_df <- hpk_all %>%
  dplyr::filter(stat_name == 'WHIP') %>%
  dplyr::select(
    team_key, date, value
  )
names(whip_df)[3] <- 'WHIP'
whip_df$WHIP<- as.numeric(whip_df$WHIP)


ip_df <- ip_df %>%
  dplyr::inner_join(
    era_df,
    by = c("team_key" = "team_key", "date" = "date")
  ) %>%
  dplyr::inner_join(
    whip_df,
    by = c("team_key" = "team_key", "date" = "date")
  )

head(ip_df)

ip_df$run_per_inning <- ip_df$ERA / 9
ip_df$run_per_out <- ip_df$ERA / 27
ip_df$wh_per_out <- ip_df$WHIP / 3



## ----facet---------------------------------------------------------------

ggplot(
  data = hpk_all[!is.na(hpk_all$value), ],
  aes(
    x = as.numeric(value),
    group = stat_id
  )
) + 
geom_density() +
facet_grid(
  year ~ stat_name,
  scales = "free",
) +
theme(
  panel.grid = element_blank()
) +
theme_classic()


