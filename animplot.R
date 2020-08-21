library(tidyverse)
library(readxl)
library(gganimate)

premier_league <- read.csv(file.choose())

home <- premier_league %>%
  group_by(HomeTeam, Season) %>%
  summarise(home_goal = sum(FTHG)) %>%
  group_by(Season) %>%
  arrange(desc(home_goal), .by_group = TRUE)

away <- premier_league %>%
  group_by(AwayTeam, Season) %>%
  summarise(away_goal = sum(FTAG)) %>%
  group_by(Season) %>%
  arrange(desc(away_goal), .by_group = TRUE)

names(away)[names(away)=="AwayTeam"] <- "Team" # Rename to "Team" name 
names(home)[names(home)=="HomeTeam"] <- "Team" # Rename to "Team" name

groupped <- home %>% full_join(away, by = c("Team","Season")) %>%
  mutate(Total = home_goal + away_goal) %>%
  group_by(Season) %>% group_by(Team) %>%
  mutate(Kumulatif = cumsum(Total)) %>%
  group_by(Season) %>% arrange(desc(Kumulatif), .by_group = TRUE) %>%
  group_by(Season) %>% slice_max(order_by = Kumulatif, n = 10) %>%
  group_by(Kumulatif)
groupped <- groupped[-(111),] # I remove that because in 2003/04 season there is a same kumulatif number

ranking <- data.frame(Rank = rep(seq(1,10,1), times = 27)) #create a rank dataframe
clean_data <- groupped %>%
  mutate(value_lbl = paste0(" ",round(Kumulatif))) %>% cbind(ranking)
clean_data <- as.data.frame(clean_data)

# Create a plot animation
staticplot = ggplot(clean_data, aes(Rank, group = Team, 
                                    fill = as.factor(Team), 
                                    color = as.factor(Team))) + 
  geom_tile(aes(y = Kumulatif/2,
                height = Kumulatif,
                width = 0.9), alpha = 0.8, color = NA) + 
  geom_text(aes(y = 0, label = paste(Team, " ")), 
            vjust = 0.2, hjust = 1, size = 8) + 
  geom_text(aes(y=Kumulatif, label = value_lbl, hjust=0), size = 8) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.text = element_text(size = 40),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=30, hjust=0.5, face="bold", colour="black", vjust=2),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="red"),
        plot.caption =element_text(size=18, hjust=0.5, face="italic", color="red"),
        plot.background=element_blank(),
        plot.margin = margin(2,4,2,6, "cm"))

anim = staticplot + transition_states(Season, transition_length = 4, state_length = 1) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'Cummulative Goals per Season : {closest_state}',  
       subtitle  =  "Top 10 Teams",
       caption  = "Data Source: football-data.co.uk")

# GIF Animation
animate(anim, 200, fps = 80,  width = 1500, height = 1000, 
        renderer = gifski_renderer("animateplot.gif"))
