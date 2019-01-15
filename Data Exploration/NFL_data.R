## IMPORT PACKAGES
require(data.table)
require(glue)
require(ggplot2)

##$$##$$## DATA IMPORT

#### NFL GAMES
file.games = "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/games.csv"
games = fread(file.games)

dim(games)
colnames(games)
head(games)
games[,.N,by=list(season, week)]


#### NFL PLAYS
file.plays = "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/plays.csv"
plays = fread(file.plays)
plays$gameplayId = paste(plays$gameId, plays$playId, sep="-")

dim(plays)
colnames(plays)
head(plays)
plays[,.N,by=gameId][order(-N)]


#### NFL PLAYERS
file.players = "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/players.csv"
players = fread(file.players)


dim(players)
colnames(players)
head(players)
players[,.N,by=College][order(-N)]


##$$##$$## DATA CLEAN -- NYG

games.nyg = games[homeTeamAbbr == "NYG" | visitorTeamAbbr == "NYG",]
games.nyg.loc = games.nyg[,.(gameId, loc = ifelse(homeTeamAbbr == "NYG", "home","away"))]
games.nyg.ids = games.nyg$gameId
plays.nyg = plays[gameId %in% games.nyg.ids,]
tracking.nyg = data.table()
for (i in 1:length(games.nyg.ids)) {
  game.id = games.nyg.ids[i]
  file.tracking = glue("https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/tracking_gameId_{game.id}.csv")
  tracking.temp = fread(file.tracking)
  team.flag = ifelse(tracking.temp$displayName == "football", "football", games.nyg.loc$loc[i])
  tracking.temp$team.flag = team.flag
  tracking.temp$gameplayId = paste(tracking.temp$gameId,tracking.temp$playId,sep="-")
  
  tracking.nyg = rbindlist(list(tracking.nyg,tracking.temp))  
}

players.nyg = merge(x = unique(tracking.nyg[team.flag == team,.(nflId,displayName)]),
                    y = players,
                    all.x = TRUE,
                    by.x = "nflId",
                    by.y = "nflId")

## Clean Data Sets

possession.nyg = plays.nyg[GameClock == "15:00:00" & quarter == 1 & isSTPlay == TRUE,
                           .(gameId, gameplayId, ballfirst = ifelse(possessionTeam == "NYG", TRUE, FALSE))]

df.nyg.v1 = merge(x=tracking.nyg,
                  y=merge(x=plays.nyg, y=possession.nyg, by.x="gameplayId", by.y="gameplayId")[,.(gameId = gameId.x,
                                                                                                  quarter,
                                                                                                  yardlineSide,
                                                                                                  ballfirst)],
                  all.x=TRUE,
                  by.x="gameId",
                  by.y="gameId")

df.nyg.v2 = df.nyg.v1[,.(nflId,
                         event,
                         quarter,
                         adj_x = ifelse(ballfirst == TRUE,
                                        ifelse(quarter == 2 | quarter == 4,
                                               ifelse(yardlineSide == "NYG", (160/3)-y, y),
                                               ifelse(yardlineSide == "NYG", y, (160/3)-y)),
                                        ifelse(quarter == 2 | quarter == 4,
                                              ifelse(yardlineSide == "NYG", y, (160/3)-y),
                                              ifelse(yardlineSide == "NYG", (160/3)-y, y))
                         ),
                         adj_y = ifelse(ballfirst == TRUE,
                                        ifelse(quarter == 2 | quarter == 4,
                                               ifelse(yardlineSide == "NYG", x, (120)-x),
                                               ifelse(yardlineSide == "NYG", (120)-x, x)),
                                        ifelse(quarter == 2 | quarter == 4,
                                               ifelse(yardlineSide == "NYG", (120)-x, x),
                                               ifelse(yardlineSide == "NYG", x, (120)-x))),
                         gameId = as.factor(gameId),
                         gameplayId)]


ggplot(data = df.nyg.v2, aes(x=adj_x, y=adj_y)) + geom_point()

##$$##$$## DATA EXPLORATION

## PLAYER PROFILES
players.top.colleges = players[,.N,by=College][order(-N)][1:20]
players.heat = players[College %in% players.top.colleges$College,.(count = .N),by=list(College, PositionAbbr)]
players.heat$class = ifelse(players.heat$count >= 5, "top", ifelse(players.heat$count >= 3, "mid", "low"))
ggplot(players.heat, aes(PositionAbbr, College)) + 
  geom_tile(aes(fill = class)) +
  scale_fill_manual(values=c("#e0e6ee", "#f4f293", "#0bcd72"))

## PLAYERS CATCHES


## FIELD GOALS
plays.fg = merge(x=plays[SpecialTeamsPlayType == "Field Goal" & isPenalty == FALSE,],
                 y=games[,.(gameId, homeTeamAbbr)],
                 by.x="gameId",
                 by.y="gameId",
                 all.x=TRUE)

plays.fg$fgDistance = ifelse(plays.fg$possessionTeam == plays.fg$yardlineSide,
                             plays.fg$yardlineNumber + 67,
                             plays.fg$yardlineNumber + 17)

plays.fg$isGood = ifelse(plays.fg$possessionTeam == plays.fg$homeTeamAbbr,
                         ifelse(plays.fg$HomeScoreAfterPlay > plays.fg$HomeScoreBeforePlay,
                                TRUE,
                                FALSE),
                         ifelse(plays.fg$VisitorScoreAfterPlay > plays.fg$VisitorScoreBeforePlay,
                                TRUE,
                                FALSE))

plays.fg.attempts = merge(x=plays.fg,
                          y=df.nyg.v2[nflId == "2556771" & event == "field_goal",],
                          by.x="gameplayId",
                          by.y="gameplayId")


ggplot(data = plays.fg.attempts, aes(adj_x, adj_y)) + geom_point() +
  xlim(0, (160/3)) +
  ylim(0, 120) +
  geom_hline(yintercept = c(0,10,60,110,120)) +
  geom_hline(yintercept = c(0,10,60,110,120)) +
  geom_vline(xintercept = c(0,160/3)) + 
  annotate("rect", ymin = 0, ymax = 10, xmin = 0, xmax = (160/3), fill = "#0d46bf", alpha = .3) +
  annotate("rect", ymin = 110, ymax = 120, xmin = 0, xmax = (160/3), fill = "#0d46bf", alpha = .3) +
  annotate("rect", ymin = 10, ymax = 110, xmin = 0, xmax = (160/3), fill = "green", alpha = .3) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="none")


tracking.player.v1 = tracking.nyg[nflId == "2543496",]
tracking.player.v1$gameplayId = paste(tracking.player.v1$gameId,tracking.player.v1$playId,sep="-")
plays.player.v1 = plays.nyg[playId %in% unique(tracking.player.v1$playId),]
plays.player.v1$gameplayId = paste(plays.player.v1$gameId,plays.player.v1$playId,sep="-")
tracking.player.v2 = merge(x=tracking.player.v1, y=plays.player.v1, by.x = "gameplayId", by.y = "gameplayId", all.x=TRUE)

tracking.player.v3 = tracking.player.v2[,.(frame.id,
                                           yardlineSide,
                                           quarter,
                                           isSTPlay,
                                           team.flag,
                                           event,
                                           adj_x = ifelse(team.flag == "home",
                                                          ifelse(quarter == 2 | quarter == 4,
                                                                 ifelse(yardlineSide == "NYG", (160/3)-y, y),
                                                                 ifelse(yardlineSide == "NYG", y, (160/3)-y)),
                                                          ifelse(quarter == 2 | quarter == 4,
                                                                 ifelse(yardlineSide == "NYG", y, (160/3)-y),
                                                                 ifelse(yardlineSide == "NYG", (160/3)-y, y))
                                                          ),
                                           adj_y = ifelse(team.flag == "home",
                                                          ifelse(quarter == 2 | quarter == 4,
                                                                 ifelse(yardlineSide == "NYG", x, (120)-x),
                                                                ifelse(yardlineSide == "NYG", (120)-x, x)),
                                                          ifelse(quarter == 2 | quarter == 4,
                                                                 ifelse(yardlineSide == "NYG", (120)-x, x),
                                                                 ifelse(yardlineSide == "NYG", x, (120)-x))),
                                           gameId = as.factor(gameId.x),
                                           gameplayId)]

# gameId    N
# 1: 2017091800, home 2511
# 2: 2017092409, away 3392
# 3: 2017100111, away 4775
# 4: 2017100805, away 4109


tracking.player.v3[,.N,by=event][order(-N)]

ggplot(data = tracking.player.v3[gameId == "2017100805" & isSTPlay == "FALSE",],
       aes(x = adj_x, y = adj_y, color = ifelse(is.na(event), "", ifelse(event == "pass_outcome_caught" | event == "touchdown","catch", "")))) +
       xlim(0, (160/3)) +
       ylim(0, 120) +
       geom_hline(yintercept = c(0,10,60,110,120)) +
       geom_hline(yintercept = c(0,10,60,110,120)) +
       geom_vline(xintercept = c(0,160/3)) + 
       annotate("rect", ymin = 0, ymax = 10, xmin = 0, xmax = (160/3), fill = "#0d46bf", alpha = .3) +
       annotate("rect", ymin = 110, ymax = 120, xmin = 0, xmax = (160/3), fill = "#0d46bf", alpha = .3) +
       annotate("rect", ymin = 10, ymax = 110, xmin = 0, xmax = (160/3), fill = "green", alpha = .3) +
       theme(axis.title.x=element_blank(),
             axis.text.x=element_blank(),
             axis.ticks.x=element_blank(),
             axis.title.y=element_blank(),
             axis.text.y=element_blank(),
             axis.ticks.y=element_blank(),
             legend.position="none") +
       scale_color_manual(values=c("#999999", "#FFFFFF")) +
       geom_point()

plays.player.v1[gameId == "2017091800" & quarter == 1 & possessionTeam == "NYG",]

player.v2 = player.v1[,.(playId, adj_x = ifelse(yardlineSide == "NYG", yardlineNumber, yardlineNumber + 50))]
player.v2$big_event = ifelse(is.na(player.v2$event),FALSE,ifelse(player.v2$event == "pass_outcome_caught",TRUE,FALSE))
player.v2$point_size = ifelse(player.v2$big_event,1,0.5)
ggplot(player.v2, aes(y, adj_x, color = as.factor(gameId), size = point_size)) + geom_point() 




obj.game1_new = merge(obj.game1, obj.game1.plays.yardline, by.x = "playId", by.y = "playId", all.x = TRUE)



ggplot(obj.game1_new[playId == 278,], aes(y, x, color= big_event, size = point_size)) + geom_point() + xlim(0, 50) + ylim(0, 100)





library(tidyverse)
file.tracking <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/tracking_gameId_2017091800.csv"
tracking.example <- read_csv(file.tracking)

file.game <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/games.csv"
games.sum <- read_csv(file.game) 

file.plays <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/plays.csv"
plays.sum <- read_csv(file.plays) 

tracking.example.merged <- tracking.example %>% inner_join(games.sum) %>% inner_join(plays.sum) 

example.play <- tracking.example.merged %>% filter(playId == 278)

example.play %>% select(playDescription) %>% slice(1)

example.play





library(gganimate)
library(cowplot)

## General field boundaries
xmin <- 0
xmax <- 160/3
hash.right <- 38.35
hash.left <- 12
hash.width <- 3.3


## Specific boundaries for a given play
ymin <- max(round(min(example.play$x, na.rm = TRUE) - 10, -1), 0)
ymax <- min(round(max(example.play$x, na.rm = TRUE) + 10, -1), 120)
df.hash <- expand.grid(x = c(0, 23.36667, 29.96667, xmax), y = (10:110))
df.hash <- df.hash %>% filter(!(floor(y %% 5) == 0))
df.hash <- df.hash %>% filter(y < ymax, y > ymin)

animate.play <- ggplot() +
  geom_point(data = example.play, aes(x = (xmax-y),
                                      y = x, 
                                      colour = team,
                                      group = nflId,
                                      pch = team,
                                      size = team)) + 
  geom_text(data = example.play, aes(x = (xmax-y), y = x, label = jerseyNumber), colour = "white", 
            vjust = 0.36, size = 3.5) + 
  scale_size_manual(values = c(6, 4, 6), guide = FALSE) + 
  scale_shape_manual(values = c(19, 16, 19), guide = FALSE) +
  scale_colour_manual(values = c("#e31837", "#654321", "#002244"), guide = FALSE) + 
  annotate("text", x = df.hash$x[df.hash$x < 55/2], 
           y = df.hash$y[df.hash$x < 55/2], label = "_", hjust = 0, vjust = -0.2) + 
  annotate("text", x = df.hash$x[df.hash$x > 55/2], 
           y = df.hash$y[df.hash$x > 55/2], label = "_", hjust = 1, vjust = -0.2) + 
  annotate("segment", x = xmin, 
           y = seq(max(10, ymin), min(ymax, 110), by = 5), 
           xend =  xmax, 
           yend = seq(max(10, ymin), min(ymax, 110), by = 5)) + 
  annotate("text", x = rep(hash.left, 11), y = seq(10, 110, by = 10), 
           label = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G"), 
           angle = 270, size = 4) + 
  annotate("text", x = rep((xmax - hash.left), 11), y = seq(10, 110, by = 10), 
           label = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   "), 
           angle = 90, size = 4) + 
  annotate("segment", x = c(xmin, xmin, xmax, xmax), 
           y = c(ymin, ymax, ymax, ymin), 
           xend = c(xmin, xmax, xmax, xmin), 
           yend = c(ymax, ymax, ymin, ymin), colour = "black") + 
  ylim(ymin, ymax) + 
  coord_fixed() +  
  theme_nothing() +
  transition_time(frame.id)  +
  gganimate::ease_aes('linear') + 
  NULL

## Ensure timing of play matches 10 frames-per-second
play.length.ex <- length(unique(example.play$frame.id))
gganimate::gg_animate(animate.play, fps = 10, nframe = play.length.ex)
