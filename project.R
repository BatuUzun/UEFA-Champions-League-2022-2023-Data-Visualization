library(dplyr)
library(moments)
library(ggplot2)
library(tidyr)

calculate_sd_for_numeric_columns <- function(df) {
  sd_values <- round(sapply(df[, sapply(df, is.numeric)], sd), digits = 2)
  sd_values <- as.data.frame.list(sd_values)
  return(sd_values)
}

calculate_n_for_numeric_columns <- function(df) {
  
  n <- sapply(df[, sapply(df, is.numeric)], length)
  n_values <- as.data.frame.list(n)
  return(n_values)
}

calculate_mean_for_numeric_columns <- function(df) {
  
  mean <- sapply(df[, sapply(df, is.numeric)], mean)
  mean_values <- as.data.frame.list(mean)
  return(mean_values)
}

calculate_median_for_numeric_columns <- function(df) {
  
  median <- sapply(df[, sapply(df, is.numeric)], median)
  median_values <- as.data.frame.list(median)
  return(median_values)
}

calculate_mad_for_numeric_columns <- function(df) {
  
  mad <- round(sapply(df[, sapply(df, is.numeric)], mad),digits = 2)
  mad_values <- as.data.frame.list(mad)
  return(mad_values)
}

calculate_min_for_numeric_columns <- function(df) {
  
  min <- round(sapply(df[, sapply(df, is.numeric)], min),digits = 2)
  min_values <- as.data.frame.list(min)
  return(min_values)
}

calculate_max_for_numeric_columns <- function(df) {
  
  max <- round(sapply(df[, sapply(df, is.numeric)], max),digits = 2)
  max_values <- as.data.frame.list(max)
  return(max_values)
}

calculate_first_quantile_for_numeric_columns <- function(df) {
  
  numeric_columns <- df[, sapply(df, is.numeric)]
  quantile_values <- sapply(numeric_columns, quantile, probs = 0.25)
  result <-  as.data.frame.list(quantile_values)
  return(result)
}

calculate_third_quantile_for_numeric_columns <- function(df) {
  
  numeric_columns <- df[, sapply(df, is.numeric)]
  quantile_values <- sapply(numeric_columns, quantile, probs = 0.75)
  result <-  as.data.frame.list(quantile_values)
  return(result)
}

calculate_na_for_numeric_columns <- function(df) {
  numeric_columns <- df[, sapply(df, is.numeric)]
  na_values <- colSums(sapply(numeric_columns, is.na))
  result <-  as.data.frame.list(na_values)
  return(result)
}

calculate_skewness_for_numeric_columns <- function(df) {
  numeric_columns <- df[, sapply(df, is.numeric)]
  skewness_values <- round(sapply(numeric_columns, skewness), digits = 2)
  result <-  as.data.frame.list(skewness_values)
  return(result)
}

calculate_kurtosis_for_numeric_columns <- function(df) {
  numeric_columns <- df[, sapply(df, is.numeric)]
  kurtosis_values <- round(sapply(numeric_columns, kurtosis), digits = 2)
  result <-  as.data.frame.list(kurtosis_values)
  return(result)
}

calculate_standard_error_for_numeric_columns <- function(df) {

  numeric_columns <- df[, sapply(df, is.numeric)]
  se_values <- round(sapply(numeric_columns, function(x) if (length(x) > 1) sd(x) / sqrt(length(x)) else NA),digits = 2)
  result <- as.data.frame.list(se_values)
  return(result)
}

calculate_trimmed_mean_for_numeric_columns <- function(df, trim_fraction = 0.1) {

  numeric_columns <- df[, sapply(df, is.numeric)]
  trimmed_mean_values <- sapply(numeric_columns, function(x) if (length(x) > 1) mean(x, trim = trim_fraction) else NA)
  result <- as.data.frame.list(trimmed_mean_values)
  return(result)
}


goals <- read.csv(file = "goals.csv", header = T)
goals

teams <- read.csv(file = "teams.csv", header = T)
teams

assists <- read.csv(file = "assists.csv", header = T)
assists

saves <- read.csv(file = "saves.csv", header = T)
saves

nations <- read.csv(file = "nations.csv", header = T)
nations

games <- read.delim(file = "games_won.txt", header = F)
games

# GOALS

# n, mean, sd, median, trimmed mean, mad, min, max, 1st qua, 3nd qua, NAs, skewness, kurtosis, se
nGoals <- calculate_n_for_numeric_columns(goals)
nGoals

meanGoals <- calculate_mean_for_numeric_columns(goals)
meanGoals

sdGoals <- calculate_sd_for_numeric_columns(goals)
sdGoals

medianGoals <- calculate_median_for_numeric_columns(goals)
medianGoals

trimmedMeanGoals <- calculate_trimmed_mean_for_numeric_columns(goals)
trimmedMeanGoals

madGoals <- calculate_mad_for_numeric_columns(goals)
madGoals

minGoals <- calculate_min_for_numeric_columns(goals)
minGoals

maxGoals <- calculate_max_for_numeric_columns(goals)
maxGoals

firstQuaGoals <- calculate_first_quantile_for_numeric_columns(goals)
firstQuaGoals

thirdQuaGoals <- calculate_third_quantile_for_numeric_columns(goals)
thirdQuaGoals

naGoals <- calculate_na_for_numeric_columns(goals)
naGoals

skewnessGoals <- calculate_skewness_for_numeric_columns(goals)
skewnessGoals

kurtosisGoals <- calculate_kurtosis_for_numeric_columns(goals)
kurtosisGoals

seGoals <- calculate_standard_error_for_numeric_columns(goals)
seGoals


# TEAMS
teams$market.value = as.numeric(teams$market.value)
# n, mean, sd, median, trimmed mean, mad, min, max, 1st qua, 3nd qua, NAs, skewness, kurtosis, se
nTeams <- calculate_n_for_numeric_columns(teams)
nTeams

meanTeams <- calculate_mean_for_numeric_columns(teams)
meanTeams

sdTeams <- calculate_sd_for_numeric_columns(teams)
sdTeams

medianTeams <- calculate_median_for_numeric_columns(teams)
medianTeams

trimmedMeanTeams <- calculate_trimmed_mean_for_numeric_columns(teams)
trimmedMeanTeams

madTeams <- calculate_mad_for_numeric_columns(teams)
madTeams

minTeams <- calculate_min_for_numeric_columns(teams)
minTeams

maxTeams <- calculate_max_for_numeric_columns(teams)
maxTeams

firstQuaTeams <- calculate_first_quantile_for_numeric_columns(teams)
firstQuaTeams

thirdQuaTeams <- calculate_third_quantile_for_numeric_columns(teams)
thirdQuaTeams

naTeams <- calculate_na_for_numeric_columns(teams)
naTeams

skewnessTeams <- calculate_skewness_for_numeric_columns(teams)
skewnessTeams

kurtosisTeams <- calculate_kurtosis_for_numeric_columns(teams)
kurtosisTeams

seTeams <- calculate_standard_error_for_numeric_columns(teams)
seTeams


# Assists
assists$ignore <- 0
assists
# n, mean, sd, median, trimmed mean, mad, min, max, 1st qua, 3nd qua, NAs, skewness, kurtosis, se
nAssists <- calculate_n_for_numeric_columns(assists)
nAssists

meanAssists <- calculate_mean_for_numeric_columns(assists)
meanAssists

sdAssists <- calculate_sd_for_numeric_columns(assists)
sdAssists

medianAssists <- calculate_median_for_numeric_columns(assists)
medianAssists

trimmedMeanAssists <- calculate_trimmed_mean_for_numeric_columns(assists)
trimmedMeanAssists

madAssists <- calculate_mad_for_numeric_columns(assists)
madAssists

minAssists <- calculate_min_for_numeric_columns(assists)
minAssists

maxAssists <- calculate_max_for_numeric_columns(assists)
maxAssists

firstQuaAssists <- calculate_first_quantile_for_numeric_columns(assists)
firstQuaAssists

thirdQuaAssists <- calculate_third_quantile_for_numeric_columns(assists)
thirdQuaAssists

naAssists <- calculate_na_for_numeric_columns(assists)
naAssists

skewnessAssists <- calculate_skewness_for_numeric_columns(assists)
skewnessAssists

kurtosisAssists <- calculate_kurtosis_for_numeric_columns(assists)
kurtosisAssists

seAssists <- calculate_standard_error_for_numeric_columns(assists)
seAssists


# SAVES

# n, mean, sd, median, trimmed mean, mad, min, max, 1st qua, 3nd qua, NAs, skewness, kurtosis, se
nSaves <- calculate_n_for_numeric_columns(saves)
nSaves

meanSaves <- calculate_mean_for_numeric_columns(saves)
meanSaves

sdSaves <- calculate_sd_for_numeric_columns(saves)
sdSaves

medianSaves <- calculate_median_for_numeric_columns(saves)
medianSaves

trimmedMeanSaves <- calculate_trimmed_mean_for_numeric_columns(saves)
trimmedMeanSaves

madSaves <- calculate_mad_for_numeric_columns(saves)
madSaves

minSaves <- calculate_min_for_numeric_columns(saves)
minSaves

maxSaves <- calculate_max_for_numeric_columns(saves)
maxSaves

firstQuaSaves <- calculate_first_quantile_for_numeric_columns(saves)
firstQuaSaves

thirdQuaSaves <- calculate_third_quantile_for_numeric_columns(saves)
thirdQuaSaves

naSaves <- calculate_na_for_numeric_columns(saves)
naSaves

skewnessSaves <- calculate_skewness_for_numeric_columns(saves)
skewnessSaves

kurtosisSaves <- calculate_kurtosis_for_numeric_columns(saves)
kurtosisSaves

seSaves <- calculate_standard_error_for_numeric_columns(saves)
seSaves

# NATIONS

# n, mean, sd, median, trimmed mean, mad, min, max, 1st qua, 3nd qua, NAs, skewness, kurtosis, se
nNations <- calculate_n_for_numeric_columns(nations)
nNations

meanNations <- calculate_mean_for_numeric_columns(nations)
meanNations

sdNations <- calculate_sd_for_numeric_columns(nations)
sdNations

medianNations <- calculate_median_for_numeric_columns(nations)
medianNations

trimmedMeanNations <- calculate_trimmed_mean_for_numeric_columns(nations)
trimmedMeanNations

madNations <- calculate_mad_for_numeric_columns(nations)
madNations

minNations <- calculate_min_for_numeric_columns(nations)
minNations

maxNations <- calculate_max_for_numeric_columns(nations)
maxNations

firstQuaNations <- calculate_first_quantile_for_numeric_columns(nations)
firstQuaNations

thirdQuaNations <- calculate_third_quantile_for_numeric_columns(nations)
thirdQuaNations

naNations <- calculate_na_for_numeric_columns(nations)
naNations

skewnessNations <- calculate_skewness_for_numeric_columns(nations)
skewnessNations

kurtosisNations <- calculate_kurtosis_for_numeric_columns(nations)
kurtosisNations

seNations <- calculate_standard_error_for_numeric_columns(nations)
seNations



# GRAPHICS

goals
colors <- c("cyan4", "cornsilk4", "brown2", "dimgray", "darkorange1", "darkorchid","forestgreen", "deeppink", "lightcoral", "khaki")

# Goals
ggplot(goals, aes(x = Player, y = Goals, fill = colors)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7, color = "black") +
  theme_minimal() +
  labs(title = "Most Scorer Players",
       x = "Player",
       y = "Number of Goals", ) +
  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5, margin = margin(b = 10), face = "bold", size = 13)
        ,axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5, margin = margin(l = 30), face = "bold", size = 13),title = element_text(size = 25, hjust = 0.5),
        axis.title = element_text(size = 20,hjust = 0.5),
        axis.title.x = element_text(size = 20,hjust = 0.5),plot.title = element_text(size = 25,hjust = 0.5, margin = margin(b=30))
        
  ) +
  coord_cartesian(ylim = c(0,maxGoals$Goals)) +
  
  theme(legend.position = "none")


# Attempts
ggplot(goals, aes(x = Player, y = Total.Attempts, fill = colors)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7, color = "black") +
  theme_minimal() +
  labs(title = "Total Attempts of Most Scorer Players",
       x = "Player",
       y = "Total Attempts", ) +
  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5, margin = margin(b = 10), face = "bold", size = 13)
        ,axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5, margin = margin(l = 30), face = "bold", size = 13),title = element_text(size = 25, hjust = 0.5),
        axis.title = element_text(size = 20,hjust = 0.5),
        axis.title.x = element_text(size = 20,hjust = 0.5),plot.title = element_text(size = 25,hjust = 0.5, margin = margin(b=30))
        
  ) +
  coord_cartesian(ylim = c(0,maxGoals$Total.Attempts)) +
  
  theme(legend.position = "none")


goals$attempt_needed <- round(goals$Total.Attempts/goals$Goals, digits = 2)
goals
goals <- goals[order(goals$attempt_needed), ]
goals
# Attempts needed

goals %>%
  arrange(attempt_needed) %>%
  mutate(Player = factor(Player, unique(Player))) %>%
  ggplot() + aes(x=Player, y=attempt_needed) +
  geom_segment( aes(x=Player, xend=Player, y=0, yend=attempt_needed), color=colors, linewidth = 2) +
  geom_point( color=colors, size=4) +
  geom_text(aes(label = attempt_needed), hjust = -1) + 
  labs(
       x = "Player",
       y = "Attempts Needed for a Goal") +
  theme_light() +
  coord_flip() +
  theme(axis.text.x = element_text( vjust = 0.5, hjust = 0.5, margin = margin(b = 10), face = "bold", size = 13),
        axis.text.y = element_text( vjust = 0.5, hjust = 0.5, margin = margin(b = 10, l = 20), face = "bold", size = 13)
        ,title = element_text(size = 25, hjust = 0.5),
        axis.title = element_text(size = 25,hjust = 0.5)
        
  ) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )



# Average Age
ggplot(teams, aes(x = average.age,  y = team)) +
  geom_point(size = 3) +
  labs(title = "Average Age of Teams",
       x = "Average Age",
       y = "Team")+theme_light() +
  geom_text(aes(label = average.age), vjust = -0.7) +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5, margin = margin(b = 10), face = "bold", size = 13)
        ,axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5, margin = margin(l = 30), face = "bold", size = 13),title = element_text(size = 25, hjust = 0.5),
        axis.title = element_text(size = 20,hjust = 0.5),
        axis.title.x = element_text(size = 20,hjust = 0.5),plot.title = element_text(size = 25,hjust = 0.5, margin = margin(b=30))
        
  )


# Market Value


donut_colors <- c("cyan4", "cornsilk4", "brown2", "dimgray", "darkorange1", "darkorchid","forestgreen", "deeppink", "lightcoral", "khaki", "aquamarine3","bisque3", "brown2", "burlywood2", "darkgrey", "darkolivegreen2", "darkorange3", "darkorchid", "darkseagreen", "darkslategray", "darkslategray3", "deeppink", "gray52", "deepskyblue1", "forestgreen", "cornsilk2", "coral2", "chartreuse2", "cadetblue4", "burlywood4", "darkgoldenrod1", "darkgoldenrod4")                                              


ggplot(teams, aes(x=2, y=market.value/100, fill=team))+
  geom_col(width=1) +
  xlim(0.5, 2.5) +
  scale_fill_viridis_d() +
  labs(title = "Market Values of Teams",
       x = "",
       y = "")+theme_light() +
  coord_polar("y") +theme_void()+
  theme(
    plot.title = element_text(size = 25,hjust = 0.5, margin = margin(t = 30)))+
  geom_text(aes(label = paste0(round(market.value/sum(market.value)*100, 1), "%")), position = position_stack(vjust = 0.5))+
  scale_fill_manual(values = donut_colors)

# Assists

assists <- assists[order(assists$Assists, decreasing = T), ]
assists

assists %>%
  arrange(Assists) %>%
  mutate(Player = factor(Player, unique(Player))) %>%
  ggplot(aes(x = Assists, y = Player)) +
  geom_bar(stat = "identity", fill = colors) +
  labs(title = "Player Assists",
       x = "Assists",
       y = "Player") +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5, margin = margin(b = 10), face = "bold", size = 13)
        ,axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5, margin = margin(l = 30), face = "bold", size = 13),title = element_text(size = 25, hjust = 0.5),
        axis.title = element_text(size = 20,hjust = 0.5),
        axis.title.x = element_text(size = 20,hjust = 0.5),plot.title = element_text(size = 25,hjust = 0.5, margin = margin(b=30))
        
  )+
  lims(x = c(0, max(assists$Assists)+1))

                       

# Saves
colors_saves <- c("darkorchid3", "deepskyblue", "forestgreen", "darkorange3")
label_saves <- c("Clean Sheets", "Goals Conceded", "Matches Played", "Saves")
saves_long <- gather(saves, key = "Variable", value = "Value", -Player)
saves_long
ggplot(saves_long, aes(x = Player, y = Value, fill = Variable)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = ifelse(Value != 0, Value, "")), position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Goalkeeper Statistics",
       x = "Player",
       y = "")+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5, margin = margin(b = 10), face = "bold", size = 13)
                          ,axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5, margin = margin(l = 30), face = "bold", size = 13),title = element_text(size = 25, hjust = 0.5),
                          axis.title = element_text(size = 20,hjust = 0.5),
                          axis.title.x = element_text(size = 20,hjust = 0.5),plot.title = element_text(size = 25,hjust = 0.5, margin = margin(b=30))
                          
       )+
  scale_fill_manual(values = colors_saves, name = "", labels = label_saves)+
  theme(legend.margin = margin(l = 20))


# Nations
nations
nations_no_points <- nations[, -6]
nations_long <- gather(nations_no_points, key = "Variable", value = "Value", -Country)
nations_long
colors_nations <- c("deepskyblue", "forestgreen", "darkorange3", "deeppink3")
label_nations <- c("Number of Teams", "Draws", "Losses", "Wins")

ggplot(nations_long, aes(x = Country, y = Value, fill = Variable)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = ifelse(Value != 0, Value, "")), position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Nation Statistics",
       x = "Nation",
       y = "")+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5, margin = margin(b = 10), face = "bold", size = 13)
                     ,axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5, margin = margin(l = 30), face = "bold", size = 13),title = element_text(size = 25, hjust = 0.5),
                     axis.title = element_text(size = 20,hjust = 0.5),
                     axis.title.x = element_text(size = 20,hjust = 0.5),plot.title = element_text(size = 25,hjust = 0.5, margin = margin(b=30))
                     
       )+
  scale_fill_manual(values = colors_nations, name = "", labels = label_nations)+
  theme(legend.margin = margin(l = 20))


# Points per team
nations$points.per.team <- round(nations$Points / nations$Clubs, digits = 2)
nations
color_nations <- c(colors, "mediumvioletred", "navajowhite1", "slateblue2", "seagreen3", "tomato")


nations %>%arrange(points.per.team) %>%mutate(Country = factor(Country, unique(Country))) %>%
  ggplot(aes(x = points.per.team, y = Country)) +
  geom_bar(stat = "identity", fill = color_nations) +
  labs(title = "Points per Team",
       x = "Points Claimed",
       y = "Nation") +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5, margin = margin(b = 10), face = "bold", size = 13)
        ,axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5, margin = margin(l = 30), face = "bold", size = 13),title = element_text(size = 25, hjust = 0.5),
        axis.title = element_text(size = 20,hjust = 0.5),
        axis.title.x = element_text(size = 20,hjust = 0.5),plot.title = element_text(size = 25,hjust = 0.5, margin = margin(b=30))
        
  )+
  lims(x = c(0, max(nations$points.per.team)+5))
  


# Games
games_table <- table(games)
games_table
games <- as.data.frame(games_table)
name <- c("Home","Away","Draw")
names(games_table) <- name
games_table
n <- sum(games$Freq)
n
color_games <- c("darkslategray3", "darkolivegreen4", "chocolate3")

ggplot(games, aes(x=1, y=Freq, fill=V1))+
  geom_col(width=1) +
  xlim(0.5, 1.5) +
  scale_fill_viridis_d() +
  labs(title = "Percentage of Winners Side",
       x = "",
       y = "")+
  coord_polar("y") +
  theme_void()+geom_text(aes(label = paste0(round(Freq/n*100, 1), "%")),size = 10, position = position_stack(vjust = 0.5))+
  scale_fill_manual(values = color_games, name = "Games", labels = name)+
  theme(legend.margin = margin(l = 20), legend.text = element_text(size = 13), legend.title = element_text(size = 20))+
  theme(
       plot.title = element_text(size = 25,hjust = 0.5, margin = margin(t = 30)))
