# Load packages
library(tidyverse)
library(dplyr)
library(ggplot2)

# Load data
setwd("C:/Users/gijsi/Desktop/School/Eindverslag/Data 2526")
files <- list.files()

originalalldata <- read.csv(files[1])[0,]
for (file in files) {
  data <- read.csv(file)
  data <- subset(data, key_resp_5.keys == "space")
  data <- subset(data, key_resp.duration == "None")
  originalalldata <- rbind(originalalldata, data)
}

# Turn to factor variable
alldata$target_ori <- factor(originalalldata$target_ori)
alldata$target_presence <- factor(alldata$target_presence)

# Remove participants with too much incorrect answers (accuracy below 0.55)
alldata <- alldata %>%
  group_by(participant) %>%
  filter(mean(key_resp.corr) > 0.6) %>%
  ungroup()

# Remove all incorrect trials
total <- nrow(alldata)
alldata <- filter(alldata, key_resp.corr == 1)
incorrect_proportion = (total - nrow(alldata))/total

# Remove technically impossible reaction times
alldata <- filter(alldata, key_resp.rt > 0.1)

# Remove outliers per participant
total <- nrow(alldata)
alldata <- alldata %>%
  group_by(participant, target_presence, target_ori) %>%
  filter({
    Q1 <- quantile(key_resp.rt, 0.25)
    Q3 <- quantile(key_resp.rt, 0.75)
    IQR <- Q3 - Q1
    key_resp.rt > (Q1 - 1.5 * IQR) & key_resp.rt < (Q3 + 1.5 * IQR)
  }) %>%
  ungroup()
remove_amount <- total - nrow(alldata)

# Calculate mean reaction time, SE, slopes
participant_means <- alldata %>%
  group_by(participant, n_items, target_presence, target_ori) %>%
  summarise(mean_rt = mean(key_resp.rt))

mean_calc <- participant_means %>% 
  group_by(target_ori, target_presence, n_items) %>%
  summarize(
    mean = mean(mean_rt),
    SE = sd(mean_rt)/sqrt(n()),
  )

ggplot(mean_calc, mapping = aes(x = n_items, y = mean, color = target_presence)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), width = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_grid(~target_ori,
  labeller = labeller(target_ori = c("117" = "target tilted", "135" = "target vertical"))) +
  xlab("Number of items") +
  ylab("Reaction Time (s)") +
  scale_x_continuous(breaks = c(1, 4, 8, 12)) +
  scale_color_manual(values = c("1" = "blue", "0" = "red"),
                     labels = c("1" = "target present", "0" = "target absent")) + 
  theme_bw()
  

# Calculate and plot regression
regression <- alldata %>%
  group_by(participant, target_ori, target_presence) %>%
  summarize(
    coefs = coef(lm(key_resp.rt ~ n_items))[2],
    .groups = "drop"
  )
  
regression$target_presence <- factor(regression$target_presence, levels = c("1", "0"))

ggplot(regression, mapping = aes(x=target_ori, y=coefs, color = target_presence)) +
  geom_boxplot() +
  scale_x_discrete(limits = c("135", "117"), 
                   labels = c("117" = "Target tilted", "135" = "Target vertical")) +
  scale_color_manual(values = c("1" = "blue", "0" = "red"),
                     labels = c("1" = "Target present", "0" = "Target absent")) +
  xlab("") +
  ylab("Slope") +
  theme_bw()


# Do anova analysis
anova <- aov(coefs ~ target_ori * target_presence, data=regression)
summary(anova)
