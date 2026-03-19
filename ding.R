# Import libraries
library(ggplot2)
library(tidyverse)

dat <- read.csv("804_VisualSearch_2526_3_2026-02-18_11h57.14.869.csv")
dat <- subset(dat, key_resp_5.keys == "space")
dat <- subset(dat, key_resp.duration == "None")

dat_corr <- filter(dat, key_resp.corr == 1)

dat_corr <- filter(dat_corr, key_resp.rt > 0.1)

# Find outliers
dat_corr <- dat_corr %>%
  group_by(target_presence, target_ori) %>%
  mutate(
    Q1 = quantile(key_resp.rt, 0.25),
    Q3 = quantile(key_resp.rt, 0.75),
    IQD = Q3 - Q1,
    lowBound = Q1 - 1.5*IQD,
    upBound = Q3 + 1.5*IQD
  ) %>%
  filter(key_resp.rt > lowBound & key_resp.rt < upBound)

# Do regression
lmvalues <- dat_corr %>%
  group_by(target_ori, target_presence) %>%   # grouping
  nest() %>%                                  # nest the data by group
  mutate(
    model = map(data, ~ lm(key_resp.rt ~ n_items, data = .x)),    # run lm per group
    tidied = map(model, tidy)                                     # tidy the result
  ) %>%
  unnest(tidied)    

# Plot cleaned data
ggplot(dat_corr, mapping = aes(n_items, key_resp.rt)) +
  geom_point() +
  geom_smooth(method = "lm", se=TRUE) +
  facet_grid(
    target_presence ~ target_ori,
    labeller = labeller(
      target_presence = c("0" = "target absent", "1" = "target present"),
      target_ori = c( "117" = "target tilted", "135" = "target vertical")
    )
  ) +
  xlab("Number of items") +
  ylab("Reaction Time (s)") +
  scale_x_continuous(breaks = c(1, 4, 8, 12)) +
  theme_bw()
