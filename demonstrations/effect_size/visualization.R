
library(dplyr)
library(tidyr)
library(distributional)
library(ggdist)
library(ggplot2)
library(cowplot)
library(patchwork)

theme_set(theme_gray())

knitr::opts_chunk$set(echo = TRUE)

rational = read.csv("./data/all_rational.csv") |> select(-X)
behavioral = read.csv("./data/all_behavioral.csv") |> select(-X)


ggplot() +
  stat_slab(data = behavioral, aes(y = mean, x = behavioral), fill = "#7570b3", point_size=1.5) +
  stat_slab(data = behavioral, aes(y = mean, x = calibrated_behavioral), fill = "#d95f02", point_size=1.5) +
  stat_slab(data = behavioral, aes(y = mean, x = calibrated_behavioral_updating), fill = "#1b9e77", point_size=1.5) +
  geom_vline(xintercept = rational [[1]][1], linetype = "dashed", size = 1) +
  geom_vline(xintercept = rational [[1]][2], linetype = "dashed", size = 1) + 
  labs(x = "", y = "") +
  facet_grid(rows = vars(vis)) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major = element_line(colour = "grey"),
        axis.line.x = element_line(linewidth = 1.5, colour = "grey80"),
        panel.background = element_rect(fill = "white", color = "white"),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(colour = "grey"))
