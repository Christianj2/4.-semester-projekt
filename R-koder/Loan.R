library(mFilter)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(zoo)
library(grid)
library(tidyverse)
library(lubrtheme)
library(ggthemes)

setwd("~/Desktop")
kredit <- read.csv2("kredit.csv")

kredit$Quaterly <- as.Date(kredit$Quaterly, "%d/%m/%Y")

ggplot() +
  geom_line(kredit, mapping = aes(x = Quaterly, y = procentdeposit, color = "Realkreditlån")) +
  geom_line(kredit, mapping = aes(x = Quaterly, y = procentmortage, color = "Banklån")) +
  labs(title ="%-vis forskel på banklån og realkreditlån", x = "Årstal", y = "i %.")  + labs(color="") +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("2 years")) +
  scale_y_continuous(breaks=seq(0,70,5))


