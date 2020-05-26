library(ggplot2)
library(ggthemes)
library(scales)
library(tidyverse)
library(dplyr)
library(magrittr)
library(gridExtra)

setwd("~/Desktop")
kredit_andel <- read.csv2("realkredit-andel.csv")

kredit_andel$dato <- as.Date(kredit_andel$dato, "%d/%m/%Y")

outloan <- ggplot() +
  geom_line(kredit_andel, mapping = aes(x = dato, y = A, color = "Fast forrentet med afdrag")) +
  geom_line(kredit_andel, mapping = aes(x = dato, y = C, color = "Variabelt forrentet med afdragsfrihed")) +
  geom_line(kredit_andel, mapping = aes(x = dato, y = B, color = "Variabelt forrentet med afdrag")) +
  geom_line(kredit_andel, mapping = aes(x = dato, y = D, color = "Fast forrentet med afdragsfrihed")) +
  labs(title = bquote(bold("B.1") ~ "Udlånstyper som andel af samlede realkreditudlån"), x = "Årstal", y = "%-andel.")  + labs(color="") +
  scale_y_continuous(name = "%-andel", breaks = scales::pretty_breaks(n = 14)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("2 years")) +
  theme(legend.position="bottom", legend.box = "horizontal")

#my <- grid.arrange(outloan, arrangeGrob(interest_rate, ncol = 1, nrow = 1)

#ggsave("Udlånstyper.pdf",outloan,width = 30,height = 12,units = "cm",path ="~/Desktop")

#ggsave("Udlånstyper-obli.pdf",my,width = 40,height = 12,units = "cm",path ="~/Desktop")
