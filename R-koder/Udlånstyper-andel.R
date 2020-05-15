library(ggplot2)
library(ggthemes)
library(scales)
library(tidyverse)
library(dplyr)
library(magrittr)

setwd("~/Desktop")
kredit_andel <- read.csv2("realkredit-andel.csv")

kredit_andel$dato <- as.Date(kredit_andel$dato, "%d/%m/%Y")

p1 <- ggplot() +
  geom_line(kredit_andel, mapping = aes(x = dato, y = A, color = "Fast forrentet")) +
  geom_line(kredit_andel, mapping = aes(x = dato, y = C, color = "Variabelt forrentet med afdragsfrihed")) +
  geom_line(kredit_andel, mapping = aes(x = dato, y = B, color = "Variabelt forrentet")) +
  labs(title = bquote(bold("B.1") ~ "Udlånstyper som andel af samlede realkreditudlån"), x = "Årstal", y = "%-andel.")  + labs(color="") +
  scale_y_continuous(name = "%-andel", breaks = scales::pretty_breaks(n = 14)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("2 years")) +
  scale_color_manual(values=c("blue2", "red2", "darkorchid2")) +
  theme(legend.position="bottom", legend.box = "horizontal")

my <- grid.arrange(p1,                             # First row with one plot spaning over 2 columns
             arrangeGrob(o1, ncol = 1), # Second row with 2 plots in 2 different columns
             nrow = 1)


ggsave("Udlånstyper-obli.pdf",my,width = 40,height = 12,units = "cm",path ="~/Desktop")
