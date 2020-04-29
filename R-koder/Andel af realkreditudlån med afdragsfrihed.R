library(ggplot2)
library(ggthemes)
library(scales)
setwd("~/Desktop")
kredit <- read.csv2("realkredit-afdrag.csv")

kredit$dato <- as.Date(kredit$dato, "%d/%m/%Y")

ggplot() +
  geom_line(kredit, mapping = aes(x = dato, y = af.andel, color = "Afdragsfrie nominallån", group=1)) +
  geom_line(kredit, mapping = aes(x = dato, y = afv.andel, color = "Afdragsfrie variable forrentede nominallån", group=1)) +
  labs(title ="Andel af realkreditudlån med afdragsfrihed", x = "Årstal", y = "i mia. kr.")  + labs(color="") +
  scale_y_continuous(name = "%-vis.", breaks = scales::pretty_breaks(n = 10)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("1 years"))




