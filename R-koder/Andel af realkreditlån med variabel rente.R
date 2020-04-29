library(ggplot2)
library(ggthemes)
library(scales)
setwd("~/Desktop")
kredit <- read.csv2("realkredit.csv")

kredit$dato <- as.Date(kredit$dato, "%d/%m/%Y")

ggplot() +
  geom_line(kredit, mapping = aes(x = dato, y = andelvariabel, color = "Variabelt forrentede nominallån", group=1)) +
  theme(axis.text.x = element_text(angle = 0)) + 
  labs(title ="Andel af realkreditudlån med variabel rente", x = "Årstal", y = "i mia. kr.")  + labs(color="") +
  scale_y_continuous(name = "%-vis.", breaks = scales::pretty_breaks(n = 10)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("1 years"))




