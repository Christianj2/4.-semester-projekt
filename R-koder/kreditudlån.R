library(ggplot2)
library(ggthemes)
library(scales)
setwd("~/Desktop")
kredit <- read.csv2("creditloan.csv")

kredit$Dato <- as.Date(kredit$Dato, "%d/%m/%Y")

ggplot() +
  geom_line(kredit, mapping = aes(x = Dato, y = Samlet.udlån, color = "Samlet udlån", group=1)) +
  geom_line(kredit, mapping = aes(x = Dato, y = Fast.forrentede.lån, color = "Fast forrentede lån", group=1)) +
  geom_line(kredit, mapping = aes(x = Dato, y = Variabelt.forrentede.nominallån, color = "Variabelt forrentede lån", group=1)) + 
  labs(title ="Realkreditinstitutternes udlån", x = "Årstal", y = "i mia. kr.")  + labs(color="") +
  scale_y_continuous(name = "i mia. kr.", breaks = scales::pretty_breaks(n = 10)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("1 years"))

ggplot() +
  geom_line(kredit, mapping = aes(x = Dato, y = Samlet.udlån.1, color = "Samlet udlån", group=1)) +
  geom_line(kredit, mapping = aes(x = Dato, y = Fast.forrentede.lån.1, color = "Fast forrentede lån", group=1)) +
  geom_line(kredit, mapping = aes(x = Dato, y = Variabelt.forrentede.nominallån.1, color = "Variabelt forrentede lån", group=1)) +
  labs(title ="Realkreditinstitutternes udlån med afdragsfrihed", x = "Årstal", y = "i mia. kr.")  + labs(color="") +
  scale_y_continuous(name = "i mia. kr.", breaks = scales::pretty_breaks(n = 10)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("1 years"))




