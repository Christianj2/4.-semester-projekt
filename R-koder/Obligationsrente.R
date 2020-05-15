setwd("~/Desktop")
library(ggplot2)
library(scales)

my.data1 <- read.csv2("Obligationsrente1.csv", sep = ";", header = FALSE, skip = 1)
my.data1 <- my.data1[nrow(my.data1):1,]

Dato <- as.Date(paste("1", my.data1$V2, my.data1$V1 , sep = "-"), format = "%w-%W-%Y")

o1 <- ggplot() +
  geom_line(my.data1, mapping = aes(x = Dato, y = my.data1$V4, color = "Lange rente")) +
  geom_line(my.data1, mapping = aes(x = Dato, y = my.data1$V3, color = "Korte rente")) +
  labs(title =bquote(bold("B.2") ~ "Obligationsrente"), x = "Årstal", y = "i %.")  + labs(color="") +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("2 years")) +
  scale_y_continuous(name = "%-vis.", breaks = scales::pretty_breaks(n = 10)) +
  scale_color_manual(values=c("blue2", "red2")) +
  guides(colour = guide_legend(reverse = T)) +
  theme(legend.position="bottom", legend.box = "horizontal");o1

ggsave("Obligationsrente.pdf",o1,width = 30,height = 12,units = "cm",path ="~/Desktop")
