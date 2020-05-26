library(ggplot2)
library(scales)
library(cowplot)
library(fpp)
library(stargazer)
setwd("~/Desktop")
dataset <- read.csv2("data2.csv")

#Sæson korrigering af data

kredit_t = ts(dataset$kredit, start = 1950, freq = 4)
decompose_kredit = decompose(kredit_t, "additive")
Kredit_t = kredit_t - decompose_kredit$seasonal
Kredit<-ts(log(Kredit_t), start=1950, freq=4)

boligpriser_t = ts(dataset$huspriser2, start = 1950, freq = 4)
decompose_boligpriser = decompose(boligpriser_t, "additive")
Boligpriser_t = boligpriser_t - decompose_boligpriser$seasonal
Boligpriser <- ts(log(Boligpriser_t), start=1950, freq=4)

BNP<- ts(log(dataset$bnp), start=1950, freq=4)

#Opretning af ny data frame

ny_data <- as.data.frame(dataset$dato)
ny_data$Kredit <- as.numeric(Kredit)
ny_data$BNP <- as.numeric(BNP)
ny_data$Boligpriser <- as.numeric(Boligpriser)

ny_data <- ts(ny_data[2:4], start = 1950, freq =4)

library(mFilter)

#### HP-FILTER ####

#For kredir#
kreditF <- hpfilter(ny_data[,1], freq = 400000)

##For BNP##
bnpF <- hpfilter(ny_data[,2], freq = 400000)

##For huspriser##
husF <- hpfilter(ny_data[,3], freq = 400000)

##Cycle dataframe##
cycles <- cbind(kreditF$cycle, bnpF$cycle, husF$cycle)
plot(cycles)

#### Krydskorrelation ####

library(plyr)

##CFF krydskorrelation med hinanden

auto <- as.data.frame(c(-8:8))
auto <- rename(auto, replace = c("c(-8:8)"="lag"))

#for bnp#

autobnp <- ccf(cycles[,2], cycles[,2], lag.max = 8, plot=F)
auto$autobnp <- as.numeric(autobnp$acf)

#for kredit#
autokredit <- ccf(cycles[,1], cycles[,1], lag.max = 8, plot=F)
auto$autokredit <- as.numeric(autokredit$acf)

#for huspriser#
autohuspriser <- ccf(cycles[,3], cycles[,3], lag.max = 8, plot=F)
auto$autohuspriser <- as.numeric(autohuspriser$acf)

ccf_auto <- ggplot() +
  geom_line(auto, mapping = aes(x = lag, y = autobnp, color = "BNP")) +
  geom_line(auto, mapping = aes(x = lag, y = autohuspriser, color = "Boligpriser")) +
  geom_line(auto, mapping = aes(x = lag, y = autokredit, color = "Kredit")) +
  geom_segment(aes(x = -8, y = 0, xend = 8, yend = 0)) +
  ylim(-1, 1) +
  scale_x_continuous(name = "Lag", breaks = scales::pretty_breaks(n = 16)) +
  theme(legend.position="bottom", legend.box = "horizontal") +
  labs(title ="Autokorrelationer", x = "Lag", y = "Korrelation")  + labs(color="") +
  scale_colour_manual(values = c("BNP" = "#F5766D", "Boligpriser" = "#00BA35", "Kredit" = "#619CFF"))


#CFF Krydskorrelation for kredit
ccf_k1 <- ccf(x=cycles[,3], y=cycles[,1], lag.max = 8, plot = F)
ccf_k2 <- ccf(x=cycles[,2], y=cycles[,1], lag.max = 8, plot = F)

kryds_kredit <- as.data.frame(c(-8:8))
kryds_kredit <- rename(kryds_kredit, replace = c("c(-8:8)"="lag"))

kryds_kredit$ccf_k1 <- as.numeric(ccf_k1$acf)
kryds_kredit$ccf_k2 <- as.numeric(ccf_k2$acf)

ccf_kredit <- ggplot() +
  geom_line(kryds_kredit, mapping = aes(x = lag, y = ccf_k1, color = "Boligpriser")) +
  geom_line(kryds_kredit, mapping = aes(x = lag, y = ccf_k2, color = "BNP")) +
  geom_segment(aes(x = -8, y = 0, xend = 8, yend = 0)) +
  ylim(-1, 1) +
  scale_x_continuous(name = "Lag", breaks = scales::pretty_breaks(n = 16)) +
  theme(legend.position="bottom", legend.box = "horizontal") +
  labs(title ="Krydskorrelationer: Kredit", x = "Lag", y = "Korrelation")  + labs(color="") +
  scale_colour_manual(values = c("BNP" = "#F5766D", "Boligpriser" = "#00BA35"))


#CCF Krydskorrelation for BNP

ccf_b1 <- ccf(x=cycles[,1], y=cycles[,2], lag.max = 8, plot = F)
ccf_b2 <- ccf(x=cycles[,3], y=cycles[,2], lag.max = 8, plot = F)

kryds_bnp <- as.data.frame(c(-8:8))
kryds_bnp <- rename(kryds_bnp, replace = c("c(-8:8)"="lag"))

kryds_bnp$ccf_b1 <- as.numeric(ccf_b1$acf)
kryds_bnp$ccf_b2 <- as.numeric(ccf_b2$acf)

ccf_BNP <- ggplot() +
  geom_line(kryds_bnp, mapping = aes(x = lag, y = ccf_b1, color = "Kredit")) +
  geom_line(kryds_bnp, mapping = aes(x = lag, y = ccf_b2, color = "Boligpriser")) +
  geom_segment(aes(x = -8, y = 0, xend = 8, yend = 0)) +
  ylim(-1, 1) +
  scale_x_continuous(name = "Lag", breaks = scales::pretty_breaks(n = 16)) +
  theme(legend.position="bottom", legend.box = "horizontal") +
  labs(title ="Krydskorrelationer: BNP", x = "Lag", y = "Korrelation")  + labs(color="") +
  scale_colour_manual(values = c("Boligpriser" = "#00BA35", "Kredit" = "#619CFF"))


#CCF Krydskorrelation for huspriser

ccf_h1 <- ccf(x=cycles[,1], y=cycles[,3], lag.max = 8, plot = F)
ccf_h2 <- ccf(x=cycles[,2], y=cycles[,3], lag.max = 8, plot = F)

kryds_hus <- as.data.frame(c(-8:8))
kryds_hus <- rename(kryds_hus, replace = c("c(-8:8)"="lag"))

kryds_hus$ccf_h1 <- as.numeric(ccf_h1$acf)
kryds_hus$ccf_h2 <- as.numeric(ccf_h2$acf)

ccf_huspriser <- ggplot() +
  geom_line(kryds_hus, mapping = aes(x = lag, y = ccf_h1, color = "Kredit")) +
  geom_line(kryds_hus, mapping = aes(x = lag, y = ccf_h2, color = "BNP")) +
  geom_segment(aes(x = -8, y = 0, xend = 8, yend = 0)) +
  ylim(-1, 1) +
  scale_x_continuous(name = "Lag", breaks = scales::pretty_breaks(n = 16)) +
  theme(legend.position="bottom", legend.box = "horizontal") +
  labs(title ="Krydskorrelationer: Boligpriser", x = "Lag", y = "Korrelation")  + labs(color="") +
  scale_colour_manual(values = c("BNP" = "#F5766D", "Kredit" = "#619CFF"))

#Plot med alle fire figurer

plot_grid(ccf_auto, ccf_kredit, ccf_BNP, ccf_huspriser, nrow = 2, ncol = 2)

# Tabel over de tre krydskorrelationstest

stargazer(kryds_kredit[1:17,], summary=FALSE, rownames=FALSE,type = 'latex')
stargazer(kryds_bnp[1:17,], summary=FALSE, rownames=FALSE,type = 'latex')
stargazer(kryds_hus[1:17,], summary=FALSE, rownames=FALSE,type = 'latex')
