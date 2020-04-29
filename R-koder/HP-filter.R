library(mFilter)
library(ggplot2)
library(gridExtra)
library(zoo)
library(grid)
setwd("~/Desktop")
Brutto <- read.csv2("BNP.csv")
Kredit <- read.csv2("Kredit.csv")
Bolig <- read.csv2("Boligpriser.csv")
data <- read.csv2("data2.csv")

BNP_1<-ts(log(Brutto$BNP), start=1970, freq=4)
Kredit_1<-ts(log(Kredit$Total), start=1970, freq=4)
Boligpriser_1 <- ts(log(Bolig$Boligpriser), start=1970, freq=4)

BNP_2 <- ts(log(data$bnp), start=1971, freq=4)
Kredit_2<-ts(log(data$kredit), start=1971, freq=4)
Boligpriser_2 <- ts(log(data$huspriser), start=1971, freq=4)

## HP-filter

BNP.hp <- hpfilter(BNP_1, freq = 16000)
plot(BNP.hp)

Kredit.hp <- hpfilter(Kredit_1)
plot(Kredit.hp)

Boligpriser.hp <- hpfilter(Boligpriser_1, freq=16000)
plot(Boligpriser.hp)

plot(Boligpriser.hp$cycle)
lines(Kredit.hp$cycle, col="red")
lines(BNP.hp$cycle, col="blue")

BNP_2.hp <- hpfilter(BNP_2, freq = 32000) #100.000
plot(BNP_2.hp)

Kredit_2.hp <- hpfilter(Kredit_2, freq = 200000) #400.000
plot(Kredit_2.hp)

Boligpriser_2.hp <- hpfilter(Boligpriser_2, freq=400000)
plot(Boligpriser_2.hp)

plot(Boligpriser_2.hp$cycle)
lines(Kredit_2.hp$cycle, col="red")
lines(BNP_2.hp$cycle, col="blue")

##GG-plot for boligpriser

objectList = list(Boligpriser_2.hp$x,Boligpriser_2.hp$trend,Boligpriser_2.hp$cycle)
names(objectList) = c("Boligpriser","trend","cycle")

fn_ts_to_DF = function(x)  {
  
  DF = data.frame(date=zoo::as.Date(time(objectList[[x]])),tseries=as.matrix(objectList[[x]])) 
  colnames(DF)[2]=names(objectList)[x]
  return(DF)
}

DFList=lapply(seq_along(objectList),fn_ts_to_DF)
names(DFList) = c("Boligpriser","trend","cycle")

seriesTrend = merge(DFList$Boligpriser,DFList$trend,by="date")
cycleSeries = DFList$cycle


ggplot(cycleSeries,aes(x=date,y=cycle)) + geom_line(color="#619CFF") + ggtitle("Cyclical component (deviations from trend)")

##GG-plot for kredit

objectList1 = list(Kredit_2.hp$x,Kredit_2.hp$trend,Kredit_2.hp$cycle)
names(objectList1) = c("Kredit","trend","cycle")

fn_ts_to_DF = function(x)  {
  
  DF = data.frame(date=zoo::as.Date(time(objectList1[[x]])),tseries=as.matrix(objectList1[[x]])) 
  colnames(DF)[2]=names(objectList1)[x]
  return(DF)
}

DFList1=lapply(seq_along(objectList1),fn_ts_to_DF)
names(DFList1) = c("Kredit","trend","cycle")

seriesTrend1 = merge(DFList1$Kredit,DFList$trend,by="date")
cycleSeries1 = DFList1$cycle


ggplot(cycleSeries1,aes(x=date,y=cycle)) + geom_line(color="#619CFF") + ggtitle("Cyclical component (deviations from trend)")

##GG-plot for BNP

objectList2 = list(BNP_2.hp$x,BNP_2.hp$trend,BNP_2.hp$cycle)
names(objectList2) = c("BNP","trend","cycle")

fn_ts_to_DF = function(x)  {
  
  DF = data.frame(date=zoo::as.Date(time(objectList2[[x]])),tseries=as.matrix(objectList2[[x]])) 
  colnames(DF)[2]=names(objectList2)[x]
  return(DF)
}

DFList2=lapply(seq_along(objectList2),fn_ts_to_DF)
names(DFList2) = c("BNP","trend","cycle")

seriesTrend2 = merge(DFList2$BNP,DFList$trend,by="date")
cycleSeries2 = DFList2$cycle


ggplot(cycleSeries2,aes(x=date,y=cycle)) + geom_line(color="#619CFF") + ggtitle("Cyclical component (deviations from trend)") +
 geom_line(cycleSeries, aes(x=date, y=cycle))

ggplot() + 
  geom_line(data = cycleSeries, aes(x = date, y = cycle, color = "Boligpriser")) +
  geom_line(data = cycleSeries1, aes(x = date, y = cycle, color = "Kredit")) +
  geom_line(data = cycleSeries2, aes(x = date, y = cycle, color = "BNP")) +
  labs(title ="Cykliske komponent (afvigelse fra trenden)", x = "Årstal", y = "Cykliske udsving (%)")  + labs(color="") +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + expand_limits(y=c(-0.3,0.4))

#GGPLOT med begge y akser

ggplot() + 
  geom_line(data = cycleSeries, aes(x = date, y = cycle*100, color = "Boligpriser")) +
  geom_line(data = cycleSeries2, aes(x = date, y = cycle*500, color = "BNP")) + 
  geom_line(data = cycleSeries1, aes(x = date, y = cycle*100, color = "Kredit")) +
  labs(title ="Cykliske komponent (afvigelse fra trenden)", x = "Årstal", y = "Cykliske udsving (%)")  + labs(color="") +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
  scale_y_continuous(name = "%-vis afvigelse fra trenden", sec.axis = sec_axis( trans=~./5, name="%-vis afvigelse fra BNP-trend", breaks = scales::pretty_breaks(n = 10)), breaks = scales::pretty_breaks(n = 10))
