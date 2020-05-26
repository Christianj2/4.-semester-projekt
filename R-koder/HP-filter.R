library(mFilter)
library(ggplot2)
library(gridExtra)
library(zoo)
library(grid)
library(fpp)
setwd("~/Desktop")
data <- read.csv2("data2.csv")

# Sæsonkorrigering af data #

BNP_t<- ts(log(data$bnp), start=1980, freq=4)

kredit_t = ts(data$kredit, start = 1980, freq = 4)
decompose_kredit = decompose(kredit_t, "additive")
Kredit_t = kredit_t - decompose_kredit$seasonal
Kredit<-ts(log(Kredit_t), start=1980, freq=4)

boligpriser_t = ts(data$huspriser2, start = 1980, freq = 4)
decompose_boligpriser = decompose(boligpriser_t, "additive")
Boligpriser_t = boligpriser_t - decompose_boligpriser$seasonal
Boligpriser <- ts(log(Boligpriser_t), start=1980, freq=4)

#### HP-filter ####
BNP.hp <- hpfilter(BNP_t, freq = 200000) #32.000 #100.000 #200.000
plot(BNP.hp)

Kredit.hp <- hpfilter(Kredit, freq = 200000) #32.000 #100.000 #200.000
plot(Kredit.hp)

Boligpriser.hp <- hpfilter(Boligpriser, freq = 200000) #32.000 #100.000 #200.000
plot(Boligpriser.hp)

plot(Boligpriser.hp$cycle)
lines(Kredit.hp$cycle, col="red")
lines(BNP.hp$cycle, col="blue")


#### GG-plot for boligpriser ####

objectList = list(Boligpriser.hp$x,Boligpriser.hp$trend,Boligpriser.hp$cycle)
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

#### GG-plot for kredit ####

objectList1 = list(Kredit.hp$x,Kredit.hp$trend,Kredit.hp$cycle)
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

#### GG-plot for BNP ####

objectList2 = list(BNP.hp$x,BNP.hp$trend,BNP.hp$cycle)
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

#### GGPLOT med begge y akser ####

 p <- ggplot() + 
  geom_line(data = cycleSeries, aes(x = date, y = cycle*100, color = "Boligpriser")) +
  geom_line(data = cycleSeries2, aes(x = date, y = cycle*500, color = "BNP")) + 
  geom_line(data = cycleSeries1, aes(x = date, y = cycle*100, color = "Kredit")) +
  labs(title ="Cykliske komponenter", x = "Årstal", y = "Cykliske udsving (%)")  + labs(color="") +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
  scale_y_continuous(name = "%-vis afvigelse fra trenden", sec.axis = sec_axis( trans=~./5, name="%-vis afvigelse fra BNP-trend", breaks = scales::pretty_breaks(n = 10)), breaks = scales::pretty_breaks(n = 10))

ggsave("Cykliske komponenter.pdf",p,width = 30,height = 12,units = "cm",path ="~/Desktop")

#########

#### HP-Filter for Boligpriser ####

b_1 <- ggplot() +
  geom_line(data = seriesTrend, aes(x = date, y=Boligpriser.hp$x, color='Boligpriser')) +
  geom_line(data = seriesTrend, aes(x = date, y=Boligpriser.hp$trend, color = "Trend")) +
  scale_color_manual(values=c("blue1", "red1")) +
  labs(title ="Hodrick-Prescott Filter for boligpriser", x = "Årstal", y = "")  + labs(color="") +
  theme(legend.position = 'bottom', 
        legend.spacing.x = unit(1.0, 'cm'))

b_2 <- ggplot() +
  geom_line(data=cycleSeries, aes(x = date, y= Boligpriser.hp$cycle, color="Afvigelser fra trenden")) +
  labs(title ="Cykliske komponent", x = "Årstal", y = "")  + labs(color="") +
  scale_color_manual(values=c("blue1")) +
  theme(legend.position = 'bottom', 
        legend.spacing.x = unit(1.0, 'cm'))

#### HP-filter for BNP ####

brutto_1 <- ggplot() +
  geom_line(data = seriesTrend2, aes(x = date , y=BNP.hp$x, color='BNP')) +
  geom_line(data = seriesTrend2, aes(x = date, y=BNP.hp$trend, color = "Trend")) +
  scale_color_manual(values=c("blue1", "red1")) +
  labs(title ="Hodrick-Prescott Filter for BNP", x = "Årstal", y = "")  + labs(color="") +
  theme(legend.position = 'bottom', 
        legend.spacing.x = unit(1.0, 'cm'))

brutto_2 <- ggplot() +
  geom_line(data=cycleSeries2, aes(x = date, y= BNP.hp$cycle, color="afvigelser fra trenden")) +
  labs(title ="Cykliske komponent", x = "Årstal", y = "")  + labs(color="") +
  scale_color_manual(values=c("blue1")) +
  theme(legend.position = 'bottom', 
        legend.spacing.x = unit(1.0, 'cm'))

#### HP-Filter for kredit ####

k_1 <- ggplot() +
  geom_line(data = seriesTrend1, aes(x = date, y=Kredit.hp$x, color='Kredit')) +
  geom_line(data = seriesTrend1, aes(x = date, y=Kredit.hp$trend, color = "Trend")) +
  scale_color_manual(values=c("blue1", "red1")) +
  labs(title ="Hodrick-Prescott Filter for kredit", x = "Årstal", y = "")  + labs(color="") +
  theme(legend.position = 'bottom', 
        legend.spacing.x = unit(1.0, 'cm'))

k_2 <- ggplot() +
  geom_line(data=cycleSeries, aes(x = date, y= Kredit.hp$cycle, color="Afvigelser fra trenden")) +
  labs(title ="Cykliske komponent", x = "Årstal", y = "")  + labs(color="") +
  scale_color_manual(values=c("blue1")) +
  theme(legend.position = 'bottom', 
        legend.spacing.x = unit(1.0, 'cm'))

#### MULTIPLOT-funktion ####
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}            

#### 3 figurer med multiplot ####
multiplot(b_1,b_2)
multiplot(brutto_1,brutto_2)
multiplot(k_1,k_2)

#### Stresstest med forskellige lambda-værdier ####

grid.arrange(p,                             # First row with one plot spaning over 2 columns
             arrangeGrob(b_1, brutto_1, k_1, ncol = 2), # Second row with 2 plots in 2 different columns
             nrow = 2)


#ggsave("house.pdf", house, width = 30,height = 12,units = "cm",path ="~/Desktop")

