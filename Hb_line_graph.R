setwd("D:/Anaemia_Desktop")
getwd()
Hb<-read_csv("DHIS_Hb2.csv")
Hb


library(tidyverse)
library(zoo)
#Creating the date variable

Hb2<-read_csv("Hb_trend.csv")
names(Hb2)
Hb2$date<-as.Date(Hb$Period, "%d/%m/%Y")
Hb2$date

p <- ggplot(Hb2, aes(x =date))
p <- p + geom_point(aes(y = Hb2$Hb)) +labs(x = "YEAR", y = "Reported Cases (Hb<11g/dl)", caption = "(DHIS2 data, https://hiskenya.org)") + theme_bw() 

p<-p+geom_smooth(aes(y=Hb), 
                   method="auto",se = T,
                   col="blue")

show(p)


