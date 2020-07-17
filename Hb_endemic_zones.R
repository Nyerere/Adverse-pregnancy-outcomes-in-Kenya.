setwd("D:/Anaemia_Desktop")
getwd()
Hb<-read_csv("DHIS_Hb3.csv")
View(Hb)

library(tidyverse)
library(zoo)
#Creating the date variable
Hb$date<-as.Date(Hb$Period, "%d-%m-%y")
Hb$date

Hb$Risk<-factor(Hb$RiskID,
                    levels = c(1,2,3,4,5),
                    labels = c("Lake endemic","Coast endemic","Highland","Seasonal","Low risk"))


pp<-ggplot(Hb, aes(x = date, y = Hb), col = Risk) +
  geom_line() +
  facet_wrap(~Month, scales = "free") +
  theme_bw() +
  theme(panel.grid = element_blank()) + xlab("Date") + ylab("No.of Clients (Hb<11g/dl)")

show(pp)

###################
p <- ggplot(Hb, aes(x =date), col= Month)
p <- p + geom_point(aes(y = Hb, na.rm=FALSE, colour = "No.of Clients (Hb<11g/dl)")) +facet_wrap(~Risk, scales = "free") +
  theme_bw()

p<-p+geom_smooth(aes(y=Hb), 
                 method="loess",se = FALSE,
                 col="blue")

show(p)



hh
p <- ggplot(Hb, aes(x =date, y = Hb))
p <- p + geom_line(aes(color = Ris)) 

p<-p+geom_smooth(aes(y=Risk), 
                 method="loess",se = TRUE,
                 col="red")

show(p)


