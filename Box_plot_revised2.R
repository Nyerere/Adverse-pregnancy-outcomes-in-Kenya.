library(tidyverse)
library(zoo)
library(lubridate)
library(forecast)


#setwd("D:/Anaemia_Desktop/2016 - 2018")
#getwd()
#Hb_data<-read_csv("DHIS_Final.csv")

setwd("C:/Users/nyere/OneDrive - University of Kwazulu-Natal/PhD_UKZN/Research data")
getwd()
Hb2<-read_csv("Quarterly_Data_Analysis.csv")
str(Hb2) #



#BOX PLOT FOR REPORTED CASES - 290 COUNTIES
Year<-as.factor(Hb_data$year)

#TREND
#p <- ggplot(Hb_data, mapping = aes(Date,hb, group = endemicity)) +labs(x = "YEAR", y = "Prevalence (%)")+ theme_grey()
#p <- p + geom_line()
#show(p)

#ggplot(Hb_data, aes(x=Year, y=hb, group=endemicity)) +
# geom_line(aes(linetype= county))+
#geom_point()

#Hb2<-read_csv("Hb_trend.csv")
names(Hb2)
Hb2$date<-as.Date(Hb2$Date, "%d/%m/%Y")
Hb2$date

#####
#Seasonal Plot Type1
####################################################################################

#Plot Type2
####################################################################################

# plot
ggplot(Hb2, aes(x=date)) + 
  geom_point(aes(y=Hb)) + geom_smooth(aes(y=Hb))
  labs(title="Sub-county Series", 
       subtitle="Returns Percentage from Economics Dataset", 
       caption="Source: DHIS2", 
       y="Reported Hb<11g/dl") +  # title and caption
  scale_x_date(labels = lbls, 
               breaks = brks) +  # change to monthly ticks and labels
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),  # rotate x axis text
        panel.grid.minor = element_blank())  # turn off minor grid


###################################################################################
p <- ggplot(Hb2, aes(x =date, Hb))
p <- p + geom_point(aes(y = Hb2$Hb))
p<-p+geom_smooth(aes(y=Hb, colour="Hb cases"), 
                 method="auto",se = F) 
names(Hb2)

p <- p + geom_point(aes(x=date, y = Hb2$Prev*10000))
p<-p+geom_smooth(aes(y=Hb2$Prev*10000, colour = "Prevalence"),
                 method="loess",se = T)
p <- p + scale_y_continuous(sec.axis = sec_axis(~./100, name = "Prevalence (%)"))

p <- p + theme(legend.position = c(0.8, 0.9))

p <- p + scale_colour_manual("", values = c("blue", "red")) + theme(legend.position = c(0.9, 0.9)) + theme_classic()
p <- p + labs(y = "Reported cases (Hb<11g/dl)",
              x = "YEAR")

p <- p + labs(y = "Reported cases (Hb<11g/dl)",
              x = "YEAR") 
p <- p + theme(legend.position = c(0.8, 0.9)) 

show(p)


#p <- ggplot(Hb_data, aes(county,hb))
#p <- p + geom_boxplot(aes(y = Hb_data$prevnb), colour = "tomato") +labs(x = "YEAR", y = "Prevalence (%)", caption = "(DHIS2 data, https://hiskenya.org)") + theme_grey() + coord_flip()

#p<-p+geom_smooth(aes(y=Hb_data$prevnb), 
# method="loess",se = F,
#col="blue")

#show(p)


#BOXPLOT FOR COUNTS
p1 <- ggplot(Hb_data, aes(Year,hb)) +labs(x = "YEAR", y = "Reported Cases [Hb<11g/dl]", title = "Reported case count, 2016 - 2018")+ theme_grey()
p1 <- p1  +  stat_boxplot(geom ='errorbar', width = 0.1) + geom_boxplot(fill = "white", colour = "black", alpha = 0.45, width = 0.6)  + scale_y_continuous(breaks = pretty(c(0,8000), n = 5))+ geom_jitter(color = "tomato", alpha = 0.10)
#geom_jitter(alpha = 0.29, color = "tomato")

show(p1)

p1<-p1+geom_smooth(aes(y=hb), 
                   method="auto",se = T,
                   col="green")
show(p1)

#BOX PLOT FOR PREVALENCE
p2 <- ggplot(Hb_data, aes(Year, prevnb*100)) +labs(x = "YEAR", y = "Prevalence (%)", title = "Prevalence, 2016 - 2018") +  theme_grey() 
p2<- p2 + stat_boxplot(geom ='errorbar', width = 0.1) + geom_boxplot(colour = "black") + geom_jitter(color = "red", alpha = 0.10)
show(p2)


#PLOTTING THREE PLOTS

ggarrange(p,                                                 # First row with scatter plot
          ggarrange(p1, p2, ncol = 2, labels = c("B", "C")), # Second row with box and dot plots
          nrow = 2, 
          labels = "A"                                        # Labels of the scatter plot
) 


#####
pdata<-read_csv("Prev_type2.csv")
pdata
pdata$Prev<-pdata$prevnb*100

Year<-as.factor(pdata$year)
Category<-pdata$Prev_type
Prev_type<-as.factor(pdata$Prev_type)
id<-as.factor(pdata$id)

p3 <- ggplot(pdata, aes(Year, Prev, color = Category)) +labs(x = "YEAR", y = "Prevalence (%)", title = "Prevalence, 2016 - 2018",  caption = "(DHIS2 data, https://hiskenya.org)") +  theme_grey() 
p3<- p3 + stat_boxplot(geom ='errorbar', width = 0.6) + geom_boxplot(width = 0.6, alpha = 0.99)  + geom_jitter(width = 0.75, alpha = 0.12)
show(p3)

library(ggpubr)

figure<- (ggarrange(p, p3, ncol = 1, labels = c("B", "C")))  # Label of the line plot

show(figure)




#COUNTY - BOX PLOTS
names(Hb_data)

pp1<- ggplot(Hb_data, aes(x=county,hb, fill = endemicity)) + geom_boxplot(show.legend = FALSE) + stat_boxplot(geom ='errorbar', width = 0.3) + coord_flip() +  labs(x = "County", y = "Reported Cases (Hb<11g/dl)", title = "Reported cases by county") + theme_gray()

show(pp1)


pp2<- ggplot(Hb_data, aes(x="",prevnb*100, fill = endemicity)) + geom_boxplot()  +
  labs(x = "", y = "Estimated Prevalence (%)", fill = " Endemic zone" ,title = " Prevalence by malaria endemic zones, 2016 - 2018", caption = "(DHIS2 data, https://hiskenya.org)") + theme_gray()

show(pp2)

library(ggpubr)

figure2<- ggarrange(pp1, pp2, ncol = 2, labels = c("A", "B"), 
                    nrow = 1)    # Label of the line plot

show(figure2)



#COMBINED
p2 <- ggplot(Hb_data, aes(Year,Prev, fill = Endemicity)) +labs(x = "Region", y = "Reported Cases (Hb<11g/dl)", caption = "(DHIS2 data, https://hiskenya.org)") + theme_bw() 
p2 <- p2 + geom_boxplot(aes(group = cut_width(Year, 0.25)),fill = "white", colour = "#3366FF")
show(p2)



#OTHER

p3 <- ggplot(Hb_data, aes(County,Hb)) +labs(x = "Region", y = "Reported Cases (Hb<11g/dl)", caption = "(DHIS2 data, https://hiskenya.org)") + theme_bw() 
p3 <- p3 + geom_boxplot(aes(fill = Endemicity)) +coord_flip()+
  scale_fill_manual(values = c("#00AFBB", "#E7B699", "#FC0E87", "#E7B450", "#FC4E17", "#FC9E77"))
show(p3)


#COMBINED DATA

Hb_data%>%
  ggplot( aes(x=Year, y=Hb, fill=Endemicity)) +
  geom_boxplot() +
  
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_bw()+
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("A boxplot with jitter") +
  xlab("")
