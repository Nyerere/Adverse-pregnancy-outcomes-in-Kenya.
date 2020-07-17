library(ggplot2)

setwd("C:/Users/Student/OneDrive - University of Kwazulu-Natal/PhD_UKZN/Anemia Risk Surface")
getwd()


#importing data into R
FHS<-read.csv("frmgham2.csv")
names(FHS)

#Converting sex to a factor variable
FHS$SEX<-factor(x = FHS$SEX,
                levels = c(1,2),
                labels = c("M","F"))

q<-ggplot(data = FHS,
          aes(x=DIABP,
              y=SYSBP,
              color = SEX))
q<-q + geom_point(aes(x=DIABP,
                      y=SYSBP,
                      color = SEX))
q = q + stat_smooth(aes(x =DIABP,
                        y = SYSBP,
                        color = SEX),
                    method = "loess",
                    se=F);

q = q + scale_color_manual(values = c("blue","green"));

show(q)


###BOX PLOTS

q1<-ggplot(data = FHS) + theme_bw();
q1<-q1 + geom_boxplot(aes(x=SEX,
                          y=BMI));
q1<-q1 + coord_flip();
show(q1)


FHS$AGEcut<-cut(x=FHS$AGE, breaks = c(30,40,50,60,70,80,90),
                labels = c("30s","40s","50s","60s","70s","80s"),
                include.lowest = T);

#Stratify the data and getting the Boxplot
q2<-ggplot(data = FHS) + theme_bw();
q2<-q2 + geom_boxplot(aes(x=AGEcut,y=BMI, color=SEX));
q2<-q2 + scale_colour_manual(values = c("royalblue","coral"));
show(q2)

#adding the colour argument
q3<-ggplot
q3<-q3 + geom_boxplot(aes(x=AGEcut,y=BMI, fill=SEX));
q3<-q3 + scale_colour_manual(values = c("royalblue","coral"));
show(q3)

###
q4<-ggplot(data=FHS[FHS$AGE<80,]) +theme_bw();
q4<-q4 + geom_boxplot(aes(x=AGEcut,y=BMI, fill=SEX));
q4<-q4 + scale_colour_manual(values = c("royalblue","coral"));
q4<-q4 + labs(x ="Age group",fill ="Sex");
q4<-q4 + ggtitle("BMI by Age and Sex")
show(q4)

