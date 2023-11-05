sleepdata <- read.csv("Sleep_Efficiency.csv")
summary(sleepdata)
head(sleepdata)
library(tidyr) #will use the seperate function from this library to modify the bed time variable
library(lattice) #for bwplots
library(plotrix) #for fan plots
library(car) #for density plots
library(smoothr)

#Getting rid of the NA values
sleepdata <- sleepdata[!is.na(sleepdata$Exercise.frequency),]
sleepdata <- sleepdata[!is.na(sleepdata$Alcohol.consumption),]
sleepdata <- sleepdata[!is.na(sleepdata$Caffeine.consumption),]
sleepdata <- sleepdata[!is.na(sleepdata$Bedtime),]
sleepdata <- sleepdata[!is.na(sleepdata$Awakenings),]

#Converting Gender variable to a factor
sleepdata$Gender <- as.factor(sleepdata$Gender)

#Creating a new categorical variable categorizing subjects into those who slept before midnight and those who slept after
sleepdata <- separate(sleepdata,Bedtime,into=c('sleep.date','sleep.time'),sep=' ',remove=T) #Separated Bedtime variable into two different variables of date and time
sleepdata <- separate(sleepdata,sleep.time,into=c("hour","minute","second"),sep=":",remove=T) #Separated the new bed time variable into theree new variables of hour, minute, and second.
sleepdata$hour <- as.numeric(sleepdata$hour) #Converts the hour variable into numeric so that the variable can be included in graphing.

midnight<-c() #the new midnight variable categorizes subjects into those who went to bed before midnight and those who went after.
midnight[sleepdata$hour>18]<-"Before Midnight"
midnight[sleepdata$hour<18]<-"After Midnight"
sleepdata<-cbind(sleepdata,midnight)
remove(midnight)
sleepdata$midnight <- factor(sleepdata$midnight,levels=c("Before Midnight","After Midnight")) #converts the new midnight variable into factor

#Creating a new dataset where adults (>=18) got 7+ hours of sleep and children (<18) got 9+ hours of sleep
#Will only be using this dataset when using the midnight vaiable to graph. I want to investigate the impact of bed time on sleep efficiency and I don't want sleep duration to become a confounding variable given that those who go to sleep late might also be getting less sleep because of that.
enoughsleep <- sleepdata[(sleepdata$Age<18 & sleepdata$Sleep.duration>=9),]
enoughsleep <- sleepdata[(sleepdata$Age>=18 & sleepdata$Sleep.duration>=7),]

##Changing the level names of the smoking status variable to make it more explanatory
sleepdata$Smoking.status <- as.character(sleepdata$Smoking.status)
sleepdata$Smoking.status[sleepdata$Smoking.status=="Yes"] <- "Smoker"
sleepdata$Smoking.status[sleepdata$Smoking.status=="No"] <- "Non-smoker"
sleepdata$Smoking.status <- as.factor(sleepdata$Smoking.status)

#Creating a new variable that categorizes exercize frequency into levels
exerciselvl<-c()
exerciselvl[sleepdata$Exercise.frequency == 0] <- "No exercise"
exerciselvl[sleepdata$Exercise.frequency >=1 & sleepdata$Exercise.frequency <= 2] <- "Light exercise"
exerciselvl[sleepdata$Exercise.frequency == 3] <- "Medium exercise"
exerciselvl[sleepdata$Exercise.frequency >= 4 & sleepdata$Exercise.frequency <= 5] <- "Heavy exercise"
sleepdata<-cbind(sleepdata,exerciselvl)
remove(exerciselvl)
sleepdata$exerciselvl <- factor(sleepdata$exerciselvl,levels=c("No exercise","Light exercise","Medium exercise","Heavy exercise")) #using factor to put the levels in an order that makes sense

#Creating a new variable that categorizes caffeine use in the past 24 hours into yes or no
caffeine<-c()
caffeine[sleepdata$Caffeine.consumption == 0] <- "No"
caffeine[sleepdata$Caffeine.consumption > 0] <- "Yes"
sleepdata<-cbind(sleepdata,caffeine)
remove(caffeine)
sleepdata$caffeine <- factor(sleepdata$caffeine)

#Creating a new variable that categorizes alcohol use in the past 24 hours into yes or no
alcohol<-c()
alcohol[sleepdata$Alcohol.consumption == 0] <- "No"
alcohol[sleepdata$Alcohol.consumption > 0] <- "Yes"
sleepdata<-cbind(sleepdata,alcohol)
remove(alcohol)
sleepdata$alcohol <- factor(sleepdata$alcohol)

#Removing the variables that I won't be using from the dataset
sleepdata <- sleepdata[,-c(1,4,5,6,7,8,15,16,18)]


## Graphing starts here:

############# Box Plots ################

#Creating summary statistics and boxplots with the exercise level variable
summary(sleepdata$Sleep.efficiency[sleepdata$exerciselvl=="No exercise"])
summary(sleepdata$Sleep.efficiency[sleepdata$exerciselvl=="Light exercise"])
summary(sleepdata$Sleep.efficiency[sleepdata$exerciselvl=="Medium exercise"])
summary(sleepdata$Sleep.efficiency[sleepdata$exerciselvl=="Heavy exercise"])
boxplot(Sleep.efficiency~exerciselvl,data=sleepdata,ylab="Proportion of Time in Bed Spent Asleep",xlab="Exercise Level",main="Sleep Efficiency by Exercise Level",col=c("deepskyblue1","darkolivegreen3","gold","firebrick1"))

summary(sleepdata$REM.sleep.percentage[sleepdata$exerciselvl=="No exercise"])
summary(sleepdata$REM.sleep.percentage[sleepdata$exerciselvl=="Light exercise"])
summary(sleepdata$REM.sleep.percentage[sleepdata$exerciselvl=="Medium exercise"])
summary(sleepdata$REM.sleep.percentage[sleepdata$exerciselvl=="Heavy exercise"])
boxplot(REM.sleep.percentage~exerciselvl,data=sleepdata,ylab="Percentage of Time Asleep Spent in REM Sleep",xlab="Exercise Level",main="REM Sleep Percentage by Exercise Level",col=c("deepskyblue1","darkolivegreen3","gold","firebrick1"))

summary(sleepdata$Deep.sleep.percentage[sleepdata$exerciselvl=="No exercise"])
summary(sleepdata$Deep.sleep.percentage[sleepdata$exerciselvl=="Light exercise"])
summary(sleepdata$Deep.sleep.percentage[sleepdata$exerciselvl=="Medium exercise"])
summary(sleepdata$Deep.sleep.percentage[sleepdata$exerciselvl=="Heavy exercise"])
boxplot(Deep.sleep.percentage~exerciselvl,data=sleepdata,ylab="Percentage of Time Asleep Spent in Deep Sleep",xlab="Exercise Level",main="Deep Sleep Percentage by Exercise Level",col=c("deepskyblue1","darkolivegreen3","gold","firebrick1"))

summary(sleepdata$Light.sleep.percentage[sleepdata$exerciselvl=="No exercise"])
summary(sleepdata$Light.sleep.percentage[sleepdata$exerciselvl=="Light exercise"])
summary(sleepdata$Light.sleep.percentage[sleepdata$exerciselvl=="Medium exercise"])
summary(sleepdata$Light.sleep.percentage[sleepdata$exerciselvl=="Heavy exercise"])
boxplot(Light.sleep.percentage~exerciselvl,data=sleepdata,ylab="Percentage of Time Asleep Spent in Light Sleep",xlab="Exercise Level",main="Light Sleep Percentage by Exercise Level",col=c("deepskyblue1","darkolivegreen3","gold","firebrick1"))

#Creating summary statistics and boxplots with the midnight variable
summary(enoughsleep$Sleep.efficiency[sleepdata$midnight=="After Midnight"])
summary(enoughsleep$Sleep.efficiency[sleepdata$midnight=="Before Midnight"])
boxplot(enoughsleep$Sleep.efficiency~enoughsleep$midnight,col=c("Cadetblue2","Royalblue3"),ylab="Proportion of Time Spent in Bed Asleep",xlab="Bed Time",main="Sleep Efficiency by Bed Time")

summary(enoughsleep$Light.sleep.percentage[sleepdata$midnight=="After Midnight"])
summary(enoughsleep$Light.sleep.percentage[sleepdata$midnight=="Before Midnight"])
boxplot(Light.sleep.percentage~midnight,data=enoughsleep, xlab="Bed Time",ylab="Percentage of Time Asleep Spent in Light Sleep",main="Light Sleep Percentage by Bed Time",col=c("Cadetblue2","Royalblue3"))

summary(enoughsleep$Deep.sleep.percentage[sleepdata$midnight=="After Midnight"])
summary(enoughsleep$Deep.sleep.percentage[sleepdata$midnight=="Before Midnight"])
boxplot(Deep.sleep.percentage~midnight,data=enoughsleep, xlab="Bed Time",ylab="Percentage of Time Asleep Spent in Deep Sleep",main="Deep Sleep Percentage by Bed Time",col=c("Cadetblue2","Royalblue3"))

summary(enoughsleep$REM.sleep.percentage[sleepdata$midnight=="After Midnight"])
summary(enoughsleep$REM.sleep.percentage[sleepdata$midnight=="Before Midnight"])
boxplot(REM.sleep.percentage~midnight,data=enoughsleep, xlab="Bed Time",ylab="Percentage of Time Asleep Spent in REM Sleep",main="REM Sleep Percentage by Bed Time",col=c("Cadetblue2","Royalblue3"))

############### Histograms ##############

#Histograms with overlaid density plots for Sleep efficiency, Deep sleep percentage, and Light sleep percentage, separated by Bed time
hist(enoughsleep$Sleep.efficiency[enoughsleep$midnight=="Before Midnight"],col=rgb(1,0,0.3,0.25),freq=F,xlab="Proportion of Time in Bed Spent Asleep",main="Distribution of Sleep Efficiency by Bed Time")
hist(enoughsleep$Sleep.efficiency[enoughsleep$midnight=="After Midnight"],col=rgb(0,0.3,1,0.25),add=T,freq=F)
legend("topleft",c("Before Midnight","After Midnight"),col=c(rgb(1,0,0.3,0.25),rgb(0,0.3,1,0.25)),lwd=10)
dens11<-density(enoughsleep$Sleep.efficiency,bw=0.025)
lines(dens11,col="red",lwd=2)

hist(enoughsleep$Deep.sleep.percentage[enoughsleep$midnight=="Before Midnight"],col=rgb(1,0,0.3,0.25),freq=F,xlab="Percentage of Time Asleep Spent in Deep Sleep",main="Distribution of Deep Sleep Percentage by Bed Time",xlim=c(10,80),ylim=c(0,0.078),breaks=seq(0,80,5))
hist(enoughsleep$Deep.sleep.percentage[enoughsleep$midnight=="After Midnight"],col=rgb(0,0.3,1,0.25),add=T,freq=F,breaks=seq(0,80,5))
legend("topleft",c("Before Midnight","After Midnight"),col=c(rgb(1,0,0.3,0.25),rgb(0,0.3,1,0.25)),lwd=10)
dens22<-density(enoughsleep$Deep.sleep.percentage,bw=2)
lines(dens22,col="red",lwd=2)

hist(enoughsleep$Light.sleep.percentage[enoughsleep$midnight=="Before Midnight"],col=rgb(1,0,0.3,0.25),freq=F,xlab="Percentage of Time Asleep Spent in Light Sleep",main="Distribution of Light Sleep Percentage by Bed Time",xlim=c(0,70),ylim=c(0,0.08),breaks=seq(0,80,5))
hist(enoughsleep$Light.sleep.percentage[enoughsleep$midnight=="After Midnight"],col=rgb(0,0.3,1,0.25),add=T,freq=F,breaks=seq(0,80,5))
legend("topright",c("Before Midnight","After Midnight"),col=c(rgb(1,0,0.3,0.25),rgb(0,0.3,1,0.25)),lwd=10)
dens33<-density(enoughsleep$Light.sleep.percentage,bw=2)
lines(dens33,col="red",lwd=2)

#Histograms with overlaid density plots for Sleep efficiency, Deep sleep percentage, and Light sleep percentage, separated by Exercise Level
hist(sleepdata$Sleep.efficiency[sleepdata$exerciselvl=="No exercise"],col=rgb(0,0.2,1,0.25),ylim=c(0,11),freq=F,xlab="Proportion of Time in Bed Spent Asleep",main="Distribution of Sleep Efficiency by Exercise Level",breaks=7)
hist(sleepdata$Sleep.efficiency[sleepdata$exerciselvl=="Light exercise"],add=T,col=rgb(0,1,0,0.25),freq=F,breaks=7)
hist(sleepdata$Sleep.efficiency[sleepdata$exerciselvl=="Medium exercise"],add=T,col=rgb(1,1,0,0.25),freq=F,breaks=7)
hist(sleepdata$Sleep.efficiency[sleepdata$exerciselvl=="Heavy exercise"],add=T,col=rgb(1,0,0,0.25),freq=F,breaks=7)
legend("topleft",c("No exercise","Light exercise","Medium Exercise","Heavy exercise"),col=c(rgb(0,0.2,1,0.25),rgb(0,1,0,0.25),rgb(1,1,0,0.25),rgb(1,0,0,0.25)),lwd=10)
dens1<-density(sleepdata$Sleep.efficiency,bw=0.025)
lines(dens1,col="red",lwd=2)

hist(sleepdata$Deep.sleep.percentage[sleepdata$exerciselvl=="No exercise"],col=rgb(0,0.2,1,0.25),ylim=c(0,0.1),xlim=c(10,80),freq=F,xlab="Percentage of Time Asleep Spent in Deep Sleep",main="Distribution of Deep Sleep Percentage by Exercise Level",breaks=seq(10,100,5))
hist(sleepdata$Deep.sleep.percentage[sleepdata$exerciselvl=="Light exercise"],add=T,col=rgb(0,1,0,0.25),freq=F,breaks=seq(10,100,5))
hist(sleepdata$Deep.sleep.percentage[sleepdata$exerciselvl=="Medium exercise"],add=T,col=rgb(1,1,0,0.25),freq=F,breaks=seq(10,100,5))
hist(sleepdata$Deep.sleep.percentage[sleepdata$exerciselvl=="Heavy exercise"],add=T,col=rgb(1,0,0,0.25),freq=F,breaks=seq(10,100,5))
legend("topleft",c("No exercise","Light exercise","Medium Exercise","Heavy exercise"),col=c(rgb(0,0.2,1,0.25),rgb(0,1,0,0.25),rgb(1,1,0,0.25),rgb(1,0,0,0.25)),lwd=10)
dens2<-density(sleepdata$Deep.sleep.percentage,bw=2)
lines(dens2,col="red",lwd=2)

hist(sleepdata$Light.sleep.percentage[sleepdata$exerciselvl=="No exercise"],col=rgb(0,0.2,1,0.25),ylim=c(0,0.1),xlim=c(5,70),freq=F,xlab="Percentage of Time Asleep Spent in Light Sleep",main="Distribution of Light Sleep Percentage by Exercise Level",breaks=seq(0,100,5))
hist(sleepdata$Light.sleep.percentage[sleepdata$exerciselvl=="Light exercise"],add=T,col=rgb(0,1,0,0.25),freq=F,breaks=seq(0,100,5))
hist(sleepdata$Light.sleep.percentage[sleepdata$exerciselvl=="Medium exercise"],add=T,col=rgb(1,1,0,0.25),freq=F,breaks=seq(0,100,5))
hist(sleepdata$Light.sleep.percentage[sleepdata$exerciselvl=="Heavy exercise"],add=T,col=rgb(1,0,0,0.25),freq=F,breaks=seq(0,100,5))
legend("topright",c("No exercise","Light exercise","Medium Exercise","Heavy exercise"),col=c(rgb(0,0.2,1,0.25),rgb(0,1,0,0.25),rgb(1,1,0,0.25),rgb(1,0,0,0.25)),lwd=10)
dens3<-density(sleepdata$Light.sleep.percentage,bw=2)
lines(dens3,col="red",lwd=2)

##################### Bar Plots ######################

#Barplots using table function
extab <- table(sleepdata$exerciselvl)
barplot(extab,xlab="Exercise Level",ylab="Count",main="Exercise Levels of 432 Subjects",col=c("deepskyblue1","darkolivegreen3","gold","firebrick1")) #Barplot shows how many subjects are in each exercise level

durtab <- table(sleepdata$Sleep.duration)
barplot(durtab,xlab="Hours Slept",ylab="Count",main="Distribution of Sleep Duration",col="salmon") #Barplot shows distribution of subjects based on sleep duration

#Barplots using aggregate function

#Bar plot that illustrates mean sleep efficiency for each exercise level.
avgexeff <- aggregate(Sleep.efficiency~exerciselvl,data=sleepdata, FUN=mean)
barplot(avgexeff$Sleep.efficiency, names.arg=avgexeff$exerciselvl, col=c("deepskyblue1","darkolivegreen3","gold","firebrick1"),xlab="Exercise Level",ylab="Mean Proportion of Time in Bed Spent Asleep",main="Mean Sleep Efficiency by Exercise Level") #this was included in HW3, see below for new plots

#Bar plot that illustrates the mean number of awakenings for each exercise level
awex <- aggregate(Awakenings~exerciselvl,data=sleepdata, FUN=mean)
barplot(awex$Awakenings,names.arg=awex$exerciselvl,col=c("deepskyblue1","darkolivegreen3","gold","firebrick1"),ylab="Mean Number of Awakenings",xlab="Exercise Level",main="Mean Number of Awakenings by Exercise Level",ylim=c(0,2))

#Bar plot that illustrates the mean sleep efficiency separated by bed time
avgeff <- aggregate(Sleep.efficiency~midnight,data=enoughsleep,FUN=mean)
barplot(avgeff$Sleep.efficiency,names.arg=avgeff$midnight,col=c("Cadetblue2","Royalblue3"),ylab="Proportion of Time in Bed Spent Asleep",xlab="Bed Time",main="Mean Sleep Efficiency by Bed Time")

#Bar plot that illustrates the mean REM sleep percentage separated by bed time
avgREM <- aggregate(REM.sleep.percentage~midnight,data= enoughsleep,FUN=mean)
barplot(avgREM$REM.sleep.percentage,names.arg=avgREM$midnight,col=c("Cadetblue2","Royalblue3"),ylab="Percentage of Time Asleep Spent in REM Sleep",xlab="Bed Time",main="Mean REM Sleep Percentage by Bed Time")

#Bar plot that illustrates the mean deep sleep percentage separated by bed time
avgDeep <- aggregate(Deep.sleep.percentage~midnight,data= enoughsleep,FUN=mean)
barplot(avgDeep$Deep.sleep.percentage,names.arg=avgDeep$midnight,col=c("Cadetblue2","Royalblue3"),ylab="Percentage of Time Asleep Spent in Deep Sleep",xlab="Bed Time",main="Mean Deep Sleep Percentage by Bed Time")

#Bar plot that illustrates the mean light sleep percentage separated by bed time
avgLight <- aggregate(Light.sleep.percentage~midnight,data= enoughsleep,FUN=mean)
barplot(avgLight$Light.sleep.percentage,names.arg=avgLight$midnight,col=c("Cadetblue2","Royalblue3"),ylab="Percentage of Time Asleep Spent in Light Sleep",xlab="Bed Time",main="Mean Light Sleep Percentage by Bed Time")

#Bar plot that illustrates mean sleep efficiency separated by gender and bed time
genmid <- aggregate(Sleep.efficiency~Gender*midnight,data=enoughsleep,FUN=mean)
barplot(genmid$Sleep.efficiency,ylab="Proportion of Time in Bed Spent Asleep",names.arg=c("Before Midnight","","After Midnight",""),col=c("Pink","steelblue1"),space=c(1,0,1,0),ylim=c(0,1.2),xlab="Bed Time",main="Sleep Efficiency by Bed Time and Gender")
legend("topright",c("Female","Male"),col=c("Pink","steelblue1"),lwd=10)

#Bar plot that illustrates mean sleep efficiency separated by bed time and gender
midgen <- aggregate(Sleep.efficiency~midnight*Gender,data=enoughsleep,FUN=mean)
barplot(midgen$Sleep.efficiency,ylab="Proportion of Time in Bed Spent Asleep",names.arg=c("Female","","Male",""),col=c("Cadetblue2","Royalblue3"),space=c(1,0,1,0),ylim=c(0,1.3),xlab="Gender",main="Mean Sleep Efficiency by Gender and Bed Time")
legend("topright",c("Before Midnight","After Midnight"),title="Bed Time",lwd=10,col=c("Cadetblue2","Royalblue3"))


########## Scatterplots and Regression ##########

#Scatterplot of Sleep efficiency by Light sleep percentage
plot(sleepdata$Light.sleep.percentage,sleepdata$Sleep.efficiency,ylab="Proportion of Time in Bed Spent Asleep",xlab="Percentage of Time Asleep Spent in Light Sleep",main="Relationship Between Sleep Efficiency and Light Sleep Percentage")
#Same scatterplot but separated by smoking status category
plot(sleepdata$Light.sleep.percentage[sleepdata$Gender=="Female"],sleepdata$Sleep.efficiency[sleepdata$Gender=="Female"],ylab="Proportion of Time in Bed Spent Asleep",xlab="Percentage of Time Asleep Spent in Light Sleep",main="Relationship Between Sleep Efficiency and Light Sleep Percentage",ylim=c(0.45,1),xlim=c(5,65),col="violetred1")
points(sleepdata$Light.sleep.percentage[sleepdata$Gender=="Male"],sleepdata$Sleep.efficiency[sleepdata$Gender=="Male"],col="royalblue1")
legend("topright",c("Female","Male"),pch=1,col=c("violetred1","royalblue1"))
#Creating a regression model
mLight <- lm(Sleep.efficiency~Light.sleep.percentage,data=sleepdata)
abline(mLight)
abline(v=35,col="seagreen",lwd=1.5) #I decided to add a vertical line at 35%. There appears to be two clear clusters in the data such that sleep efficiency for those who get less than 35% light sleep is between 0.7 and 1 whereas that for those who get more than 35% light sleep tends to be between 0.5 and 0.7. This tells us that those who get less than 35% of light sleep have a better chance of getting more efficient sleep.
text(38,0.9,label="35%",col="seagreen")
mLight
# Model: Sleep efficiency = 0.966648 - (0.007241 * Light sleep percentage)
# We predict to observe a 0.007241 unit decrease in sleep efficiency for each 1 unit increase in light sleep percentage
# R^2: 0.6665, p value: <2e-16 ***

#Scatterplot of Sleep efficiency by Deep sleep percentage
plot(sleepdata$Deep.sleep.percentage,sleepdata$Sleep.efficiency,ylab="Proportion of Time in Bed Spent Asleep",xlab="Percentage of Time Asleep Spent in Deep Sleep",main="Relationship between Sleep Efficiency and Deep Sleep Percentage")
#Same scatterplot but separated by gender category
plot(sleepdata$Deep.sleep.percentage[sleepdata$Gender=="Female"],sleepdata$Sleep.efficiency[sleepdata$Gender=="Female"],ylab="Proportion of Time in Bed Spent Asleep",xlab="Percentage of Time Asleep Spent in Deep Sleep",main="Relationship between Sleep Efficiency and Deep Sleep Percentage",ylim=c(0.45,1),xlim=c(10,80),col="violetred1")
points(sleepdata$Deep.sleep.percentage[sleepdata$Gender=="Male"],sleepdata$Sleep.efficiency[sleepdata$Gender=="Male"],col="royalblue1")
legend("topleft",c("Female","Male"),pch=1,col=c("violetred1","royalblue1"))
#Creating a regression model
mDeep <- lm(Sleep.efficiency~Deep.sleep.percentage,data=sleepdata)
abline(mDeep)
abline(v=45,col="seagreen",lwd=1.5) #I decided to add a vertical line at 45%. There appears to be two clear clusters in the data such that sleep efficiency for those who get less than 45% deep sleep is between 0.5 and 0.7 whereas that for those who get more than 45% deep sleep tends to be between 0.7 and 1. This tells us that those who get more than 45% of deep sleep have a better chance of getting more efficient sleep.
text(42,0.9,label="45%",col="seagreen")
mDeep
# Model: Sleep efficiency = 0.425998 + (0.006877 * Deep sleep percentage)
# We predict to observe a 0.006877 unit increase in sleep efficiency for each 1 unit increase in deep sleep percentage
# R^2: 0.6217, p value: <2e-16 ***

#Scatterplot of Sleep efficiency by REM sleep percentage
plot(sleepdata$REM.sleep.percentage,sleepdata$Sleep.efficiency,ylab="Proportion of Time in Bed Spent Asleep",xlab="Percentage of Time Asleep Spent in REM Sleep",main="Relationship Between Sleep Efficiency and REM Sleep Percentage")
#Same scatterplot but separated by smoking status category
plot(sleepdata$REM.sleep.percentage[sleepdata$Gender=="Female"],sleepdata$Sleep.efficiency[sleepdata$Gender=="Female"],ylab="Proportion of Time in Bed Spent Asleep",xlab="Percentage of Time Asleep Spent in REM Sleep",main="Relationship Between Sleep Efficiency and REM Sleep Percentage",ylim=c(0.45,1),xlim=c(15,30),col="violetred1")
points(sleepdata$REM.sleep.percentage[sleepdata$Gender=="Male"],sleepdata$Sleep.efficiency[sleepdata$Gender=="Male"],col="royalblue1")
legend("topleft",c("Female","Male"),pch=1,col=c("violetred1","royalblue1"))
#Creating a regression model
mREM <- lm(Sleep.efficiency~REM.sleep.percentage,data=sleepdata)
abline(mREM)
mREM
# Model: Sleep efficiency = 0.731798 + (0.002533 * REM sleep percentage)
# We predict to observe a 0.002533 unit increase in sleep efficiency for each 1 unit increase in REM sleep percentage
# R^2: 0.001521, p value: 0.208

################### Other Plots ##############

#Violin plot of number of awakenings by bed time
bwplot(Awakenings~exerciselvl,data=sleepdata,panel=panel.violin,xlab="Exercise Level",main="Number of Awakenings by Bed Time",col=c("deepskyblue1","darkolivegreen3","gold","firebrick1"))

#Boxplot of sleep efficiency by number of awakenings
boxplot(Sleep.efficiency~Awakenings,data=sleepdata,ylab="Proportion of Time in Bed Spent Asleep",xlab="Number of Awakenings",main="Sleep Efficiency by Number of Awakenings",col=c("steelblue1","steelblue2","steelblue3","steelblue","steelblue4"))

#Density plot of the relationship between Sleep efficiency and Awakenings
sp(sleepdata$Awakenings, sleepdata$Sleep.efficiency, jitter = list(x=2,y=2),ylab="Proportion of Time in Bed Spent Asleep",xlab="Number of Awakenings During the Night",main="Relationship Between Sleep Efficiency and Awakenings",smooth=T,regLine=F,boxplots=F)

#Plots for caffeine
boxplot(Sleep.efficiency~caffeine,data=sleepdata,col=c("peachpuff3","saddlebrown"),ylab="Proportion of Time in Bed Spent Asleep",xlab="Caffeine Intake Within the Past 24 Hours Prior to Bed Time",main="Sleep Efficiency by Caffeine Intake")

hist(sleepdata$Sleep.efficiency[sleepdata$caffeine=="Yes"],col=rgb(1,0,0.5,0.25),ylim=c(0,7),freq=F,xlab="Proportion of Time in Bed Spent Asleep",main="Distribution of Sleep Efficiency by Caffeine Intake")
hist(sleepdata$Sleep.efficiency[sleepdata$caffeine=="No"],add=T,col=rgb(0.5,1,0.5,0.25),freq=F)
legend("topleft",c("Yes","No"),col=c(rgb(1,0,0.5,0.25),rgb(0.5,1,0.5,0.25)),title="Caffeine Intake 24 Prior to Bed Time",lwd=10)
dens1<-density(sleepdata$Sleep.efficiency,bw=0.025)
lines(dens1,col="red",lwd=2)

#Plots for alcohol
summary(sleepdata$Sleep.efficiency[sleepdata$alcohol=="Yes"])
summary(sleepdata$Sleep.efficiency[sleepdata$alcohol=="No"])
boxplot(Sleep.efficiency~alcohol,data=sleepdata,col=c("lightcyan1","lightcyan4"),ylab="Proportion of Time in Bed Spent Asleep",xlab="Alcohol Intake Within the Past 24 Hours Prior to Bed Time",main="Sleep Efficiency by Alcohol Intake")

hist(sleepdata$Sleep.efficiency[sleepdata$alcohol=="Yes"],col=rgb(0,0.5,1,0.25),ylim=c(0,7),freq=F,xlab="Proportion of Time in Bed Spent Asleep",main="Distribution of Sleep Efficiency by Alcohol Intake")
hist(sleepdata$Sleep.efficiency[sleepdata$alcohol=="No"],add=T,col=rgb(1,0,0,0.25),freq=F)
legend("topleft",c("Yes","No"),col=c(rgb(0,0.5,1,0.25),rgb(1,0,0,0.25)),title="Alcohol Intake 24 Prior to Bed Time",lwd=10)
dens1<-density(sleepdata$Sleep.efficiency,bw=0.025)
lines(dens1,col="red",lwd=2)

#Plots for smoking status
summary(sleepdata$Sleep.efficiency[sleepdata$Smoking.status=="Smoker"])
summary(sleepdata$Sleep.efficiency[sleepdata$Smoking.status=="Non-smoker"])
boxplot(Sleep.efficiency~Smoking.status,data=sleepdata,col=c("indianred","indianred4"),ylab="Proportion of Time in Bed Spent Asleep",xlab="Smoking Status",main="Sleep Efficiency by Smoking Status")

hist(sleepdata$Sleep.efficiency[sleepdata$Smoking.status=="Smoker"],col=rgb(1,0,0.2,0.25),ylim=c(0,7),freq=F,xlab="Proportion of Time in Bed Spent Asleep",main="Distribution of Sleep Efficiency by Smoking Status")
hist(sleepdata$Sleep.efficiency[sleepdata$Smoking.status=="Non-smoker"],add=T,col=rgb(0,1,0.7,0.25),freq=F)
legend("top",c("Smoker","Non-smoker"),col=c(rgb(1,0,0.2,0.25),rgb(0,1,0.7,0.25)),title="Smoking Status",lwd=10)
dens1<-density(sleepdata$Sleep.efficiency,bw=0.025)
lines(dens1,col="red",lwd=2)
