# devtools::install_github("hrbrmstr/ggalt")
install.packages("ggfortify")
install.packages("ggthemes")
install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("reshape")
install.packages("dplyr")
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(reshape)
library(ggplot2)
library(ggalt)
library(dplyr)

#Set working directory as the folder containing the data 
setwd("~/Downloads/[Vy]MSBA")
#Import student attribute spreadsheet 
attribute <- read.csv("~/Downloads/[Vy]MSBA/attribute.csv")
#Import report "dp" (deposit paid) downloaded from TargetX
dp <- read.csv("~/Downloads/[Vy]MSBA/deposit.csv")
#Delete these two columns before merge to avoid duplication of columns (as we have those in dp as well)
attribute$Deposit.Paid.Date <- NULL
attribute$Round <- NULL
#Add a column Student Name for dp 
dp$Student.Name <- apply(dp[,c("First.Name","Last.Name")], 1 , paste , collapse=" ")
#Merge two data sets
master <- merge(attribute, dp, by="Student.Name")
#Convert date columns into the readable format in R
master$Application.Submit.Date <- as.Date(master$Application.Submit.Date,"%m/%d/%y")
master$Decision.Publish.Date <- as.Date(master$Decision.Publish.Date,"%m/%d/%y")
master$Reply.By.Date <- as.Date(master$Reply.By.Date,"%m/%d/%y")
master$Deposit.Paid.Date <- as.Date(master$Deposit.Paid.Date,"%m/%d/%y")
#Reorder factor First Name in dp by "Decision Publish Date" 
master$First.Name <- factor(master$First.Name, levels=master$First.Name[order(master$Decision.Publish.Date)])


#We change "country" for US permanent residents from their citizenship to US
master$Country <- master$Citizenship.1
for (i in 1:length(master$First.Name)){
  if (master$Citizenshipstatus[i]=="Permanent Resident"){
    master$Country[i]<-"US"
  }
}
#Plot countries
ggplot(master,aes(x=Country))+geom_bar(width=0.5)+xlab("Country")+ylab("Total Count")+labs(title="Where are the enrolled students from?") + geom_text(stat='count',aes(label=..count..),size=3,position=position_stack(vjust=0.5),colour="white")
#Plot standardized test by country
ggplot(master,aes(x=Country, fill=Std..Test)) + geom_bar(width=0.5) + xlab("Country") + ylab("Total Count") + labs(fill="Test",title="Standardized Test by Country") + geom_text(stat='count',aes(label=..count..),size=3,position=position_stack(vjust=0.5))
#Plot skills by country
ggplot(master, aes(x=Country, fill=Primary.Skill)) + geom_bar(width=0.5) + xlab("Country") + ylab("Total Count") + labs(fill="Primary Skill",title="Skill Buckets by Country") + geom_text(stat='count',aes(label=..count..),size=3,position=position_stack(vjust=0.5))
#Plot primary skills (gender fill)
ggplot(master, aes(x=Primary.Skill, fill=Gender)) + geom_bar(width=0.5) + xlab("Skills") + ylab("Total Count") + labs(fill="Gender",title="Primary Skills") + geom_text(stat='count',aes(label=..count..),size=3,position=position_stack(vjust=0.5))
#Plot Work Experience 
#First we reorder the factor "Profile" from the alphabetical default to Fresh > Early > Mid)
master$Profile
master$Profile <- factor(master$Profile,levels(master$Profile)[c(2,1,3)])
ggplot(master, aes(x=Profile, fill=Gender)) + geom_bar(width=0.5) + xlab("Work Experience") + ylab("Total Count") + labs(fill="Gender",title="Work Experience") + geom_text(stat='count',aes(label=..count..),size=3,position=position_stack(vjust=0.5))
#Plot background - D,I,U (profile fill)
#First, we create a new column with the full background (instead of abbreviation) 
master$fullbg <- NA
for (i in 1:length(master$First.Name)){
  if (master$Background[i]=="D"){
    master$fullbg[i]<-"Domestic"
  } else if (master$Background[i]=="U"){
    master$fullbg[i]<-"US-Experienced"
  } else {
    master$fullbg[i]<-"International"
  }
}
#We reorder the factor "Background" from the alphabetical default to Domestic > US Experienced > International
master$Background
master$fullbg <- factor(master$fullbg)
master$fullbg
master$fullbg <- factor(master$fullbg,levels(master$fullbg)[c(1,3,2)])
ggplot(master, aes(x=fullbg, fill=Profile)) + geom_bar(width=0.5) + xlab("Background") + ylab("Total Count") + labs(fill="Profile",title="Background and Work Experience") + geom_text(stat='count',aes(label=..count..),size=3,position=position_stack(vjust=0.5))

#Plot time line of enrollment
#We add a column time gap which specifies the latency from when the students received their acceptance letter to when they paid their deposit. 
master$time_gap<-NA
for (i in 1:length(master$First.Name)){
  master$time_gap[i]<-master$Deposit.Paid.Date[i]-master$Decision.Publish.Date[i]
}
#dp$time_gap<-factor(dp$time_gap)
master$Color<-NA
for (i in 1:length(master$First.Name)){
  if (master$Background[i]=="I"){
    master$Color[i]<-"Blue"
  } else if (master$Background[i]=="D"){
    master$Color[i]<-"Red"
  } else {
    master$Color[i]<-"Green"
  }
}
#Combine the student name with their associate background to make it the label for y axis
master$name_bg <- apply(master[,c("First.Name","Background")], 1 , paste , collapse="-")
#We reorder the factor "Round" from the alphabetical default to 1, 2A, 2, 3, 4, 4B, 5
master$Round
master$Round <- factor(master$Round,levels(master$Round)[c(1,3,2,4,5,6,7)])
#Order name by decision published dates
master$name_bg <- factor(master$name_bg, levels=master$name_bg[order(master$Decision.Publish.Date)])
ggplot(master,aes(x=Decision.Publish.Date,xend=Deposit.Paid.Date,y=name_bg,colour=Round)) + geom_dumbbell()+labs(x="Date",y="First Name",title="From Decision Published Date to Deposit Paid Date")+geom_text(data=master,aes(x=Decision.Publish.Date,y=name_bg,label=time_gap),size=2.5,vjust=0.5,hjust=1.5)+scale_color_manual(values=c('1'='red','2'='green','2A'='blue','3'='orange','4'='black','4B'='purple','5'='pink'))



#REPLICATION of previous analysis by Prof. Hemant Bhargava on a subset of enrolled students.
#GRE and GMAT
gre_q <- master$Quant..ile[master$Std..Test=="GRE"]
gre_v <- master$Verbal..ile[master$Std..Test=="GRE"]
gre<-data.frame(gre_v,gre_q)
gmat_q <- master$Quant..ile[master$Std..Test=="GMAT"]
gmat_v <- master$Verbal..ile[master$Std..Test=="GMAT"]
gmat<-data.frame(gmat_v,gmat_q)
par(mfrow=c(1,1))
boxplot(gre_v,gre_q,gmat_v,gmat_q, range=0.5, las=2,names=c("GRE-V", "GRE-Q", "GMAT-V", "GMAT-Q"),main="Distribution of Scores",xlab="Test",ylab="Percentile",cex.axis=0.6)
gre <- gre[order(gre$gre_q),]
gre.model <- lm(gre$gre_v ~ gre$gre_q , data=gre)
summary(gre.model)
cor(gre$gre_v,gre$gre_q)
gmat<-gmat[order(gmat$gmat_q),]
gmat.model<- lm(gmat$gmat_v ~ gmat$gmat_q, data=gmat)
summary(gmat.model)
cor(gmat$gmat_v,gmat$gmat_q)
par(mfrow=c(1,2))
plot(gre_q,gre_v,main="GRE",xlab="Quantitative Percentile",ylab="Verbal Percentile",col="red")
plot(gmat_q,gmat_v,main="GMAT",xlab="Quantitative Percentile",ylab="Verbal Percentile",col="blue")
# Strengths: Verbal, Quant or Both
#GRE
par(mfrow=c(1,2), cex=0.75)
plot(x=1:length(gre$gre_v), y=gre$gre_q, type="p", ylab="GRE Quant Pct", xlab="GRE Takers", cex.lab=1.3)
abline(h=90,col="gray")
par(new = T)
plot(1:length(gre$gre_v), gre$gre_v, lty=3, type="p", pch=16, col="red", axes=F, xlab=NA, ylab=NA)
abline(h=80,col="red",lty=2)
axis(side = 4)
mtext(side = 4, line = 3, "GRE Verbal Pct")  
legend("bottomright",legend=c("Quant","Verbal"),col=c("black","red"),pch=c(1,16))
#GMAT
par(mfrow=c(1,2), cex=0.75)
plot(1:length(gmat$gmat_v), gmat$gmat_q, type="p", ylab="GMAT Quant Pct", xlab="GMAT Takers", cex.lab=1.3)
abline(h=90,col="gray")
par(new = T)
plot(1:length(gmat$gmat_v), gmat$gmat_v, lty=3, type="p", pch=16, col="red", axes=FALSE, xlab=NA, ylab=NA)
abline(h=80,col="red",lty=2)
axis(side = 4)
mtext(side = 4, line = 3, "GMAT Verbal Pct")  
legend("bottomright",legend=c("Quant","Verbal"),col=c("black","red"),pch=c(1,16))

#Test Performance by regions
par(mar=c(3,3,1,1))
par(mfrow=c(2,2))
GRE.data <- subset(master,master$Std..Test=="GRE")
Countries <- levels(as.factor(GRE.data$Country))
plot.default(as.factor(GRE.data$Country), GRE.data$Verbal..ile, axes=FALSE, frame=TRUE, xlab="Country", ylab="")
title("GRE Verbal Pct")
axis(1, 1: length(Countries), Countries)
axis(2)
n.Country <- length(Countries) + 0.5
polygon(c(0,n.Country, n.Country, 0), c(65,65,100,100),col=rgb(0,0.5, 0,0.5), border=NA)

plot.default(as.factor(GRE.data$Country), GRE.data$Quant..ile, axes=FALSE, frame=TRUE, xlab="Country", ylab="")
title("GRE Quant Pct")
axis(1, 1: length(Countries), Countries)
axis(2)
n.Country <- length(Countries) + 0.5
polygon(c(0,n.Country, n.Country, 0), c(85,85,100,100),col=rgb(0,0.5, 0,0.5), border=NA)

GMAT.data <- subset(master,master$Std..Test=="GMAT")
Countries <- levels(as.factor(GMAT.data$Country))

plot.default(as.factor(GMAT.data$Country), GMAT.data$Verbal..ile, axes=FALSE, frame=TRUE, xlab="Country", ylab="")
title("GMAT Verbal Pct")
axis(1, 1: length(Countries), Countries)
axis(2)
n.Country <- length(Countries) + 0.5
polygon(c(0,n.Country, n.Country, 0), c(65,65,100,100),col=rgb(0,0.5, 0,0.5), border=NA)

plot.default(as.factor(GMAT.data$Country), GMAT.data$Quant..ile, axes=FALSE, frame=TRUE, xlab="Country", ylab="")
title("GMAT Quant Pct")
axis(1, 1: length(Countries), Countries)
axis(2)
n.Country <- length(Countries) + 0.5
polygon(c(0,n.Country, n.Country, 0), c(85,85,100,100),col=rgb(0,0.5, 0,0.5), border=NA)

#Top performers 
par(mfrow=c(1,2))
plot(GMAT.data$Quant..ile,GMAT.data$Verbal..ile, xlab="GMAT Quant", ylab="GMAT Verbal", pch=11+as.integer(as.factor(GMAT.data$Country)), col= as.factor(GMAT.data$Country)) 
legend("bottomleft", legend=unique(GMAT.data$Country), col=unique(as.factor(GMAT.data$Country)), pch=11+unique(as.integer(as.factor(GMAT.data$Country))))

polygon(c(70,80,90,100,100,70), c(80,80,70,60,100,100),col=rgb(0.1,0.1, 0,0.1), border=NA)

plot(x=GRE.data$Quant..ile,y=GRE.data$Verbal..ile, xlab="GRE Quant", ylab="GRE Verbal",pch=11+as.integer(as.factor(GRE.data$Country)), col= as.factor(GRE.data$Country)) 
legend("bottomleft", legend=unique(GRE.data$Country), col=unique(as.factor(GRE.data$Country)), pch=11+unique(as.integer(as.factor(GRE.data$Country))))

polygon(c(80,90,100,100,80), c(80,70,60,100,100),col=rgb(0.2,0.2, 0,0.2), border=NA)

#Bubble plot 
theme_set(theme_bw())  # pre-set the bw theme.
g <- ggplot(GRE.data, aes(Verbal..ile, Quant..ile)) + 
  labs(title="GRE Score")+ xlab("Verbal Percentile") + ylab("Quant Percentile")

g + geom_jitter(aes(col=GRE.data$Primary.Skill, size=GRE.data$Work.Exp..yr.)) + labs(col="Primary Skill",size="Work Experience")


g <- ggplot(GMAT.data, aes(Verbal..ile, Quant..ile)) + 
  labs(title="GMAT Score")+ xlab("Verbal Percentile") + ylab("Quant Percentile")

g + geom_jitter(aes(col=GMAT.data$Primary.Skill, size=GMAT.data$Work.Exp..yr.)) + labs(col="Primary Skill",size="Work Experience")


g <- ggplot(GMAT.data, aes(Verbal..ile, Quant..ile)) + 
  labs(title="GMAT by country and work experience")+ xlab("Verbal Percentile") + ylab("Quant Percentile")

g + geom_jitter(aes(col=GMAT.data$Country, size=GMAT.data$Work.Exp..yr.)) + labs(col="Country",size="Work Experience")

g <- ggplot(GRE.data, aes(Verbal..ile, Quant..ile)) + 
  labs(title="GRE by country and work experience")+ xlab("Verbal Percentile") + ylab("Quant Percentile")

g + geom_jitter(aes(col=GRE.data$Country, size=GRE.data$Work.Exp..yr.)) + labs(col="Country",size="Work Experience")
#Histogram of Work Experience
par(mfrow=c(1,1))
hist(master$Work.Exp..yr., main="Histogram of Work Experience",xlab="Work Experience (Years)",col="deepskyblue3")
median(master$Work.Exp..yr.)
abline(v=median(master$Work.Exp..yr.), col="red")
text(5, 22, "Median = 2", col = "red")
?text
#Creating admission funnels
ap <- read.csv("~/Downloads/[Vy]MSBA/allapp.csv")
#ap <- read.table(file="~/Downloads/[Vy]MSBA/allapp.csv",header=T, na.strings="", sep=",")
#Clean up data:

#Combine First name, Last name and Mailing City to find out the real duplicates
#We have two cases for duplicates: 
#One person, but different applications --> real duplicates
#Same name, but different people 
ap$Name <- apply(ap[,c("First.Name","Last.Name","Mailing.City")], 1 , paste , collapse=" ")
#Count how many unique name?
length(unique(as.character(ap$Name)))
#Get the duplicate names and store them as a vector
dup.names <- as.character(ap[which(duplicated(as.character(ap$Name))), "Name"])
dup.names
#Next, take a look at the records of duplicates 
dup <- ap[which(ap$Name %in% dup.names),]
#Go back to the original data and count the number of NA and blank
#Convert data to character
ap_char <- data.frame(lapply(ap, as.character), stringsAsFactors=FALSE)
#Count the number of NA and blank in every row
rs <- rowSums(is.na(ap_char) | ap_char=="")
#Order the data by First name, last name and the number of NAs and blanks
ap_ordered <- ap[order(ap$First.Name, ap$Last.Name, ap$Mailing.City,rs), ]
#Remove duplicates and keep the one that has fewer NAs and blanks
ap_clean <- ap_ordered[!duplicated(ap_ordered[c('First.Name','Last.Name','Mailing.City')]), ]
#Keep a record of the raw data
ap_raw <- ap
#Fix the data to the cleaned version
ap <- ap_clean

#Convert data into date format
ap$Application.Submit.Date <- as.Date(ap$Application.Submit.Date,"%m/%d/%Y")
ap$Decision.Publish.Date <- as.Date(ap$Decision.Publish.Date,"%m/%d/%Y")
ap$Reply.By.Date <- as.Date(ap$Reply.By.Date,"%m/%d/%Y")
ap$Deposit.Paid.Date <- as.Date(ap$Deposit.Paid.Date,"%m/%d/%Y")
#Insert a column called "status"
ap$status<-NA
ap$status<-as.character(ap$status)
for (i in 1:length(ap$First.Name)){
  if (ap$Current.Decision.Code[i]=="YG - Candidate Enrolled Regular"){
    ap$status[i]<-"Enrolled"
  } else if (!is.na(ap$Reply.By.Date[i]) & ap$Current.Decision.Code[i]!="YG - Candidate Enrolled Regular"){
    ap$status[i]<-"Declined Offer"
  } else if (ap$Round[i]!="" & is.na(ap$Reply.By.Date[i])){
    ap$status[i]<-"Rejected"
  } else {
    ap$status[i]<-"Not submitted"
  }
}
count_enrolled <-length(which(ap$status=="Enrolled"))
count_declined <-length(which(ap$status=="Declined Offer"))
count_rejected <-length(which(ap$status=="Rejected"))
count_notsubmitted <-length(which(ap$status=="Not submitted"))
count_total <- length(ap$status)
count_submitted <- count_enrolled + count_declined + count_rejected
count_accepted <- count_enrolled + count_declined

#Add citizenship status for some students 
nocs<-subset(ap,ap$Citizenshipstatus[i]=="")
for (i in 1:length(ap$First.Name)){
  if (ap$Citizenshipstatus[i]==""){
    if (ap$Mailing.Country[i]=="US"){
      ap$Citizenshipstatus[i]<-"U.S. Citizen"
    } else{
      ap$Citizenshipstatus[i]<-"International"
    }
  }
}
#Insert a column for Domestic vs Non-Domestic
ap$Background<-NA
for (i in 1:length(ap$First.Name)){
  if (ap$Citizenshipstatus[i]=="International"){
    ap$Background[i]<-"Non-Domestic"
  } else{
    ap$Background[i]<-"Domestic"
  }
}

applied <- ap
applied$group <- NA
for (i in 1:length(applied$First.Name)){
  applied$group[i]<-"Applied"
}
submitted<-subset(applied,status=="Rejected" | status=="Declined Offer" | status=="Enrolled")
admitted<-subset(applied,status=="Declined Offer" | status=="Enrolled")
enrolled<-subset(applied,status=="Enrolled")
declined<-subset(applied,status=="Declined Offer")

for (i in 1:length(submitted$group)){
  submitted$group[i]<-"Submitted"
}
for (i in 1:length(admitted$group)){
  admitted$group[i]<-"Admitted"
}
for (i in 1:length(enrolled$group)){
  enrolled$group[i]<-"Enrolled"
}
for (i in 1:length(declined$group)){
  declined$group[i]<-"Declined"
}
big_funnel<-rbind(applied,submitted,admitted,enrolled)
big_funnel$group <- factor(big_funnel$group,levels=big_funnel$group)
#Plot funnel by citizenship
ggplot(big_funnel, aes(x=group, fill=Citizenshipstatus)) + geom_bar(width=0.5) + xlab("Steps") + ylab("Total Count") + labs(fill="Background",title="Funnel by Citizenship")+ geom_text(stat='count',aes(label=..count..),size=3,position=position_stack(vjust=0.5))
#Create table of citizenship

Background <- c("International","Permanent Resident", "US Citizen")
Start <- c(966,34,109)
Submit <- c(530,16,41)
Admit <- c(73,5,16)
Enroll <- c(32,3,8)
funnel_country <- data.frame(Background, Start, Submit, Admit, Enroll)

#Plot funnel by background
ggplot(big_funnel, aes(x=group, fill=Background)) + geom_bar(width=0.5) + xlab("Steps") + ylab("Total Count") + labs(fill="Background",title="Funnel by Background")+ geom_text(stat='count',aes(label=..count..),size=3,position=position_stack(vjust=0.5))

ggplot(funnel, aes(x=steps,y=numbers))+geom_bar(stat='identity') + geom_text(aes(label=paste(numbers)),vjust=-0.5)+labs(x="Steps",y="Numbers")
#Plot Horizontal Funnel
steps <- c("Applied","Submitted","Admitted","Enrolled")
numbers <- c(1109,587,94,43)
rates <- c(100,(587/1109)*100,(94/1109)*100,(43/1109)*100)
funnel <- data.frame(steps,numbers,rates)
total <- subset(funnel,rates==100)$numbers
funnel$padding<-(total-funnel$numbers)/2
molten <- melt(funnel[,-3],id.var='steps')
molten <- molten[order(molten$variable, decreasing=T), ]
levels(molten$steps)
molten$steps
molten$steps <- factor(molten$steps,levels(molten$steps)[c(3,1,4,2)])
funnel$steps <- factor(funnel$steps,levels=funnel$steps)
#Horizontal Funnel
ggplot(molten,aes(x=steps)) + geom_bar(aes(y=value, fill=variable), stat='identity', position='stack') + geom_text(data=funnel,aes(y=total/2, label=numbers, colour='black')) + scale_fill_manual(values=c('pink',NA)) + coord_flip() + theme(legend.position='none') + labs(x='Steps',y='Volume',title="Admission Funnel")
#Vertical Funnel 
ggplot(funnel, aes(x=steps,y=numbers))+geom_bar(stat='identity') + geom_text(aes(label=paste(numbers)),vjust=-0.5)+labs(x="Steps",y="Numbers")


