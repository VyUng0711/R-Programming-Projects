library(foreign)
library(Matching)
data <- read.dta('nsw_dw.dta')
#(1)
#Point estimate of the treatment effect
observed.diffmeans <- mean(data$re78[data$treat==1]) - mean(data$re78[data$treat==0])
observed.diffmeans 
mean(data$re78[data$treat==1])




#univariate linear regression
ulr = lm(formula = re78 ~ treat, data = data)
summary(ulr)
confint(ulr, 'treat', level=0.95)
#(2)
cpscontrols <- read.dta('cps_controls.dta')
nswtreat <- data[which(data$treat == 1),]
data2 <- rbind(nswtreat, cpscontrols)
mean(data2$re74)
mean(data2$re78[data$treat==1])
mean(data2$re78[data$treat==0])
observed.diffmeans2 <- mean(data2$re78[data2$treat==1]) - mean(data2$re78[data2$treat==0])
observed.diffmeans2
ulr2 <- lm(formula = re78 ~ treat, data = data2)
summary(ulr2)
confint(ulr2, 'treat', level=0.95)

#(3)
#use propensity score matching to produce an estimated treatment effect and confidence interval.
pscore.glm<-glm(treat ~ age+education+black+hispanic+married+nodegree+re74+re75,
                family=binomial(logit), data=data2)
pscore.glm2<-glm(treat ~ age+I(age^2)+education+I(education^2)+black+hispanic+married+nodegree+re74+I(re74^2)+re75+I(re75^2),
                 family=binomial, data=data2)
tr1 <- data2$treat
y1 <- data2$re78
x1 <- fitted(pscore.glm)
summary(r1)
r1 <- Match(Y=y1, Tr=tr1, X=x1, M=1)
xi <- fitted(pscore.glm2)
ri <- Match(Y=y1, Tr=tr1, X=xi, M=1)
summary(ri)
mb1 <- MatchBalance(treat ~ age+education+black+hispanic+married+nodegree+re74+re75, data=data2, match.out=r1, nboots=10)
lower_bound1 <- 1440.4 - 1.96*819.78
upper_bound1 <- 1440.4 + 1.96*819.78
ci1 <- c(lower_bound1,upper_bound1)
cii1 <- c(1732.9 - 1.96*987.78, 1732.9 + 1.96*987.78)
#(4)
#Use the Match() function to run a multivariate matching procedure 
#that uses all the covariates and also includes the estimated propensity scores. 
#get covariates
x2 <- data2[,c(3,4,5,6,7,8,9,10)]
x3 <- cbind(pscore.glm$fitted,x2)
x5 <- cbind(pscore.glm2$fitted,x2)
#
rr <- Match(Y=y1, Tr=tr1, X=x3)
mb <- MatchBalance(tr1~pscore.glm$fitted+data2$age+data2$education+data2$black+data2$hispanic
                     +data2$married+data2$nodegree+data2$re74+data2$re75,match.out=rr, nboots=500)
summary(rr)
r2 <- Match(Y=y1, Tr=tr1, X=x3, distance.tolerance=0.1, exact=c(F,T,F,F,F,F,F,F,F))
summary(r2)
mb2 <- MatchBalance(tr1~pscore.glm$fitted+data2$age+data2$education+data2$black+data2$hispanic
                    +data2$married+data2$nodegree+data2$re74+data2$re75,match.out=r2, nboots=500)
r4 <- Match(Y=y1, Tr=tr1, X=x3, distance.tolerance=0.1, exact='True')
summary(r4)
mb4 <- MatchBalance(tr1~pscore.glm$fitted+data2$age+data2$education+data2$black+data2$hispanic
                    +data2$married+data2$nodegree+data2$re74+data2$re75,match.out=r4, nboots=500)
r5 <- Match(Y=y1, Tr=tr1, X=x5)
summary(r5)

lower_bound2 <- 1863.4 - 1.96*891.83
upper_bound2 <- 1863.4 + 1.96*891.83
ci2 <- c(lower_bound2,upper_bound2)
ci2
cii2 <- c(2071 - 1.96*969.96, 2071 + 1.96*969.96)
cii2 
#(5) 
#Repeat #3 and #4 with genetic matching
#Genetic matching on propensity score (does not make sense)
genout1 <- GenMatch(Tr=tr1, X=x1, estimand="ATT", pop.size=100, max.generations=10, replace=TRUE)
mout1 <- Match(Y=y1, Tr=tr1, X=x1, estimand="ATT", Weight.matrix=genout1)
summary(mout1)
mb1 <- MatchBalance(tr1~pscore.glm$fitted, match.out=mout1, nboots=500)
cf1 <- c(1595.6 - 1.96*816.21, 1595.6 + 1.96*816.21)
cf1
#Genetic matching on propensity score (estimated from regression with interaction terms) and covariates
genout2 <- GenMatch(Tr=tr1, X=x5, estimand="ATT", pop.size=100, max.generations=10, replace=TRUE)
mout2 <- Match(Y=y1, Tr=tr1, X=x5, estimand="ATT", Weight.matrix=genout2, replace=TRUE)
summary(mout2)
mb2 <- MatchBalance(tr1~pscore.glm2$fitted+data2$age+data2$education+data2$black+data2$hispanic
                    +data2$married+data2$nodegree+data2$re74+data2$re75, match.out=mout2, nboots=500)
cf2 <- c(1559.5 - 1.96*1004.5, 1559.5 + 1.96*1004.5)
cf2
#Genetic matching on propensity score (estimated from regression without interaction terms) and covariates
genout3 <- GenMatch(Tr=tr1, X=x3, estimand="ATT", pop.size=100, max.generations=10, replace=TRUE)
mout3 <- Match(Y=y1, Tr=tr1, X=x3, estimand="ATT", Weight.matrix=genout3, replace=TRUE)
summary(mout3)
mb3 <- MatchBalance(tr1~pscore.glm$fitted+data2$age+data2$education+data2$black+data2$hispanic+
                      data2$married+data2$nodegree+data2$re74+data2$re75, match.out=mout3, nboots=500)
cf3 <- c(2220.8 - 1.96*904.12, 2220.8 + 1.96*904.12)
cf3
#Genetic matching without replacement 
genout4 <- GenMatch(Tr=tr1, X=x3, estimand="ATT", pop.size=100, max.generations=10, replace=FALSE)
mout4 <- Match(Y=y1, Tr=tr1, X=x3, estimand="ATT", Weight.matrix=genout4, replace=FALSE)
summary(mout4)
mb4 <- MatchBalance(tr1~pscore.glm$fitted+data2$age+data2$education+data2$black+data2$hispanic+
                      data2$married+data2$nodegree+data2$re74+data2$re75, match.out=mout4, nboots=500)

#Genetic matching with the condition that the algorithm will stop after there are 10 consecutive generations without improvements 
genout5 <- GenMatch(Tr=tr1, X=x3, estimand="ATT", pop.size=100, max.generations=10, wait.generations=10, replace=TRUE)
mout5 <- Match(Y=y1, Tr=tr1, X=x3, estimand="ATT", Weight.matrix=genout5, replace=TRUE)
summary(mout5)
mb5 <- MatchBalance(tr1~pscore.glm$fitted+data2$age+data2$education+data2$black+data2$hispanic
                    +data2$married+data2$nodegree+data2$re74+data2$re75, match.out=mout5, nboots=500)
#Genetic matching with large pop.size (=1000) and with wait.generations=10
genout6 <- GenMatch(Tr=tr1, X=x3, estimand="ATT", pop.size=1000, max.generations=10, wait.generations=10, replace=TRUE)
mout6<- Match(Y=y1, Tr=tr1, X=x3, estimand="ATT", Weight.matrix=genout6, replace=TRUE)
summary(mout6)
mb6 <- MatchBalance(tr1~pscore.glm$fitted+data2$age+data2$education+data2$black+data2$hispanic
                    +data2$married+data2$nodegree+data2$re74+data2$re75, match.out=mout6, nboots=500)
#Genetic matching with M = 2 (there should be 2 number of matches for each control unit)
genout7 <- GenMatch(Tr=tr1, X=x3, estimand="ATT", pop.size=100, max.generations=10, replace=TRUE, M=2)
mout7 <- Match(Y=y1, Tr=tr1, X=x3, estimand="ATT", M=2, Weight.matrix=genout7, replace=TRUE)
summary(mout7)
mb7 <- MatchBalance(tr1~pscore.glm$fitted+data2$age+data2$education+data2$black+data2$hispanic+
                      data2$married+data2$nodegree+data2$re74+data2$re75, match.out=mout7, nboots=500)

