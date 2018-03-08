library(arm)
library(tree)
library(rpart)
library(randomForest)
library(QuantPsyc)
data(lalonde)
#regression
mr = lm(formula = re78 ~ age+educ+black+hisp+married+nodegr+re74+treat, data = lalonde)
summary(mr)
#confidence interval
confint(mr, 'treat', level=0.95)
#Does your regression produce different effects for different individuals?
x1 <- predict(mr,lalonde)
lalondecopy <- lalonde
lalondecopy$treat <- 1 - lalonde$treat
x2 <- predict(mr,lalondecopy)
diff1 <- data.frame(x1,x2,lalonde$treat,lalondecopy$treat)
indx0 = which(diff1$lalonde.treat == 0)
re.treateffect <- NA
diff1$re.treateffect[indx0] <- diff1$x2[indx0]-diff1$x1[indx0]
diff1$re.treateffect[-indx0] <- diff1$x1[-indx0]-diff1$x2[-indx0]


#In case there is an interaction between no degree and treatment effect, doing through simulation: 
treat_educ <- lm(re78 ~ nodegr + treat + I(treat*nodegr), lalonde)
display (treat_educ)
treat_educ.sim <- sim(treat_educ)
coef(treat_educ.sim)
plot(0, 0, xlim=c(-1,2), ylim=c(-2000,1000), xlab="High School Degree", ylab="Treatment Effect", main="Treatment Effect vs High School Degree")
abline(h=0,lwd=.5,lty=2)
for (i in 1:100) {
  abline(a = coef(treat_educ.sim)[i,2], b = coef(treat_educ.sim)[i,4], lwd = .5, col = "gray")
}
mean_a <- mean(coef(treat_educ.sim)[,2])
mean_b <- mean(coef(treat_educ.sim)[,4])
abline(a = mean_a, b = mean_b, lwd = .5, col = "black")

#Compare treatment effect for two groups:
yesDegree <- lalonde[which(lalonde$nodegr == 0),]
yesDegree
noDegree <- lalonde[which(lalonde$nodegr == 1),]
noDegree

noDegreecopy <- noDegree
noDegreecopy$treat <- 1-noDegree$treat
yesDegreecopy <- yesDegree
yesDegreecopy$treat <- 1-yesDegree$treat

noDeGree <- lm(formula = re78 ~ age+educ+black+hisp+married+re74+treat, data = lalonde, subset = (nodegr==1))
summary(noDeGree)
DeGree <- lm(formula = re78 ~ age+educ+black+hisp+married+nodegr+re74+treat, data = lalonde, subset = (nodegr ==0))
summary(DeGree)

#Calculate treatment effect for no-degree group:
rs.noDegree <- lm(re78 ~ age+educ+black+hisp+married+nodegr+re74+treat, data = noDegree)
rs.noDegree.cf <- predict(rs.noDegree, noDegreecopy)
rs.noDegree.data <- data.frame(noDegree$re78, rs.noDegree.cf, noDegree$treat, noDegreecopy$treat)
rs.noDegree.data.effect <- NA
indx1 = which(rs.noDegree.data$noDegree.treat == 0)
rs.noDegree.data$rs.noDegree.data.effect[indx1] <- rs.noDegree.data$rs.noDegree.cf[indx1]-rs.noDegree.data$noDegree.re78[indx1]
rs.noDegree.data$rs.noDegree.data.effect[-indx1] <- rs.noDegree.data$noDegree.re78[-indx1]-rs.noDegree.data$rs.noDegree.cf[-indx1]
#Calculate treatment effect for yes-degree group: 
rs.yesDegree <- randomForest(re78~ age+educ+black+hisp+married+nodegr+re74+treat, data=yesDegree, importance = TRUE, ntree = 2000)
rs.yesDegree.cf <- predict(rs.yesDegree, yesDegreecopy)
rs.yesDegree.data <- data.frame(yesDegree$re78, rs.yesDegree.cf, yesDegree$treat, yesDegreecopy$treat)
rs.yesDegree.data.effect <- NA
indx2 = which(rs.yesDegree.data$yesDegree.treat == 0)
rs.yesDegree.data$rs.yesDegree.data.effect[indx2] <- rs.yesDegree.data$rs.yesDegree.cf[indx2]-rs.yesDegree.data$yesDegree.re78[indx2]
rs.yesDegree.data$rs.yesDegree.data.effect[-indx2] <- rs.yesDegree.data$yesDegree.re78[-indx2]-rs.yesDegree.data$rs.yesDegree.cf[-indx2]
#Run t-test to see if there is a significant difference between two group
t.test(rs.yesDegree.data$rs.yesDegree.data.effect,rs.noDegree.data$rs.noDegree.data.effect,mu = 0, conf.level = 0.95)


#Random Forest 
set.seed(415)
forest <- randomForest(re78~ age+educ+black+hisp+married+nodegr+re74+treat, data=lalonde, importance = TRUE, ntree = 2000)
summary(forest)$predicted
varImpPlot(forest)
?randomForest

#Average treatment effect for the treated using random forest 
treatedgroup <- lalonde[which(lalonde$treat == 1),]
treatedcopy <- treatedgroup
treatedcopy$treat <- 1-treatedgroup$treat
treat.cf <- predict(forest, treatedcopy)
treateffdata <- data.frame(treat.cf, treatedgroup$re78, treatedgroup$treat, treatedcopy$treat)
treateffdata[,1] <- as.numeric(as.character(treateffdata[,1]))
treateffdata$treatmenteffect <- (treateffdata$treatedgroup.re78 - treateffdata$treat.cf)
rf.effect <- treateffdata$treatmenteffect
average.rf.effect <- mean(rf.effect)
average.rf.effect
#Does your tree produce different effects for different individuals?
datacopy <- lalonde
datacopy$treat <- 1 - lalonde$treat
rf.predict <- predict(forest, datacopy)
rf.predict0 <- predict(forest, lalonde)
rf.effect <- data.frame(datacopy$treat, lalonde$treat, rf.predict, rf.predict0)
rf.effect$treatmenteffect <- NA
indx5 = which(rf.effect$lalonde.treat == 0)
rf.effect$treatmenteffect[indx5] <- rf.effect$rf.predict[indx5] - rf.effect$rf.predict0[indx5]
rf.effect$treatmenteffect[-indx5] <- rf.effect$rf.predict0[-indx5] - rf.effect$rf.predict[-indx5]

#Compare treatment effect between with and without highschool degree
#Calculate treatment effect for no-degree group:
rf.noDegree <- randomForest(re78~ age+educ+black+hisp+married+nodegr+re74+treat, data=noDegree, importance = TRUE, ntree = 2000)
rf.noDegree.cf <- predict(rf.noDegree, noDegreecopy)
rf.noDegree.data <- data.frame(noDegree$re78, rf.noDegree.cf, noDegree$treat, noDegreecopy$treat)
rf.noDegree.data.effect <- NA
indx3 = which(rf.noDegree.data$noDegree.treat == 0)
rf.noDegree.data$rf.noDegree.data.effect[indx3] <- rf.noDegree.data$rf.noDegree.cf[indx3]-rf.noDegree.data$noDegree.re78[indx3]
rf.noDegree.data$rf.noDegree.data.effect[-indx3] <- rf.noDegree.data$noDegree.re78[-indx3]-rf.noDegree.data$rf.noDegree.cf[-indx3]
#Calculate treatment effect for yes-degree group: 
rf.yesDegree <- randomForest(re78~ age+educ+black+hisp+married+nodegr+re74+treat, data=yesDegree, importance = TRUE, ntree = 2000)
rf.yesDegree.cf <- predict(rf.yesDegree, yesDegreecopy)
rf.yesDegree.data <- data.frame(yesDegree$re78, rf.yesDegree.cf, yesDegree$treat, yesDegreecopy$treat)
rf.yesDegree.data.effect <- NA
indx4 = which(rf.yesDegree.data$yesDegree.treat == 0)
rf.yesDegree.data$rf.yesDegree.data.effect[indx4] <- rf.yesDegree.data$rf.yesDegree.cf[indx4]-rf.yesDegree.data$yesDegree.re78[indx4]
rf.yesDegree.data$rf.yesDegree.data.effect[-indx4] <- rf.yesDegree.data$yesDegree.re78[-indx4]-rf.yesDegree.data$rf.yesDegree.cf[-indx4]
#Run t-test to see if there is a significant difference between two group
t.test(rf.yesDegree.data$rf.yesDegree.data.effect,rf.noDegree.data$rf.noDegree.data.effect,mu = 0, conf.level = 0.95)

#Bootstrap:
library(boot)
#function to obtain outcome from the data 
rfboot <- function(data, indices){
  d <- data[indices,]
  forest <- randomForest(re78~ age+educ+black+hisp+married+nodegr+re74+treat, data=lalonde, importance = TRUE, ntree = 2000)
  fit <- predict(forest, data)
  return (fit)
}
#bootstrapping using built-in function of r, with 1000 replications
results <- boot (data=lalonde, statistic=rfboot, R=1000)
results
plot(results)
boot.ci(results, type="bca")

#bootstrapping using normal method, with 1000 replications,  
bootstrap <- function(N){
  list <- NULL
  for (i in 1:N)
  {
    s <- sample(1:nrow(lalonde), replace=TRUE)
    p <- predict(forest,lalonde[s,])
    list[i] <- mean(p)
  }
  return(list)
} 
result <- bootstrap(1000)
hist(result)
plot(density(unique(result)))
ci <- quantile(result, prob = c(0.025, 0.975))
ci

#FISHER TEST 
#Calculate the observed difference of mean 
observed.diffmeans <- mean(lalonde$re78[lalonde$treat==1]) - mean(lalonde$re78[lalonde$treat==0])
print(observed.diffmeans)

# Assignment function
assignment <- function() {
  control.ind <- sample(1:nrow(lalonde), 260)
  treatment.ind <- c(1:445)[-control.ind]
  control.group <- lalonde[control.ind,]$re78
  treatment.group <- lalonde[treatment.ind,]$re78
  return(mean(treatment.group) - mean(control.group))
}
assignment()

s.vector <- NULL
re <- iter.RI()

# Exploring the results

interval <- quantile(re, prob = c(0.025, 0.975))
interval
#unique(results)

hist(re)
plot(density(unique(re)))
abline(v = 1794.343, lwd = 2, col = "red")
abline(v = -1225.362, lwd = 2, col = "green")
abline(v = 1265.678, lwd = 2, col = "green")






