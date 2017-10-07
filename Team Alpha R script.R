#Load libraries
library(ISLR)
library(ggplot2)
library(glmnet)
library(caret)
library(faraway)
library(shinythemes)
library(psych)
library(foreach)

#Read and clean data
myFile <- "https://raw.githubusercontent.com/karthiksajeev94/RProject/master/Final.csv"
data1 <- read.table(file=myFile, header=T, sep=",")
data1$Num_Students <- as.numeric(data1$Num_Students)
data1$X._Inter_Students <- as.numeric(data1$X._Inter_Students)
univ = subset(data1, select = c(Teaching_Rating,Research_Rating,Citations_Rating,Industry_Income_Rating,Placement_Rating,X._Inter_Students))
univ1 = subset(data1, select = c(Teaching_Rating,Research_Rating,Citations_Rating,Industry_Income_Rating,Total_Score,Placement_Rating,X._Inter_Students))

#Scatter Plots
p1 <- ggplot(data1, aes(x=Teaching_Rating, y=Total_Score)) + geom_point()
p1 <- p1 + geom_smooth(method="lm") + labs(x="Teaching Rating", y="Total Score")
p1
p2 <- ggplot(data1, aes(x=Inter_Outlook_Rating, y=Total_Score)) + geom_point()
p2 <- p2 + geom_smooth(method="lm") + labs(x="International Outlook Rating", y="Total Score")
p2
p3 <- ggplot(data1, aes(x=Research_Rating, y=Total_Score)) + geom_point()
p3 <- p3 + geom_smooth(method="lm") + labs(x="Research Rating", y="Total Score")
p3
p4 <- ggplot(data1, aes(x=Citations_Rating, y=Total_Score)) + geom_point()
p4 <- p4 + geom_smooth(method="lm") + labs(x="Citations Rating", y="Total Score")
p4
p5 <- ggplot(data1, aes(x=Industry_Income_Rating, y=Total_Score)) + geom_point()
p5 <- p5 + geom_smooth(method="lm") + labs(x="Industry Income Rating", y="Total Score")
p5
p6 <- ggplot(data1, aes(x=Num_Students, y=Total_Score)) + geom_point()
p6 <- p6 + geom_smooth(method="lm") + labs(x="Number of Students", y="Total Score")
p6
p7 <- ggplot(data1, aes(x=Placement_Rating, y=Total_Score)) + geom_point()
p7 <- p7 + geom_smooth(method="lm") + labs(x="Placement Rating", y="Total Score")
p7
p8 <- ggplot(data1, aes(x=X._Inter_Students, y=Total_Score)) + geom_point()
p8 <- p8 + geom_smooth(method="lm") + labs(x="Percentage of international students", y="Total Score")
p8

#Simple Linear regression
lmTR  <- lm(Total_Score ~ Teaching_Rating,data=data1)
summary(lmTR)
lmIOR  <- lm(Total_Score ~ Inter_Outlook_Rating,data=data1)
summary(lmIOR)
lmRR <- lm(Total_Score ~ Research_Rating,data=data1)
summary(lmRR)
lmCR  <- lm(Total_Score ~ Citations_Rating,data=data1)
summary(lmCR)
lmIIR <- lm(Total_Score ~ Industry_Income_Rating,data=data1)
summary(lmIIR)
lmNS  <- lm(Total_Score ~ Num_Students,data=data1)
summary(lmNS)
lmSSR <- lm(Total_Score ~ Placement_Rating,data=data1)
summary(lmSSR)
lmXIS  <- lm(Total_Score ~ X._Inter_Students,data=data1)
summary(lmXIS)

#Multiple Linear Regression
mlr <- lm(Total_Score ~ Teaching_Rating + Research_Rating + Citations_Rating + Industry_Income_Rating + Placement_Rating + X._Inter_Students,data=data1)
mlr
summary(mlr)

#Plots for checking assumptions
checkAssum <- function(mlr) {
  par(mfcol=c(2,3))
  cook<-cooks.distance(mlr) 
  halfnorm(cook,3,ylab="Cooks distance", main="Influences") 
  boxplot(cook, ylab="Cooks distance"
          , main="Boxplot Cooks Distances")
  plot(fitted(mlr),residuals(mlr),xlab="Fitted",ylab="Residuals", col="blue"
       , pch=19, type='p', main="Resid vs. Fitted") 
  abline(h=0) 
  plot(fitted(mlr),abs(residuals(mlr)),xlab="Fitted",ylab="Abs(Residuals)"
       , main="Abs(Resid) vs. Fitted", col="blue", pch=19)
  qqnorm(residuals(mlr),ylab="Residuals", pch=19, col="blue") 
  qqline(residuals(mlr)) 
  hist(residuals(mlr), col="blue", main="Historgram of Residuals")
}

checkAssum(mlr)
preds <- predict(mlr, data=univ)
head(preds, n=20)

#Yhat vs Y plot
plot(data1$Total_Score, preds, main="Yhat vs Y", xlab="Y", ylab="Yhat")
abline(lm(preds ~ data1$Total_Score))
abline(a=0, b=1, col="red")

#Strength of correlation
cor(univ)

#General Linear Test
redmodel <- lm(univ1$Total_Score~univ1$Citations_Rating+univ1$Industry_Income_Rating+univ1$Placement_Rating+univ1$X._Inter_Students)
fullmodel <- lm(univ1$Total_Score~univ1$Teaching_Rating+univ1$Research_Rating+univ1$Citations_Rating+univ1$Industry_Income_Rating+univ1$Placement_Rating+univ1$X._Inter_Students)
summary(fullmodel)
anova(redmodel,fullmodel)

redmodel1 <- lm(univ1$Total_Score~univ1$Teaching_Rating+univ1$Citations_Rating+univ1$Industry_Income_Rating+univ1$Placement_Rating+univ1$X._Inter_Students)
summary(redmodel1)
anova(redmodel1,fullmodel)

redmodel2 <- lm(univ1$Total_Score~univ1$Research_Rating+univ1$Citations_Rating+univ1$Industry_Income_Rating+univ1$Placement_Rating+univ1$X._Inter_Students)
summary(redmodel2)
anova(redmodel2,fullmodel)

#Partial Residual Plot or Partial Correlation Plot
regTeaching1 <- lm(univ1$Total_Score~univ1$Research_Rating+univ1$Citations_Rating+univ1$Industry_Income_Rating+univ1$Placement_Rating+univ1$X._Inter_Students)
regTeaching2 <- lm(univ1$Teaching_Rating~univ1$Research_Rating+univ1$Citations_Rating+univ1$Industry_Income_Rating+univ1$Placement_Rating+univ1$X._Inter_Students)
residTeaching1 <- residuals(regTeaching1)
residTeaching2 <- residuals(regTeaching2)
plot(residTeaching1,residTeaching2)

regResearch1 <- lm(univ1$Total_Score~univ1$Teaching_Rating+univ1$Citations_Rating+univ1$Industry_Income_Rating+univ1$Placement_Rating+univ1$X._Inter_Students)
regResearch2 <- lm(univ1$Research_Rating~univ1$Teaching_Rating+univ1$Citations_Rating+univ1$Industry_Income_Rating+univ1$Placement_Rating+univ1$X._Inter_Students)
residResearch1 <- residuals(regResearch1)
residResearch2 <- residuals(regResearch2)
plot(residResearch1,residResearch2)

#Ridge regression
#Plot and fit
ridgeX <- model.matrix(~.,univ1[,-c(5)])
ridgeY <- univ1$Total_Score
RR <- cv.glmnet(ridgeX, ridgeY, alpha = 0, standardize = T, type.measure = "mse")
plot(RR)
ridge <- predict(RR, s = RR$lambda.min, newx = ridgeX)
ridgeRMSE <- mean((ridge - ridgeY)^2)^0.5
ridgeR2 <- 1 - sum((ridge - ridgeY)^2)/sum((ridgeY - mean(ridgeY))^2)
ridgeStats<- data.frame(Model = 'Ridge',Test_Train = 'univ1', RMSE = ridgeRMSE, R_Squared = ridgeR2)
ridge
ridgeStats

#Coefficients
x <- as.matrix(univ1[,-5])
y <- univ1$Total_Score
set.seed(99)
cvRidge <- cv.glmnet(x, y, parallel=TRUE, standardize=TRUE, type.measure='auc')
coef(cvRidge, s=cvRidge$lambda.min)

#Lasso Regression
#Plot and fit
lassoX <- model.matrix(~.,univ1[,-c(5)])
lassoY <- univ1$Total_Score
LR <- cv.glmnet(lassoX,lassoY, alpha = 1, standardize = T, type.measure = "mse")
plot(LR)
lasso <- predict(LR, s = LR$lambda.min, newx = lassoX)
lassoRMSE <- mean((lasso - lassoY)^2)^0.5
lassoR2 <- 1 - sum((lasso - lassoY)^2)/sum((lassoY - mean(lassoY))^2)
lassoStats<- data.frame(Model = 'Lasso',Test_Train = 'univ1', RMSE = lassoRMSE, R_Squared = lassoR2)
lasso
lassoStats

#Coefficients
cvLasso <- glmnet::cv.glmnet(x,univ1[,5])
coef(cvLasso, s = "lambda.1se")

# The following code has been commented out as it involves the use of the library olsrr, which is not supported on 
# RStudio version 3.4.0.We ran this code on an older version of R and genereated plots which are included in the Report  

# #Breusch pagan test and F test
# ols_bp_test(mlr) 
# ols_f_test(mlr) 
# 
# #Influential observations - Cook's distance
# cdBarPlot <- ols_cooksd_barplot(mlr)
# cdChart <- ols_cooksd_chart(mlr)
# cdBarPlot
# cdChart
#
# #DFBETAS
# dfB <- ols_dfbetas_panel(mlr)
#
# #DFFITS 
# ols_dffits_plot(mlr)
# 
# #Outliers
# #Studentized Residual
# studResid <- ols_srsd_plot(mlr)
# studResidChart <- ols_srsd_chart(mlr)
# studResid
# studResidChart
#
# #Deleted studentized residual plot
# ols_dsrvsp_plot(mlr)
# 
# #Checking basic assumptions
# ols_rsd_qqplot(mlr)
# #Shapiro wilk test
# ols_norm_test(mlr)
# ols_rvsp_plot(mlr)
# ols_rsd_hist(mlr) 
# 
# #Diagnotics for outliers and influential observations
# #Variable selection
# ols_all_subset(mlr)
# oas <- ols_all_subset(mlr)
# plot(oas)  
# 
# ols_best_subset(mlr)
# obs <- ols_best_subset(mlr)
# plot(obs)
# 
# #Stepwise forward elimination
# stepFwd <- ols_step_forward(mlr) 
# stepFwd
# plot(stepFwd)
# 
# #Stepwise backward elimination
# stepBack <- ols_step_backward(mlr) 
# stepBack
# plot(stepBack)
# 
# #Stepwise regression
# stepReg <- ols_stepwise(mlr)
# stepReg
# plot(stepReg)
# 
# #AIC forward
# aicFwd <- ols_stepaic_forward(mlr)
# aicFwd
# plot(aicFwd)
# 
# #AIC backward
# aicBack <- ols_stepaic_backward(mlr) 
# aicBack
# plot(aicBack)
# 
# #AIC both ways
# aicBoth <- ols_stepaic_both(mlr) 
# aicBoth
# plot(aicBoth)
# 
# #Tolerance and VIF for collinearity
# collinearity <- ols_coll_diag(mlr)
# collinearity