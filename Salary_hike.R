######
######
##### Assignment Question no 4 , Churn out rate, Predict  the churn out rate based on salary hike
###### Y(output) is Salary and X(input) is years Exp

salaryhike <- read.csv(file.choose())
attach(salaryhike)
View(salaryhike)

summary(salaryhike)

plot(YearsExperience,Salary)
### YearsExperience is X, and Salary is Y
### after visualization of scatter plot, we can say it is positive in direction 
### strength is strong
cor(YearsExperience,Salary)
Smodel1 = lm(Salary ~ YearsExperience)
Smodel1
summary(Smodel1)
predict(Smodel1)
Smodel1$residuals
confint(Smodel1, level = 0.95)
predict(Smodel1, interval = "confidence")
Srmse <- sqrt(mean(Smodel1$residuals^2))
Srmse


####### LOG MODEL

plot(log(YearsExperience), Salary)
### YearsExperience is X, and Salary is Y
### after visualization of scatter plot, we can say it is positive in direction 
### strength is moderate
cor(log(YearsExperience), Salary)
Smodel2 = lm(Salary ~ log(YearsExperience))
summary(Smodel2)
Srmse2 <- sqrt(mean(Smodel2$residuals^2))
Srmse2


##### expoential Model

plot(YearsExperience, log(Salary))
### strength is moderate.
cor(YearsExperience,log(Salary))
Smodel3 = lm(log(Salary) ~ YearsExperience)
summary(Smodel3)

log_S <- predict(Smodel3, interval = "confidence")
log_S
Exp_sal <- exp(log_S)

S_err <- YearsExperience - Exp_sal
S_err

Srmse3 = sqrt(mean(S_err^2))
Srmse3


########Polynomial

Smodel4 = lm(Salary ~ YearsExperience)
summary(Smodel4)

confint(Smodel4, level = 0.95)
S_logres = predict(Smodel4,interval = "confidence")
Spoly <- exp(S_logres)
Spoly
err_Spoly <- YearsExperience - Spoly
err_Spoly

Srmse4 <- sqrt(mean(err_Spoly^2))
Srmse4