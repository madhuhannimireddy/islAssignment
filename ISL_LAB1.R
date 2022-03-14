

setwd("C:\Users\MADHU\Documents\R")

college <- read.csv("C:\Users\MADHU\Documents\R\College.csv")
head(College)

rownames (college) <- college[, 1]
View (college)
college <- college[, -1]
head(college)
View (college)

summary(college)

college$Private <- as.factor(college$Private)

head(college)



plot(college$Private, college$Outstate, xlab = "Private", ylab = "Out-of-state tuition in dollars")



Elite <- rep("No", nrow(college))
Elite[college$Top10perc > 50] <- "Yes"
Elite <- as.factor(Elite)
college <- data.frame(college, Elite)
summary(Elite)
plot(college$Elite, college$Outstate, xlab = "Elite University", ylab = "Tuition in $")

par(mfrow=c(2,2))
hist(college$Apps, xlab = "Applications Received", col="red",main = "")
hist(college$perc.alumni, col=2, xlab = "Perc of alumni who donate",main = "")
hist(college$S.F.Ratio, col=3, breaks=10, xlab = "Student/Faculty ratio",main = "")
hist(college$Expend, breaks=100, xlab = "Instructional expenditure per student", col="green",main = "")

summary(college$Apps)
summary(college$PhD)
row.names(college)[which.max(college$Top10perc)]
acceptance_rate <- college$Accept / college$Apps
row.names(college)[which.min(acceptance_rate)] 
plot(college$Outstate, college$Grad.Rate)

Auto <- read.csv("Auto.csv",na.strings="?")
Auto <- na.omit(Auto)
str(Auto) 



sapply(Auto[, -c(4, 9)], mean)
qualitative_columns <- which(names(Auto) %in% c("name", "origin", "origins"))
qualitative_columns
sapply(Auto[, -qualitative_columns], range)

sapply(Auto[, -qualitative_columns], mean)
sapply(Auto[, -qualitative_columns], sd)

subset <- Auto[-c(10:85), -c(4,9)]
sapply(subset, range)
sapply(subset, mean)
sapply(subset, sd)

pairs(Auto)
pairs(Auto[, -qualitative_columns])
with(Auto, plot(mpg, weight))
with(Auto, plot(mpg, cylinders))

Auto.sample <- Auto[sample(1:nrow(Auto), 20), ]

Auto.sample <- Auto.sample[order(Auto.sample$mpg), ]

with(Auto.sample, dotchart(mpg, name, xlab = "mpg"))
with(Auto, plot(origin, mpg), ylab = "mpg")

Auto$horsepower <- as.numeric(Auto$horsepower)
cor(Auto$weight, Auto$horsepower)
cor(Auto$weight, Auto$displacement)
cor(Auto$displacement, Auto$horsepower)




library(MASS)
Boston$chas <- as.factor(Boston$chas)
nrow(Boston)
ncol(Boston)

par(mfrow = c(2, 2))
plot(Boston$nox, Boston$crim)
plot(Boston$rm, Boston$crim)
plot(Boston$age, Boston$crim)
plot(Boston$dis, Boston$crim)
pairs(Boston)




par(mfrow = c(2, 2))
plot(Boston$crim ~ Boston$zn, log = 'xy', col = 'green')
plot(Boston$crim ~ Boston$age, log = 'xy', col = 'green')
plot(Boston$crim ~ Boston$dis, log = 'xy', col = 'red')
plot(Boston$crim ~ Boston$lstat, log = 'xy', col = 'red')
hist(Boston$crim, breaks = 50)
nrow(Boston[Boston$crim > 20, ])

hist(Boston$tax, breaks = 50)
nrow(Boston[Boston$tax == 666, ])

hist(Boston$ptratio, breaks = 50)
nrow(Boston[Boston$ptratio > 20, ])

nrow(Boston[Boston$chas == 1, ])


t(subset(Boston,medv==min(Boston$medv)))
row.names(Boston[min(Boston$medv), ])
range(Boston$tax)
Boston[min(Boston$medv), ]$tax



nrow(Boston[Boston$rm > 7, ])
nrow(Boston[Boston$rm > 8, ])


library(ISLR)
library(MASS)
data("Auto")
head(Auto)
lm.fit<-lm(mpg~horsepower,data=Auto)
summary(lm.fit)
#Yes we do have relation between predictor and response because p value is 2e-16
#4.a.ii. How strong is the relationship between the predictor and the response?
#R^{2} value is equal to 61% of variable (horsepower) in mpg. Mean for mpg is 23.44 and RSE of lm.fit is 4.9 which shows the % error of 20.9%
#4.a.iii. Is the relationship between the predictor and the response positive or negative?
#The relationship between predictor and response is negative. The linear regression indicates horsepower a automobile has.



intervals?
  predict(lm.fit,data.frame(horsepower=c(98)),interval="prediction")
predict(lm.fit,data.frame(horsepower=c(98)),interval="confidence")

attach(Auto)
plot(horsepower,mpg)
abline(lm.fit,lwd=5,col="blue")
#5.c.ii. Which predictors appear to have a statistically significant relationship to the response?
#We have relationship for origin, weight, year and for displacement. We can say this by seeing the p-values for predictors.
#5.c.iii. What does the coefficient for the year variable suggest?
#All the predictors are constant except for mpg.  For every year cars are nearly 1 mpg/year fuel efficient. 
#The coefficient of year variable shows the effect of raise for one year is equivalent to 0.75 in mpg.

which.max(hatvalues(lm.fit))
par(mfrow = c(2,2))
plot(lm.fit)
#In addition, when looking at the Residuals vs. Leverage plot we can observe a few things. First,
#there are a number of observations with standardized residual values with absolute value greater than or equal to 3. 
#Those are likely outliers. This is confirmed by looking at the Scale-Location plot, which has ???|Standardized residual| as the y-axis. 
#Points with ???Standardized residual???1.732 have |Standardized residual|???3, which again means that they are likely outliers. Going back the Residuals vs. Leverage plot, we also see that there are a couple points with unusually high leverage. Again, remember that after dropping the rows with null values, there are 392 observations in the data set, giving an average leverage value of 9/392???0.0239/392???0.023. There is one point with a leverage value of about 0.10, which is almost 5 times greater than the average.
#There is another point with a leverage of about 0.20, which is almost 10 times greater than the average.



pairs(Auto)


Auto$name<-NULL
cor(Auto,method = c("pearson"))

lm.fit<-lm(mpg~.,data=Auto)
summary(lm.fit)

which.max(hatvalues(lm.fit))
par(mfrow = c(2,2))
plot(lm.fit)

lm.fit = lm(mpg ~.-name+displacement:weight, data = Auto)
summary(lm.fit)

par(mfrow = c(2, 2))
plot(log(Auto$horsepower), Auto$mpg)
plot(sqrt(Auto$horsepower), Auto$mpg)
plot((Auto$horsepower)^2, Auto$mpg)
lm.fit = lm(mpg ~.-name+I((displacement)^2)+log(displacement)+displacement:weight, data = Auto)
summary(lm.fit)



library("ISLR")
?Carseats
head(Carseats)
str(Carseats)
lm.fit = lm(Sales ~ Price+Urban+US, data= Carseats)
summary(lm.fit)
#6.b) Provide an interpretation of each coefficient in the model. Be careful-some of the variables in the model are qualitative!
  #The coefficient for PRICE variable can be said as the average of price raise of a dollar is in decrease of 54.4588492 units in the sales remaining predictors are fixed. 
  #For URBAN variable the coefficient can be interpreted as average of unit sales in urban location are 21.9161508 units lesser than of rural location adn all others are fixied.
  #For US variable can be interpreted as average of unit sales in US store is 1200.5726978 higher than that of non US stores and the remaining predictors are fixed.
#6.c) Write out the model in equation form, being careful to handle the qualitative variables properly.
  #Sales=13.0434689+(???0.0544588) Price+(???0.0219162) Urban+(1.2005727) US+??
  #US=1 if the store is in the US and 0 if not, and with Urban=1 if the store is in an urban location and 0 if not.
#6.d) For which of the predictors can you reject the null hypothesis H0 : ??j = 0?
  #For PRICE and US variables we will reject null hypothesis.

fits <- lm(Sales ~ Price + US, data = Carseats)
summary(fits)

confint(fits)

par(mfrow = c(2, 2))
plot(fits)

