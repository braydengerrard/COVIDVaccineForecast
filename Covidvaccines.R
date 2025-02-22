library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(scales)
library(ggplot2)
library(forecast)
library(hrbrthemes)
library(mgcv)
options(scipen=1000)

#Bring in data
Share <- read.csv("share-people-vaccinated-covid.csv")
Share$Day <- as.Date(Share$Day, format = "%Y-%m-%d")
USA <- subset(Share, Code == "USA")

#Adjusts data to remove children and 30% of adults
USA$Adjusted <- ((USA$people_vaccinated_per_hundred/0.816)/0.7)/100
Day <- seq(as.Date("2020-12-01"), as.Date("2021-03-15"), by="days")
US <- as.data.frame(Day)
US <- merge(US,USA,all.x=T) 
US$Adjusted <- ifelse(Day < "2020-12-20",0,US$Adjusted)

#Logistic regression
fitUS <- glm(Adjusted ~ Day, data = US, family = "quasibinomial")

#Days for the full curve
Day <- seq(as.Date("2020-12-01"), as.Date("2021-09-30"), by="days")
US2 <- as.data.frame(Day)
US <- merge(US,US2,all.y=T)

#Predict the full logistic curve
US$Predicted <- predict(fitUS, newdata = data.frame(Day = Day), type = "response")

#Plot the logistic regression
ggplot(US, aes(x=Day, y=Adjusted)) +
  geom_point(size=2) +
  geom_line(data=US, aes(x=Day,y=Predicted), color="blue", size=2) +
  theme_ipsum_rc(grid="Y") +
  scale_y_continuous(label=scales::percent) +
  labs(y= "Vaccinated",
       title = "Share of Population Vaccinated",
       subtitle = "Logistic growth model") +
  theme(axis.title.x = element_blank())

#Save the plot
ggsave("VaccineLog.png", type = "cairo-png", height = 5, width = 9.19)

#Create forecast
Day <- seq(as.Date("2020-12-20"), as.Date("2021-03-15"), by="days")
US <- as.data.frame(Day)
US <- merge(US,USA,all.x=T) 
time <- 0:85
time <- as.data.frame(time)
US$time <- time$time

#Calculate max population
(1-0.1874)*0.7
US$Adjusted <- US$people_vaccinated_per_hundred/100


#Transform data, a=lower bound, b=upper bound, x= y variable
f <- function (x, a, b) log((x - a) / (b - x))
US$Trans <- f(US$Adjusted, 0, 0.56882)
US$weekday <- as.factor(weekdays(as.Date(US$Day)))
US$weekday <- as.integer(factor(US$weekday, levels = c("Monday", "Tuesday", "Wednesday", 
                          "Thursday", "Friday", "Saturday", "Sunday"),
            ordered = TRUE))

weekday <- rep(c(7,1,2,3,4,5,6), times = 40)
weekday <- as.data.frame(weekday)

#Fit the model
time <- 0:85
gam_m <- gam(Trans ~  s(weekday, bs='cc', k=7) + s(time), method = "REML", data=US)

Day <- seq(as.Date("2020-12-20"), as.Date("2021-09-25"), by="days")
predict <- as.data.frame(Day)
predict <- merge(predict,USA,all.x=T)
US$Adjusted <- (US$people_vaccinated_per_hundred/100)
US$Trans <- f(US$Adjusted, 0, 0.56882)
time <- 0:279
time <- as.data.frame(time)
US$time <- time$time

#Create predictions
p <- predict(gam_m, data.frame(time = time, weekday = weekday), type = "link", se.fit = TRUE)
US$Predtrans <- predict(gam_m, data.frame(time = time, weekday = weekday))

#Check the model diagnostics
par(mfrow = c(2,2))
gam.check(gam_m)

#Create confidence intervals
upr <- p$fit + (2 * p$se.fit)
lwr <- p$fit - (2 * p$se.fit)

US$upr <- gam_m$family$linkinv(upr)
US$lwr <- gam_m$family$linkinv(lwr)

#Dates that were predicted
Date <- seq(as.Date("2020-12-20"), as.Date("2021-09-25"), by="days")
New <- as.data.frame(Date)

#Transform the data back to original form
finv <- function (x, a, b) (b * exp(x) + a) / (exp(x) + 1)
New$Actual <- finv(US$Trans, 0, 0.56882)
New$Predicted <- finv(US$Predtrans, 0, 0.56882)
New$upr <- finv(US$upr, 0, 0.56882)
New$lwr <- finv(US$lwr, 0, 0.56882)

#Plot the forecast
par(mfrow = c(1,1))

ggplot(New, aes(x=Date, y=Predicted)) +
  geom_point(aes(x=Date, y=Actual), size=1) +
  geom_line(color="blue", size=1) +
  theme_ipsum_rc(grid="Y") +
  geom_ribbon(aes(ymin=lwr, ymax=upr, x=Date, fill="band"), alpha=0.3, fill="deepskyblue") +
  scale_y_continuous(label=scales::percent, limits=c(0,1)) +
  labs(y = "Vaccinated",
       title = "Forecast of US Vaccination",
       subtitle = "Generalized Additive Model") +
  theme(axis.title.x = element_blank())

#Save the plotted forecast
ggsave("GAMVac.png", type = "cairo-png", height = 5, width = 9.19)

#Creates residuals in normal form
New$Residuals <- New$Actual-New$Predicted

#plotting the residuals against actual values
plot(New$Actual,New$Residuals)

