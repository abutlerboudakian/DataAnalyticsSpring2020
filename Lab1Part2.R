multivariate <- read.csv("C:/Users/Chandrew/Documents/RPI/Spring 2020/Data Analytics/Labs/Lab 1.2/data/multivariate.csv")
attatch(multivariate)
names(multivariate)
multivariate

plot(multivariate$Income, multivariate$Immigrant, main="Scatterplot")
plot(multivariate$Immigrant, multivariate$Homeowners)

help(lm)
mm <- lm(multivariate$Homeowners~multivariate$Immigrant)
mm
summary(mm)$coef

plot(multivariate$Homeowners~multivariate$Immigrant)
abline(mm)
abline(mm,col=2,lwd=3)

summary(mm)
attributes(mm)
mm$coefficients

newImmigrantdata <- data.frame(multivariate$Homeowners=c(0,20))
mm %>% predict(newImmigrantdata)

abline(mm)
abline(mm,col=3,lwd=3)
attributes(mm)
mm$coefficients

HP <- multivariate$Homeowners / multivariate$Population
PD <- multivariate$Population / multivariate$area
mm <- lm(multivariate$Immigrants ~ multivariate$Income + multivariate$Population + HP + PD)
summary(mm)
cm <- coef(mm)
cm

plot(mtcars$wt, mtcars$mpg)
library(ggplot2)
qplot(mtcars$wt, mtcars$mpg)
qplot(wt,mpg,data = mtcars)
ggplot(mtcars,aes(x=wt,y=mpg)) + geom_point()
plot(pressure$temperature,pressure$pressure,type="l")
points(pressure$temperature,pressure$pressure)

lines(pressure$temperature,pressure$pressure/2,col="red")
points(pressure$temperature,pressure$pressure/2,col="blue")
qplot(pressure$temperature,pressure$pressure,geom="line")
qplot(temperature,pressure,data=pressure,geom="line")
ggplot(pressure,aes(x=temperature,y=pressure)) + geom_line() + geom_point()

barplot(BOD$demand, names.arg=BOD$Time)
table(mtcars$cyl)
barplot(table(mtcars$cyl))
qplot(mtcars$cyl)
qplot(factor(mtcars$cyl))
qplot(factor(cyl),data=mtcars)
ggplot(mtcars,aes(x=factor(cyl))) + geom_bar()

hist(mtcars$mpg)
hist(mtcars$mpg,breaks=10)
hist(mtcars$mpg,breaks=5)
hist(mtcars$mpg,breaks=12)
qplot(mpg, data=mtcars,binwidth=4)
ggplot(mtcars,aes(x=mpg)) + geom_histogram(binwidth=4)
ggplot(mtcars,aes(x=mpg)) + geom_histogram(binwidth=5)

plot(ToothGrowth$supp, ToothGrowth$len)
boxplot(len ~ supp, data=ToothGrowth)
boxplot(len ~ supp + dose, data=ToothGrowth)
qplot(ToothGrowth$supp, ToothGrowth$len, geom="boxplot")
ggplot(ToothGrowth, aes(x=supp, y=len)) + geom_boxplot()
qplot(interaction(supp, dose), len, data=ToothGrowth, geom="boxplot")
ggplot(ToothGrowth, aes(x=interaction(supp,dose),y=len)) + geom_boxplot()

library(dplyr)
library(nycflights13)
head(flights)
summary(flights)

filter(flights,month==10,day==4,carrier=='AA')
head(filter(flights,month==10,day==4,carrier=='AA'))
head(flights[flights$month==10 && flights$day == 4])
slice(flights,1:15)

arrange(flights,year,month,day,arr_time)
head(arrange(flights,year,month,day,arr_time))
head(arrange(flights,year,month,day,desc(arr_time)))


select(flights,carrier)
head(select(flights,carrier))

head(select(flights,carrier, arr_time))
head(select(flights, carrier, arr_time, day))
head(rename(flights,Airline.carrier=carrier))

distinct(select(flights,carrier))

head(mutate(flights,MyNewColumn=arr_delay - dep_delay))
head(transmute(flights,MyNewColumn=arr_delay - dep_delay))

summarise(flights, avg_air_time=mean(air_time,na.rm=TRUE))
summarise(flights, TotalFlightTime=sum(air_time,na.rm=TRUE))

sample_n(flights,15)
sample_n(flights,71)

sample_n(flights,30)
sample_frac(flights,0.5)

df_mtcars <- mtcars
head(df_mtcars)

filter(df_mtcars,mpg>20)
sample_n(filter(df_mtcars,mpg>10),10)
arrange(sample_n(filter(df_mtcars,mpg>20),10),desc(mpg))
results_mpg <- arrange(sample_n(filter(df_mtcars,mpg>20),10),desc(mpg))
results_mpg