EPI_Data <- read.csv("C:/Users/Chandrew/Documents/RPI/Spring 2020/Data Analytics/Labs/Lab 1/data/2010EPI_data.csv", skip=1)
View(EPI_Data)

EPI <- EPI_Data$EPI

summary(EPI)
fivenum(EPI, na.rm=TRUE)

stem(EPI)
hist(EPI)
hist(EPI, seq(30., 95., 1.0), prob=TRUE)
lines(density(EPI, na.rm=TRUE, bw=1))
rug(EPI)

plot(ecdf(EPI), do.points=FALSE, verticals=TRUE)

par(pty="s")
qqnorm(EPI)
qqline(EPI)

x <- seq(30, 95, 1)
qqplot(qt(ppoints(250), df=5), x, xlab="Q-Q plot for tdsn")
qqline(x)

DALY <- EPI_Data$DALY

summary(DALY)
fivenum(DALY, na.rm=TRUE)

stem(DALY)
hist(DALY)
hist(DALY, seq(30., 95., 1.0), prob=TRUE)
lines(density(DALY, na.rm=TRUE, bw=1))
rug(DALY)

plot(ecdf(DALY), do.points=FALSE, verticals=TRUE)

par(pty="s")
qqnorm(DALY)
qqline(DALY)


WATER_H <- EPI_Data$WATER_H

summary(WATER_H)
fivenum(WATER_H, na.rm=TRUE)

stem(WATER_H)
hist(WATER_H)
hist(WATER_H, seq(30., 95., 1.0), prob=TRUE)
lines(density(WATER_H, na.rm=TRUE, bw=1))
rug(WATER_H)

plot(ecdf(WATER_H), do.points=FALSE, verticals=TRUE)

par(pty="s")
qqnorm(WATER_H)
qqline(WATER_H)

boxplot(EPI, DALY)
qqplot(EPI, DALY)

boxplot(EPI, EPI_Data$ENVHEALTH, EPI_Data$ECOSYSTEM, DALY, EPI_Data$AIR_H, EPI_Data$WATER_H, EPI_Data$AIR_E, EPI_Data$WATER_E, EPI_Data$BIODIVERSITY)

EPILand <- EPI[!EPI_Data$Landlock]
ELand <- EPILand[!is.na(EPILand)]
hist(ELand)
hist(ELand, seq(30., 95., 1.0), prob=TRUE)

EPINsw <- EPI[!EPI_Data$No_surface_water]
ENsw <- EPINsw[!is.na(EPINsw)]
hist(ENsw)
hist(ENsw, seq(30., 95., 1.0), prob=TRUE)

EPIDesert <- EPI[!EPI_Data$Desert]
EDesert <- EPIDesert[!is.na(EPIDesert)]
hist(EDesert)
hist(EDesert, seq(30., 95., 1.0), prob=TRUE)

EPIHpd <- EPI[!EPI_Data$High_Population_Density]
EHpd <- EPIHpd[!is.na(EPIHpd)]
hist(EHpd)
hist(EHpd, seq(30., 95., 1.0), prob=TRUE)


EPI_South_Asia <- EPI[EPI_Data$GEO_subregion == "South Asia"]

