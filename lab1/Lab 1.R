library(XLS)
library(xlsx)
library(readxl)

gpw <- read.csv('GPW3_GRUMP_SummaryInformation_2010.csv')

epi <- read_excel('EPI2010_data.xls', sheet="EPI2010_onlyEPIcountries")

gpw_data <- data.frame(gpw)
epi_data <- data.frame(epi)

View(epi_data)

gPop <- (gpw_data$PopulationPerUnit)

#ePop <- as.numeric(epi_data$PopulationDensity07)
EPI <- as.numeric(epi_data$EPI)

View(EPI)

boxplot(gPop, main = 'GPW Population Per Unit')

tf <- is.na(EPI) 
EPI <- EPI[!tf]   #filter out NA values

boxplot(EPI, main = "EPI")

# Exercise 1
summary(EPI)
fivenum(EPI, na.rm = "True")
stem(EPI)
hist(EPI)
rug(EPI)

plot(ecdf(EPI), do.points=FALSE, verticals=TRUE) # cumulative density function

par(pty="s") 
qqnorm(EPI); qqline(EPI)

x<-seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn") #qq plot against the generating distribution 
qqline(x)

DALY <- as.numeric(epi_data$DALY)
View(DALY)
tf <- is.na(DALY) 
DALY <- DALY[!tf]   #filter out NA values
boxplot(DALY, main = "DALY")
summary(DALY)
fivenum(DALY, na.rm = "True")
stem(DALY)
hist(DALY)
rug(DALY)
plot(ecdf(DALY), do.points=FALSE, verticals=TRUE) # cumulative density function
par(pty="s") 
qqnorm(DALY); qqline(DALY)
x<-seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for DALY") #qq plot against the generating distribution 
qqline(x)

WATER <- as.numeric(epi_data$WATER_H)
View(WATER)
tf <- is.na(WATER) 
WATER <- WATER[!tf]   #filter out NA values
boxplot(WATER, main = "WATER")
summary(WATER)
fivenum(WATER, na.rm = "True")
stem(WATER)
hist(WATER)
rug(WATER)
plot(ecdf(WATER), do.points=FALSE, verticals=TRUE) # cumulative density function
par(pty="s") 
qqnorm(WATER); qqline(WATER)
x<-seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for WATER") #qq plot against the generating distribution 
qqline(x)

boxplot(EPI, DALY, WATER, names=c("EPI", "DALY", "WATER"))
qqplot(EPI,DALY)
qqplot(EPI,WATER)
qqplot(DALY, WATER)

ENV <- as.numeric(epi_data$ENVHEALTH)
tf <- is.na(ENV) 
ENV <- ENV[!tf]   #filter out NA values

ECO <- as.numeric(epi_data$ECOSYSTEM)
tf <- is.na(ECO) 
ECO <- ECO[!tf]   #filter out NA values

AIR <- as.numeric(epi_data$AIR_H)
tf <- is.na(AIR) 
AIR <- AIR[!tf]   #filter out NA values

BIO <- as.numeric(epi_data$BIODIVERSITY)
tf <- is.na(BIO) 
BIO <- BIO[!tf]   #filter out NA values

boxplot(ENV, ECO, AIR, BIO, names=c("ENV", "ECO", "AIR", "BIO"))

# Exercise 2
EPI <- read_excel('EPI2010_data.xls', sheet="EPI2010_onlyEPIcountries")
epi_data <- data.frame(epi)

EPILand <- epi_data[epi_data$Landlock==1,]

#tf <- is.na(EPILand) 
#EPILand <- EPILand[!tf]   #filter out NA values
EPILand <- EPILand[!is.na(EPILand)]

View(EPILand)

EPILAND <- gsub("NA", "0", EPILAND)
EPILAND <- as.numeric(EPILand)

# CANT GET IT TO CONVERT TO NUMERIC

#EPILAND
EPILAND <- as.numeric(EPILand)
#EPILand <- EPILand[!is.na(EPILand)]
#EPILAND <- as.numeric(EPILand)

hist(EPILand)
hist(EPILand, seq(30., 95., 1.0), prob=TRUE)
