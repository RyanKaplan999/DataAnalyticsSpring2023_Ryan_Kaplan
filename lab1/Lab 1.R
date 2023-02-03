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
#EPI <- read_excel('EPI2010_data.xls', sheet="EPI2010_onlyEPIcountries")
EPI_data <- read.csv('EPI2010_data.csv')
names(EPI_data)<-EPI_data[1,]
EPI_data <- EPI_data[-1,]
View(EPI_data)

epi_data <- data.frame(EPI_data)

EPILand <- epi_data[epi_data$Landlock==1,]

View(EPILand) #epiLand is my data frame, that works. I'd still like to make a hist of EPI landlocked countries


land_column <- epi_data$Landlock # issue is I was using the matrix notation

land_column <- as.numeric(land_column)
land_column

class(land_column)

hist(land_column) # with na's

# REMOVE THE NA VALUES
Eland <- EPILand[!is.na(EPILand)]


Elandfixed <- Eland[!is.na(Eland)]
Elandfixed

Eland <- as.numeric(Elandfixed)

hist(Eland)

# repeat exercise 1

summary(Eland)
fivenum(Eland, na.rm = "True")
stem(Eland)
hist(Eland)
rug(Eland)

plot(ecdf(Eland), do.points=FALSE, verticals=TRUE) # cumulative density function

par(pty="s") 
qqnorm(Eland); qqline(Eland)

x<-seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn") #qq plot against the generating distribution 
qqline(x)


View(EPILand)

DALY <- as.numeric(EPILand$DALY)
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

WATER <- as.numeric(EPILand$WATER_H)
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

ENV <- as.numeric(EPILand$ENVHEALTH)
tf <- is.na(ENV) 
ENV <- ENV[!tf]   #filter out NA values

ECO <- as.numeric(EPILand$ECOSYSTEM)
tf <- is.na(ECO) 
ECO <- ECO[!tf]   #filter out NA values

AIR <- as.numeric(EPILand$AIR_H)
tf <- is.na(AIR) 
AIR <- AIR[!tf]   #filter out NA values

BIO <- as.numeric(EPILand$BIODIVERSITY)
tf <- is.na(BIO) 
BIO <- BIO[!tf]   #filter out NA values

boxplot(ENV, ECO, AIR, BIO, names=c("ENV", "ECO", "AIR", "BIO"))

View(EPILand)

No_water <- as.numeric(EPILand$No_surface_water)
tf <- is.na(No_water)
No_water <- No_water[!tf]

Desert <- as.numeric(EPILand$Desert)
tf <- is.na(Desert)
Desert <- Desert[!tf]

High_Population_Density <- as.numeric(EPILand$High_Population_Density)
tf <- is.na(High_Population_Density)
High_Population_Density <- High_Population_Density[!tf]

boxplot(No_water, Desert, High_Population_Density, names=c("No_Water", "Desert", "High_Pop"))

##########################################################################################################################################################
# repeating for gpw
gpw <- read.csv('GPW3_GRUMP_SummaryInformation_2010.csv')
gpw_data <- data.frame(gpw)

View(gpw_data)

gPop <- (gpw_data$PopulationPerUnit) # EPI now gPop

View(gPop)

tf <- is.na(gPop) 
gPop <- gPop[!tf]   #filter out NA values

boxplot(gPop, main = "gpw_pop")

# Exercise 1
summary(gPop)
fivenum(gPop, na.rm = "True")
stem(gPop)
hist(gPop)
rug(gPop)

plot(ecdf(gPop), do.points=FALSE, verticals=TRUE) # cumulative density function

par(pty="s") 
qqnorm(gPop); qqline(gPop)

x<-seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn") #qq plot against the generating distribution 
qqline(x)

AREA <- as.numeric(gpw_data$Area)
View(AREA)
tf <- is.na(AREA) 
AREA <- AREA[!tf]   #filter out NA values
boxplot(AREA, main = "AREA")
summary(AREA)
fivenum(AREA, na.rm = "True")
stem(AREA)
hist(AREA)
rug(AREA)
plot(ecdf(AREA), do.points=FALSE, verticals=TRUE) # cumulative density function
par(pty="s") 
qqnorm(AREA); qqline(AREA)

RES <- as.numeric(gpw_data$Resolution)
View(RES)
tf <- is.na(RES) 
RES <- RES[!tf]   #filter out NA values
boxplot(RES, main = "Resolution")
summary(RES)
fivenum(RES, na.rm = "True")
stem(RES)
hist(RES)
rug(RES)
plot(ecdf(RES), do.points=FALSE, verticals=TRUE) # cumulative density function
par(pty="s") 
qqnorm(RES); qqline(RES)
x<-seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for Resolution") #qq plot against the generating distribution 
qqline(x)

boxplot(gPop, AREA, RES, names=c("gPop", "AREA", "RES"))
qqplot(gPop,AREA)
qqplot(AREA,RES)
qqplot(RES, gPop)

# filtering

gpwAfrica <- gpw_data[gpw_data$Continent==1,]

View(gpwAfrica) 

gPop_AF <- (gpwAfrica$PopulationPerUnit) 

View(gPop_AF)

tf <- is.na(gPop_AF) 
gPop_AF <- gPop_AF[!tf]   #filter out NA values

boxplot(gPop_AF, main = "gpw_pop_AF")

# Exercise 1
summary(gPop_AF)
fivenum(gPop_AF, na.rm = "True")
stem(gPop_AF)
hist(gPop_AF)
rug(gPop_AF)

plot(ecdf(gPop_AF), do.points=FALSE, verticals=TRUE) # cumulative density function

par(pty="s") 
qqnorm(gPop_AF); qqline(gPop_AF)

AREA_AF <- as.numeric(gpwAfrica$Area)
View(AREA_AF)
tf <- is.na(AREA_AF) 
AREA_AF <- AREA_AF[!tf]   #filter out NA values
boxplot(AREA_AF, main = "AREA_AF")
summary(AREA_AF)
fivenum(AREA_AF, na.rm = "True")
stem(AREA_AF)
hist(AREA_AF)
rug(AREA_AF)
plot(ecdf(AREA_AF), do.points=FALSE, verticals=TRUE) # cumulative density function
par(pty="s") 
qqnorm(AREA_AF); qqline(AREA_AF)

RES_AF <- as.numeric(gpwAfrica$Resolution)
View(RES_AF)
tf <- is.na(RES_AF) 
RES_AF <- RES_AF[!tf]   #filter out NA values
boxplot(RES_AF, main = "Resolution")
summary(RES_AF)
fivenum(RES_AF, na.rm = "True")
stem(RES_AF)
hist(RES_AF)
rug(RES_AF)
plot(ecdf(RES_AF), do.points=FALSE, verticals=TRUE) # cumulative density function
par(pty="s") 
qqnorm(RES_AF); qqline(RES_AF)
x<-seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for Resolution_AF") #qq plot against the generating distribution 
qqline(x)

boxplot(gPop_AF, AREA_AF, RES_AF, names=c("gPop_AF", "AREA_AF", "RES_AF"))
qqplot(gPop_AF,AREA_AF)
qqplot(AREA_AF,RES_AF)
qqplot(RES_AF, gPop_AF)




