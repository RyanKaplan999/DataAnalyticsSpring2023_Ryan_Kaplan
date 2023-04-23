library(XLS)
library(xlsx)
library(readxl)

install.packages('nortest')
library(nortest)


nyc <- read.csv('NYC_Citywide_Annualized_Calendar_Sales_Update.csv')
nyc_data <- data.frame(nyc)
View(nyc_data)

# Manhattan is borough # 1
man_data <- nyc_data[nyc_data$BOROUGH==1,]
View(man_data)

summary(man_data)

sale_prices <- man_data$SALE.PRICE
summary(sale_prices)

plot(sale_prices)

boxplot(sale_prices, names=c("Sale Prices"))

# removing the top quarter to get a comprehensible boxplot
df <- man_data[man_data$SALE.PRICE <= 1860000 ,]
cutoff_sale_prices <- df$SALE.PRICE
boxplot(cutoff_sale_prices, names=c("Sale Prices"))
plot(cutoff_sale_prices)
summary(cutoff_sale_prices)

df <- man_data[man_data$SALE.PRICE > 500000000 ,]
high_sale_prices <- df$SALE.PRICE
summary(high_sale_prices)
View(high_sale_prices)

zeros <- man_data[man_data$SALE.PRICE == 0 ,]
zeros <- zeros$SALE.PRICE
summary(zeros)

upper <- man_data[man_data$SALE.PRICE > 4525140 ,]
upper <- upper$SALE.PRICE
summary(upper)

# removing outliers
no_out <- man_data[man_data$SALE.PRICE < 4525140,]
no_out <- no_out[no_out$SALE.PRICE > 100 ,]
summary(no_out)
View(no_out)


# predict the Sales Price using Gross Square feet, Land Square feet. W
GROSS <- as.numeric(no_out$GROSS.SQUARE.FEET)
GROSS[is.na(GROSS)] <- 0

LAND <-  as.numeric(no_out$LAND.SQUARE.FEET)
LAND[is.na(LAND)] <- 0

PRICE <-  as.numeric(no_out$SALE.PRICE)
PRICE[is.na(PRICE)] <- 0

frame <- data.frame(GROSS, LAND, PRICE)
View(frame)

# Create a linear model
linear_model <- lm(PRICE~GROSS+LAND, data = frame)

sam <- frame[sample(1:nrow(frame), 20), ]  # Sample rows of data with Base R
sam                                     # Print sampled data

# 0  789 2150000
# 428    0  725000
# 638    0 1033523

# Creating a data frame
newFrame <- data.frame(GROSS = c(0), LAND = c(789))
prediction <- predict(linear_model, newdata = newFrame)
prediction

newFrame <- data.frame(GROSS = c(428), LAND = c(0))
prediction <- predict(linear_model, newdata = newFrame)
prediction

newFrame <- data.frame(GROSS = c(638), LAND = c(0))
prediction <- predict(linear_model, newdata = newFrame)
prediction

View(no_out)


# shuffle our data
no_out <- sample(no_out) 

# Assembling our frame to do knn
TAX <- as.numeric(no_out$TAX.CLASS.AT.TIME.OF.SALE)
TAX[is.na(TAX)] <- 0 

BLOCK <- as.numeric(no_out$BLOCK)
BLOCK[is.na(BLOCK)] <- 0 

LOT <- as.numeric(no_out$LOT)
LOT[is.na(BLOCK)] <- 0 

ZIP <- as.numeric(no_out$ZIP.CODE)
ZIP[is.na(ZIP)] <- 0 

RES <- as.numeric(no_out$RESIDENTIAL.UNITS)
RES[is.na(RES)] <- 0 

COM <- as.numeric(no_out$COMMERCIAL.UNITS)
COM[is.na(COM)] <- 0 

TOT <- as.numeric(no_out$TOTAL.UNITS)
TOT[is.na(TOT)] <- 0 

LAN <- as.numeric(no_out$LAND.SQUARE.FEET)
LAN[is.na(LAN)] <- 0 

GRO <- as.numeric(no_out$GROSS.SQUARE.FEET)
GRO[is.na(GRO)] <- 0 

YEAR <- as.numeric(no_out$YEAR.BUILT)
YEAR[is.na(YEAR)] <- 0 

COM_B <- as.numeric(no_out$Community.Board)
COM_B[is.na(COM_B)] <- 0 

COUN <- as.numeric(no_out$Council.District)
COUN[is.na(COUN)] <- 0 

CEN <- as.numeric(no_out$Census.Tract)
CEN[is.na(CEN)] <- 0 

BIN <- as.numeric(no_out$BIN)
BIN[is.na(BIN)] <- 0 

BBL <- as.numeric(no_out$BBL)
BBL[is.na(BBL)] <- 0 

SALE <- as.numeric(no_out$SALE.PRICE)
SALE[is.na(SALE)] <- 0

kFrame <- data.frame(TAX, BLOCK, ZIP, RES, COM, TOT, LAN, GRO, YEAR, COM_B, CEN, BIN, BBL)
#View(kFrame)

#Normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

normed <- as.data.frame(lapply(kFrame, normalize))

# create a train set with 70% of the data, and a test set with 30%
train_set <- normed[1:35203, ]
test_set <- normed[35204:50290, ]

#View(train_set)

train_labels <- SALE[1:35203]
test_labels <- SALE[35204:50290]

#Install class package
#install.packages('class')
# Load class package
#library(class)

#Find the number of observation
NROW(train_labels) # we set k to sqrt(n)

knn_187 <- knn(train=train_set, test=test_set, cl=train_labels, k=187)

#Calculate the proportion of correct classification for k = 187
acc <- 100 * sum(test_labels == knn_187)/NROW(test_labels)
acc

