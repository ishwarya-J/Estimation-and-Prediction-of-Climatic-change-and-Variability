library(ggplot2)
library(readr)
library(fpp2)

temp_data <- read.csv("C:/Users/dharani/Downloads/GlobalTemperatures.csv")
str(temp_data)
head(temp_data)
summary(temp_data)

options(repr.plot.width=3, repr.plot.height=4)
ggplot(temp_data, aes(x=LandAverageTemperature))+geom_histogram(aes(y=..density..),fill="green")+geom_density(col="blue")

options(repr.plot.width=5, repr.plot.height=5)
temp_ts<-ts(temp_data[,2],start=c(1980,1),end=c(2015,12),frequency=12)

temp_fc<-snaive(temp_ts,h=36)
autoplot(temp_fc)
summary(temp_fc)

data <-read.csv("C:/Users/dharani/Downloads/GlobalLandTemperaturesByCountry.csv")
data<-na.omit(data)
names(data)

library(lubridate)

data$Year <- year(data$dt)

IND_data <- subset(data, Country == "India")
summary(IND_data)
str(IND_data)

IND_data$dt <- as.Data(IND_data$dt)
str(IND_data)

IND_data$Year <- year(IND_data$dt)
names(IND_data)

Avg_temp_IND <- aggregate(AverageTemperature ~ Year, FUN=mean, data = IND_data)

library(ggplot2)

IND_50 <- subset(Avg_temp_IND, Year>1963)

ggplot(IND_50, aes(Year, AverageTemperature)) + 
  geom_line(size = 2, aes(colour = AverageTemperature)) +
  scale_colour_gradient(low="green", high="red") +
  scale_x_continuous(breaks=seq(1960, 2015, 5)) + 
  scale_y_continuous(breaks=seq(23.5, 26, 0.2)) +  
  theme(plot.background = element_rect(fill='NA') ,legend.key=element_rect(fill=NA)) +
  geom_smooth(method = "lm", se=FALSE, color="red") +
  ggtitle("Surface Temperature of India in the last 50 Years")

IND_25 <- subset(Avg_temp_IND, Year > 1988)

ggplot(IND_25, aes(Year, AverageTemperature)) + 
  geom_line(size = 2, aes(colour = AverageTemperature)) + 
  scale_colour_gradient(low="green", high="red") +
  scale_x_continuous(breaks=seq(1990, 2015, 3)) + 
  scale_y_continuous(breaks=seq(23.5, 26, 0.2)) +  
  theme(plot.background = element_rect(fill='NA') ,legend.key=element_rect(fill=NA)) +
  geom_smooth(method = "lm", se=FALSE, color="red") +
  ggtitle("Surface Temperature of India in the last 25 Years")

IND_10 <- subset(Avg_temp_IND, Year > 2003)

ggplot(IND_10, aes(Year, AverageTemperature)) +
  geom_line(size = 2, aes(colour = AverageTemperature)) + 
  scale_colour_gradient(low="green", high="red") +
  scale_x_continuous(breaks=seq(2003, 2015, 1)) + 
  scale_y_continuous(breaks=seq(24, 26, 0.1)) +  
  theme(plot.background = element_rect(fill='NA') ,legend.key=element_rect(fill=NA)) +
  geom_smooth(method = "lm", se=FALSE, color="red") +
  ggtitle("Surface Temperature of India in the last 10 Years")


