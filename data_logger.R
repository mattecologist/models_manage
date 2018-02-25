library (tidyr)
library(dplyr)

########################################################################
############################## THOONA ##################################
########################################################################

##### read in the weather data
thoona <- read.csv("./data/weather/binns.csv/Thoona_1-Table 1.csv")
thoona <- thoona[,2:3]
colnames(thoona) <- c("Date", "Temp")


thoona <- separate(thoona, Date, into=c("Date", "Time"), sep = " ")
thoona$Time <- NULL

## Give each unique day a running number
thoona$Julday <- 0
x <- 1
for (i in unique (thoona$Date)){
  thoona$Julday[thoona$Date == i] <- x
  x <- x + 1
}


thoona_dates <- separate(thoona, Date, into=c("Day", "Month", "Year"), sep="/") 
thoona_dates$Day <- as.integer(thoona_dates$Day)
thoona_dates$Month <- as.integer (thoona_dates$Month)

## for some reason, the days and months get switched after the 13th of the month...

thoona_dates$newDay[thoona_dates$Month <= 12] <- thoona_dates$Day[thoona_dates$Month <= 12]
thoona_dates$newDay[thoona_dates$Month > 12] <- thoona_dates$Month[thoona_dates$Month > 12]

thoona_dates$newMonth[thoona_dates$Month <= 12] <- thoona_dates$Month[thoona_dates$Month <=12]
thoona_dates$newMonth[thoona_dates$Month > 12] <- thoona_dates$Day[thoona_dates$Month > 12]

 thoona_dates$Day <- thoona_dates$newDay
 thoona_dates$Month <- thoona_dates$newMonth
 
#### yeah... !! 

 thoona_dates <- thoona_dates[,c("Day", "Month", "Year", "Julday")]
 thoona_dates$Year <- as.numeric(thoona_dates$Year) + 2000

thoona_all <- thoona %>%
  group_by(Julday) %>%
  summarise(Tmin=min(Temp), Tmax=max(Temp), Ta=mean(Temp)) %>%
  as.data.frame() %>%
  left_join(thoona_dates) %>%
  distinct()


########################################################################
############################## ELMORE ##################################
########################################################################
##### read in the weather data
elmore <- read.csv("./data/weather/binns.csv/Elmore-Table 1.csv")
elmore <- elmore[,2:3]
colnames(elmore) <- c("Date", "Temp")


elmore <- separate(elmore, Date, into=c("Day", "Time"), sep = " ")
elmore <- separate(elmore, Time, into=c("Hours", "Min"), sep=":")

## Give each unique day a running number
elmore$Julday <- 0
x <- 1
for (i in unique (elmore$Day)){
  elmore$Julday[elmore$Day == i] <- x
  x <- x + 1
}

elmore <- separate(elmore, Day, into=c("Day", "Month", "Year"), sep="/") 
elmore$Year <- as.numeric(elmore$Year) + 2000

### need to break down to hourly observations and then fix NAs...

Ta <- elmore %>%
  group_by(Julday) %>%
  summarise(m=mean(Temp))




