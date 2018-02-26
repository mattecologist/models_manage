library (tidyr)
library(dplyr)

########################################################################
############################## THOONA ##################################
########################################################################

##### read in the weather data
thoona <- read.csv("./data/Binns/thoona_datalogger.csv")
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

thoona_dates <- separate(thoona, Date, into=c( "Month", "Day", "Year"), sep="/") 
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
 thoona_dates$length <- nchar(thoona_dates$Year)
 thoona_dates$Year <- as.numeric(thoona_dates$Year)
 thoona_dates$Year[thoona_dates$length ==2] <- thoona_dates$Year[thoona_dates$length ==2] + 2000
 

thoona_all <- thoona %>%
  group_by(Julday) %>%
  summarise(Tmin=min(Temp), Tmax=max(Temp), Ta=mean(Temp)) %>%
  as.data.frame() %>%
  left_join(thoona_dates) %>%
  distinct()

write.csv (thoona_all, file="./data/thoona_all.csv", row.names=F)

########################################################################
############################## ELMORE ##################################
########################################################################
##### read in the weather data
elmore_dl <- read.csv("./data/Binns/elmore_datalogger.csv")
elmore_ib <- read.csv("./data/Binns/elmore_ibutton.csv")

elmore_dl <- elmore_dl[,2:3]
colnames(elmore_dl) <- c("Date", "Temp")
## note this is taking air temp, not soil temp..
elmore_ib <- elmore_ib[,c(1,3)]
colnames(elmore_ib) <- c("Date", "Temp")

elmore_dl <- separate(elmore_dl, Date, into=c("Day", "Time"), sep = " ")
elmore_ib <- separate(elmore_ib, Date, into=c("Day", "Time"), sep = " ")

elmore <- rbind(elmore_dl, elmore_ib)

elmore$Temp <- as.numeric (elmore$Temp)



elmore_dates <- separate(elmore, Day, into=c("Month", "Day", "Year"), sep="/") 
elmore_dates$Day <- as.integer(elmore_dates$Day)
elmore_dates$Month <- as.integer (elmore_dates$Month)
elmore_dates$length <- nchar(elmore_dates$Year)
elmore_dates$Year <- as.numeric(elmore_dates$Year)
elmore_dates$Year[elmore_dates$length ==2] <- elmore_dates$Year[elmore_dates$length ==2] + 2000

## for some reason, the days and months get switched after the 13th of the month...

elmore_dates$newDay[elmore_dates$Month <= 12] <- elmore_dates$Day[elmore_dates$Month <= 12]
elmore_dates$newDay[elmore_dates$Month > 12] <- elmore_dates$Month[elmore_dates$Month > 12]

elmore_dates$newMonth[elmore_dates$Month <= 12] <- elmore_dates$Month[elmore_dates$Month <=12]
elmore_dates$newMonth[elmore_dates$Month > 12] <- elmore_dates$Day[elmore_dates$Month > 12]

elmore_dates$Day <- elmore_dates$newDay
elmore_dates$Month <- elmore_dates$newMonth

elmore_dates$Date <- as.Date(paste0(elmore_dates$Year, "/", elmore_dates$newMonth, "/", elmore_dates$newDay), "%Y/%m/%d")

elmore_dates <- elmore_dates[order(elmore_dates$Date),]

test <- as.data.frame(seq(from=min(elmore_dates$Date), to=max(elmore_dates$Date), by=1))

colnames(test) <- "Date"

elmore_dates <- elmore_dates %>%
  group_by(Date) %>%
  summarise(Tmin=min(Temp), Tmax=max(Temp), Ta=mean(Temp))%>%
  as.data.frame()%>%
  right_join(test)

#elmore_dates <- separate(elmore_dates, Day, into=c("Month", "Day", "Year"), sep="/") 




## FINISH


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


seq_dates <- as.data.frame(unique(elmore_dates$Date))
seq_dates$count <- seq(from=0, to=nrow(seq_dates)-1, by=1)
seq_dates$error <- seq_dates[,1]-seq_dates$count
seq_dates$flag <- 0
seq_dates$flag[seq_dates$error-seq_dates[1,1] != 0] <- 1



