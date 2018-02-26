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

elmore_dl$Day <- as.Date(elmore_dl$Day, "%m/%d/%Y")
elmore_ib$Day <- as.Date(elmore_ib$Day, "%d/%m/%Y")

elmore_dl$source <- "DL"
elmore_ib$source <- "IB"

elmore <- rbind(elmore_dl, elmore_ib)

elmore$Temp <- as.numeric (elmore$Temp)

elmore_dates <- separate(elmore, Day, into=c("Year", "Month", "Day"), sep="-") 
elmore_dates$Day <- as.integer(elmore_dates$Day)
elmore_dates$Month <- as.integer (elmore_dates$Month)
elmore_dates$length <- nchar(elmore_dates$Year)
elmore_dates$Year <- as.numeric(elmore_dates$Year)
elmore_dates$Year[elmore_dates$length ==2] <- elmore_dates$Year[elmore_dates$length ==2] + 2000


elmore_dates$Date <- as.Date(paste0(elmore_dates$Year, "/", elmore_dates$Month, "/", elmore_dates$Day), "%Y/%m/%d")

elmore_dates <- elmore_dates[order(elmore_dates$Date),]

test <- as.data.frame(seq(from=min(elmore_dates$Date), to=max(elmore_dates$Date), by=1))

colnames(test) <- "Date"

elmore_dates$Time <- NULL

elmore_test <- elmore_dates %>%
  group_by(Date) %>%
  filter(!is.na(Temp))%>%
  summarise(Tmin=min(Temp), Tmax=max(Temp), Ta=mean(Temp))%>%
  as.data.frame()%>%
  right_join(test, by="Date")

######################################################################################################
##### Now patches in missing data from nearest weather station, Echuca.

echuca <- read.csv("./data/weather/Echuca.csv")
echuca$Date <- as.Date(paste0(echuca$Year, "/", echuca$Month, "/", echuca$Day), "%Y/%m/%d")
echuca$Julday <- seq(from=1, to=nrow(echuca), by=1)

### some dodgy points to remove.. ### 2017-04-20
elmore_test$Tmax[elmore_test$Date > "2017-04-20" & elmore_test$Date < "2017-05-05"] <- NA

climate <- echuca

## pass off to Maddie'sfunction

#then calculate mean per day

library(dplyr)
Ta <- hour.climate.data %>%
  group_by(Date) %>%
  summarise(m=mean(Ta))

echuca$Ta <- Ta$m

plot (echuca$Date, echuca$Tmin, pch=20, col="red", xlim=c(min(elmore_test$Date), max(elmore_test$Date)))
points (elmore_test$Date, elmore_test$Tmin, pch=20, col="blue")

plot (echuca$Date, echuca$Tmax, pch=20, col="red", xlim=c(min(elmore_test$Date), max(elmore_test$Date)))
points (elmore_test$Date, elmore_test$Tmax, pch=20, col="blue")


fix <- which(!complete.cases(elmore_test))

for (i in fix){
  ii <- elmore_test[i,]$Date
  elmore_test[i,]$Tmin <- echuca$Tmin[echuca$Date==ii]
  elmore_test[i,]$Tmax <- echuca$Tmax[echuca$Date==ii]
  elmore_test[i,]$Ta <- echuca$Ta[echuca$Date==ii]
}

elmore_test <- separate(elmore_test, Date, into=c("Year", "Month", "Day"), sep="-") 

write.csv (elmore_test, file="./data/elmore_all.csv", row.names=F)
