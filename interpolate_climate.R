tmin <- read.csv ("./data/weather/082170_tmin.csv")
tmax <- read.csv ("./data/weather/082170_tmax.csv")


tmax <- tmax[3805:nrow(tmax),]
tmin <- tmin[3805:nrow(tmin),]

tmin <- tmin[1:nrow(tmin)-1,]

tmin <- tmin[,c("Year", "Month", "Day", "Minimum.temperature..Degree.C.")]
tmax <- tmax[,c("Year", "Month", "Day", "Maximum.temperature..Degree.C.")]



colnames(tmax) <- c("Year", "Month", "Day", "Tmax")
colnames(tmin) <- c("Year", "Month", "Day", "Tmin")

tmax$Julday <- seq(from=1, to=nrow(tmax), by=1)

climate <- cbind(tmax[,c("Julday", "Year", "Month", "Day")], tmin$Tmin, tmax$Tmax)

colnames(climate) <- c("Julday","Year", "Month", "Day", "Tmin", "Tmax")

climate.orig <- climate

## pass off to Maddie'sfunction

#then calculate mean per day

library(dplyr)
Ta <- hour.climate.data %>%
  group_by(Julday) %>%
  summarise(m=mean(Ta))


climate.orig$Ta <- Ta$m

##### go back to the degree day script...


stations <- c("Benalla")

library (Interpol.T)


Th_int_list <- Th_int_series(cal_times = calibration_l,
                             cal_shape = calibration_shape,
                             TMIN=tmin, TMAX=tmax,
                             start_year = 2016, end_year = 2018,
                             active_IDs = stations)

Tm_list <- daily_mean(hourly_list = Th_int_list, series_names = c("Benalla"))

Tm <- cbind(Tm_list$Date, Tm_list$Benalla$Tm)

colnames(Tm)[4] <- "Tm"


fix <- which(is.na(Tm$Tm))
for (i in fix){
  Tm$Tm[i] <- mean(Tm$Tm[(i-7):(i+7)], na.rm=T)
}

Tm <- Tm[1-nrow(Tm)-1,]
tmin <- tmin[1-nrow(tmin)-1,]

avg_temp <- (tmax$Benalla + tmin$Benalla)/2
