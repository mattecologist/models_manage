tmin <- read.csv ("./data/weather/IDCJAC0011_082170_1800_Data.csv")
tmax <- read.csv ("./data/weather/IDCJAC0010_082170_1800_Data.csv")



## tmin has one more observation thatn tmax
tmax <- tmax[307:4349,]
tmin <- tmin[307:4349,]

## setting up data frame
degday <- data.frame(cbind(tmax[,c("Year", "Month", "Day", "Maximum.temperature..Degree.C.")], tmin$Minimum.temperature..Degree.C.))
colnames (degday) <- c("Year", "Month", "Day", "tmax", "tmin")


## any missing vales, take average over 2 weeks
fix <- which(is.na(degday$tmax))
for (i in fix){
  degday[i,4] <- mean(degday[(i-7):(i+7),4], na.rm=T)
}

fix <- which(is.na(degday$tmin))
for (i in fix){
  degday[i,5] <- mean(degday[(i-7):(i+7),5], na.rm=T)
}


degday$Date <- as.Date(paste0(degday$Year,"/",degday$Month,"/", degday$Day))

## thresholds
tlow <- 5.3
thigh <- 19

# tempdata = dataframe with "Date", "tmax", "tmin" columns

gdd <- function (tempdata = tempdata, tlow=tlow, thigh=thigh){
  tempdata$GDD <- 0
  AvgDailyTemp <- (tempdata$tmax + tempdata$tmin)/2
  
  # if tempdata$Tmax < tlow
  # GDD = GDD + 0
  # - add 0
  
  # if tempdata$Tmax > tlow & tempdata$Tmin > tlow
  # GDD = GDD + AvgDailyTemp - tlow
  tempdata$GDD[tempdata$tmax > tlow & tempdata$tmin > tlow] <- AvgDailyTemp[tempdata$tmax > tlow & tempdata$tmin > tlow] - tlow
  
  # if tempdata$Tmax > tlow & tempdata$Tmin < tlow
  # GDD = GDD + (1/pi) * [ (AvgDailyTemp – tlow) * ( ( pi/2 ) – arcsine(theta ) ) + ( a * cos( arcsine( theta ) ) ) ]
  a <- (tempdata$tmax - tempdata$tmin)/2
  theta <- ((tlow - AvgDailyTemp)/a)
  
  tempdata$GDD[tempdata$tmax > tlow & tempdata$tmin <= tlow] <- (1/pi) * (
    (AvgDailyTemp[tempdata$tmax > tlow & tempdata$tmin <= tlow] - tlow) * ( ( pi/2 ) -
                                                                          asin( theta[tempdata$tmax > tlow & tempdata$tmin <= tlow])) + 
      (a[tempdata$tmax > tlow & tempdata$tmin <= tlow] * cos(asin(theta[tempdata$tmax > tlow & tempdata$tmin <= tlow]))))
  
  #if NAs produced:
  ## dodgy fix! 
  fix <- which(is.na(tempdata$GDD))  
  
  for (i in fix){
    tempdata[i,"GDD"] <- mean(tempdata[(i-2):(i+2),"GDD"], na.rm=T)
    
  }
  
  return(tempdata)
  
}


hatchling <- gdd(degday, 5.3, 19)
moult1 <- gdd(degday, 7.5, 23.3)
moult2 <- gdd(degday, 8.0, 28)
moult3 <- gdd(degday, 8.7, 28)
moult4 <- gdd(degday, 8.4, 28)




cumulgdd <- function (start.date=start.date, tempdata=tempdata, DD=DD){
y <- as.Date(start.date, origin="1970-01-01")
CDD <- 0
res.1 <- c()
res.2 <- c()
while (CDD < DD){ # value here for x needs to be the degree days need for = (165 + 101)
  CDD <- CDD+tempdata$GDD[tempdata$Date == y]
  #print(CDD)
  #print(y)
  y <- y+1
  res.1 <- c(res.1, CDD)
  res.2 <- c(res.2, as.Date(y))
}

out <- as.data.frame(cbind (res.2, res.1))

colnames (out) <- c("Date", "CGDD")

out$Date <- as.Date(out$Date, origin="1970-01-01")

out$CGDD[nrow(out)] <- 0

return (out)
}

## this date was worked out by first appearance of 1st instars, and working backwards in GDD for 2017
begin.date <- as.Date("2017-06-14")
randates <- runif (10, min=begin.date-14, max=begin.date+14)

outdata <- list()

idx <- 1

for (i in randates){


  
hatchling.cgdd <- cumulgdd(paste(as.Date(i, origin="1970-01-01")), hatchling, 165.7)
moult1.cgdd <- cumulgdd(as.Date(max(hatchling.cgdd$Date)), moult1, 101.3)
moult2.cgdd <- cumulgdd(as.Date(max(moult1.cgdd$Date)), moult2, 87)
moult3.cgdd <- cumulgdd(as.Date(max(moult2.cgdd$Date)), moult3, 90.7)
moult4.cgdd <- cumulgdd(as.Date(max(moult3.cgdd$Date)), moult4, 141.6)

hatchling.cgdd$Stage <- "hatchling"
moult1.cgdd$Stage <- "moult1"
moult2.cgdd$Stage <- "moult2"
moult3.cgdd$Stage <- "moult3"
moult4.cgdd$Stage <- "moult4"

all_data <- rbind (hatchling.cgdd, moult1.cgdd, moult2.cgdd, moult3.cgdd, moult4.cgdd)

all_data$run <- paste(idx)
outdata[[idx]] <- all_data

idx <- idx+1
CDD <-0

}

all_data <- do.call("rbind", outdata)


ggplot (all_data, aes(x=Date, y=CGDD, fill=Stage))+
  geom_point(aes(colour=Stage))+
  stat_smooth(method="loess")

    
degday$CGDD <- degday$GDD[1]
x = 2
while (x <= length(degday$GDD)){
  
  y = x - 1
  degday$CGDD[x] <-  degday$CGDD[y] + degday$GDD[x]
  x = x + 1
}   



#################################################################################################################################################
library (reshape2)
library (ggplot2)
lifecycle <- read.csv ("./data/lifecycle_data.csv")

lifecycle <- lifecycle[,c("fieldID", "date.end","eurearwigAM", "eurearwigAF", "eurearwigAG",
                          "X1instar", "X2instar", "X3instar", "X4instar")]

lifecycle <- melt (lifecycle, id.vars = c("fieldID", "date.end"))

lifecycle$date.end <- as.Date(lifecycle$date.end, "%d/%m/%Y")

lifecycle <- lifecycle[which(!is.na(lifecycle$date.end)),]

thoona <- lifecycle[lifecycle$fieldID == "Thoona",]

thoona <- thoona[thoona$value > 0,]



thoona <- aggregate(thoona$value,
          list(Date = thoona$date.end,
               Stage = thoona$variable),
          sum)

thoona
ggplot (thoona, aes(date.end, value, fill=variable), colour=variable)+
  geom_point(aes(colour=variable))

ggplot (thoona, aes(date.end, value, fill=variable), colour=variable)+
  geom_bar(stat="identity")


## calculatue GDD in reverse from the date 

y <- as.Date("2017-04-14")
x <- 0
while (x < 165){ # value here for x needs to be the degree days need for = (165 + 101)
  x <- x+hatchling$GDD[hatchling$Date == y]
  print(x)
  print(y)
  y <- y-1
}
  


