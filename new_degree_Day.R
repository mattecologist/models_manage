##############################################################################################################
## Earwig day degree models
## Matt Hill 2018
## Use and distribute as required..
##############################################################################################################

## Set trial location (Thoona or Elmore)
trial <- "Thoona"


## house keeping per trial
## these dates were worked out by first appearance of 1st instars, and working backwards in GDD for 2017
if (trial == "Elmore"){
  degday <- read.csv("./data/elmore_all.csv")
  begin.date <- as.Date("2017-09-28")
}else { if (trial == "Thoona"){
  degday <- read.csv("./data/thoona_all.csv")
  begin.date <- as.Date("2017-06-15") 
}else{print("Error")
  degday <- NA}}


## any missing vales, take average over 2 weeks
fix <- which(is.na(degday$Tmax))
for (i in fix){
  degday[i,"Tmax"] <- mean(degday[(i-7):(i+7),"Tmax"], na.rm=T)
}

fix <- which(is.na(degday$Tmin))
for (i in fix){
  degday[i,"Tmin"] <- mean(degday[(i-7):(i+7),"Tmin"], na.rm=T)
}

fix <- which(is.na(degday$Ta))
for (i in fix){
  degday[i,"Ta"] <- mean(degday[(i-7):(i+7),"Ta"], na.rm=T)
}

degday$Date <- as.Date(paste0(degday$Year,"/",degday$Month,"/", degday$Day))

## The GDD function 
# tempdata = dataframe with "Date", "tmax", "tmin" columns

gdd <- function (tempdata = tempdata, tlow=tlow, thigh=thigh){
  tempdata$GDD <- 0
  AvgDailyTemp <- tempdata$Ta
  
  # if tempdata$Tmax < tlow
  # GDD = GDD + 0
  # - add 0
  
  # if tempdata$Tmax > tlow & tempdata$Tmin > tlow
  # GDD = GDD + AvgDailyTemp - tlow
  tempdata$GDD[tempdata$Tmax > tlow & tempdata$Tmin > tlow] <- AvgDailyTemp[tempdata$Tmax > tlow & tempdata$Tmin > tlow] - tlow
  
  # if tempdata$Tmax > tlow & tempdata$Tmin < tlow
  # GDD = GDD + (1/pi) * [ (AvgDailyTemp – tlow) * ( ( pi/2 ) – arcsine(theta ) ) + ( a * cos( arcsine( theta ) ) ) ]
  a <- (tempdata$Tmax - tempdata$Tmin)/2
  theta <- ((tlow - AvgDailyTemp)/a)
  
  tempdata$GDD[tempdata$Tmax > tlow & tempdata$Tmin <= tlow] <- (1/pi) * (
    (AvgDailyTemp[tempdata$Tmax > tlow & tempdata$Tmin <= tlow] - tlow) * ( ( pi/2 ) -
                                                                          asin( theta[tempdata$Tmax > tlow & tempdata$Tmin <= tlow])) + 
      (a[tempdata$Tmax > tlow & tempdata$Tmin <= tlow] * cos(asin(theta[tempdata$Tmax > tlow & tempdata$Tmin <= tlow]))))
  
  #if NAs produced: Maybe incorporate a triangle method here????
  ## dodgy fix! 
  fix <- which(is.na(tempdata$GDD))  
  
  for (i in fix){
    if (i > 2){
      tempdata[i,"GDD"] <- mean(tempdata[(i-2):(i+2),"GDD"], na.rm=T)
    }else {
      tempdata[i,"GDD"] <- mean(tempdata[(i):(i+4),"GDD"], na.rm=T)
    }
    
    
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

#out$CGDD[nrow(out)] <- 0

return (out)
}

##########33 RUN THE MODELLLLLS
randates <- runif (1000, min=begin.date-2, max=begin.date+2)


outdata <- list()

idx <- 1

for (i in randates){

## The noise in parameters here is from the Moerkens et al paper. 
  
hatchling <- gdd(degday, 5.3+runif(1, -0.8, 0.8), 19)  
hatchling.cgdd <- cumulgdd(paste(as.Date(i, origin="1970-01-01")), hatchling, 165.7+runif(1, -14.9, 14.9))

moult1 <- gdd(degday, 7.5+runif(1, -0.7, 0.7), 23.3)
moult1.cgdd <- cumulgdd(as.Date(max(hatchling.cgdd$Date)), moult1, 101.3+runif(1, -11.5, 11.5)) 
moult1.cgdd$CGDD <- moult1.cgdd$CGDD+max(hatchling.cgdd$CGDD)

moult2 <- gdd(degday, 8.0+runif(1, -0.5, 0.5), 28)
moult2.cgdd <- cumulgdd(as.Date(max(moult1.cgdd$Date)), moult2, 87+runif(1, -12, 12))
moult2.cgdd$CGDD <- moult2.cgdd$CGDD+max(moult1.cgdd$CGDD)

moult3 <- gdd(degday, 8.7+runif(1, -0.8, 0.8), 28)
moult3.cgdd <- cumulgdd(as.Date(max(moult2.cgdd$Date)), moult3, 90.7 +runif(1, -20.2, 20.2))
moult3.cgdd$CGDD <- moult3.cgdd$CGDD+max(moult2.cgdd$CGDD)

moult4 <- gdd(degday, 8.4+runif(1, -1.7, 1.7), 28)
moult4.cgdd <- cumulgdd(as.Date(max(moult3.cgdd$Date)), moult4, 141.6+runif(1, -23.1, 23.1))
moult4.cgdd$CGDD <- moult4.cgdd$CGDD+max(moult3.cgdd$CGDD)

completed <- moult4.cgdd[nrow(moult4.cgdd),]
completed$Stage <- "Complete"

hatchling.cgdd$Stage <- "hatchling"
moult1.cgdd$Stage <- "moult1"
moult2.cgdd$Stage <- "moult2"
moult3.cgdd$Stage <- "moult3"
moult4.cgdd$Stage <- "moult4"

all_data <- rbind (hatchling.cgdd, moult1.cgdd, moult2.cgdd, moult3.cgdd, moult4.cgdd, completed)

all_data$run <- paste(idx)
outdata[[idx]] <- all_data

idx <- idx+1
#CDD <-0

}

all_data <- do.call("rbind", outdata)

library (ggplot2)
library (viridis)
ggplot (all_data, aes(x=Date, y=CGDD))+
  geom_point(aes(colour=Stage), alpha=0.2, size=2)+
  scale_color_viridis(discrete = TRUE)+
  stat_smooth(method="loess")

ggplot (all_data, aes(x=Date, y=CGDD))+
  geom_boxplot(aes(colour=Stage), alpha=0.2, size=2)+
  scale_color_viridis(discrete = TRUE)

library(dplyr)
out_dates <- all_data %>%
  group_by(Stage) %>%
  summarise(max=max(Date), min=min(Date), mean=mean(Date), spread=max(Date)-min(Date))

out_dates <- as.data.frame(out_dates)
out_dates

ggplot (all_data[all_data$Stage == "Complete",], aes(Stage, Date))+
  geom_boxplot()

    
# degday$CGDD <- degday$GDD[1]
# x = 2
# while (x <= length(degday$GDD)){
#   
#   y = x - 1
#   degday$CGDD[x] <-  degday$CGDD[y] + degday$GDD[x]
#   x = x + 1
# }   



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
  


