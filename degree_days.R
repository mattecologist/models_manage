#### Degree Day.

tlow <- 11
thigh <- 30
degday <- read.csv ("./data/static.csv", header=T)

###################################################################################
## Single Triangle method
###################################################################################

degday$AppThresLow <- tlow
degday$AppThresHigh <- thigh



degday$DD_low <- ifelse((degday$Tmax < degday$AppThresLow), 0, 

                      ifelse (degday$Tmin > degday$AppThresLow, ((degday$Tmax + degday$Tmin)/2) - degday$AppThresLow, 
              
                          ((degday$Tmax-degday$AppThresLow)/2) * ((degday$Tmax - degday$AppThresLow)/(degday$Tmax - degday$Tmin))))


degday$DD_high <- ifelse((degday$Tmax < degday$AppThresHigh), 0, 
                        
                        ifelse (degday$Tmin > degday$AppThresHigh, ((degday$Tmax + degday$Tmin)/2) - degday$AppThresHigh, 
                                
                                ((degday$Tmax-degday$AppThresHigh)/2) * ((degday$Tmax - degday$AppThresHigh)/(degday$Tmax - degday$Tmin))))

degday$DD <- degday$DD_low - degday$DD_high

degday$CDDlow <- degday$DD_low[1]
x = 2
      while (x <= length(degday$DD_low)){
  
          y = x - 1
          degday$CDDlow[x] <-  degday$CDDlow[y] + degday$DD_low[x]
          x = x + 1
      }  

degday$CDD <- degday$DD[1]
x = 2
while (x <= length(degday$DD)){
  
  y = x - 1
  degday$CDD[x] <-  degday$CDD[y] + degday$DD[x]
  x = x + 1
}   

degday$step <- seq(from=1, to=length(degday[,1]), by=1)

###################################################################################
## Single Sine method
###################################################################################

## http://r.789695.n4.nabble.com/Issue-with-asin-td4484462.html

degday$GDD <- 0
AvgDailyTemp <- (degday$Tmax + degday$Tmin)/2

# if degday$Tmax < tlow
# GDD = GDD + 0
# - add 0

# if degday$Tmax > tlow & degday$Tmin > tlow
# GDD = GDD + AvgDailyTemp - tlow
degday$GDD[degday$Tmax > tlow & degday$Tmin > tlow] <- AvgDailyTemp[degday$Tmax > tlow & degday$Tmin > tlow] - tlow

# if degday$Tmax > tlow & degday$Tmin < tlow
# GDD = GDD + (1/pi) * [ (AvgDailyTemp – tlow) * ( ( pi/2 ) – arcsine(theta ) ) + ( a * cos( arcsine( theta ) ) ) ]
a <- (degday$Tmax - degday$Tmin)/2
theta <- ((tlow - AvgDailyTemp)/a)
degday$GDD[degday$Tmax > tlow & degday$Tmin <= tlow] <- (1/pi) * (
  (AvgDailyTemp[degday$Tmax > tlow & degday$Tmin <= tlow] - tlow) * ( ( pi/2 ) -
                                                                asin( theta[degday$Tmax > tlow & degday$Tmin <= tlow])) + 
    (a[degday$Tmax > tlow & degday$Tmin <= tlow] * cos(asin(theta[degday$Tmax > tlow & degday$Tmin <= tlow]))))

degday$CGDD <- degday$GDD[1]
x = 2
while (x <= length(degday$GDD)){
  
  y = x - 1
  degday$CGDD[x] <-  degday$CGDD[y] + degday$GDD[x]
  x = x + 1
}   

#########################################################################################################################
## devRate
#########################################################################################################################

## https://cran.r-project.org/web/packages/devRate/vignettes/example_tropical.html

library (devRate)

hatchling <- read.csv ("./data/images/hatchling.csv", header=FALSE)
hatchling[2][hatchling[2] < .001] <- 0
hatchling <- round(hatchling, 3)

moult1 <- read.csv ("./data/images/moult1.csv", header=FALSE)
moult1 [2][moult1 [2] < .001] <- 0
moult1 <- round(moult1 , 3)

moult2 <- read.csv ("./data/images/moult2.csv", header=FALSE)
moult2 [2][moult2 [2] < .001] <- 0
moult2 <- round(moult2 , 3)

moult3 <- read.csv ("./data/images/moult3.csv", header=FALSE)
moult3 [2][moult3 [2] < .001] <- 0
moult3 <- round(moult3 , 3)

moult4 <- read.csv ("./data/images/moult4.csv", header=FALSE)
moult4 [2][moult4 [2] < .001] <- 0
moult4 <- round(moult4 , 3)

devRateInfo(eq = briere1_99)

#Briere, J.F., Pracros, P., le Roux, A.Y. and Pierre, S. (1999) A novel rate
#model of temperature-dependent development for arthropods. Environmental
#Entomology, 28, 22-29.

#rT ~ aa * T * (T - Tmin) * (Tmax - T)^(1/2)

devRatePlotInfo (eq = briere1_99, sortBy = "ordersp",
                 ylim = c(0, 0.20), xlim = c(0, 50))

## with the Brier model 


## having troubles estimating the start parameters - think a lot of problem comes from the Tmax threshold.      
      
      #rT ~ aa * T * (T - Tmin) * (Tmax - T)^(1/2)
      hatchling$test <- 0.00014 * hatchling$V1 * (hatchling$V1 - 5) * (32 - hatchling$V1)^(1 / 2)
      plot (hatchling$V2 ~ hatchling$V1)
      plot (hatchling$test ~ hatchling$V1)
      
      
      moult1$test <- 0.00018 * moult1$V1 * (moult1$V1 - 5.3) * (23.3 - moult1$V1)^(1 / 2)
      
      summary (nls (0 ~ f -V2*(1-eta*V1), data=hatchling, start=list(eta=0, f=0.0014))
               
               nls (V2 ~ V1, data=hatchling)
      
      newMod <- nls(V2 ~ a*V1^b, data=moult1, start = list(a=exp(9.947),b=-2.011))
      predict(newMod, newdata = data.frame(weeks=c(1,2,3,4,5,6,7,8,9,10)))

m_hatchling <- devRateModel(eq = briere1_99, temp = hatchling[,1], devRate = hatchling[,2], startValues = list(aa = 0.00014, Tmin= 5.3, Tmax=19))
m_moult1 <- devRateModel(eq = briere1_99, temp = moult1[,1], devRate = moult1[,2], startValues = list(aa = 0.00018, Tmin= 7.5, Tmax=23.3))
m_moult2 <- devRateModel(eq = briere1_99, temp = moult2[,1], devRate = moult2[,2], startValues = list(aa = 0.00020, Tmin= 8.0, Tmax=23.4))
m_moult3 <- devRateModel(eq = briere1_99, temp = moult3[,1], devRate = moult3[,2], startValues = list(aa = 0.00019, Tmin= 8.7, Tmax=23.5))
m_moult4 <- devRateModel(eq = briere1_99, temp = moult4[,1], devRate = moult4[,2], startValues = list(aa = 0.00011, Tmin= 8.4, Tmax=23.4))


## testing with the Taylor model.....
m_hatchling <- devRateModel(eq = taylor_81, temp = hatchling[,1], devRate = hatchling[,2], startValues = list(Rm = 0.05, To= 5.3,Tm=19))
m_moult1 <- devRateModel(eq = taylor_81, temp = moult1[,1], devRate = moult1[,2], startValues = list(Rm = 0.1, To= 7.5, Tm=23.3))
m_moult2 <- devRateModel(eq = taylor_81, temp = moult2[,1], devRate = moult2[,2], startValues = list(Rm = 0.1, To= 8.0, Tm=23.4))
m_moult3 <- devRateModel(eq = taylor_81, temp = moult3[,1], devRate = moult3[,2], startValues = list(Rm = 0.5, To= 8.7, Tm=23.5))
m_moult4 <- devRateModel(eq = taylor_81, temp = moult4[,1], devRate = moult4[,2], startValues = list(Rm = 0.1, To= 8.4, Tm=23.4))

##Lactin model: rT ~ exp(aa * T) - exp(aa * Tmax - (Tmax - T)/deltaT)
m_hatchling <- devRateModel(eq = lactin1_95, temp = hatchling[,1], devRate = hatchling[,2], startValues = list(aa = 0.15,Tmax=19, deltaT=5))
m_moult1 <- devRateModel(eq = lactin1_95, temp = moult1[,1], devRate = moult1[,2], startValues = list(aa = 0.15,Tmax=23, deltaT=5.5))
m_moult2 <- devRateModel(eq = lactin1_95, temp = moult2[,1], devRate = moult2[,2], startValues = list(aa = 0.15,Tmax=23, deltaT=5.5))
m_moult3 <- devRateModel(eq = lactin1_95, temp = moult3[,1], devRate = moult3[,2], startValues = list(aa = 0.15,Tmax=23, deltaT=5.5))
m_moult4 <- devRateModel(eq = lactin1_95, temp = moult4[,1], devRate = moult4[,2], startValues = list(aa = 0.15,Tmax=23, deltaT=5.5))

par(mfrow=c(3, 2))

devRatePlot(eq= lactin1_95, nlsDR=m_hatchling, temp = hatchling[,1], devRate = hatchling[,2], pch = 16, ylim = c(0, 0.2), main="Hatchling")
devRatePlot(eq= lactin1_95, nlsDR=m_moult1, temp = moult1[,1], devRate = moult1[,2], pch = 16, ylim = c(0, 0.2), main="Moult 1")
devRatePlot(eq= lactin1_95, nlsDR=m_moult2, temp = moult2[,1], devRate = moult2[,2], pch = 16, ylim = c(0, 0.2), main="Moult 2")
devRatePlot(eq= lactin1_95, nlsDR=m_moult3, temp = moult3[,1], devRate = moult3[,2], pch = 16, ylim = c(0, 0.2), main="Moult 3")
devRatePlot(eq= lactin1_95, nlsDR=m_moult4, temp = moult4[,1], devRate = moult4[,2], pch = 16, ylim = c(0, 0.2), main="Moult 4")

par (mfrow=c(1,1))


devRatePlot(eq = taylor_81, nlsDR = m_moult4, temp = hatchling[,1], devRate = hatchling[,2],
            pch = 16, ylim = c(0, 0.2), spe=TRUE)


forecastForficula <- devRateIBM(
  tempTS = rnorm(n = 100, mean = 15, sd = 1),
  timeStepTS = 1,
  models = list(m_hatchling, m_moult1, m_moult2),
  numInd = 500,
  stocha = 0.015,
  timeLayEggs = 1)


##########################################################################################################3
###
##########################################################################################################

## Earwig site 
#"Thoona" = Benalla (station ID 082170)

# Max temps
tmax <- read.csv ("./data/weather/082170_tmax.csv")
tmax <- tmax[,3:6]
colnames (tmax) <- c("Year", "Month", "Day", "Tmax")

## any missing vales, take average over 2 weeks
fix <- which(is.na(tmax$Tmax))
for (i in fix){
  tmax[i,4] <- mean(tmax[(i-7):(i+7),4], na.rm=T)
}

# Min temps
tmin <- read.csv ("./data/weather/082170_tmin.csv")
tmin <- tmin[,3:6]
colnames (tmin) <- c("Year", "Month", "Day", "Tmin")


  
temp_data <- climate.orig$Ta

## any missing vales, take average over 2 weeks
fix <- which(is.na(temp_data))
for (i in fix){
  temp_data[i] <- mean(temp_data[(i-7):(i+7)], na.rm=T)
}

forecastForficula <- devRateIBM(
 # tempTS = temp_data$Tmin,
  tempTS = temp_data,
  timeStepTS = 1,
  models = list(m_hatchling, m_moult1, m_moult2, m_moult3, m_moult4),
  numInd = 5000,
  stocha = 0.04,
  timeLayEggs = 30)


par(mar = c(5,5,2,5))
devRateIBMPlot(ibm = forecastForficula, typeG = "density", threshold = .1)

par(new=T)
with (climate.orig, plot (Julday, Ta, pch=16, axes=F, xlab=NA, ylab=NA, cex=0.6, col="light grey"))
axis(side = 4)
mtext(side = 4, line = 3, 'Daily Max Temperature')
abline (23, 0)



par(mar = c(5,5,2,5))

devRateIBMPlot(ibm = forecastForficula, typeG = "hist")

par(new=T)
with (climate.orig, plot (Julday, Ta, pch=16, axes=F, xlab=NA, ylab=NA, cex=0.6, col="light blue"))
axis(side = 4)
mtext(side = 4, line = 3, 'Daily Avg. Temperature')
abline (23, 0)







#daily code = 136
#monthy code = 139

bomdata<- function(station,code){
  for(i in 1: length(station)){
    p.url<-paste("http://www.bom.gov.au/jsp/ncc/cdio/weatherData/av?p_stn_num=",station[i],"&p_display_type=availableYears&p_nccObsCode=",code,sep ="")
    download.file(p.url,"test.txt")
    filelist <- list.files(pattern = ".txt")
    foo<- file(filelist,"r")
    text<- suppressWarnings(readLines(foo))
    close(foo)
    l<- regexpr(":",text[1])
    m<- unlist(gregexpr(",", text[1], perl = TRUE))
    pc<- substr(text[1],l[[1]]+1,l[[1]]+(m[2]-(l[[1]]+1)))
    url<-paste("http://www.bom.gov.au/jsp/ncc/cdio/weatherData/av?p_display_type=dailyZippedDataFile&p_stn_num=",station[i],"&p_c=",pc,"&p_nccObsCode=",code,"&p_startYear=2013", sep ="")
    suppressWarnings(download.file(url,paste(station[i],".zip",sep= ""), mode = "wb"))
    unlink("test.txt")
  }
}

bomdata(82170,136)

zipF <- "./80023.zip"
outDir<-"./"
unzip(zipF,exdir=outDir)

weather_data <- read.csv("IDCJAC0009_80023_1800_Data.csv", header=T)

