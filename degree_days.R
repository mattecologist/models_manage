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

#development rate (r)




