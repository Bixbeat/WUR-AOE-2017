library(zoo)

## "c:\student\MODIS"
setwd(c("c:/student/MODIS/"))
getwd() ## to check what your working directory is.

update.packages(ask=FALSE)

## we install the latest bfast package from the R-forge website
install.packages("bfast", repos="http://R-Forge.R-project.org", dependencies=TRUE)

## a function to create a regular "ts" (time series) object in R using time information (dt)
timeser <- function(index,dt) {
  z <- zoo(index,dt)
  yr <- as.numeric(format(time(z), "%Y"))
  jul <- as.numeric(format(time(z), "%j"))
  delta <- min(unlist(tapply(jul, yr, diff))) # 16
  zz <- aggregate(z, yr + (jul - 1) / delta / 23)
  (tso <- as.ts(zz))
  return(tso) 
}

## a function to remove values (set NA)
## that do not equal a certain criteria
sel <- function(y,crit){
  ts.sel <- y
  ts.sel[!crit] <- NA
  return(ts.sel)
}

fluxtower <- c("fn_nlloobos.txt")  
filename <- paste("https://modis.ornl.gov/subset/C5_MOD13Q1/data/MOD13Q1.", fluxtower,sep="")

## now download the file only if the .csv file does not exist yet on your computer
## then read the csv file using the 'read.csv' function

if(!file.exists(fluxtower)) {
  download.file(filename,fluxtower)
  modis <- read.csv(fluxtower, colClasses = "character") 
} else {
  modis <- read.csv(fluxtower, colClasses = "character") 
}

fluxtower <- c("fn_autumbar.txt")
str(modis[1,1:7])

names(modis)[1:8] # Each pixel has a separate name

# Selecting band 5 & 7
ndvibandname <- modis$Band[5] 
rel <- modis$Band[7]

# Get pixel data for the fux tower
j <- (436)+6 # we are adding 6 since the first data column is the 7th column  
reliability <- as.numeric(modis[modis$Band == rel, j]) # reliability data
NDVI <- as.numeric(modis[modis$Band == ndvibandname, j]) # NDVI data
DATUM <- modis[modis$Band == ndvibandname, 3] # dates
DATUM <- as.Date(DATUM,"A%Y%j") # convert to a datum type

# Scale NDVI data
ts.rel <- timeser(reliability, DATUM)
ts.NDVI <- timeser(NDVI/10000, DATUM)

plot(ts.NDVI, ylab = "NDVI")

# 3-by-3 window
# To be continued

# relno0 <- sel(rel, 0)

## this is an example for two pixels
## try it out and customize for your own needs
j <- c(442:444)
t <- modis[modis$Band == ndvibandname, j] # extract NDVI data
tt <- data.matrix(t)/10000 ## convert to a data matrix and divide by 10000
ttt <- ts(apply(tt, 2, timeser, DATUM), start=c(2000,4), freq=23) 
## convert to a regular time series object
## plot(ttt) ## plot all the time series
## derive the statistics (max, mean):
maxt <- ts(apply(ttt, 1, max, na.rm=TRUE), start=c(2000,4), freq=23)
meant <- ts(apply(ttt, 1, mean, na.rm=TRUE), start=c(2000,4), freq=23)
mediant <- ts(apply(ttt, 1, median, na.rm=TRUE), start=c(2000,4), freq=23)
## plot
plot(maxt, col="green", ylim=c(0,1), ylab="NDVI")
lines(meant, col="red")
lines(mediant, col="blue")
grid()

# Use MODIS reliability to clean NDVI time series
plot(ts.NDVI, ylab = "NDVI", main = "NDVI Time Series")
lines(sel(ts.NDVI,ts.rel <= 1), col = "seagreen3", type = "p", pch=1, cex=1.5)
lines(sel(ts.NDVI,ts.rel > 1), col = "red", type = "p", pch=1, cex=1.5)
legend("bottomleft",c("pixels with a high reliablity","pixels with a low reliablity"), pch = c(1,1), cex = c(1,1), col = c("seagreen3", "red"))



