#Big Data Assignment 1- Week 3
#Dina Pinchuck 
#Odel Fhima 

#Library for plotting graphs
library(ggplot2)
#library used for hour function
library(lubridate)

#read data
A <- read.delim('table.tsv')
#change time to eastern standard
A$DateTime <- as.POSIXct( A$megawatthours, tz = "EST", "%H:%M EST %m/%d/%Y" )

pdf('week3_power.pdf') 

#Question 1

#B has the fixed times
B <- A[ order(A$DateTime), ] 
dates<-B[,'DateTime']
#creates time data for each net generation/demand
times<-c(dates,dates,dates,dates,dates,
         dates,dates,dates,dates,dates,dates)

#List of all the locations
locations <- c( "BPAT", "CISO", "CPLE", "ERCO", "FPL", "ISNE",
                "MISO" , "NYIS" , "PACW" , "PJM","United.States.Lower.48..region."  )

#creating a list of locations for each time so the dimensions are the same in order to build the data cube
locationsList<-vector()
for (location in locations)
{
  for (i in 1:1536)
  {
    locationsList<-c(locationsList,location)
  }
}

#all the net generation columns
nets<-list("Net.generation","Net.generation.1","Net.generation.2",
           "Net.generation.3","Net.generation.4","Net.generation.5",
           "Net.generation.6","Net.generation.7","Net.generation.8",
           "Net.generation.9","Net.generation.10")
#connecting net generation to the rest of the information
net<-vector()
for (i in nets)
{
  net<-c(net,B[,i])
}
#data cube
dataF<-data.frame(location=locationsList,time=times,netgen=net)
row.names(dataF)<-NULL
dCube <-
  tapply(dataF$netgen,
         dataF[,c("location","time")],
         FUN = sum )
#dice cube for the week that we want
cube<-dCube[,720:887]

#roll up
up <-   cut(as.numeric(as.POSIXct(dimnames(cube)$time )),
            c(as.numeric(as.POSIXct("2021-02-06 23:00:00 EST")),
              as.numeric(as.POSIXct("2021-02-07 23:00:00 EST")),
              as.numeric(as.POSIXct("2021-02-08 23:00:00 EST")),
              as.numeric(as.POSIXct("2021-02-09 23:00:00 EST")),
              as.numeric(as.POSIXct("2021-02-10 23:00:00 EST")),
              as.numeric(as.POSIXct("2021-02-11 23:00:00 EST")),
              as.numeric(as.POSIXct("2021-02-12 23:00:00 EST")),
              as.numeric(as.POSIXct("2021-02-13 23:00:00 EST"))),
            dig.lab = 8)
up.group <- split(dimnames(cube)$time, up )
n.time_levels <- length(up.group)

new.3d <- lapply(
  up.group, function(k)
    apply( cube[,k], c("location"), sum, na.rm = T )
)

new.cube <- cube[, 1:n.time_levels]



# apply the new values
for (i in seq(n.time_levels))
  new.cube[,i] <- new.3d[[i]]

dimlist <- dimnames(new.cube)
where.time <- which(names(dimlist) == "time")
names(dimlist)[where.time] <- "day"

 dimlist$day <- c("2021-02-07", "2021-02-08", "2021-02-09", "2021-02-10",
                 "2021-02-11", "2021-02-12", "2021-02-13") 

dimnames(new.cube) <- dimlist

#graph 1

means <- vector()
for ( i in dimlist$day )
{
  means[[ i ]] <- mean( new.cube[,i] )
}
#mean and sum create the "same" graph, just scaled to different numbers
DF <- data.frame ( x = dimlist$day, y = means)
DF$x <- as.Date(DF$x)
# plot
#scale_x_discrete(limits=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))
print(ggplot(DF, aes(x, y)) +geom_point()+
        geom_line()+ggtitle("Mean Net Generation")+labs(y="Net Generation", x = "Day"))


#Question 2
#time and locations were already calculated in the first question

demandColumns<-list("Demand","Demand.1","Demand.2",
                    "Demand.3","Demand.4","Demand.5",
                    "Demand.6","Demand.7","Demand.8",
                    "Demand.9","Demand.10")
#connecting the demands to the rest of the information
demands<-vector()
for (i in demandColumns)
{
  demands<-c(demands,B[,i])
}

#data cube
demandDataF<-data.frame(location=locationsList,time=times,demand=demands)
row.names(demandDataF)<-NULL
demandC <-
  tapply(demandDataF$demand,
         demandDataF[,c("location","time")],
         FUN = sum )
#dice data cube for requested locations


demandCube<-demandC[c("PJM","NYIS","ISNE","FPL","CPLE"),]

#fix the times to only contain the requested hours
demandCube<-demandCube[, (hour(dimnames(demandCube)$time)>=10 & hour(dimnames(demandCube)$time)<=18)|
                         (hour(dimnames(demandCube)$time)>=20 & hour(dimnames(demandCube)$time)<=23)|
                         (hour(dimnames(demandCube)$time)>=0 & hour(dimnames(demandCube)$time)<=3)]

dimnames(demandCube)$time<-dimnames(demandCube)$time #hour



#roll up
hourUp <- cut(hour(dimnames(demandCube)$time),
              c(-1,0,1,2,3,10,11,12,13,14,15,16,17,18,20,21,22,23),
              dig.lab = 17)

hourUp.group <- split(dimnames(demandCube)$time, hourUp )
n.hour_levels <- length(hourUp.group)
print(n.hour_levels)
new.d <- lapply(hourUp.group, function(k)
  apply(demandCube[,k], c("location"), sum, na.rm = T ))

new.demandCube <- demandCube[, 1:n.hour_levels]
# apply the new values
for (i in seq(n.hour_levels))
  new.demandCube[,i] <- new.d[[i]]

demanddimlist <- dimnames(new.demandCube)
where.time <- which(names(demanddimlist) == "time")
names(demanddimlist)[where.time] <- "hour"

demanddimlist$hour <- c("00:00", "01:00", "02:00","03:00", "10:00","11:00","12:00","13:00","14:00","15:00","16:00","17:00","18:00","20:00","21:00","22:00","23:00")
dimnames(new.demandCube) <- demanddimlist


tenToSix<-new.demandCube[,c("10:00","11:00","12:00","13:00","14:00","15:00","16:00","17:00","18:00")]
eightToThree<-new.demandCube[,c("20:00","21:00","22:00","23:00","00:00", "01:00", "02:00","03:00")]

demandmeans <- vector()
for ( i in dimnames(tenToSix)$hour )
{
  demandmeans[[ i ]] <- mean( tenToSix[,i] )
}
demandDF <- data.frame ( x =dimnames(tenToSix)$hour, y = demandmeans)
demandDF$x= factor(dimnames(tenToSix)$hour, levels=c( "10:00","11:00","12:00","13:00","14:00","15:00","16:00","17:00", "18:00"))
demandDF$x<-as.numeric(demandDF$x)
lmDF1<-demandDF[c(1:7),c(1:2)]
demand.lm1<-lm(formula=y ~x ,data=lmDF1)
a1<-coef(demand.lm1)[1]
b1<-coef(demand.lm1)[2]
lmDF2<-demandDF[c(7:9),c(1:2)]
demand.lm2<-lm(formula=y ~x ,data=lmDF2)
a2<-coef(demand.lm2)[1]
b2<-coef(demand.lm2)[2]
graph2<-ggplot(demandDF, aes(x, y,colour="line1"), group = 1 ) + scale_x_discrete( limits=c( "10:00","11:00","12:00","13:00","14:00","15:00","16:00","17:00","18:00")) +
  geom_abline(aes(intercept = a1,slope=b1,colour="line2") , show.legend =TRUE ) + geom_abline(aes(intercept = a2,slope=b2,colour="line3"),show.legend =TRUE ) + scale_colour_manual(labels = c("Regular", "Linear 10-16","Linear 16-18 "),values=c("line1"="black","line2"="#9979ff","line3"="#4060ff"))+theme(legend.position="bottom") +ggtitle("Demand Per Minute")+labs(y="Demand",x="Time")  +geom_point() +geom_line(show.legend=TRUE)
print(graph2)

demandmeans2 <- vector()
for ( i in dimnames(eightToThree)$hour )
{
  demandmeans2[[ i ]] <- mean( eightToThree[,i] )
}
demandDF2 <- data.frame ( x =dimnames(eightToThree)$hour, y = demandmeans2)
demandDF2$x= factor(dimnames(eightToThree)$hour, levels=c("20:00","21:00","22:00","23:00","00:00", "01:00", "02:00","03:00" ))
demandDF2$x<-as.numeric(demandDF2$x)
lmDF3<-demandDF2[c(1:8),c(1:2)]
demand.lm3<-lm(formula=y ~x ,data=lmDF3)
a3<-coef(demand.lm3)[1]
b3<-coef(demand.lm3)[2]
graph3<-ggplot(demandDF2, aes(x, y, colour="line1"), lwd=1, group = 1 ) + scale_x_discrete( limits=c( "20:00","21:00","22:00","23:00","00:00", "01:00", "02:00","03:00")) +
  geom_abline(aes(intercept = a3,slope=b3,colour= "line2"), lwd=1 , show.legend =TRUE ) +ggtitle("Demand Per Minute")+labs(y="Demand",x="Time")  +geom_point()+ scale_colour_manual(labels = c("Regular Graph", "Linear Regression"),values=c("line1"="black","line2"="#4ca4f4")) +
  theme(legend.position = "bottom") +geom_line(show.legend=TRUE)
print(graph3)

dev.off()
