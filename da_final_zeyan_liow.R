library(mongolite)

is.POSIXct <- function(x) inherits(x, "POSIXct")

m <- mongo(collection = "HomeTemp", db = "bigdata_final_zeyan_liow", url = "mongodb://localhost")
hTemp <- m$find(limit = 1000000, skip = 0, fields = '{ "_id" : true, "temp" : true }')

m1 <- mongo(collection = "HomePower", db = "bigdata_final_zeyan_liow", url = "mongodb://localhost")
hPower <- m1$find(limit = 1000000, skip = 0, fields = '{ "_id" : true, "power" : true }')

m2 <- mongo(collection = "Dublin", db = "bigdata_final_zeyan_liow", url = "mongodb://localhost")
dublin <- m2$find(limit = 1000000, skip = 0, fields = '{ "_id" : false, "date" : true, "irain" : true, "rain" : true, "itemp" : true, "temp" : true, "iwb" : true, "wetb" : true, "dewpt" : true, "vappr" : true, "rhum" : true, "msl" : true, "iwdsp" : true, "wdsp" : true, "iwddir" : true, "wddir" : true, "ww" : true, "w" : true, "sun" : true, "vis" : true, "clht" : true, "clamt" : true }')

hTemp <- lapply(hTemp, function(x) if(is.character(x)) as.POSIXct(x,format="%Y-%m-%d %H:%M:%S") else x)
hPower <- lapply(hPower, function(x) if(is.character(x)) as.POSIXct(x,format="%Y-%m-%d %H:%M:%S") else x)

names(hTemp) <- c("date", "hmTemp")
names(hPower) <- c("date", "hmPow")

print(hTemp)
print(hPower)
print(dublin)

merge1 <- merge(hTemp,hPower)
data <- merge(merge1,dublin)

month <- (format(as.Date(data$date), "%m"))
month <- as.numeric(month)
data <- cbind(data, month)

day <- (format(as.Date(data$date), "%d"))
day <- as.numeric(day)
data <- cbind(data, day)

hour <- (format(as.POSIXct(strptime(data$date,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H"))
hour <- as.numeric(hour)
data <- cbind(data, hour)

View(data)
str(data)

#1################################################################################1

###Check for missing data and outliers
length(which(is.na(data)==TRUE))
#data <- na.omit(data)
summary(data)

boxplot(data$hmTemp,ylab="Home Temperature(°C)",cex.lab=1.5, cex.axis=1.5,cex=1.5)
boxplot(data$hmPow,ylab="Home Power(kWh)",cex.lab=1.5, cex.axis=1.5,cex=1.5)
boxplot(data$rain,ylab="Precipitation Amount(mm)",cex.lab=1.5, cex.axis=1.5,cex=1.5)
boxplot(data$temp,ylab="Air Temperature(°C)",cex.lab=1.5, cex.axis=1.5,cex=1.5)
boxplot(data$wetb,ylab="Wet Bulb Air Temperature(°C)",cex.lab=1.5, cex.axis=1.5,cex=1.5)
boxplot(data$dewpt,ylab="Dew Point Air Temperature(°C)",cex.lab=1.5, cex.axis=1.5,cex=1.5)
boxplot(data$vappr,ylab="Vapour Pressurehpa(hpa)",cex.lab=1.5, cex.axis=1.5,cex=1.5)
boxplot(data$rhum,ylab="Relative Humidity(%)",cex.lab=1.5, cex.axis=1.5,cex=1.5)
boxplot(data$msl,ylab="Mean Sea Level Pressure(hPa)",cex.lab=1.5, cex.axis=1.5,cex=1.5)
boxplot(data$wdsp,ylab="Mean Hourly Wind Speed(kt)",cex.lab=1.5, cex.axis=1.5,cex=1.5)
boxplot(data$wddir,ylab="Predominant Hourly wind Direction(kt)",cex.lab=1.5, cex.axis=1.5,cex=1.5)
boxplot(data$vis,ylab="Visibility(m)",cex.lab=1.5, cex.axis=1.5,cex=1.5)
boxplot(data$clht,ylab="Cloud Ceiling Height(100s feet)",cex.lab=1.5, cex.axis=1.5,cex=1.5)
boxplot(data$clamt,ylab="Cloud Amount(okta)",cex.lab=1.5, cex.axis=1.5,cex=1.5)

hist(data$irain,xlab="Index",cex.lab=1.8, cex.axis=1.8,cex=1.8,cex.main=1.8,main = "Histogram Data of Rain Indecator")
hist(data$itemp,xlab="Index",cex.lab=1.8, cex.axis=1.8,cex=1.8,cex.main=1.8,main = "Histogram Data of Temperatur Indecator")
hist(data$iwb,xlab="Index",cex.lab=1.8, cex.axis=1.8,cex=1.8,cex.main=1.8,main = "Histogram Data of Wet Bulb Air Temperature Indecator")
hist(data$iwdsp,xlab="Index",xlim=c(0,7),cex.lab=1.8, cex.axis=1.8,cex.main=1.8,cex=1.8,main = "Histogram Data of Mean Hourly Wind Speed Indecator")
hist(data$iwddir,xlab="Index",cex.lab=1.8, cex.axis=1.8,cex=1.8,cex.main=1.8,main = "Histogram Data of Predominant Hourly Wind Direction Indecator")
hist(data$ww,xlab="Index",cex.lab=1.8, cex.axis=1.8,cex=1.8,cex.main=1.8,main = "Histogram Data of Synop Code Present Weather")
hist(data$w,xlab="Index",cex.lab=1.8, cex.axis=1.8,cex=1.8,cex.main=1.8,main = "Histogram Data of Synop Code Past Weather")
hist(data$sun,cex.lab=1.8, cex.axis=1.8,cex=1.8,cex.main=1.8,xlab="Hour",main = "Histogram Data of Sunshine Duration")

data$Colour[(format(as.Date(data$date), "%m"))=="01"]="#a83232"
data$Colour[(format(as.Date(data$date), "%m"))=="02"]="#a88332"
data$Colour[(format(as.Date(data$date), "%m"))=="03"]="#94a832"
data$Colour[(format(as.Date(data$date), "%m"))=="04"]="#53a832"
data$Colour[(format(as.Date(data$date), "%m"))=="05"]="#32a892"
data$Colour[(format(as.Date(data$date), "%m"))=="06"]="#325fa8"
data$Colour[(format(as.Date(data$date), "%m"))=="07"]="#5132a8"
data$Colour[(format(as.Date(data$date), "%m"))=="08"]="#9e32a8"
data$Colour[(format(as.Date(data$date), "%m"))=="09"]="#ffabab"
data$Colour[(format(as.Date(data$date), "%m"))=="10"]="#abf2ff"
data$Colour[(format(as.Date(data$date), "%m"))=="11"]="#ffabeb"
data$Colour[(format(as.Date(data$date), "%m"))=="12"]="#000000"

#set.seed(1)
pairs(~hmPow+hmTemp+temp+
        wetb+dewpt+vappr+rhum+msl+wdsp+wddir+vis+clht+clamt+day+hour
      ,data = data, panel = panel.smooth, col=data$Colour, pch = "*")
#par(xpd = TRUE)
#legend(2.8,-1,title="Month", fill = data$Colour, legend = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

n <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
boxplot(data$hmTemp~data$month,ylab="Home Temperature(°C)",xlab="Month",cex.lab=1.5, cex.axis=1.5,cex=1.5,names=n)
boxplot(data$hmPow~data$month,ylab="Home Power(kWh)",xlab="Month",cex.lab=1.5, cex.axis=1.5,cex=1.5,names=n)
boxplot(data$rain~data$month,ylab="Precipitation Amount(mm)",xlab="Month",cex.lab=1.5, cex.axis=1.5,cex=1.5,names=n)
boxplot(data$temp~data$month,ylab="Air Temperature(°C)",xlab="Month",cex.lab=1.5, cex.axis=1.5,cex=1.5,names=n)
boxplot(data$wetb~data$month,ylab="Wet Bulb Air Temperature°C",xlab="Month",cex.lab=1.5, cex.axis=1.5,cex=1.5,names=n)
boxplot(data$dewpt~data$month,ylab="Dew Point Air Temperature(°C)",xlab="Month",cex.lab=1.5, cex.axis=1.5,cex=1.5,names=n)
boxplot(data$vappr~data$month,ylab="Vapour Pressurehpa(hpa)",xlab="Month",cex.lab=1.5, cex.axis=1.5,cex=1.5,names=n)
boxplot(data$rhum~data$month,ylab="Relative Humidity(%)",xlab="Month",cex.lab=1.5, cex.axis=1.5,cex=1.5,names=n)
boxplot(data$msl~data$month,ylab="Mean Sea Level Pressure(hPa)",xlab="Month",cex.lab=1.5, cex.axis=1.5,cex=1.5,names=n)
boxplot(data$wdsp~data$month,ylab="Mean Hourly Wind Speed(kt)",xlab="Month",cex.lab=1.5, cex.axis=1.5,cex=1.5,names=n)
boxplot(data$wddir~data$month,ylab="Predominant Hourly wind Direction(kt)",xlab="Month",cex.lab=1.5, cex.axis=1.5,cex=1.5,names=n)
boxplot(data$vis~data$month,ylab="Visibility(m)",xlab="Month",cex.lab=1.5, cex.axis=1.5,cex=1.5,names=n)
boxplot(data$clht~data$month,ylab="Cloud Ceiling Height(100s feet)",xlab="Month",cex.lab=1.5, cex.axis=1.5,cex=1.5,names=n)
boxplot(data$clamt~data$month,ylab="Cloud Amount(okta)",xlab="Month",cex.lab=1.5, cex.axis=1.5,cex=1.5,names=n)


#2#########################################################################2
data_sd <- data[,c("rain","temp","wetb","dewpt","vappr","rhum","msl","wdsp","wddir","vis","clht")]
data_sd<-scale(data[,c("rain","temp","wetb","dewpt","vappr","rhum","msl","wdsp","wddir","vis","clht")])
data_sd<- na.omit(data_sd)

round(apply(as.matrix(data_sd,8760,27),2,mean),2)
apply(as.matrix(data_sd,48,4),2,sd)

wss <- (nrow(data_sd)-1)*sum(apply(data_sd,2,var))
for(i in 2:15){wss[i]<- sum(kmeans(data_sd,centers=i,nstart = 5,iter.max = 30)$withinss)}  

plot(1:15, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

km <- kmeans(data_sd, 2, nstart = 5)
pairs(data[,c("rain","temp","wetb","dewpt","vappr","rhum","msl","wdsp","wddir","vis","clht")], col=km$cluster,pch=17)

print(km)

#3################################################################3

pca<-prcomp(data_sd)
plot(pca,type="l")
summary(pca)
print(pca)

#4###################################################################4

#install.packages("corrplot")
#install.packages("corrgram")
library(corrplot)
library(corrgram)

pairs(~hmPow+hmTemp++temp+
        wetb+dewpt+vappr+rhum+msl+wdsp+wddir+month+day+hour
      ,data = data, panel = panel.smooth, col=data$Colour, pch = "*")

data_cor <- data[,c("hmPow","hmTemp","temp","wetb","dewpt","vappr","rhum","msl","vis","clht","clamt","month","day","hour")]
data_cor<- na.omit(data_cor)
corrplot(corr = corrgram(data_cor), method = 'color', addCoef.col="grey") 

#5####################################################################5
fit<-lm(hmPow~hmTemp,data=data)
summary(fit)
##(Home Power)hmPower = 499.034 - 11.649 * hmTemp(Home Temperature)
##In the Pr (> | t |) column, you can see that the P values 
#are all less than 0.05, so the regression coefficients are significant (! = 0). 
##When hmTemp increased by one °C, the hmPow decreased by 11.649 kWh.
##Among the variation in the number of hmPow,
#1.513% can be determined by the hmPow and hmTemp
##The maximum value of the residual is 2621.6 and the minimum value is -364.5. 
##The residual standard error is 331.1, 
#which means that when hmTemp is used to estimate the hmPow, the average estimated error is 331.1.


#6#######################################################################6

summary(data$hmPow)
#Because the Mean is much larger than the Median
#the histogram is right-skewed
hist(data$hmPow)
#In my data, the electricity consumption of most houses is 0-600 in 2018

data_cor2 <- data[,c("hmPow","hmTemp","rain","temp","wetb","dewpt","vappr","rhum","msl","vis","clht","clamt","month")]
data_cor2<- na.omit(data_cor2)
corrplot(corr = corrgram(data_cor2), method = 'color', addCoef.col="grey") 

#full fit
#H0:B(hmTemp) = B(temp) = B(wetb) = B(dewpt) = B(vappr) = B(msl) = B(vis) = B(clht) = B(wetb) = B(month) = B(Winter) = 0
#H1:At least 1 coefficient != 0
#p-value for temp,dewpt,msl,clht,clamt,month is greater than 0.05 fail to reject H0
fit1 <- lm(hmPow~.,data=data_cor2)
summary(fit1)

data$Winter[(format(as.Date(data$date), "%m"))=="12"]<-1
data$Winter[(format(as.Date(data$date), "%m"))=="01"]<-1
data$Winter[(format(as.Date(data$date), "%m"))=="02"]<-1
data$Winter[(format(as.Date(data$date), "%m"))=="03"]<-0
data$Winter[(format(as.Date(data$date), "%m"))=="04"]<-0
data$Winter[(format(as.Date(data$date), "%m"))=="05"]<-0
data$Winter[(format(as.Date(data$date), "%m"))=="06"]<-0
data$Winter[(format(as.Date(data$date), "%m"))=="07"]<-0
data$Winter[(format(as.Date(data$date), "%m"))=="08"]<-0
data$Winter[(format(as.Date(data$date), "%m"))=="09"]<-0
data$Winter[(format(as.Date(data$date), "%m"))=="10"]<-0
data$Winter[(format(as.Date(data$date), "%m"))=="11"]<-0

data$Winter <- as.numeric(data$Winter)

data_cor2 <- data[,c("hmPow","hmTemp","rain","temp","vappr","vis","Winter")]
data_cor2<- na.omit(data_cor2)
corrplot(corr = corrgram(data_cor2), method = 'color', addCoef.col="grey") 

#H0:B(hmTemp) = B(wetb) = B(vappr) = B(vis) = B(Winter) = 0
#H1:At least 1 coefficient != 0
#p-value for temp,dewpt,msl,clht,clamt,month is greater than 0.05 fail to reject H0
fit2 <- lm(hmPow~hmTemp+rain+temp+vappr+vis+Winter,data=data_cor2)
summary(fit2)

BIC(fit1,fit2)

#fit1:126582.1,fit2:126525.7
#fit2 is better

AIC(fit1,fit2)
#fit1:126497.1,fit2:126476.1
#fit2 is better

#The equation of the line is :
#HmPow = 587.014931 -8.7721(hmTemp)+15.475970(wetb)-23.248305(vappr)-0.001308(vis)+39.621989(winter)

#adjusted R2 fit1:0.01895    , fit2: 0.02074 
#Residual S.E   fit1: 330.5   , fit2:  330.2
confint(fit2)

anova(fit1,fit2)

#This model explains 2.13%% of the variation in home power
#Month  does not seem to be as important here as the home temperature is
#helping to explain the usage of power in home.


