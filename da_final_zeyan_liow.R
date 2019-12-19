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

boxplot(data$hmTemp,main="Home Temperature")
boxplot(data$hmPow,main="Home Power")
boxplot(data$rain,main="Precipitation Amount")
boxplot(data$temp,main="Air Temperature")
boxplot(data$wetb,main="Wet Bulb Air Temperature")
boxplot(data$dewpt,main="Dew Point Air Temperature")
boxplot(data$vappr,main="Vapour Pressure	")
boxplot(data$rhum,main="Relative Humidity")
boxplot(data$msl,main="Mean Sea Level Pressure	")
boxplot(data$wdsp,main="Mean Hourly Wind Speed	")
boxplot(data$wddir,main="Predominant Hourly wind Direction")
boxplot(data$vis,main="Visibility")
boxplot(data$clht,main="Cloud Ceiling Height")
boxplot(data$clamt,main="Cloud Amount")

hist(data$i)
hist(data$itemp)
hist(data$iwb)
hist(data$iwdsp,xlim=c(0,7))
hist(data$iwddir)
hist(data$ww)
hist(data$w)
hist(data$sun)

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
        wetb+dewpt+vappr+rhum+msl+wdsp+wddir+month+day+hour
      ,data = data, panel = panel.smooth, col=data$Colour, pch = "*")
#par(xpd = TRUE)
#legend(2.8,-1,title="Month", fill = data$Colour, legend = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

boxplot(data$hmTemp~data$month,names=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
boxplot(data$hmPow~data$month,names=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
boxplot(data$~data$month,names=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
boxplot(data$temp~data$month,names=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
boxplot(data$wetb~data$month,names=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
boxplot(data$dewpt~data$month,names=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
boxplot(data$vappr~data$month,names=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
boxplot(data$rhum~data$month,names=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
boxplot(data$msl~data$month,names=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
boxplot(data$wdsp~data$month,names=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
boxplot(data$wddir~data$month,names=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
boxplot(data$vis~data$month,names=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
boxplot(data$clht~data$month,names=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
boxplot(data$clamt~data$month,names=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))



#2#########################################################################2
data_sd <- data
data_sd[,c("hmTemp","hmPow","temp","wetb","dewpt","vappr","rhum","msl","wdsp","wddir")]<-scale(data[,c("hmTemp","hmPow","","temp","wetb","dewpt","vappr","rhum","msl","wdsp","wddir")])
data_sd[,c("hmTemp","hmPow","temp","wetb","dewpt","vappr","rhum","msl","wdsp","wddir")]<- na.omit(data_sd[,c("hmTemp","hmPow","","temp","wetb","dewpt","vappr","rhum","msl","wdsp","wddir")])

data_sd <- round(data_sd,2)
sapply(data_sd, sd)

wss <- (nrow(data_sd)-1)*sum(apply(data_sd,2,var))
for(i in 2:15){wss[i]<- sum(kmeans(data_sd,centers=i,nstart = 3)$withinss)}  

plot(1:15, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

km <- kmeans(data_sd, 2, nstart = 3)
pairs(data[,c("hmTemp","hmPow","temp","wetb","dewpt","vappr","rhum","msl","wdsp","wddir","day","hour")], col=km$cluster,pch=17)

print(km)

#3################################################################3

my_data <-scale(data[,c("hmTemp","hmPow","temp","wetb","dewpt","vappr","rhum","msl","wdsp","wddir")])
my_data<- na.omit(my_data)
pca<-prcomp(my_data)
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

data_cor <- data[,c("hmPow","hmTemp","temp","wetb","dewpt","vappr","msl","vis","clht","clamt","month","day","hour")]
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
#In our data, the electricity consumption of most houses is 0-600 in 2018

data_cor2 <- data[,c("hmPow","hmTemp","temp","wetb","dewpt","vappr","msl","vis","clht","clamt","month")]
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

data_cor2 <- data[,c("hmPow","hmTemp","temp","wetb","dewpt","vappr","msl","vis","clht","clamt","Winter")]
data_cor2<- na.omit(data_cor2)
corrplot(corr = corrgram(data_cor2), method = 'color', addCoef.col="grey") 

#H0:B(hmTemp) = B(wetb) = B(vappr) = B(vis) = B(Winter) = 0
#H1:At least 1 coefficient != 0
#p-value for temp,dewpt,msl,clht,clamt,month is greater than 0.05 fail to reject H0
fit2 <- lm(hmPow~hmTemp+wetb+vappr+vis+Winter,data=data_cor2)
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


