library(mongolite)
is.POSIXct <- function(x) inherits(x, "POSIXct")

m <- mongo(collection = "HomeTemp", db = "bigdata_final_zeyan_liow", url = "mongodb://localhost")
homeTemp <- m$find(limit = 1000000, skip = 0, fields = '{ "_id" : true, "temp" : true }')

m1 <- mongo(collection = "HomePower", db = "bigdata_final_zeyan_liow", url = "mongodb://localhost")
homePower <- m1$find(limit = 1000000, skip = 0, fields = '{ "_id" : true, "power" : true }')

m2 <- mongo(collection = "Dublin", db = "bigdata_final_zeyan_liow", url = "mongodb://localhost")
dublin <- m2$find(limit = 1000000, skip = 0, fields = '{ "_id" : false, "date" : true, "irain" : true, "rain" : true, "itemp" : true, "temp" : true, "iwb" : true, "wetb" : true, "dewpt" : true, "vappr" : true, "rhum" : true, "msl" : true, "iwdsp" : true, "wdsp" : true, "iwddir" : true, "wddir" : true, "ww" : true, "w" : true, "sun" : true, "vis" : true, "clht" : true, "clamt" : true }')

dublin <- lapply(dublin, function(x) if(is.POSIXct(x)) as.character(x) else x)

names(homeTemp) <- c("date", "homeTemp")
names(homePower) <- c("date", "homePower")

print(homeTemp)
print(homePower)
print(dublin)

merge1 <- merge(homeTemp,homePower)
data <- merge(merge1,dublin,all=TRUE)
print(data)
print(merge1)

do.call(dublin, unlist(foolist, recursive=FALSE))