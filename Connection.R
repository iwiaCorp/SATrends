library(twitteR)

#conexion a la API twitter
#verificar conexion
consumer_key <- "JuoX9f4eAPYqhhZmAf34RmoHM"
consumer_secret <- "YDY4aRUoNk6M4w4wNcipuis1nK0tYwMK1dT1Wo2RVTs7KT2q5v"
access_tokenTwitter <- "22375183-EkgshwuaPQ38kZw3945LBOqKtmPIE56OldDYz9ibu"
access_secret <- "RfJx4wizQ77Yq0XS0AvuZuacnxwDkET4wnyk1ng6Zysj1"

createTwitterConection <- function(){
  
  
  
  setup_twitter_oauth(consumer_key, consumer_secret, access_tokenTwitter, access_secret)
  
}

#proceso #1: busqueda de informacion
searchTweets <- function(searchText, sinceDate, untilDate, geoCode, ratio){
  
  valueOp <- ifelse(is.na(sinceDate), 0, 1)
  sinceDate <- switch(valueOp+1, 
                      NULL,
                      as.character(sinceDate))
  valueOp <- ifelse(is.na(untilDate), 0, 1)
  
  untilDate <- switch(valueOp+1, 
                      NULL,
                      as.character(untilDate))
  
  geoCodeValue <-paste(geoCode$lat[1],",",geoCode$lng[1], ",",ratio,"km",sep = '')
  
  #tweets <- searchTwitter(searchText, lang='es',since= sinceDate, until= untilDate,geocode = '-0.22985,-78.5249481,30km') #Quito, tiene datos OK
  tweets <- searchTwitter(searchText, lang='es',since= sinceDate, until= untilDate,geocode = geoCodeValue) #Quito, tiene datos OK
  
}