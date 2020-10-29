library(twitteR)

#conexion a la API twitter
#verificar conexion
consumer_key <- "xxxxxxx"
consumer_secret <- "xxxxxxxx"
access_tokenTwitter <- "xxxxxxxxxx"
access_secret <- "xxxxxxxxxxx"

#conexion API twitter y gestion de error conexion
createTwitterConection <- function(){
  
  
  resultConnection <- tryCatch(
    expr = {
      setup_twitter_oauth(consumer_key, consumer_secret, access_tokenTwitter, access_secret)
    },
    error = function(c)
    {
      message("Error: No se pudo establecer autorización con API Twitter")
     
    }
    
  )
  
  
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
