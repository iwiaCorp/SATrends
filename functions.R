library(readr)
library(twitteR)
library(dplyr)
library(tidyr)
#library(qdap)
library(sentimentr)



#funciones basicas 

#proceso #2: estructurar datos, se convierte a DF los tweets obtenidos
#carga datos local
createDataLocal <- function(tweets){
  
  
  #verifica valores na y reemplaza   
  tweets[is.na(tweets)] <- "ND"
  
  tweets<- tweets %>%
    mutate(element_id =  row_number())
  
  cities_ec$nombre <- tolower(cities_ec$nombre)
  
  tweets <- tweets %>%
    left_join( cities_ec, by = c("Ciudad" = "nombre"))
 
  
  return(tweets)
  
}

#carga datos en linea
createData <- function(tweets){
  
  #Convirtiendo los tweets en un data frame
  # if(is.list(tweets))
  #    {
  #      tweetsDF.df <- twListToDF(tweets)
  # }
  # else
  # {
  #  tweetsDF.df <- tweets
  #}
  tweetsDF.df <- tweets
  
  #tweetsDF.df  %>%
  #  mutate(rowNumber = row_number())
  
  text <- tweetsDF.df$text
  createdDate <- tweetsDF.df$created
  userName <- tweetsDF.df$screenName
  #recordNum <- tweetsDF.df$rowNumber
  datosTweets <- data.frame(text, createdDate, userName) 
  
  tweetsFecha <- datosTweets %>%
    mutate(fechaTweet = as.Date(datosTweets$createdDate)) %>%
    arrange(fechaTweet)
  
  #usersTwitter <- lookupUsers(datosTweets$userName) #retorna screen name, nombre, ubicacion
  nombre <- tweetsDF.df$screenName
  usersTwitter <- lookupUsers(nombre) #retorna screen name, nombre, ubicacion
 
  userTwitterDF <- twListToDF(usersTwitter) %>%
    select(c(screenName, name,location)) %>%
    separate(name, sep = " ", into = c("Nombre", "Apellido")) %>%
    separate(location, sep = ",", into = c("Ciudad", "Pais"))
  
 
  #aplicar funcion para genero de acuerdo al nombre del usuario
 
 
  userGenero <- userTwitterDF %>%
    mutate(Genero =  sapply(userTwitterDF$Nombre, getGender))
             
  
  tweetsUnion <- tweetsFecha %>%
    left_join( userGenero, by = c("userName" = "screenName"))

 
#  tweetsUnion %>%
 #   arrange(recordNum) 
  
  #colnames(tweetsUnion) <- c("Texto", "Fecha creación", "Nombre usuario", "Fecha tweet", "Nombre", "Apellido", "Ciudad", "País", "Género")
  
  #TODO: pendiente cambiar valor male to hombre
  #library(plyr)
  
 # tweetsUnion$Genero <- revalue(tweetsUnion$Genero, c("male" = "hombre"))
  #tweetsUnion$Genero <- as.character(tweetsUnion$Genero)
  #tweetsUnion[tweetsUnion$Genero == "female"] <- "mujer"
  
  # tweetsUnion<- tweetsUnion %>%
  #   mutate(Genero = ifelse(nchar(Genero) == 10, "male", Genero)) 
  
  tweetsUnion<- tweetsUnion %>%
    mutate(Genero = ifelse(Genero == "NULL", "ND", Genero)) 
 
  tweetsUnion<- tweetsUnion %>%
    mutate(Genero = ifelse(Genero == "character(0)", "ND", Genero))
  
  tweetsUnion<- tweetsUnion %>%
    mutate(Genero = apply(tweetsUnion, 1, FUN = fixGender))
  
  tweetsUnion<- tweetsUnion %>%
    mutate(Genero = ifelse(Genero == "female", "Mujer", Genero)) 
  
  tweetsUnion<- tweetsUnion %>%
    mutate(Genero = ifelse(Genero == "male", "Hombre", Genero))
  
  # tweetsUnion<- tweetsUnion %>%
  #   mutate(Ciudad = ifelse(Ciudad == "" || nchar(Ciudad) == 0, "ND", tweetsUnion$Ciudad)) 
  # 
  # tweetsUnion<- tweetsUnion %>%
  #   mutate(Ciudad = sapply(tweetsUnion$Ciudad, fillCitiesBlank)) 
  # 
  tweetsUnion<- tweetsUnion %>%
    mutate(Ciudad = ifelse(nchar(Ciudad) == 0, "ND", tweetsUnion$Ciudad)) 
  
 #verifica valores na y reemplaza   
  tweetsUnion[is.na(tweetsUnion)] <- "ND"
  
  tweetsUnion$Apellido[tweetsUnion$Apellido == 0] <- "ND"
  
  tweetsUnion<- tweetsUnion %>%
    mutate(Ciudad = ifelse(Ciudad == 0, "ND", tweetsUnion$Ciudad)) 
  
  tweetsUnion<- tweetsUnion %>%
    mutate(Pais = ifelse(Pais == 0, "ND", tweetsUnion$Pais)) 
  
  
  tweetsUnion<- tweetsUnion %>%
    mutate(Ciudad =  sapply(tweetsUnion$Ciudad, createAndSearchCity)) 
  
  # #polaridad
  # tweetsUnion<- tweetsUnion %>%
  #   mutate(Polaridad =  0) 
  
  tweetsUnion<- tweetsUnion %>%
    mutate(element_id =  row_number())
  
  return(tweetsUnion)
  
}

#corregir el genero 
fixGender <- function(x){
  
  fixedGender.df <- data.frame(x, stringsAsFactors = FALSE)
  fixedGender <- fixedGender.df$Genero
  
  if(length(fixedGender.df$Genero) > 1){
    fixedGender <- "ND"
  }
  
  return(fixedGender)
  
}


#crear corpus
createCorpus <- function(text){
  library(tm)
  #corpus <- iconv(text, to = 'UTF-8-mac') #presnta error cuando se despliega en la nube
  corpus <- iconv(text, to = 'UTF-8') #corrige despliege en la nube, confirmar origen de esta configuracion
  corpus <- Corpus(VectorSource(corpus))
}

#proceso #3: limpieza datos

cleanDataTweets <- function(tweetText){
  opinionText <- tweetText
  opinionText$text <- gsub("(RT|via)|((?:\\b\\w*@\\w+)+)", "", opinionText$text)
  opinionText$text <- gsub("\\n", "", opinionText$text )
  opinionText$text <- gsub("@\\w+", "", opinionText$text )
  opinionText$text <- gsub("[[:punct:]]", "", opinionText$text)
  opinionText$text <- gsub("[[:digit:]]", "", opinionText$text)
  opinionText$text <- gsub("http\\w+", "", opinionText$text)
  opinionText$text <- gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", opinionText$text)
  #opinionText$text.x <- iconv(opinionText$text.x, to="ASCII//TRANSLIT")
  opinionText$text  <- iconv(opinionText$text, "UTF-8", "latin1", sub="") #mantiene las ñ y borrar emoticons
  #opinionText$text.x  <- iconv(opinionText$text.x , "latin1", "ASCII", sub="")  #elimina las ñ, tildes
  opinionText$text <- chartr('áéíóúñ','aeioun', opinionText$text)
  
  return(opinionText$text)
}

#version 2 para limpieza de texto de tweet
#devulte un vector
cleanDataTweetsV2 <- function(corpus, searchText){
  
  #convertir a minuscula
  corpus <- tm_map(corpus, tolower)
  #quitar la frase o texto de busqueda para evitar redundancia en el texto
  corpus <- tm_map(corpus, removeWords, searchText) #este debe ir antes de quitar las puntuaciones
  
  #quitar caracteres de puntuacion
  corpus <- tm_map(corpus, removePunctuation)
  #quitar numeros
  corpus <- tm_map(corpus, removeNumbers)
  
  #remover palabras sin mucho significado y relevancia para el análisis 
  cleanCorpus <- tm_map(corpus, removeWords, stopwords("es"))
  #remover carcteres de direccion web
  cleanCorpus <- tm_map(cleanCorpus, content_transformer(removeURL))
   #remover los doble espacion
  cleanCorpus <- tm_map(cleanCorpus, stripWhitespace)
  
  #funcion para remover emoticos y mantener las palabras español
  cleanCorpus <- tm_map(cleanCorpus, content_transformer(removeEmoticons))
  
}

#remover direcciones web
removeURL <- function(x) gsub("http[[:alnum:]]*", '', x)

#remover emoticones
removeEmoticons  <- function(x) iconv(x, "UTF-8", "latin1", sub="")

#estructurar los datos del tweet en una matriz, recibe un vector corpus limpio
structureDataTweet <- function(cleanCorpus){
  tdm <- TermDocumentMatrix(cleanCorpus)
  tdm <- as.matrix(tdm)
}

#debido a que la grafica de barra puede volverse muy lento al tratar de graficar cada palabra, vamos a crear subconjuntos 
#de palabras 
sumWordFrecuency <- function(tdm){
  w <- rowSums(tdm)
  
  #w <- subset(w, w>=20)
  
  subSetWord <- subset(w, w>=10)
  
  
  if(length(subSetWord) == 0){
    subSetWord <- subset(w, w>=2)
    
  }
 
  return (subSetWord)
}

#retorna tweets con el formato establecido y limpio
dataTweetsFormat <- function(tweets){
  
  cleanDataTweetsFormat <- tweets %>%
    mutate(TextDepurado = cleanDataTweets(tweetsUnion))
  
  return(cleanDataTweetsFormat)
}

#nube de palabras

calcWordcloud <- function(tdm){
  library(wordcloud)
  freqWord <- sort(rowSums(tdm), decreasing = TRUE)
  set.seed(375)
  wordcloud(words = names(freqWord),
            freq = freqWord,
            random.order = FALSE,
            max.words = 200,
            min.freq = 5,
            colors = brewer.pal(8, "Dark2"),
            scale = c(5, 0.4),
            rot.per = 0.5)
}

# cargar datos locales
loadLocalData <- function(fileName){
  
  if(file.exists(fileName)){
    dataLocal <- tryCatch(read_csv(fileName), error=function(e) 1)
  }
  
  return(dataLocal)

}

#obtener el genero a partir del nombre
getGender <- function(names){
  
  genderResult <- tryCatch(
    expr = {
      unique(findGivenNames(names, progress = F))
    }, 
    error = function(c)
    {
      message("Error: Nombre no especificado.")
      print(c)
    }
    
  )
  
  return(genderResult$gender)
  
}

calcSentiment <- function(tweetsData, searchedTweet){
  
  
  tweets <- cleanDataTweets(tweetsData)
  #tweets <- cleanDataTweetsV2(tweetsData$text, searchedTweet) funciona con tm
  
  
  
  keyCustomEC <- data.frame(words = tolower(lexico_ec$word),
                            polarity = lexico_ec$value,
                            stringsAsFactors = FALSE)

  myKeyCustomEC <- as_key(keyCustomEC)

  if(is_key(myKeyCustomEC) == TRUE){
 
    result <- sentiment(tweets, polarity_dt = myKeyCustomEC)

  }
  
 
    

  return(result)
}

#funcion para el calculo de la polaridad
calcPolarity <- function(sentimentResult){
  
    #calcular el valor promedio ya que necesitamos el calculo a nivel de tweet y no a nivel de sentencia
    sentiment.score <- sentimentResult %>%
      group_by(element_id) %>%
      summarise(sentiment = mean(sentiment))
    
    sentiment.score.df <- as.data.frame(sentiment.score)
    
   
    #colnames(sentiment.score.df)
    #names(sentiment.score.df)[1] <- "ID"
    #names(sentiment.score.df)[2] <- "polarity"

    # #descartar valores neutros
    #sentiment.score.df <- sentiment.score.df[sentiment.score.df$sentiment != 0, ]
    sentiment.score.df <- sentiment.score.df %>%
      #mutate(sentiment = ifelse(sentiment.score.df$polarity <0 , "Negativo", "Positivo"))
      mutate(sentiment =  apply(sentiment.score.df, 1, FUN = getPolarityText))
    
    sentiment.score.df$sentiment <- as.factor(sentiment.score.df$sentiment)

    #convertir tabla a data frame
    sentiment.score.table <- table(sentiment.score.df$sentiment)
    sentiment.score.df <- data.frame(sentiment.score.table)
    colnames(sentiment.score.df)
    names(sentiment.score.df)[1] <- "Sentimiento"
    names(sentiment.score.df)[2] <- "Polaridad"

  
  
  return(sentiment.score.df)
}

#obtener etiqueta de polaridad
getPolarityText <- function(value){
  if(value["sentiment"] >0) {
    "Positivo"
  } else if(value["sentiment"] < 0){
    "Negativo"
  }else{
    "Neutro"
  }
}

#busqueda datos twitter
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
  
  #Convirtiendo los tweets en un data frame
  tweetsDF.df <- twListToDF(tweets)%>%
    mutate(rowNumber = row_number())
  
}

#agrega nueva columna para polaridad
tweetsDataPolarity <- function(tweetsFormat, tweetsSentiment){
  
  tweetsFormat %>%
    mutate(CalculoPolaridad =  tweetsSentiment$sentiment,
           Polaridad = apply(tweetsSentiment, 1, FUN = getPolarityText))
    #mutate(Polaridad = apply(tweetsSentiment, 1, FUN = getPolarityText))
}

#crear ciudad
createAndSearchCity <- function(citySet){
  
  for(val in citySet){
    city_token <- syuzhet::get_tokens(val)
    
    citySelect <- searchCity(city_token)
    
  }
  
  return (citySelect)
}

#busca ciudades 
searchCity <- function(cities){
  
  city <- "ND"
  for (val in cities) { 
 
    value <- toupper(val) %in% toupper(cities_ec$nombre)
   
    if(value == TRUE){
      city <- val
      break
    }
  }
  
    
  return (city)
}

#establecer color basado en valor de una columna
palette_fn <- function(dataLocalTweets){
  req(!is.null(dataLocalTweets$data))
    colorFactor(palette = "Set3", domain = dataLocalTweets$polaridad)
    
  }




