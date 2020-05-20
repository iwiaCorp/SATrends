library(readr)
library(twitteR)
library(dplyr)
library(tidyr)
#library(qdap)
library(sentimentr)
library(tm)
library(tidytext)

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
  
  tweetsUnion <- tweetsUnion %>% 
    mutate(Genero =  apply(tweetsUnion, 1, FUN = listToCharacterGenero)) 
  
  tweetsUnion$Genero <- as.character(tweetsUnion$Genero)
  tweetsUnion$Genero[tweetsUnion$Genero == "female"] <- "Mujer"
  tweetsUnion$Genero[tweetsUnion$Genero == "male"] <- "Hombre"
  
  # tweetsUnion<- tweetsUnion %>%
  #   mutate(Genero = ifelse(Genero == "female", "Mujer", Genero)) 
  # 
  # tweetsUnion<- tweetsUnion %>%
  #   mutate(Genero = ifelse(Genero == "male", "Hombre", Genero))
  
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
  
 # fixedGender.df <- data.frame(x, stringsAsFactors = FALSE)
 # fixedGender <- fixedGender.df$Genero
  fixedGender <- x["Genero"]
 # if(length(fixedGender.df$Genero) > 1){
  if(length( x["Genero"]) > 1){
    fixedGender <- "ND"
  }
  
  return(fixedGender)
  
}

#convertir lista a caracter, obteniedo el primer valor
listToCharacterGenero <- function(listGenero){
  as.character(listGenero$Genero[[1]][1]) #de la lista selecciona el primero
  
}

#crear corpus
createCorpus <- function(text){
 
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
  
  #sentencia para guardar datos limpios
  #tweetCleanText.df <- opinionText %>% select(text)
  #write.csv(tweetCleanText.df, "SupermaxiLimpio.csv")
  
  #nuevas carateristicas limpieza
  
  opinionText$text <- gsub("[[:cntrl:]]", " ", opinionText$text)
  opinionText$text <- tolower(opinionText$text)
  opinionText$text <- removeWords(opinionText$text, words = stopwords("spanish")[c(-16, -263, -220, -11, -5, -217)])
  opinionText$text <- removeWords( opinionText$text, words = c("usted", "pues", "tal", "tan", "así", "dijo", "cómo", "sino", "entonces", "aunque", "don", "doña"))
  opinionText$text <- removePunctuation(opinionText$text)
  opinionText$text <- removeNumbers(opinionText$text)
  opinionText$text <- stripWhitespace(opinionText$text)
  
  ###########sentencia para guardar datos limpios########
  #tweetCleanText.df <- opinionText %>% select(text)
  #write.csv(tweetCleanText.df, "LeninLimpio_12032020.csv")
  
  
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
  
  
  #establecer formato aceptado para nuevo diccionario
  keyCustomEC <- data.frame(words = tolower(lexico_ec$word),
                            polarity = lexico_ec$value,
                            stringsAsFactors = FALSE)

  myKeyCustomEC <- as_key(keyCustomEC)
  
  #establece formato aceptado para nuevo diccionario cambio sentimiento
  valenceShiftEC <- data.frame(words = tolower(lexicoCambioSentimiento$x),
                               polarity = lexicoCambioSentimiento$y,
                               stringsAsFactors = FALSE)
  #myValenceShiftEC <- as_key(valenceShiftEC)
  myValenceShiftEC <- as_key(valenceShiftEC, comparison = myKeyCustomEC, sentiment = FALSE)
  

  if(is_key(myKeyCustomEC) == TRUE & is_key(myValenceShiftEC) == TRUE){
 
    result <- sentiment(tweets, polarity_dt = myKeyCustomEC, valence_shifters_dt = myValenceShiftEC,
                        n.before = 5, n.after = 3)

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
    
    sentiment.score.df <- sentiment.score.df %>%
      mutate(colorSentimiento =  ifelse(sentiment.score.df$sentiment == "Positivo", "blue", "red"))
    
    
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

#mapa de calor sentimiento por ciudad

headMapPlot <- function(df.tm){
  
  dataLocal <- data.frame(df.tm) %>% select(Ciudad, createdDate, 13)
   
  
  names(dataLocal) = c("Ciudad" , "createdDate",
                                     "Sentimiento")
  
  ggplot(data = dataLocal, aes(x = createdDate, y = Ciudad)) +
    geom_tile(aes(fill=Sentimiento),colour="grey5") +
    scale_y_discrete(breaks = df.tm$Ciudad) +
    
    scale_fill_gradient2(low = "#FFDDDD", midpoint=0,space="Lab", mid="#FFE500", high = "#CCEEFF") +
    ggtitle("Análisis de Sentimiento por ciudad") +
    theme(axis.text.y = element_text(colour="grey5"),
          axis.text.x = element_text(colour="grey5"),
          axis.ticks.y = element_line(colour="white"),
          axis.ticks.x = element_line(colour="white"),
          axis.title.x = element_text(colour="white"),
         
          plot.background=element_rect(fill="white"),
          panel.background=element_rect(fill="white"),
          panel.border=element_rect(fill=NA,colour="white"),
          panel.grid.minor.x = element_line(colour="white"),
          panel.grid.major.x = element_line(colour="white"),
          panel.grid.minor.y = element_line(colour="white"),
          panel.grid.major.y = element_line(colour="white"),
          legend.background = element_rect(fill = "white"),
          legend.text = element_text(colour="black"),
          legend.title = element_text(colour="black"))
}

#establecer color para sentimiento
colorPolarity <- function(dato){
  
  value <- as.integer(dato["value"])
  color <- "grey"
  if(value > 0)
  {
    color <- "blue"
  }
  else if(value == 0)
  {
    color <- "grey"
  }
  else
  {
    color <- "red"
  }
  
  return(color)
}

#grafico lollipop 

lollipopPlot <- function(textDataTweets){
  library(tidytext)
  
  cleanText <- textDataTweets%>%
              cleanDataTweets()
  
  df.tm2 <- data.frame(tweets = cleanText, stringsAsFactors = F )
  df.tm2$tweets <- as.character(df.tm2$tweets) #importante el texto que no sea factor
  
  clean_dt <- df.tm2 %>%
    unnest_tokens(word, tweets, 
                  to_lower = F) %>%
    filter(word %in% lexico_ec$word ) %>%
    count(word, sort = T)
  
  clean_dt<- clean_dt %>%
    inner_join(lexico_ec, by = c("word" = "word"))
  
  clean_dt$word <- factor(clean_dt$word,
                          levels = rev(unique(clean_dt$word)))
  
  clean_dt <- clean_dt %>%
    mutate(color = apply(clean_dt, 1, FUN = colorPolarity))
  
  ggplot(data = clean_dt[1:10,], aes(x = n, y = word)) +
    geom_segment(linetype = 'dashed',
                 size = .1,
                 aes(yend = word, 
                     x = min(n) - 50, 
                     xend = n,
                     linetype = "mean")) +
    #geom_point(size = 15, color = '#e66101') +
    geom_point(size = 15, color = clean_dt$color[1:10]) +
    geom_text(aes(label = n), size = 6) +
    coord_cartesian(xlim = c(1,15)) +
    labs(title = "Las 10 palabras más utilizadas",
         x = "Frecuencia", y = "Palabras") 
  
}

#grafico palabras de sentimiento
wordCountSentiment <- function(textData){
  
  cleanText <- textData%>%
    cleanDataTweets()
  
  df.tm2 <- data.frame(tweets = cleanText, stringsAsFactors = F )
  df.tm2$tweets <- as.character(df.tm2$tweets) #importante el texto que no sea factor
  
  clean_dt <- df.tm2 %>%
    unnest_tokens(word, tweets, 
                  to_lower = F) %>%
    filter(word %in% lexico_ec$word ) %>%
    count(word, sort = T)
  
  clean_dt<- clean_dt %>%
    inner_join(lexico_ec, by = c("word" = "word"))
  
  clean_dt$word <- factor(clean_dt$word,
                          levels = rev(unique(clean_dt$word)))
  clean_dt <- clean_dt %>%
    mutate(Polaridad = ifelse(value > 0, "Positivo", "Negativo"))
  
  # ggplot(clean_dt, aes(x = word, y = n, fill = Polaridad)) +
  #   geom_col(show.legend = F) +
  #   facet_wrap(~Polaridad, scale = "free")+
  #   coord_flip()+
  #   labs(title = "Conteo palabras de sentimiento", x = "Palabras", y = "Número")
  # tryCatch( 
  #   expr = {
  #     
  #   }
  #   )
  clean_dt %>%
    filter(n>10 ) %>%
    arrange(Polaridad) %>%
    ggplot( aes(x = reorder(word, n), y = factor(n), fill = Polaridad)) +
    geom_col( position = "identity" , colour = "black", size = 0.25, width = 0.5) +
    scale_fill_manual(values = c("#FFDDDD", "#CCEEFF"), guide = FALSE) +
    # ylim(c(3,140)) +
    facet_wrap(~Polaridad, scales = "free")+
    coord_flip()+
    labs(title = "Conteo palabras de sentimiento", x = "Palabras", y = "Número")
  
  
  # genderResult <- tryCatch(
  #   expr = {
  #     unique(findGivenNames(names, progress = F))
  #   }, 
  #   error = function(c)
  #   {
  #     message("Error: Nombre no especificado.")
  #     print(c)
  #   }
  #   
  # )
  
}

#grafico porcentaje de palabras de sentimiento
wordPercentSentiment <- function(textData){
  
  cleanText <- textData%>%
    cleanDataTweets()
  
  df.tm2 <- data.frame(tweets = cleanText, stringsAsFactors = F )
  df.tm2$tweets <- as.character(df.tm2$tweets) #importante el texto que no sea factor
  
  clean_dt <- df.tm2 %>%
    unnest_tokens(word, tweets, 
                  to_lower = F) %>%
    filter(word %in% lexico_ec$word ) %>%
    count(word, sort = T)
  
  clean_dt<- clean_dt %>%
    inner_join(lexico_ec, by = c("word" = "word"))
  
  clean_dt$word <- factor(clean_dt$word,
                          levels = rev(unique(clean_dt$word)))
  #clean_dt <- clean_dt %>%
   # mutate(Polaridad = ifelse(value > 0, "Positivo", "Negativo"))
  
  # ggplot(clean_dt, aes(x = word, y = n, fill = Polaridad)) +
  #   geom_col(show.legend = F) +
  #   facet_wrap(~Polaridad, scale = "free")+
  #   coord_flip()+
  #   labs(title = "Conteo palabras de sentimiento", x = "Palabras", y = "Número")
  
  clean_dt %>%
    mutate(perc = (n/sum(n))*100) %>%
    .[1:10, ] %>%
    ggplot(aes(word, perc)) +
    geom_bar(stat = "identity", color = "black", fill = "#87CEFA", size = 0.25, width = 0.5) +
    geom_text(aes(hjust = 1.3, label = round(perc, 2))) + 
    coord_flip() +
    labs(title = "Proporción de uso de cada palabra. \nDiez palabras más frecuentes", x = "Palabras", y = "Porcentaje de uso")
  
}



