#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(leaflet)
library(leaflet.extras)
library(ggplot2)
library(shiny)
library(genderizeR)

source("./Connection.R")
source("./LexiconCustomization_EC.R")
source("./functions.R")

createTwitterConection()

#localTweetsData <- loadLocalData("datosTweets_CNT_12072019To13072019.csv")
#localTweetsData$created <- as.Date(localTweetsData$created,"%Y/%m/%d")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  
  #mapas 
  output$EcuadorMap <- renderLeaflet({
    leaflet(options = leafletOptions(dragging = TRUE,
                                     minZoom = 7,
                                     maxZoom = 100)) %>%
      addTiles() %>%
      addSearchOSM() %>%
      #  addReverseSearchOSM() %>%
      addResetMapButton() %>%
      #setView(lat=10, lng = 0, zoom = 0) %>%
      setView(lng=-78.78442, lat=-0.94434, zoom = 7)
    
    
  })
  
  # tweeter_dataFiler <- reactive({
  #   req(input$fromToDate[1])
  #   filter(localTweetsData, created >= input$fromToDate[1] &  created <= input$fromToDate[2])
  # })
  
  dataFileLoaded <- reactive({
    req(input$fileLoaded)
    inFile <- readr::read_csv(input$fileLoaded$datapath)
    inFile$created <- as.Date(inFile$created,"%Y/%m/%d") 
    #inFile$created <- as.Date(inFile$created,"%m/%d/%Y") 
    
    return(inFile)
  })
  
  
  output$twetterDataLocal <- DT::renderDataTable(
    
    if(nrow(dataFileLoaded()) != 0 & !is.na(input$fromToDate[1])  & !is.na(input$fromToDate[2]))
    {
       # fileFilter <- filter(dataFileLoaded(), created >= input$fromToDate[1] &  created <= input$fromToDate[2])
        fileFilter <- dataFileLoaded()
       if(nrow(fileFilter) == 0)
         return(NULL)
        
        localTweets$data <- fileFilter %>%
        createData() 
        
        cityValue$data <- unique(localTweets$data$Ciudad)
     
        # cityValue$data <- unique(sapply(localTweets$data$Ciudad, createAndSearchCity))
        # se comenta porque presenta error al crear archivo
        # readr::write_csv(x = localTweets$data,path = "CNT_EC_offLine.csv")
        
        localTweets$data %>%
        DT::datatable( #colnames = c("X1","text", "created", "screenName", "longitude", "latitude"),  
         
                       options = list(pageLength = 10), 
                       rownames = FALSE)
    }
  )
  
  #valor reactivo para almacenar ciudades cargadas
  cityValue <- reactiveValues(
    data = NULL
  )
  
  #crear un observador para la carga de ciudades
  output$CitiesLoaded <- renderUI({
    cities <- cityValue$data
    if(!is.null(cities)){
        cities <- c("Todas", cities)
       
    }
    return(selectInput("Ciudades", "Ciudad", cities))

  })
  
  #evento reactivo
  localTweets <- reactiveValues(
   
    data = NULL
  )
  
  #datos con polaridad
  generalPolarity <- reactiveValues(

      data = NULL

  )
  
  #accion calculo sentimiento
  calc <- eventReactive(input$calcSentiment, {
    print("prueba DATOS ")
    # head(localTweets$data)
    # req(localTweets$data)
    # {
    #    
    #   
    #   generalPolarity$data <- calcSentiment(localTweets$data)
    #   
    #   generalPolarity$data 
    # }
  })
  
  resultadoSentimiento <- eventReactive(input$calcSentiment, {
    req(localTweets$data)
    {

      resultado <- calcSentiment(localTweets$data, "cnt_ec")


    }
  })
  
  polarityResult <- eventReactive(resultadoSentimiento() ,{
      
        #print(resultadoSentimiento() )
        polarity <- resultadoSentimiento() %>%
                      calcPolarity

  })
  
  # dataPro <- reactive({
  #   req(localTweets$data)
  #     filter(localTweets$data, created >= input$fromToDate[1] &  created <= input$fromToDate[2])
  #   
  # })
  
  
  #presenta grafico, se comenta para validar una grafica mas sencilla
  # output$scatterplot <- renderPlot({
  #   if(input$calcSentiment){
  #     ggplot(resultadoSentimiento(),
  #            aes(x = sentiment,
  #                y = element_id, fill = sentiment)) +
  #       geom_bar(stat = "identity") +
  #       labs(title = "Análisis de sentimiento \n Valoración positiva o negativa",
  #            x = "Sentimiento", y = "Frecuencia") +
  #       geom_text(aes(label = element_id),
  #                 vjust = 1.5, color = "black",
  #                 size = 5) +
  #       theme(plot.title = element_text(hjust = 0.5),
  #             axis.text = element_text(size=12),
  #             axis.title = element_text(size=14,face = "bold"),
  #             title = element_text(size=20,face = "bold"),
  #             legend.position = "none")
  #   }
  # })
  
  
  wordFrecuencyTerm <- eventReactive(input$calcSentiment, {
    req(localTweets$data)
    {
      #crear corpus
      word <- createCorpus(localTweets$data$text) %>%
        cleanDataTweetsV2("cnt_ec") %>%
        structureDataTweet() %>%
       sumWordFrecuency()
      
      return(word)
    }
  })
  
  wordcloud <- eventReactive(input$calcSentiment, {
    req(localTweets$data)
    {
      #crear corpus
      inWordcloud <- createCorpus(localTweets$data$text) %>%
        cleanDataTweetsV2("cnt_ec") %>%
        structureDataTweet() %>%
        calcWordcloud()
      
      return(inWordcloud)
    }
  })
  
  output$scatterplot <- renderPlot({
    if(input$calcSentiment){
      #plot(resultadoSentimiento(), ylab = "Valencia emocional", xlab = "Duración del texto", main = "Trazado a nivel de oración")
      plot(resultadoSentimiento())
    }
  })
  
  
  output$barPlotTDM <-renderPlot({
    if(input$calcSentiment){
      #barplot(wordFrecuencyTerm(), las = 2, col = rainbow(40), width = .1)
      barplot(wordFrecuencyTerm(), main = "Frecuencia de palabras en el documento", las = 2, col = rainbow(40), width = 0.2,
              ylab = "Número de frecuencia", xlab = "Palabras en el documento")
    }
  })
  
  output$barPlotPolarity <-renderPlot({
    if(input$calcSentiment){
      ggplot(polarityResult(),
             aes(x = Sentimiento,
                 y = Polaridad, fill = Sentimiento)) +
        geom_bar(stat = "identity") +
        labs(title = "Análisis de sentimiento \n Valoración positiva o negativa",
             x = "Sentimiento", y = "Frecuencia") +
        geom_text(aes(label = Polaridad),
                  vjust = 1.5, color = "black",
                  size = 5) +

        theme(plot.title = element_text(hjust = 0.5),
              axis.text = element_text(size=12),
              axis.title = element_text(size=14,face = "bold"),
              title = element_text(size=20,face = "bold"),
              legend.position = "right")


    }
  })
  
  
  output$wordCloudPlot <-renderPlot({
    if(input$calcSentiment){
      wordcloud()
      
    }
  })
  
  #evento reactivo
  geoSearchedTweets <- reactiveValues(
    data = NULL
  )
  
  #evento reactivo clic en mapa
  clickedValue <- reactive({
    input$EcuadorMap_click
  })
  
  #evento para boton calculo sentimiento de geolocalizacion
  geoPolarityResult <- eventReactive(input$calcGeoSentiment, {
   
    req(input$geoSearch)
    {
      geoSearchedTweets$data <- searchTweets(input$geoSearch, sinceDate = input$geoFromToDate[1], 
                                          untilDate =input$geoFromToDate[2], geoCode =  clickedValue(), ratio = input$geoRatio)
      
      tweetsDataFormat$data <- geoSearchedTweets$data %>%
                                createData() 
        
        
      tweetsDataPolarity$data <-  tweetsDataFormat$data %>%                       
                                calcSentiment(input$geoSearch) 
      
      
      polarity <- tweetsDataPolarity$data %>%
                        calcPolarity()
      
      return(polarity)
      
      
    
    }
  })
  
  #grafica analisis sentimiento geolocalizacion
  output$barplot <-renderPlot({
    if(input$calcGeoSentiment){
      ggplot(geoPolarityResult(),
             aes(x = Sentimiento,
                 y = Polaridad, fill = Sentimiento)) +
        geom_bar(stat = "identity") +
        labs(title = "Análisis de sentimiento \n Valoración positiva o negativa",
             x = "Sentimiento", y = "Frecuencia") +
        geom_text(aes(label = Polaridad),
                  vjust = 1.5, color = "black",
                  size = 5) +
        
        theme(plot.title = element_text(hjust = 0.5),
              axis.text = element_text(size=12),
              axis.title = element_text(size=14,face = "bold"),
              title = element_text(size=20,face = "bold"),
              legend.position = "right")
      
      
    }
  })
  
  #observadores para eventos validacion
  
  observeEvent(input$calcGeoSentiment, handlerExpr = {

    if(input$geoSearch == ""){
      #showNotification("Ingresar producto o servicio!")
      output$task_menu <- renderMenu({
        dropdownMenu(type = "notifications", notificationItem(status = "danger", text = "Ingresar producto o servicio para búsqueda."))
      })
    }
    
    if(is.null(clickedValue()))
    {
      output$task_menu <- renderMenu({
        dropdownMenu(type = "notifications", notificationItem(status = "danger",text = "Ingresar coordenadas en el mapa."))
      })
    }
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  #observador para mapa
  observe({
    if(input$geoRatio && !is.na(input$geoRatio))
    {
        
      if(!is.null(clickedValue()))
      {
        lat <- clickedValue()$lat[1]
        lng <- clickedValue()$lng[1]
        text<-paste("Lattitude ", lat, "Longtitude ", lng)
        #radiusLocal <- input$geoRatio*1000 #conversion a kilometros
        if(input$geoRatio)
        {
          radiusLocal <- input$geoRatio * 1000 #conversion a km
        }
        else
        {
          radiusLocal <- 10 *1000 #rango por defecto y conversion a kilometros
        }
        
        leafletProxy("EcuadorMap") %>%
          setView(lng = lng, lat = lat, zoom = 5) %>%
          
          #clearMarkers()%>%
          #addMarkers(lng = lng, lat = lat, popup =  text) %>%
          clearShapes() %>%
          addCircles(lng = lng, lat = lat, radius = radiusLocal)
        
      }
    }
  })
  
  #evento para agregar marcado en el mapa
  observe(
    {
      click <- input$EcuadorMap_click
      if(is.null(click))
        return()
      lat= click$lat
      lng= click$lng
      popupName= "Punto"
      text<-paste("Lattitude ", lat, "Longtitude ", lng)
      
      output$text <- renderText({
        text
      })
      
      if(input$geoRatio && !is.na(input$geoRatio))
      {
        radiusLocal <- input$geoRatio * 1000 #conversion a km
      }
      else
      {
        radiusLocal <- 10*1000 #rango por defecto y conversion a kilometros
      }
      
      leafletProxy("EcuadorMap") %>%
        setView(lng = lng, lat = lat, zoom = 5) %>%
        clearMarkers()%>%
        clearShapes() %>%
        addMarkers(lng = lng, lat = lat, popup =  text) %>%
        addCircles(layerId = "radioAnalisis", lng = lng, lat = lat, radius = radiusLocal)
      
    }
    
  )
  
  observe(
    {
      click = input$EcuadorMap_marker_mouseover
      if(is.null(click))
        return()
      text<-paste("Latitud ", click$lat, "Longitud ", click$lng)
      
      leafletProxy("EcuadorMap") %>%
        # setView(lng = lng, lat = lat, zoom = 5) %>%
        clearPopups()%>%
        #addPopups(layerId = "popupMarker", lng = click$lng,lat = click$lat+0.4,text)
        addPopups(layerId = "popupMarker", lng = click$lng,lat = click$lat,text)
    }
  )
  tweetsDataFormat <- reactiveValues(
    data = NULL
  )
  
  tweetsDataPolarity <- reactiveValues(
    data = NULL
  )
  
  # tweetsDataWithoutCleanText <- reactiveValues(
  #   data = NULL
  # )
  
  #agregamos evento reactivo para obtener tweets con polaridad
  tweetsDataWithoutCleanText <- eventReactive(tweetsDataPolarity$data, {
    #agregamos polaridad
    tweets.df <- tweetsDataPolarity(tweetsDataFormat$data, tweetsDataPolarity$data$sentiment)
    
    return(tweets.df)
    
  })
  
 #tabla tweets en online
  output$twitterData <- DT::renderDataTable(
    
   
    tweetsDataWithoutCleanText()%>%
      DT::datatable( #colnames = c("Número","Texto", "Fecha", "Usuario"),             
                   options = list(pageLength = 10), 
                   rownames = FALSE)
  )
  
  #diagrama por dia 
  output$dayPlot <-renderPlot({
    if(input$calcGeoSentiment){
      ggplot(tweetsDataWithoutCleanText(),
             aes(x = createdDate,
                 y = Polarity)) +
        geom_line(stat = "identity", color = "blue") +
        
        labs(title = "Análisis de sentimiento por fecha", 
             x = "Fecha", y = "Polaridad") +
        # geom_text(aes(label = Polarity),
        #           vjust = 1.5, color = "blue",
        #           size = 5) +
        
        theme(plot.title = element_text(hjust = 0.5),
              axis.text = element_text(size=12),
              axis.title = element_text(size=14,face = "bold"),
              title = element_text(size=20,face = "bold"),
              legend.position = "right")
      
      
    }
  })
  
  
  #digrama por genero
  
  
  output$genPolarityPlot <-renderPlot({
    if(input$calcGeoSentiment){
      qplot(Polarity, data = tweetsDataWithoutCleanText(), geom = "bar", binwith = 2,
            xlab = "Setimiento", ylab = "Polaridad") + 
        theme_classic()  +
        facet_wrap(~Genero)
     
      
      
    }
  })
  
  
})