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
library(tools)
source("./Connection.R")
source("./LexiconCustomization_EC.R")
source("./functions.R")

createTwitterConection()

#localTweetsData <- loadLocalData("datosTweets_CNT_12072019To13072019.csv")
#localTweetsData$created <- as.Date(localTweetsData$created,"%Y/%m/%d")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  #habilitar seguimiento
  #debugonce(calcPolarity)

  #mapas datos locales (offline)
  output$geoMapLocal <- renderLeaflet({
    req(nrow(datosLocalesTweets$data) != 0)
    {
      dataLocal <- datosLocalesTweets$data %>%
          filter(datosLocalesTweets$data$latitud != 0, datosLocalesTweets$data$longitud != 0 )
        
      leaflet(data = dataLocal, options = leafletOptions(dragging = TRUE,
                                       minZoom = 7,
                                       maxZoom = 100)) %>%
        addTiles() %>%
        addSearchOSM() %>%
        addResetMapButton() %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(lng=-78.78442, lat=-0.94434, zoom = 7)  %>%
        addCircles(lat = ~latitud,
                   lng = ~longitud,
                   stroke = FALSE,
                   popup = ~Ciudad,
                   color = ~colorFactor(palette = "Set3", domain = datosLocalesTweets$data$polaridad),
                   radius = 100,
                   fillOpacity = 0.5) %>%
        addLegend(position = "bottomright",
                  title="Sentimiento",
                  pal = colorFactor(palette = "Set3", domain = datosLocalesTweets$data$polaridad),
                  values = ~polaridad,
                  opacity = 1)
    }
                
  })
  
  #mapas en linea 
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
  
  #carga datos de archivo
  dataFileLoaded <- eventReactive(input$fileLoaded,{
    req(input$fileLoaded)
    inFile <- read.csv(input$fileLoaded$datapath)
    
    if(nrow(inFile) == 0)
      return(NULL)
    
    #crea dato local
    datosLocalesTweets$data <- inFile %>%
                                createDataLocal()
    #busca ciudad 
    cityValue$data <- unique(sapply(datosLocalesTweets$data$Ciudad, createAndSearchCity))
    
    return(datosLocalesTweets$data)
  })
  
  #valor reactivo para datos locales cargados
  datosLocalesTweets <- reactiveValues(
    data = NULL
  )

  
  #Presentacion datos locales
  # output$twetterDataLocal <- DT::renderDataTable(
  #   if(nrow(dataFileLoaded()) != 0)
  #   {
  #     DT::datatable(data = datosLocalesTweets$data, options = list(pageLength = 10), 
  #                   rownames = FALSE
  #     )
  #   }
  #   
  # )
  output$twetterDataLocal <- DT::renderDataTable(

    if(nrow(dataFileLoaded()) != 0 & !is.na(input$fromToDate[1])  & !is.na(input$fromToDate[2]))
    #if(!is.na(input$fromToDate[1])  & !is.na(input$fromToDate[2]))
    {
      #language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
        datosLocalesTweets$data[,c(-9:-10)] %>%
        DT::datatable( options = list(pageLength = 10, language = list(lengthMenu = "Mostrar _MENU_ registros", search = "Filtro", 
                                                                       info= "Mostrando registros del _START_ al _END_ de un total de _TOTAL_ registros",
                                                                       paginate = list('first'='Primero', 'last'='Último', 'next'='Siguiente', 'previous'= 'Anterior'))),
                       rownames = FALSE,
                       
                       colnames = c("Texto" = "text", "Fecha tweet" = "createdDate",
                                    "Nombre usuario"= "userName", "Nombre" = "Nombre", "Apellido" = "Apellido", "Ciudad" = "Ciudad",
                                    "País" = "Pais", "Género" = "Genero"))
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
    return(selectInput(inputId = "Ciudades", "Ciudad", cities))

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
  
  #calculo de sentimiento y actualizacion de data frame
  resultadoSentimiento <- eventReactive(input$calcSentiment, {
    req(dataFileLoaded())
    {
      resultado <- calcSentiment( datosLocalesTweets$data, input$geoLocalSearch)
      
      #agrupar
      resultadoGrouped <- data.frame(resultado)
      resultadoGrouped <- resultadoGrouped %>% group_by(element_id) %>% summarise(sentiment = mean(sentiment))

     # localTweets$data <- twetterDataLocalUpdate()
    
      datosLocalesTweets$data <-  datosLocalesTweets$data%>%
        left_join( resultadoGrouped, by = c("element_id" = "element_id")) %>%
        mutate(polaridad = apply(resultadoGrouped, 1, getPolarityText))
      
      
    
      
      #permite establecer el nombre de la columna inicial para las columnas antiguas y las nuevas del calculo polaridad
      names(datosLocalesTweets$data) = c("text" , "createdDate",
                   "userName", "Nombre", "Apellido", "Ciudad",
                   "Pais", "Genero", "element_id", "id_ciudades","latitud", "longitud", "Cálculo sentimiento", "polaridad")
     
     return(resultado)
      
    }
  })
  
  # output$DataLocalTweetsCalculed <- renderUI({
  #   dataTweets <- localTweets$data 
  #  
  #   return(dataTweets)
  #   
  # })
  
 
  
  #establecer nuevos valores 
  # output$twetterDataLocal <-  DT::renderDataTable({
  #   temp02 <- twetterDataLocalUpdate()
  # }, rownames=FALSE,
  # options = list(autoWidth = TRUE, 
  #                columnDefs = list(list(width = "125px", targets = "_all"))
  #   )
  # )
  
  #variable reactiva para el valor seleccionado en Ciudades
  cityChoosed <- reactive({ input$Ciudades})
  
  #evento observador para cambiar data frame con filtro
  observeEvent(cityChoosed(), {
    if(cityChoosed() == "Todas")
    {
      datosLocalesTweets$data <- dataFileLoaded()
    }
    else
    {
      datosLocalesTweets$data<- filter(dataFileLoaded(), Ciudad %in% cityChoosed())
    }
    
  })
  
  # output$descriptionTable <- renderText({
  #   
  #   req(cityChoosed())
  #   {
  #     paste("La tabla que se muestra a continuación muestra un total de ", nrow(dataFileLoaded()), "tweets")
  #     
  #     
  #   }
  #   
  #   })
  # 
  
  
  polarityResult <- eventReactive(resultadoSentimiento() ,{
      
       
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
  
  #calculo para frecuencia de palabras TDM
  wordFrecuencyTerm <- eventReactive(input$calcSentiment, {
    req(datosLocalesTweets$data)
    {
      #crear corpus
      word <- createCorpus(datosLocalesTweets$data$text) %>%
        cleanDataTweetsV2(input$geoLocalSearch) %>%
        structureDataTweet() %>%
       sumWordFrecuency()
      
      return(word)
    }
  })
  
  #nube de palabras
  wordcloud <- eventReactive(input$calcSentiment, {
    req(datosLocalesTweets$data)
    {
      #crear corpus
      inWordcloud <- createCorpus( datosLocalesTweets$data$text) %>%
        cleanDataTweetsV2(input$geoLocalSearch) %>%
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
  
  #matriz de termino documentos
  output$barPlotTDM <-renderPlot({
    if(input$calcSentiment){
      #barplot(wordFrecuencyTerm(), las = 2, col = rainbow(40), width = .1)
      barplot(wordFrecuencyTerm(), main = "Frecuencia de palabras en el documento", las = 2, col = rainbow(40), width = 0.2,
              ylab = "Número de frecuencia", xlab = "Palabras en el documento")
    }
  })
  
  #grafico barras datos local
  output$barPlotPolarity <-renderPlot({
    if(input$calcSentiment){
      ggplot(polarityResult(),
             aes(x = Sentimiento,
                 y = Polaridad, fill = Sentimiento)) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values=c("red", "green", "gray")) +
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
      
      
      # datosLocalesTweets$data <-  datosLocalesTweets$data%>%
      #   left_join( resultadoGrouped, by = c("element_id" = "element_id")) %>%
      #   mutate(polaridad = apply(resultadoGrouped, 1, getPolarityText))
      # 
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
        scale_fill_manual(values=c("red", "green", "gray")) +
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
    tweets.df <- tweetsDataPolarity(tweetsDataFormat$data, tweetsDataPolarity$data)
    
    return(tweets.df)
    
  })
  
 #tabla datos tweets en online
  output$twitterData <- DT::renderDataTable(
    
   
    tweetsDataWithoutCleanText()[,-10]%>%
      DT::datatable( #colnames = c("Número","Texto", "Fecha", "Usuario"),             
                   options = list(pageLength = 10), 
                   rownames = FALSE,
                   colnames = c("Texto" = "text", "Fecha creación" = "createdDate", 
                                "Nombre usuario"= "userName", "Fecha Tweet" = "fechaTweet", "Calculo polaridad" = "CalculoPolaridad", "Polaridad"="Polaridad") )
  )
  
  #diagrama por dia 
  output$dayPlot <-renderPlot({
    if(input$calcGeoSentiment){
      ggplot(tweetsDataWithoutCleanText(),
             aes(x = createdDate,
                 y = CalculoPolaridad)) +
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
  
  # creacion descripcion de la ayuda
  output$description <- renderText({
    paste("El formato es el siguiente:")
  })
  output$detailsDemo <- renderText({
    paste("El archivo debe ser guardado como tipo CSV y separado por coma. De la siguiente forma:")
  })
  
})