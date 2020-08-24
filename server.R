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
library(ggthemes)
library(shiny)
library(genderizeR)
library(tools)
library(shinyjs)
library(shinyBS)
source("./Connection.R")
source("./LexiconCustomization_EC.R")
source("./functions.R")

#asigna por sesion diccionario para edicion


createTwitterConection()

#localTweetsData <- loadLocalData("datosTweets_CNT_12072019To13072019.csv")
#localTweetsData$created <- as.Date(localTweetsData$created,"%Y/%m/%d")

source(file.path("server", "helpers.R"))

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  #habilitar seguimiento
 #debugonce(createCorpus)
# debugonce(calcSentiment)
 # debugonce(headMapPlot)
  
  # reactive values we will use throughout the app
  dataValues <- reactiveValues(
    commentsTwitter = NULL
  )
  
  output$datasetChosen <- reactive({ FALSE })
  outputOptions(output, 'datasetChosen', suspendWhenHidden = FALSE)
  
  output$datasetOnlineChosen <- reactive({FALSE})
  outputOptions(output, 'datasetOnlineChosen', suspendWhenHidden = FALSE)
  
  output$exeCalcSentiment <- reactive({FALSE})
  outputOptions(output, 'exeCalcSentiment', suspendWhenHidden = FALSE)
  
  observeEvent(input$fileLoaded, ignoreNULL = FALSE, {
    toggleState("uploadFilesButton", !is.null(input$fileLoaded))
  })
  
  observeEvent(input$fileLoaded, ignoreNULL = FALSE, {
    toggleState("calcSentiment", !is.null(input$fileLoaded))
  })
  
  observeEvent(input$edit_btn, ignoreNULL = FALSE, {
    toggleState("save_btn", !is.null(input$edit_btn))
  })
  
  observeEvent(input$editShitfValence_btn, ignoreNULL = FALSE, {
    toggleState("saveShiftValence_btn", !is.null(input$editShitfValence_btn))
  })
  
  #mapas datos locales (offline)
  # output$geoMapLocal <- renderLeaflet({
  #   req(nrow(datosLocalesTweets$data) != 0)
  #   {
  #     dataLocal <- datosLocalesTweets$data %>%
  #         filter(datosLocalesTweets$data$latitud != 0, datosLocalesTweets$data$longitud != 0 )
  #       
  #     leaflet(data = dataLocal, options = leafletOptions(dragging = TRUE,
  #                                      minZoom = 7,
  #                                      maxZoom = 100)) %>%
  #       addTiles() %>%
  #       addSearchOSM() %>%
  #       addResetMapButton() %>%
  #       addProviderTiles("CartoDB.Positron") %>%
  #       setView(lng=-78.78442, lat=-0.94434, zoom = 7)  %>%
  #       addCircles(lat = ~latitud,
  #                  lng = ~longitud,
  #                  stroke = FALSE,
  #                  popup = ~Ciudad,
  #                  color = ~colorFactor(palette = "Set3", domain = datosLocalesTweets$data$polaridad),
  #                  radius = 100,
  #                  fillOpacity = 0.5) %>%
  #       addLegend(position = "bottomright",
  #                 title="Sentimiento",
  #                 pal = colorFactor(palette = "Set3", domain = datosLocalesTweets$data$polaridad),
  #                 values = ~polaridad,
  #                 opacity = 1)
  #   }
  #               
  # })
  
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
    inFile <- read.csv(input$fileLoaded$datapath )
    
    if(nrow(inFile) == 0)
      return(NULL)
    
    #crea dato local
    datosLocalesTweets$data <- inFile %>%
                                createDataLocal()
    #busca ciudad 
    cityValue$data <- unique(sapply(datosLocalesTweets$data$Ciudad, createAndSearchCity))
    
    return(datosLocalesTweets$data)
  })
  
  ####cargar datos local##### 
  observeEvent(input$uploadFilesButton, {
    withBusyIndicator("uploadFilesButton", {
      inFile <- read.csv(input$fileLoaded$datapath )
      
      if(nrow(inFile) == 0)
        return(NULL)
      
      #crea dato local
      datosLocalesTweets$data <- inFile %>%
        createDataLocal()
      #busca ciudad 
      cityValue$data <- unique(sapply(datosLocalesTweets$data$Ciudad, createAndSearchCity))
      
      # dataValues$commentsTwitter <- datosLocalesTweets$data 
      
      output$datasetChosen <- reactive({ TRUE })
      updateTabsetPanel(session, "tabsetPanelLocalData", "resultLocalTab")
      #  updateTabsetPanel(session, "mainNav", "settingsTab")
    })
  })
  
  #valor reactivo para datos locales cargados
  datosLocalesTweets <- reactiveValues(
    data = NULL
  )
  
  #valor reactivo para datos locales cargados
  datosOnlineTweets <- reactiveValues(
    data = NULL
  )

  #valor reactivo datos limpios
  cleanDataText <- reactiveValues(
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
  
####Tabla datos locales####
  output$twetterDataLocal <- DT::renderDataTable(

   # if(nrow(dataFileLoaded()) != 0 & !is.na(input$fromToDate[1])  & !is.na(input$fromToDate[2]))
    #if(!is.na(input$fromToDate[1])  & !is.na(input$fromToDate[2]))
    #{
      #language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
        datosLocalesTweets$data[,c(-9:-10)] %>%
        DT::datatable( options = list(scrollX=TRUE, scrollCollapse=TRUE, scrollY = 500, pageLength = 10, language = list(lengthMenu = "Mostrar _MENU_ registros", search = "Buscar", 
                                                                       info= "Mostrando registros del _START_ al _END_ de un total de _TOTAL_ registros",
                                                                       paginate = list('first'='Primero', 'last'='Último', 'next'='Siguiente', 'previous'= 'Anterior'))),
                       rownames = FALSE,
                       class = 'cell-border stripe',
                      colnames = c("Texto" = "text", "Fecha tweet" = "createdDate",
                                   "Nombre usuario"= "userName", "Nombre" = "Nombre", "Apellido" = "Apellido", "Ciudad" = "Ciudad",
                                    "País" = "Pais", "Género" = "Genero"))
    #}
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
  
  ####calculo de sentimiento local y actualizacion de data frame####
  resultadoSentimiento <- eventReactive(input$calcSentiment, {
    req(dataFileLoaded())
    {
      #resultado <- calcSentiment( datosLocalesTweets$data, input$geoLocalSearch) #old
      resultado <- calcSentiment( datosLocalesTweets$data, '', input$excludedLocalWords) 
     # resultado <- calcSentiment( datosLocalesTweets$data, '', input$dicctionatConfig) #new
      #agrupar
      resultadoGrouped <- data.frame(resultado)
      resultadoGrouped <- resultadoGrouped %>% group_by(element_id) %>% summarise(sentiment = mean(sentiment))

     # localTweets$data <- twetterDataLocalUpdate()
    
      datosLocalesTweets$data <-  datosLocalesTweets$data%>%
        arrange(element_id) %>%
        left_join( resultadoGrouped, by = c("element_id" = "element_id")) %>%
        mutate(polaridad = apply(resultadoGrouped, 1, getPolarityText))
      
      
      ######exportar datos cargados y con polaridad##########
      #write.csv(datosLocalesTweets$data, "ParoPolaridad_02072020.csv")
    
      
      #permite establecer el nombre de la columna inicial para las columnas antiguas y las nuevas del calculo polaridad
      names(datosLocalesTweets$data) = c("text" , "createdDate",
                   "userName", "Nombre", "Apellido", "Ciudad",
                   "Pais", "Genero", "element_id", "id_ciudades","latitud", "longitud", "Cálculo sentimiento", "polaridad")
     
      # output$exeCalcSentiment <- reactive({ TRUE })
      # updateTabsetPanel(session, "tabsetLocalResult", "levelTweetsGraph")
      
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
  
  
  polarityResultOnline <- eventReactive(geoPolarityResult() ,{
    
    
    polarity <- geoPolarityResult() %>%
      calcPolarity
    
    return(polarity)
    
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
  
  ####calculo TDM local####
  # wordFrecuencyTerm <- eventReactive(input$calcSentiment, {
  #   req(datosLocalesTweets$data)
  #   {
  #     #datos limpios
  #     cleanDataText$data <- createCorpus(datosLocalesTweets$data$text) %>%
  #           cleanDataTweetsV2("") #new
  #          #cleanDataTweetsV2(input$geoLocalSearch)
  #     
  #     #crear corpus
  #     word <- cleanDataText$data %>%
  #             structureDataTweet() %>%
  #             sumWordFrecuency(input$frecuencyWord)
  #     
  #     # word <- createCorpus(datosLocalesTweets$data$text) %>%
  #     #   cleanDataTweetsV2(input$geoLocalSearch) %>%
  #     #   structureDataTweet() %>%
  #     #  sumWordFrecuency()
  #     
  #     return(word)
  #   }
  # })
  
  wordFrecuencyTerm <- eventReactive(input$frecuencyWord, {
    req(datosLocalesTweets$data)
    {
      #datos limpios
      cleanDataText$data <- createCorpus(datosLocalesTweets$data$text) %>%
        cleanDataTweetsV2("") #new
      #cleanDataTweetsV2(input$geoLocalSearch)
      
      #crear corpus
      word <- cleanDataText$data %>%
        structureDataTweet() %>%
        sumWordFrecuency(input$frecuencyWord)
      
      # word <- createCorpus(datosLocalesTweets$data$text) %>%
      #   cleanDataTweetsV2(input$geoLocalSearch) %>%
      #   structureDataTweet() %>%
      #  sumWordFrecuency()
      
      return(word)
    }
  })
  
  ####calculo TDM Online####
  wordFrecuencyTermOnline <- eventReactive(input$frecuencyOnlineWord, {
    req(datosOnlineTweets$data)
    {
      #datos limpios
      cleanDataText$data <- createCorpus(datosOnlineTweets$data$text) %>%
        cleanDataTweetsV2(input$geoSearch) #new
      #cleanDataTweetsV2(input$geoLocalSearch)
      
      #crear corpus
      word <- cleanDataText$data %>%
        structureDataTweet() %>%
        sumWordFrecuencyOnline(input$frecuencyOnlineWord)

      return(word)
    }
  })
  
  ####nube de palabras local####
  # wordcloud <- eventReactive(input$calcSentiment, {
  #   req(datosLocalesTweets$data)
  #   {
  #     #crear corpus
  #     inWordcloud <- createCorpus( datosLocalesTweets$data$text) %>%
  #       cleanDataTweetsV2('') %>% #new
  #       #cleanDataTweetsV2(input$geoLocalSearch) %>% @old
  #       structureDataTweet() %>%
  #       calcWordcloud(input$maxWord)
  #     
  #     return(inWordcloud)
  #   }
  # })
  
  wordcloud <- eventReactive(input$maxWord, {
    req(datosLocalesTweets$data)
    {
      #crear corpus
      inWordcloud <- createCorpus( datosLocalesTweets$data$text) %>%
        cleanDataTweetsV2('') %>% #new
        #cleanDataTweetsV2(input$geoLocalSearch) %>% @old
        structureDataTweet() %>%
        calcWordcloud(input$maxWord, input$freqMin)
      
      return(inWordcloud)
    }
  })
  
  ####calculo nube de palabras online####
  wordcloudOnline <- eventReactive(input$maxOnlineWord, {
    req(datosOnlineTweets$data)
    {
      #crear corpus
      inWordcloud <- createCorpus( datosOnlineTweets$data$text) %>%
        cleanDataTweetsV2('') %>% #new
        #cleanDataTweetsV2(input$geoLocalSearch) %>% @old
        structureDataTweet() %>%
        calcWordcloud(input$maxOnlineWord, input$minOnlineFreq)
      
      return(inWordcloud)
    }
  })
  
  ####grafica de suavizado######
  output$scatterplot <- renderPlot({
     #if(input$calcSentiment){
    if(input$minTweets | input$maxTweets){
      #plot(resultadoSentimiento(), ylab = "Valencia emocional", xlab = "Duración del texto", main = "Trazado a nivel de oración")
      #plot(resultadoSentimiento())
      result.df <- data.frame(resultadoSentimiento())
      result.df  <- result.df  %>% group_by(element_id) %>% summarise(sentiment = mean(sentiment))
      
      result.df %>%
        filter(element_id >= input$minTweets & element_id <= input$maxTweets) %>%
        ggplot( mapping =  aes(x = element_id, y = sentiment), alpha=.4) +
         #ggplot(data = result.df, mapping =  aes(x = element_id, y = sentiment), alpha=.4) +
        geom_point(alpha = 1/25, shape = 21)+
        geom_smooth(se=T,  color = "blue" ) +
        geom_hline(yintercept = 0, color = "red", linetype = 2) +
        ylab("Polaridad")+
        xlab("Tweets") +
       # xlim(0,999) 
        theme_gdocs() +
        theme(legend.position =   "bottom", legend.direction = "horizontal", legend.box = "vertical")
      
      
    }
  })
  
  ####matriz de termino documentos####
  output$barPlotTDM <-renderPlot({
    if(input$calcSentiment){
      #barplot(wordFrecuencyTerm(), las = 2, col = rainbow(40), width = .1)
      barplot(wordFrecuencyTerm(), main = "Frecuencia de palabras en el documento", las = 2, col = rainbow(40), width = 0.2,
              ylab = "Número de frecuencia")
    }
  })
  
  #grafico barras datos local
  output$barPlotPolarity <-renderPlot({
    if(input$calcSentiment){
      ggplot(polarityResult(),
             aes(x = Sentimiento,
                 y = Polaridad, fill = Sentimiento)) +
        geom_bar(stat = "identity", colour = "black", size = 0.25, width = 0.5 ) +
        scale_fill_manual(values = c("#FFDDDD", "gray", "#CCEEFF" ), guide = FALSE) +
       # scale_fill_manual(values = colorSentimiento) 
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
  
  ####grafico nube palabras local####
  output$wordCloudPlot <-renderPlot({
    if(input$calcSentiment){
      wordcloud()
      
    }
  })
  
  ####mapa de calor local####
  output$heatMapLocalData <- renderPlot(
    if(input$calcSentiment){
      headMapPlot(datosLocalesTweets$data)
      
    }
   )
  
  #grafico lollipop
  # output$lollipopPlot <- renderPlot(
  #   if(nrow(datosLocalesTweets$data) > 0){
  #     lollipopPlot(datosLocalesTweets$data)
  #   }
  # )
  
  ####grafico conteo de palabas local####
  output$sentimenWordCountsPlot <- renderPlot(
    #if(nrow(datosLocalesTweets$data) > 0){
    if(input$calcSentiment){
      wordCountSentiment(datosLocalesTweets$data)
    }
  )
  
  ####grafico porción de uso de palabras local####
  output$sentimenWordPercentPlot <- renderPlot(
    #if(nrow(datosLocalesTweets$data) > 0){
    if(input$calcSentiment){ 
      wordPercentSentiment(datosLocalesTweets$data)
    }
    
  )
  
  #evento reactivo
  geoSearchedTweets <- reactiveValues(
    data = NULL
  )
  
  #evento reactivo clic en mapa
  clickedValue <- reactive({
    input$EcuadorMap_click
  })
  
  ####cargar datos online####
  observeEvent(input$loadDataOnline, {
    withBusyIndicator("loadDataOnline", {
      
      req(input$geoSearch)
      {
        geoSearchedTweets$data <- searchTweets(input$geoSearch, sinceDate = input$geoFromToDate[1], 
                                               untilDate =input$geoFromToDate[2], geoCode =  clickedValue(), ratio = input$geoRatio)
        
        tweetsDataFormat$data <- geoSearchedTweets$data %>%
          createData() 
        
        output$datasetOnlineChosen <- reactive({ TRUE })
        updateTabsetPanel(session, "tabsetPanel", "resulOnlineTab")
        
      }
    })
  })
  
  #####calculo sentimiento online####
  geoPolarityResult <- eventReactive(input$calcGeoSentiment, {
   
    #req(input$geoSearch)
    req(tweetsDataFormat$data)
    {
      # geoSearchedTweets$data <- searchTweets(input$geoSearch, sinceDate = input$geoFromToDate[1], 
      #                                     untilDate =input$geoFromToDate[2], geoCode =  clickedValue(), ratio = input$geoRatio)
      # 
      # tweetsDataFormat$data <- geoSearchedTweets$data %>%
      #                           createData() 
      #   
        
      tweetsDataPolarity$data <-  tweetsDataFormat$data %>%                       
                                calcSentiment(input$geoSearch) 
      
      #agrupar
      resultadoGrouped <- data.frame(tweetsDataPolarity$data)
      resultadoGrouped <- resultadoGrouped %>% group_by(element_id) %>% summarise(sentiment = mean(sentiment))
      
      # localTweets$data <- twetterDataLocalUpdate()
      
      datosOnlineTweets$data <-  tweetsDataFormat$data%>%
        arrange(element_id) %>%
        left_join( resultadoGrouped, by = c("element_id" = "element_id")) %>%
        mutate(polaridad = apply(resultadoGrouped, 1, getPolarityText))
      
      #permite establecer el nombre de la columna inicial para las columnas antiguas y las nuevas del calculo polaridad
      # names(datosOnlineTweets$data) = c("text" , "createdDate",
      #                                    "userName", "Nombre", "Apellido", "Ciudad",
      #                                    "Pais", "Genero", "element_id", "id_ciudades", "Cálculo sentimiento", "polaridad")
      # 
      tweetsDataFormat$data <- datosOnlineTweets$data
      names(tweetsDataFormat$data) = c("text" , "fechaTweet",
                                         "userName", "Nombre", "Apellido", "Ciudad",
                                         "Pais", "Genero", "element_id", "Cálculo sentimiento", "polaridad")

    
      return (tweetsDataPolarity$data)
      
    
    }
  })
  
  #grafica analisis sentimiento geolocalizacion
  output$barplot <-renderPlot({
    if(input$calcGeoSentiment){
      ggplot(geoPolarityResult()(),
             aes(x = Sentimiento,
                 y = Polaridad, fill = Sentimiento)) +
        geom_bar(stat = "identity", colour = "black", size = 0.25, width = 0.5 ) +
        scale_fill_manual(values = c("#FFDDDD", "gray", "#CCEEFF" ), guide = FALSE) +
      
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
      # ggplot(geoPolarityResult(),
      #        aes(x = Sentimiento,
      #            y = Polaridad, fill = Sentimiento)) +
      #   geom_bar(stat = "identity") +
      #   scale_fill_manual(values=c("red", "green", "gray")) +
      #   labs(title = "Análisis de sentimientos \n Valoración positiva o negativa",
      #        x = "Sentimiento", y = "Frecuencia") +
      #   geom_text(aes(label = Polaridad),
      #             vjust = 1.5, color = "black",
      #             size = 5) +
      #   
      #   theme(plot.title = element_text(hjust = 0.5),
      #         axis.text = element_text(size=12),
      #         axis.title = element_text(size=14,face = "bold"),
      #         title = element_text(size=20,face = "bold"),
      #         legend.position = "right")
      # 
      
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
  
  ####tweets con polaridad online####
  tweetsDataWithoutCleanText <- eventReactive(tweetsDataPolarity$data, {
    #agregamos polaridad
    tweets.df <- tweetsDataPolarity(tweetsDataFormat$data, tweetsDataPolarity$data)
    
    return(tweets.df)
    
  })
  
 ####tabla datos online####
  output$twitterData <- DT::renderDataTable(
    
    tweetsDataFormat$data[,c(-9)] %>%
      DT::datatable(options = list(scrollX=TRUE, scrollCollapse=TRUE, scrollY = 500, pageLength = 10, language = list(lengthMenu = "Mostrar _MENU_ registros", search = "Buscar", 
                                                                                                                      info= "Mostrando registros del _START_ al _END_ de un total de _TOTAL_ registros",
                                                                                                                      paginate = list('first'='Primero', 'last'='Último', 'next'='Siguiente', 'previous'= 'Anterior'))),
                    class = 'cell-border stripe',
                    colnames = c("Texto" = "text", "Fecha tweet" = "fechaTweet",
                                 "Nombre usuario"= "userName", "Nombre" = "Nombre", "Apellido" = "Apellido", "Ciudad" = "Ciudad",
                                 "País" = "Pais", "Género" = "Genero"),
                                    rownames = FALSE)
   
  )
  
  ####grafica de suavizado online######
  output$scatterplotOnline <- renderPlot({
    #if(input$calcGeoSentiment){
    if(input$minOnlineTweets | input$maxOnlineTweets){ 
      result.df <- data.frame(geoPolarityResult())
      result.df  <- result.df  %>% group_by(element_id) %>% summarise(sentiment = mean(sentiment))
      
      result.df %>%
      filter(element_id >= input$minOnlineTweets & element_id <= input$maxOnlineTweets) %>%
      ggplot( mapping =  aes(x = element_id, y = sentiment), alpha=.4) +
        geom_point(alpha = 1/25, shape = 21)+
        geom_smooth(se=T,  color = "blue" ) +
        geom_hline(yintercept = 0, color = "red", linetype = 2) +
        ylab("Polaridad")+
        xlab("Tweets") +
        # xlim(0,999) 
        theme_gdocs() +
        theme(legend.position =   "bottom", legend.direction = "horizontal", legend.box = "vertical")

    }
  })
  
  ####grafico barras datos online####
  output$barPlotPolarityOnline <-renderPlot({
    if(input$calcGeoSentiment){
      ggplot(polarityResultOnline(),
             aes(x = Sentimiento,
                 y = Polaridad, fill = Sentimiento)) +
        geom_bar(stat = "identity", colour = "black", size = 0.25, width = 0.5 ) +
        scale_fill_manual(values = c("#FFDDDD", "gray", "#CCEEFF" ), guide = FALSE) +
      
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
  
  ####grafico TDM Online####
  output$barPlotTDMOnline <-renderPlot({
    if(input$calcGeoSentiment){
      #barplot(wordFrecuencyTerm(), las = 2, col = rainbow(40), width = .1)
      barplot(wordFrecuencyTermOnline(), main = "Frecuencia de palabras en el documento", las = 2, col = rainbow(40), width = 0.2,
              ylab = "Número de frecuencia")
    }
  })
  
  ####nube palabras online####
  output$wordCloudPlotOnline <-renderPlot({
    if(input$calcGeoSentiment){
      wordcloudOnline()
      
    }
  })
  
  ####mapa de calor online####
  output$heatMapLocalDataOnline <- renderPlot(
    if(input$calcGeoSentiment){
      headMapPlotOnline(datosOnlineTweets$data )
    }
  )
  
  ####grafico conteo de palabas online####
  output$sentimenWordCountsPlotOnline <- renderPlot(
  
    if(input$calcGeoSentiment){
      wordCountSentimentOnline(datosOnlineTweets$data)
    }
  )
  
  ####grafico porción de uso de palabras online####
  output$sentimenWordPercentPlotOnline <- renderPlot(
    if(input$calcGeoSentiment){ 
      wordPercentSentiment(datosOnlineTweets$data)
    }
    
  )
  
  #diagrama por dia 
  output$dayPlot <-renderPlot({
    if(input$calcGeoSentiment){
      ggplot(tweetsDataWithoutCleanText(),
             aes(x = createdDate,
                 y = CalculoPolaridad)) +
        geom_smooth(alpha=.1)+
        geom_hline(yintercept = 0, color = "red") +
        
        labs(title = "Análisis de sentimientos por fecha", 
             x = "Fecha", y = "Polaridad") +
        # geom_text(aes(label = Polarity),
        #           vjust = 1.5, color = "blue",
        #           size = 5) +
        theme_gdocs()
        # theme(plot.title = element_text(hjust = 0.5),
        #       axis.text = element_text(size=12),
        #       axis.title = element_text(size=14,face = "bold"),
        #       title = element_text(size=20,face = "bold"),
        #       legend.position = "right")
      
      
    }
  })
  
  
  #digrama por genero
  
  
  output$genPolarityPlot <-renderPlot({
    if(input$calcGeoSentiment){
     
      # qplot(polaridad, data = tweetsDataWithoutCleanText(), geom = "bar", binwith = 2,
      #       xlab = "Setimiento", ylab = "Polaridad") + 
      #   theme_classic()  +
      #   facet_wrap(~Genero,  scales = "free")
      # 
      tweetsDataWithoutCleanText() %>%
        #filter(n>10 ) %>%
        arrange(Polaridad) %>%
        ggplot( aes(x = element_id, y = factor(element_id), fill = Polaridad)) +
        geom_col( position = "identity" , colour = "black", size = 0.25, width = 0.5) +
        #scale_fill_manual(values = c("#FFDDDD", "#CCEEFF"), guide = FALSE) +
        # ylim(c(3,140)) +
        facet_wrap(~Genero, scales = "free")+
        coord_flip()+
        labs(title = "Sentimientos por genero", x = "Palabras", y = "Número de palabras")
      
    }
  })
  
  #digrama por ciudad
  output$cityPolarityPlot <- renderPlot({
    if(input$calcGeoSentiment){
      
        tweetsDataWithoutCleanText() %>%
        ggplot( aes(x= Genero, y = CalculoPolaridad, col = Polaridad), ylim = c(-1,1) )+
        geom_jitter()+
        labs(title = "Análisis de sentimientos por ciudad", x = "Genero", y = "Polaridad") +
        facet_wrap(~toupper(Ciudad), scales = "free")
      }
    
  })
  
  # creacion descripcion de la ayuda
  output$description <- renderText({
    paste("A continuación, un ejemplo del archivo:")
  })
  
  # lexico_ec_data <- eventReactive(nrow(lexico_ec) != 0, {
  # 
  #    lexico_ec.df <- data.frame(lexico_ec)
  #    
  # })
   
  lexico_ec_table <- reactiveVal(lexico_ec_table)
  
  proxy = DT::dataTableProxy('dictionary_ec')
  
  lexico_ec_ShiftValenceTable <- reactiveVal(lexicoCambioSentimiento_table)
  
  proxyShiftValence = DT::dataTableProxy('dictionary_ec_ShiftValence')
  
  
  #observador para agregar palabras 
  observeEvent(input$add_btn, {
    
    t = rbind(data.frame(word = "<ingrese palabra>",
                         value = 0), lexico_ec_table(), stringsAsFactors = FALSE )
    t$word <- as.character(t$word)
    
    DT::replaceData(proxy, lexico_ec_table(t), resetPaging = FALSE)  # important
    #lexico_ec_table(t)
  })
  
  #observador para eliminar palabras
  observeEvent(input$delete_btn, {
    t = lexico_ec_table()
    
    if (!is.null(input$dictionary_ec_rows_selected)) {
     
      t <- t[-as.numeric(input$dictionary_ec_rows_selected),]
      
      lexico_ec_table(t)
      
      output$newMappedWord <- renderText({
        ""
      })
      #asignacion a variable global con <<
      lexico_ec <<- lexico_ec_table() %>% select(word, value)  
      
      lexico_ec %>%
        write.csv(file="lexico_ec_custom.csv", row.names = FALSE )
    }
    else
    {
      output$newMappedWord <- renderText({
        "No se ha seleccionado un registro para eliminar."
      })
  
    }
  
    
   
  })
  
  ####configuracion del diccionario principal####
  output$dictionary_ec <- DT::renderDataTable(
    
   #if(input$showData)
    #{
        datalexico <- lexico_ec_table() %>%
        DT::datatable( options = list(pageLength = 10,
                                      language = list(lengthMenu = "Mostrar _MENU_ registros", search = "Buscar", 
                                                                       info= "Mostrando registros del _START_ al _END_ de un total de _TOTAL_ registros",
                                                                       paginate = list('first'='Primero', 'last'='Último', 'next'='Siguiente', 'previous'= 'Anterior'))),
                       rownames = FALSE,
                       editable = input$edit_btn,
                       colnames = c("Palabra" = "word", "Valor" = "value"),
                       filter = "top"
                       )
    #}
  )
  
  
  
  #evento para obtener nuevo valor y actualizar
  observeEvent(input$dictionary_ec_cell_edit, {
    info = input$dictionary_ec_cell_edit
    #str(info)
    i = info$row
    j = info$col+1
    v = info$value
    x <- lexico_ec_table()
    x[i, j] <- DT::coerceValue(v, x[i, j])
    DT::replaceData(proxy, lexico_ec_table(x), resetPaging = FALSE)  # important
  })
  
  #guardar nuevos cambios
  observeEvent(input$save_btn,{
    
    #asignacion a variable global con <<
    lexico_ec <<- lexico_ec_table() %>% select(word, value)  
    
    lexico_ec %>%
    write.csv(file="lexico_ec_custom.csv", row.names = FALSE )
    
    }
  )
  
  observeEvent(input$dictionary_ec_ShiftValence_cell_edit, {
    info = input$dictionary_ec_ShiftValence_cell_edit
    #str(info)
    i = info$row
    j = info$col+1
    v = info$value
    x <- lexico_ec_ShiftValenceTable()
    x[i, j] <- DT::coerceValue(v, x[i, j])
    DT::replaceData(proxyShiftValence, lexico_ec_ShiftValenceTable(x), resetPaging = FALSE)  # important
  })
  
  observeEvent(input$saveShiftValence_btn,{
    
    #asignacion a variable global con <<
    lexicoCambioSentimiento <<- lexico_ec_ShiftValenceTable() %>% select(x, y)  
    
    lexicoCambioSentimiento %>%
      write.csv(file="LexicoCambioSentimientos.csv", row.names = FALSE )
    
  }
  )
  
  
  
  ####Diccionario cambio de sentimiento####
  
  dictionaryShiftValence_ec <- reactiveVal(lexicoCambioSentimiento)
  
  output$dictionary_ec_ShiftValence <- DT::renderDataTable(
    
   # if(input$showDataShifValence)
   # {
      dictionaryShiftValence_ec() %>%
        DT::datatable( options = list(pageLength = 10,
                                      language = list(lengthMenu = "Mostrar _MENU_ registros", search = "Buscar", 
                                                      info= "Mostrando registros del _START_ al _END_ de un total de _TOTAL_ registros",
                                                      paginate = list('first'='Primero', 'last'='Último', 'next'='Siguiente', 'previous'= 'Anterior'))),
                       rownames = FALSE,
                       editable = input$editShitfValence_btn,
                       colnames = c("Palabra" = "x", "Valor" = "y"),
                       filter = "top"
        )
    #}
  )
  
  ####logica cargar nuevas palabras####
  #carga datos de archivo
  # dataNewWordsFileLoaded <- eventReactive(input$fileLoadedNewWords,{
  #   req(input$fileLoadedNewWords)
  #   inFile <- read.csv(input$fileLoadedNewWords$datapath )
  #   
  #   if(nrow(inFile) == 0)
  #     return(NULL)
  #   
  #   #crea dato local
  #   newWordsLocal$data <- inFile %>%
  #     createNewWords()
  #   
  #   return(newWordsLocal$data)
  # })
  
  # #valor reactivo para palabras locales cargados
  # newWordsLocal <- reactiveValues(
  #   data = NULL
  # )
  
  # output$dictionary_ec_NewWords <- DT::renderDataTable(
  #   
  #   if(nrow(dataNewWordsFileLoaded()) != 0 & input$showDataNewDictionary)
  #   {
  #    
  #     newWordsLocal$data %>%
  #       DT::datatable(  options = list(pageLength = 10, language = list(lengthMenu = "Mostrar _MENU_ registros", search = "Filtro", 
  #                                                                      info= "Mostrando registros del _START_ al _END_ de un total de _TOTAL_ registros",
  #                                                                      paginate = list('first'='Primero', 'last'='Último', 'next'='Siguiente', 'previous'= 'Anterior'))),
  #                      rownames = FALSE,
  #                      editable = TRUE,
  #                      colnames = c("Palabra" = "Palabra", "Contador palabra" = "w", "Polaridad (-1 hasta 1)" = "polarity" ))
  #   }
  # )
  
  ####Descargar archivo####
  output$saveMetaBtn <- downloadHandler(
    filename = "DatosLocales-polaridad.csv",
    
    content = function(file) {
      write.csv(datosLocalesTweets$data , file, row.names = FALSE)
    }
  )
  
  output$saveMetaBtnOnline <- downloadHandler(
    filename = "DatosOnline-polaridad.csv",
    
    content = function(file) {
      write.csv(datosOnlineTweets$data , file, row.names = FALSE)
    }
  )
  
  
  
  output$savePrimaryDictionaryBtn <- downloadHandler(
    filename = "Diccionario_principal.csv",
    
    content = function(file) {
      data.frame(lexico_ec_table()) %>% 
      write.csv( file, row.names = FALSE)
    }
  )
  
  output$saveValenceDictionaryBtn <- downloadHandler(
    filename = "Diccionario_Cambios_sentimiento.csv",
    
    content = function(file) {
      write.csv(lexicoCambioSentimiento_table , file, row.names = FALSE)
    }
  )
  
  # reactiveValues object for storing current data set.
  vals <- reactiveValues(data = NULL)
  
  dataModal <- function(failed = FALSE) {
    modalDialog(
      textInput("word", "Ingrese una nueva palabra al diccionario",
                placeholder = 'nueva palabra o expresión'
      ),
      numericInput("valorPol", label = "Valor polaridad", min = -1, max = 1, step = 0.5, value = 0),
    if (failed)
        div(tags$b("No es una palabra válida", style = "color: red;")),
      
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("ok", "OK")
      )
    )
  }
  
  # Show modal when button is clicked.
  observeEvent(input$newWord, {
    showModal(dataModal())
  })
  
  observeEvent(input$ok, {
    # Check that data object exists and is data frame.
    if (!is.null(input$word) && nzchar(input$word)) {
      
      vals$data <- paste0( input$word, input$valorPol)
      new_row <- c(input$word, input$valorPol)
    
      
      t = rbind(data.frame(word = as.character(input$word),
                           value = as.numeric(input$valorPol)), lexico_ec_table(), stringsAsFactors = FALSE )
      
    
      
      DT::replaceData(proxy, lexico_ec_table(t), resetPaging = FALSE)  # important
      
      #lexico_ec_table <- rbind(lexico_ec_table, new_row)
      # 
     lexico_ec <<- lexico_ec_table() %>% select(word, value)  
      # 
     lexico_ec %>%
       write.csv(file="lexico_ec_custom.csv", row.names = FALSE )
      removeModal()
    } else {
      showModal(dataModal(failed = TRUE))
    }
  })
  
  # # Display information about selected data
  # output$newMappedWord <- renderPrint({
  #   if (is.null(vals$data))
  #     "No se ha agregado palabra."
  #   else
  #     vals$data
  # })
  
  # Cuadro dialogo para cambios de sentimientos
  observeEvent(input$add_btnShiftValence, {
    showModal(dataModalShift())
  })
  
  dataModalShift <- function(failed = FALSE) {
    
   
    modalDialog(
      textInput("wordShif", "Ingrese una nueva palabra al diccionario",
                placeholder = 'Nueva palabra'
      ),
    #  numericInput("ValueShif", label = "Valor polaridad", min = -1, max = 1, step = 0.5, value = 0),
    div(style="display:inline-block;",
     selectInput("Category", "Categoría",   choices = c("Negador" = 1, 
                                                     "Amplificador" = 2, 
                                                     "Deamplificador"=3, 
                                                         "Conjunciones adversativas"=4)
                 )),
    div(style="display:inline-block;",
      popify(
          helpPopup(""),
          "Negador: invierte el signo de una palabra polarizada <br/>Amplificador: aumenta el impacto de una palabra polarizada <br/>Deamplificador: reduce el impacto de una palabra polarizada  <br/>Adversativa: anula la cláusula anterior que contiene una palabra polarizada"
      )),                                            

      if (failed)
        div(tags$b("No es una palabra válida", style = "color: red;")),
      
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("okShif", "OK")
      )
    )
    
  }
  
  observeEvent(input$okShif, {
    # Check that data object exists and is data frame.
    if (!is.null(input$wordShif) && nzchar(input$wordShif)) {
      
      #vals$data <- paste0( input$wordShif, input$ValueShif)
      new_row <- c(input$wordShif, input$Category)
      #print(new_row)
       
       t = rbind(data.frame(x = as.character(input$wordShif),
                            y = as.numeric(input$Category)), dictionaryShiftValence_ec(), stringsAsFactors = FALSE )
   
       DT::replaceData(proxyShiftValence, dictionaryShiftValence_ec(t), resetPaging = FALSE)  # important
    
       lexicoCambioSentimiento <<- dictionaryShiftValence_ec() %>% select(x, y)  
      
       lexicoCambioSentimiento %>%
         write.csv(file="LexicoCambioSentimientos.csv", row.names = FALSE )

      removeModal()
    } else {
      showModal(dataModalShift(failed = TRUE))
    }
  })
  
  #observador para eliminar palabras
  observeEvent(input$delete_btnShiftValence, {
    t = dictionaryShiftValence_ec()
    
    if (!is.null(input$dictionary_ec_ShiftValence_rows_selected)) {
      
      t <- t[-as.numeric(input$dictionary_ec_ShiftValence_rows_selected),]
      
      dictionaryShiftValence_ec(t)
      
      output$newMappedWordShif <- renderText({
        ""
      })
      #asignacion a variable global con <<
      lexicoCambioSentimiento <<- dictionaryShiftValence_ec() %>% select(x, y)  
      
      lexicoCambioSentimiento %>%
        write.csv(file="LexicoCambioSentimientos.csv", row.names = FALSE )
    }
    else
    {
      output$newMappedWordShif <- renderText({
        "No se ha seleccionado un registro para eliminar."
      })
      
    }
    
    
    
  })
  
  
  # ocultar el mensaje de cargando
  
  hide("loading-content", TRUE, "fade")  
  
  
})