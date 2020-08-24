#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(leaflet)
library(leaflet.extras)
library(shinythemes)
#library(shinydashboardPlus)
library(shinyjs)
#library(ggthemr) 

#agregar imagen
formatFilePlainDemo <- base64enc::dataURI(file="AyudaArchivo.png", mime="image/png")
formatInternoArchivo <- base64enc::dataURI(file="formatoCSV.png", mime="image/png")
pestanaDatosLocal <- base64enc::dataURI(file="pestanaDatosLocal.png", mime="image/png")
loadLocalFile <- base64enc::dataURI(file="loadLocalFile.png", mime="image/png")
localData <- base64enc::dataURI(file="localData.png", mime="image/png")
analisysScreen <- base64enc::dataURI(file="analisysScreen.png", mime="image/png")


source(file.path("ui", "helpers.R"))

# Set up a button to have an animated loading indicator and a checkmark
# for better user experience
# Need to use with the corresponding `withBusyIndicator` server function
# withBusyIndicator <- function(button) {
#   id <- button[['attribs']][['id']]
#   tagList(
#     button,
#     span(
#       class = "btn-loading-container",
#       `data-for-btn` = id,
#       hidden(
#         img(src = "ajax-loader-bar.gif", class = "btn-loading-indicator"),
#         icon("check", class = "btn-done-indicator")
#       )
#     )
#   )
# }

header <- dashboardHeader(dropdownMenuOutput("task_menu" ), title = "Products & Services Trends", titleWidth =  280) 
                          
sideBar <- dashboardSidebar( width = 280, 
  
  sidebarMenu( 
    
    menuItem("Datos locales", tabName = "analisys", icon  = icon("table")  ),
    menuItem("Datos en línea", tabName = "geolocalizacion", icon = icon("table")),
   # menuItem("Resultados", tabName = "Results", icon  = icon("bar-chart")),
    menuItem("Configuración de diccionarios", tabName = "configuration", icon  = icon("cog")),
    menuItem("Ayuda", tabName = "help", icon = icon("info-circle"))
  )
)

body <- dashboardBody(
  
  tabItems( 
    tabItem(tabName = "analisys",
          # titlePanel("Carga de archivo", windowTitle = "Análisis de sentimientos en Twitter"),
        
            tabsetPanel(type = "tabs", id= "tabsetPanelLocalData",
                        tabPanel(title ="Cargar datos", value = 1, class = "fade in", 
                                
                                 wellPanel(
                                   fluidRow( 
                                     column( width = 12,
                                             box( width = NULL,collapsible = FALSE,
                                                  
                                                  fileInput("fileLoaded", 
                                                            div("Archivo",
                                                                helpPopup("Debe ser un archivo <i>.csv</i>  con  información descargada de comentarios.")
                                                            ),
                                                            buttonLabel = "Buscar archivo",
                                                            
                                                            multiple = FALSE, 
                                                            accept =c("csv",
                                                                      "text/comma-separated-values",
                                                                      ".csv"))
                                                  # fileInput(
                                                  #   "uploadDataFiles",
                                                  #   div("Data files",
                                                  #       helpPopup('These are all the files exported by QuantaSoft with names ending in "_Amplitude"'),
                                                  #       br(), downloadLink("sampleDataFile", "Example data file")
                                                  #   ),
                                                  #   multiple = TRUE,
                                                  #   accept = c(
                                                  #     'text/csv',
                                                  #     'text/comma-separated-values',
                                                  #     '.csv'
                                                  #   )
                                                  # )
                                                  # withBusyIndicator(
                                                  #   actionButton(
                                                  #     "uploadFilesBtn",
                                                  #     "Upload data",
                                                  #     class = "btn-primary"
                                                  #   )
                                                  # )
                                                  #radioButtons("showData", "Mostrar datos", choices = c("Si"=1, "No"=2), selected = 2)
                                             ),
                                             withBusyIndicator(
                                               actionButton(
                                                 "uploadFilesButton",
                                                 "Cargar datos",
                                                 class = "btn-primary"
                                               )
                                             )
                                             
                                             
                                     )
                                     )
                                   
                                   
                                 ),
                                 # wellPanel(
                                 #   fluidRow(
                                 #     column( width = 12,
                                 #             box( width = NULL,collapsible = TRUE,
                                 # 
                                 #                  #textInput("geoLocalSearch", label = "Texto de búsqueda", placeholder = "Ingrese producto o servicio"),
                                 # 
                                 #                  #selectInput("citySelected", "Ciudades Test", choices = c("quito", "Guayaquil", "Cuenca")),
                                 #                  # htmlOutput("CitiesLoaded"),
                                 #                  # #radioButtons("genero", "Genero", choices = c("Femenino", "Masculino")),
                                 #                  # dateRangeInput("fromToDate", language = 'es', label = "Rango fecha", separator = "hasta", weekstart = 1),
                                 #                  p(),
                                 #                 # actionButton("calcSentiment", label = "Análisis")
                                 #                  withBusyIndicator(
                                 #                    actionButton(
                                 #                      "calcSentiment",
                                 #                      "Análisis",
                                 #                      class = "btn-primary"
                                 #                    )
                                 #                  )
                                 #             )
                                 #     ))
                                 # 
                                 # 
                                 # ),
                                 wellPanel(
                                   
                                 fluidRow(
                                  
                                   column( width = 12,
                                           div(id = "datasetDescSelect",
                                               "Resultados"),
                                           box( width = NULL,collapsible = FALSE,
   
                                     
                                     withBusyIndicator(
                                       actionButton(
                                         "calcSentiment",
                                         "Análisis",
                                         class = "btn-primary"
                                       )
                                     ),
                                     p(),
                                     radioButtons("dicctionatConfig", "Perfil diccionario:",
                                                  choices = list("Diccionarios por defecto" = 1, "Diccionarios modificados" = 2)),
                                     textInput("excludedLocalWords", label = "Excluir palabras", placeholder = "Ingrese palabras a excluir", width = '30%'),
                                     

                                     
                                    p(),
                                    
                                  
                                       tabsetPanel(type = "tabs", id= "tabsetLocalResult", 
                                                                     
                                                     tabPanel(title = "Nivel de polaridad por tweets", icon = icon("chart-line"), id = "levelTweetsGraph" , 
                                                              wellPanel(
                                                                fluidRow(
                                                                  
                                                                  #tableOutput(outputId = "dataTweets"),
                                                                  #plotOutput(outputId = "scatterplot")
                                                                  numericInput("minTweets", label = "Mínimo tweets:", min = 1, value = 1),
                                                                  numericInput("maxTweets", label = "Máximo tweets:", min = 1, value = 700),
                                                                  br(),
                                                                  br(),
                                                                  box(
                                                                    title = "Tendencia", status = "primary", solidHeader = TRUE, width = 600,
                                                                    collapsible = F,
                                                                    plotOutput(outputId = "scatterplot", height = 350, width = 800)
                                                                  )
                                                                ))),
                                                     tabPanel(title = "Diagrama de barras", icon  = icon("bar-chart"),
                                                              wellPanel(
                                                                fluidRow(
                                                                  plotOutput(outputId = "barPlotPolarity")
                                                                )
                                                              )
                                                              
                                                     ), 
                                                     tabPanel(title = "Matriz de término de documentos", icon = icon("table"),
                                                              wellPanel(
                                                                fluidRow(
                                                               #   sliderInput("frecuencyWord", label = "Número de palabras", min = 2, max = 30,value = 2),
                                                                  numericInput("frecuencyWord", label = "Frecuencia de palabras", min = 2, value = 2),
                                                                  br(),
                                                                  br(),
                                                                  plotOutput(outputId = "barPlotTDM")
                                                                 
                                                                ))
                                                              
                                                     ), 
                                                     tabPanel(title = "Nube de palabras", icon  = icon("cloud"),
                                                              wellPanel(
                                                                fluidRow(
                                                                  sliderInput("freqMin",
                                                                              "Frecuencia mínima:",
                                                                              min = 1,  max = 50, value = 5),
                                                                  sliderInput("maxWord", label = "Máximo número de palabras", min = 5, max = 200,value = 50),
                                                                  br(),
                                                                  br(),
                                                                  plotOutput(outputId = "wordCloudPlot", height = "400px")
                                                                ))
                                                              
                                                     ),
                                                     # tabPanel(title = "Geolocalización",  icon  = icon("globe-americas"),
                                                     #          wellPanel(
                                                     #            fluidRow(
                                                     #              leafletOutput(outputId = "geoMapLocal")
                                                     #            ))
                                                     #          
                                                     # ),
                                                     tabPanel(title = "Polaridad por ciudad", icon = icon("fire"),
                                                              wellPanel(
                                                                fluidRow(
                                                                  plotOutput(outputId = "heatMapLocalData")
                                                                ))
                                                              
                                                     ),
                                                     # tabPanel(title = "Grafico Lollipop", icon = icon("project-diagram"),
                                                     #          wellPanel(
                                                     #            fluidRow(
                                                     #              plotOutput(outputId = "lollipopPlot")
                                                     #            ))
                                                     #          
                                                     # ),
                                                     tabPanel(title = "Conteo palabras por sentimiento", icon = icon("font"),
                                                              wellPanel(
                                                                fluidRow(
                                                                  plotOutput(outputId = "sentimenWordCountsPlot")
                                                                )),
                                                              wellPanel(
                                                                fluidRow(
                                                                  plotOutput(outputId = "sentimenWordPercentPlot")
                                                                ))
                                                              
                                                     )
                                         ) 
                                      
                                    )
                                   )
                                     
                                   )
                                 )
                                               
                            ),
                                        
                           tabPanel( title="Resultados de polaridad", value = 2, id = "resultLocalTab",
                                     br(),
                                     conditionalPanel( condition="output.datasetChosen",
                                                       
                                                       h5(textOutput("descriptionTable")),
                                                       br(),
                                                       downloadButton("saveMetaBtn", "Descargar detalle de archivo."),
                                                       br(), br(),
                                                       wellPanel(
                                                         fluidRow(
                                                           column( width = 12,
                                                                   DT::dataTableOutput(outputId = "twetterDataLocal")
                                                           )
                                                         )
                                                         
                                                       )
                                     
                                     )   
                           )
                          
                             
                            
            
            )
    ),
    tabItem(tabName = "geolocalizacion",
            titlePanel("Búsqueda de tweets", windowTitle = "Análisis sentimiento Twitter por Geolocalización"),
            tabsetPanel(type = "tabs", id= "tabsetPanel",
                        tabPanel(title ="Geolocalización", 
                                 
                                 wellPanel(
                                   
                                   fluidRow(
                                     column( width = 12,
                                             box( width = NULL,collapsible = FALSE,
                                                  
                                                  textInput("geoSearch", label = "Texto de búsqueda", placeholder = "Ingrese producto o servicio"),
                                                  #div(style="display:inline-block;", 
                                                    dateRangeInput("geoFromToDate", language = 'es', label = div("Rango fecha", helpPopup("Fecha permitida de 1 semana (7 días) antes de la fecha actual.")),
                                                                       separator = "hasta", weekstart = 1,  start = Sys.Date()-7 ),
                                                    #helpPopup("Fecha permitida de 1 semana (7 días) antes de la fecha actual.")
                                                   # ),
                                                  div(style="display:inline-block;",
                                                    numericInput("geoRatio", "Cobertura(km)", min = 0, max = 500,value = 10, width = 200),
                                                    actionButton("loadDataOnline", label = "Cargar datos", class = "btn-primary")
                                                  ),
                                                  p(),
                                                  leafletOutput("EcuadorMap")
 
                                             )
                                     ),
                                     p()
                                    # actionButton("calcGeoSentiment", label = "Análisis")
                                    
                                   )
                                   
                                 ),
                                 
                                 wellPanel(

                                   fluidRow(

                                     column( width = 12,
                                             div(id = "onlineResult",
                                                 "Resultados"),
                                             box( width = NULL,collapsible = FALSE,
                                                  
                                              withBusyIndicator(
                                                actionButton(
                                                  "calcGeoSentiment",
                                                  "Análisis",
                                                  class = "btn-primary"
                                                )
                                              ),
                                              p(),
                                              
                                              tabsetPanel(type = "tabs", id= "tabsetLocalResultOnline", 
                                                          
                                                          tabPanel(title = "Nivel de polaridad por tweets", icon = icon("chart-line"), id = "levelTweetsGraph" , 
                                                                   wellPanel(
                                                                     fluidRow(
                                                                       numericInput("minOnlineTweets", label = "Mínimo tweets:", min = 1, value = 1),
                                                                       numericInput("maxOnlineTweets", label = "Máximo tweets:", min = 1, value = 20),
                                                                       br(),
                                                                       br(),
                                                                       box(
                                                                         title = "Tendencia", status = "primary", solidHeader = TRUE, width = 600,
                                                                         collapsible = F,
                                                                         plotOutput(outputId = "scatterplotOnline", height = 350, width = 800)
                                                                       )
                                                                     ))),
                                                          tabPanel(title = "Diagrama de barras", icon  = icon("bar-chart"),
                                                                   wellPanel(
                                                                     fluidRow(
                                                                       plotOutput(outputId = "barPlotPolarityOnline")
                                                                     )
                                                                   )
                                                                   
                                                          ), 
                                                          tabPanel(title = "Matriz de término de documentos", icon = icon("table"),
                                                                   wellPanel(
                                                                     fluidRow(
                                                                       numericInput("frecuencyOnlineWord", label = "Frecuencia de palabras", min = 2, value = 2),
                                                                       br(),
                                                                       br(),
                                                                       plotOutput(outputId = "barPlotTDMOnline")
                                                                     ))
                                                                   
                                                          ), 
                                                          tabPanel(title = "Nube de palabras", icon  = icon("cloud"),
                                                                   wellPanel(
                                                                     fluidRow(
                                                                       sliderInput("minOnlineFreq",
                                                                                   "Frecuencia mínima:",
                                                                                   min = 1,  max = 50, value = 5),
                                                                       sliderInput("maxOnlineWord", label = "Máximo número de palabras", min = 5, max = 200,value = 30),
                                                                       br(),
                                                                       br(),
                                                                       plotOutput(outputId = "wordCloudPlotOnline")
                                                                     ))
                                                                   
                                                          ),
                                                         
                                                          tabPanel(title = "Polaridad por ciudad", icon = icon("fire"),
                                                                   wellPanel(
                                                                     fluidRow(
                                                                       plotOutput(outputId = "heatMapLocalDataOnline")
                                                                     ))
                                                                   
                                                          ),
                                                         
                                                          tabPanel(title = "Conteo palabras por sentimiento", icon = icon("font"),
                                                                   wellPanel(
                                                                     fluidRow(
                                                                       plotOutput(outputId = "sentimenWordCountsPlotOnline")
                                                                     )),
                                                                   wellPanel(
                                                                     fluidRow(
                                                                       plotOutput(outputId = "sentimenWordPercentPlotOnline")
                                                                     ))
                                                                   
                                                          )
                                              )        
                                              
                             
                                 # tabsetPanel(type = "tabs", id= "tabsetResult", 
                                 #             tabPanel(title = "General",  icon  = icon("bar-chart"),
                                 #                      
                                 #                      wellPanel(plotOutput(outputId = "barplot"))
                                 #                      
                                 #             ), 
                                 #             tabPanel(title = "Día",  icon = icon("calendar-alt"),
                                 #                      wellPanel(plotOutput(outputId = "dayPlot"))
                                 #                      
                                 #             ), 
                                 #             tabPanel(title = "Género", icon = icon("venus-mars"),
                                 #                      wellPanel(plotOutput(outputId = "genPolarityPlot"))
                                 #                      
                                 #             ), 
                                 #             tabPanel(title = "Ciudad", icon = icon("city"),
                                 #                      wellPanel(plotOutput(outputId = "cityPolarityPlot"))
                                 #                      
                                 #             )
                                 #             
                                 # )
                             
                                             )
                                     )
                                   )
                                 )
                               
                        ),
                        
                        tabPanel( title="Resultados de polaridad", id = "resulOnlineTab",
                                  conditionalPanel( condition="output.datasetOnlineChosen",
                                                    
                                                    h5(textOutput("descriptionTableOnline")),
                                                    br(),
                                                    downloadButton("saveMetaBtnOnline", "Descargar detalle de archivo."),
                                                    br(), br(),
                                                    wellPanel(
                                                      fluidRow(
                                                        column( width = 12,
                                                                DT::dataTableOutput(outputId = "twitterData")
                                                        )
                                                      )
                                                      
                                                    )
                                  )
                                  
                        )
                        
            )
            
            
            
            
            
    ),

    tabItem(tabName = "configuration",
      titlePanel("Configuración de diccionarios", windowTitle = "Configuración de diccionarios"),
        tabsetPanel(type = "tabs", id= "tabsetPanel",
          tabPanel(title ="Diccionario de sentimientos",
                   br(),
                   #checkboxInput("showData", "Mostrar datos", value = FALSE),
                   #actionButton("add_btn", "Agregar"),
                   actionButton("newWord", "Agregar"),
                   actionButton("delete_btn", "Eliminar"),
                   div(style="display:inline-block;",
                    actionButton("edit_btn", "Editar"), 
                    helpPopup("Presione botón editar y realizar doble click sobre la linea que necesita modificar.")
                   ),
                   actionButton("save_btn", "Guardar"),
                   
                   span(textOutput("newMappedWord"), style="color:red"),
                   br(),
                   br(),
                   wellPanel(
                     
                     fluidRow(
                       column( width = 12, 
                             
                               fileInput("importedDictFile", 
                                         label = "Cargar nuevo diccionario principal",
                                         buttonLabel = "Buscar diccionario",
                                         
                                         multiple = FALSE, 
                                         accept =c("csv",
                                                   "text/comma-separated-values",
                                                   ".csv")),
                               div(style="display:inline-block;",
                                   actionButton("importDictionary", "Cargar Diccionario"), 
                                   helpPopup("Seleccione un archivo para importar un nuevo diccionario principal.")
                               ),
                               downloadButton("savePrimaryDictionaryBtn", "Descargar diccionario.")
                       )
                     )
                   ),
                   br(),
                   br(),
                   DT::dataTableOutput(output = "dictionary_ec")
                   
          ),
          tabPanel(title = "Diccionario cambio de sentimientos",
                 
                     
                   br(),
                  # checkboxInput("showDataShifValence", "Mostrar datos", value = FALSE),
                   actionButton("add_btnShiftValence", "Agregar"),
                   actionButton("delete_btnShiftValence", "Eliminar"),
                   div(style="display:inline-block;", actionButton("editShitfValence_btn", "Editar"),
                       helpPopup("Presione botón editar y realizar doble click sobre la linea que necesita modificar.")
                   ),
                  
                     actionButton(
                       "saveShiftValence_btn",
                       "Guardar"
                     ),
                  
                  
                   span(textOutput("newMappedWordShif"), style="color:red"),
                  # actionButton("save_btnShiftValence", "Guardar"),
                   br(),
                   br(),
                  wellPanel(
                    
                    fluidRow(
                      column( width = 12, 
                              
                              fileInput("importedDictValenceFile", 
                                        label = "Cargar nuevo diccionario de cambios de sentimientos",
                                        buttonLabel = "Buscar diccionario",
                                        
                                        multiple = FALSE, 
                                        accept =c("csv",
                                                  "text/comma-separated-values",
                                                  ".csv")),
                              div(style="display:inline-block;",
                                  actionButton("importValenceDictionary", "Cargar Diccionario"), 
                                  helpPopup("Seleccione un archivo para importar un nuevo diccionario de cambios de sentimientos.")
                              ),
                              downloadButton("saveValenceDictionaryBtn", "Descargar diccionario.")
                      )
                    )
                  ),
                  
                  br(),
                  br(),
                   DT::dataTableOutput(output = "dictionary_ec_ShiftValence")
                   
                   )
          # tabPanel(title = "Cargar  nuevas palabras",
          #          br(),
          #          fileInput("fileLoadedNewWords", label = "Archivo",
          #                    buttonLabel = "Cargar archivo",
          #                    accept =c("csv",
          #                              "text/comma-separated-values",
          #                              ".csv")),
          #          checkboxInput("showDataNewDictionary", "Mostrar datos", value = FALSE),
          #          actionButton("add_btnNewWord", "Agregar"),
          #          actionButton("delete_btnNewWord", "Eliminar"),
          #          actionButton("save_btnNewWord", "Guardar"),
          #          br(),
          #          br(),
          #          DT::dataTableOutput(output = "dictionary_ec_NewWords")
          #          
          # )
        )
      ),
    
    tabItem(tabName = "help",
            titlePanel("Procedimiento carga archivo local", windowTitle = "Ayuda"),
            h5("En los puntos detallados a continuación se explica el procedimiento para realizar el Análisis de sentimiento para datos locales"),
            titlePanel("1. Creación de archivo plano", windowTitle = "Procedimiento para crear Dataset local"),
            h5("El archivo plano debe tener 8 columnas y corresponde al formato de un archivo CSV. La información corresponde a comentarios sobre un producto o servicio que han sido emitido en la Red social Twitter."),
            h5(textOutput("description")), # Fifth level header: textOutput("description"),
            img(src = formatFilePlainDemo, height = "250px"),
            h5("La descripción de cada columna se detalla a continuación:"),
            
            h5(tags$b("Columna A:"), "corresponde al texto que se procederá al análisis de sentimiento."),
           
            h5(tags$b("Columna B:"), "fecha del tweet con el formato aaaa/mm/dd."),
            h5(tags$b("Columna C:"), "nombre de usuario correspondiente a Twitter."),
           
            h5(tags$b("Columna D:"), "nombre del usuario."),
            h5(tags$b("Columna E:"), "apellido del usuario."),
            h5(tags$b("Columna F:"), "ciudad donde se emitió el tweet."),
            h5(tags$b("Columna G:"), "país donde se emitió el tweet."),
            h5(tags$b("Columna H:"), "género de la persona que emitió el tweet."),
            h5("Con la información ingresada en la parte anterior, es neceario guardar el archivo con el tipo de formato csv."),
            h5(textOutput("detailsDemo")),
            img(src = formatInternoArchivo, height = "100px"),
            titlePanel("2. Subida de archivo plano al sistema", windowTitle = "Procedimiento para cálculo de análisis de sentimiento"),
            h5("En la parte superior izquierda, hacer click sobre Datos locales, a continuación nos presenta la pestaña para cargar el archivo generado en el paso anterior."),
            img(src = pestanaDatosLocal, height = "200px"),
            h5("Click sobre el botón Cargar archivo y seleccionar el archivo plano"),
            img(src = loadLocalFile, height = "200px"),
            h5("Revisamos los datos locales cargados desde la pestaña Datos locales"),
            img(src = localData, height = "300px"),
            h5("Para realizar el Análisis de sentimiento presionamos el botó  Análisis"),
            img(src = analisysScreen, height = "350px")
            
    )
    
  )
)

# footer <- dashboardFooter(
#   left_text = "Por Ismael Utitiaj A.",
#   right_text = "Ecuador, 2020" 
# )

#permite enviar las secciones al estilo dashboard
tagList(
  shinydisconnect::disconnectMessage2(),
  useShinyjs(),
  tags$head(
    tags$script(src = "saet.js"),
   tags$link(href = "style.css", rel = "stylesheet")
  ),
  div(id = "loading-content", "Cargando...",
      img(src = "ajax-loader-bar.gif")),
  hidden(
    div(id = "errorDiv",
        div(icon("exclamation-circle"),
            tags$b("Error: "),
            span(id = "errorMsg")
        )
    )
  ),
 
  ui <- dashboardPage(header, sideBar, body, skin = "purple")
)

                      


