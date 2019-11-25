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
#library(ggthemr) 

#agregar imagen
formatFilePlainDemo <- base64enc::dataURI(file="AyudaArchivo.png", mime="image/png")
formatInternoArchivo <- base64enc::dataURI(file="formatoCSV.png", mime="image/png")
pestanaDatosLocal <- base64enc::dataURI(file="pestanaDatosLocal.png", mime="image/png")
loadLocalFile <- base64enc::dataURI(file="loadLocalFile.png", mime="image/png")
localData <- base64enc::dataURI(file="localData.png", mime="image/png")
analisysScreen <- base64enc::dataURI(file="analisysScreen.png", mime="image/png")


header <- dashboardHeader(dropdownMenuOutput("task_menu" ), title = "Products & Services Trends", titleWidth =  280) 
                          
sideBar <- dashboardSidebar( width = 280,
  
  sidebarMenu( 
    
    menuItem("Datos locales", tabName = "analisys"),
    menuItem("Geolocalización", tabName = "geolocalizacion"),
    menuItem("Ayuda", tabName = "help")
  )
  
  
  
)

body <- dashboardBody( 
  
  tabItems( 
    tabItem(tabName = "analisys",
            titlePanel("Carga de archivo", windowTitle = "Análisis sentimiento Twitter"),
            tabsetPanel(type = "tabs", id= "tabsetPanelLocalData",
                        tabPanel(title ="Análisis sentimiento", value = 1,
                                 wellPanel(
                                   fluidRow(
                                     column( width = 12,
                                             box( width = NULL,collapsible = TRUE,
                                                  
                                                  fileInput("fileLoaded", label = "Archivo",
                                                            buttonLabel = "Cargar archivo",
                                                            accept =c("csv",
                                                                      "text/comma-separated-values",
                                                                      ".csv"))
                                                  #radioButtons("showData", "Mostrar datos", choices = c("Si"=1, "No"=2), selected = 2)
                                             )
                                     ))
                                   
                                   
                                 ),
                                 wellPanel(
                                   fluidRow(
                                     column( width = 12,
                                             box( width = NULL,collapsible = TRUE, 
                                                  
                                                  textInput("geoLocalSearch", label = "Texto de búsqueda", placeholder = "Ingrese producto o servicio"),
                                                  
                                                  #selectInput("citySelected", "Ciudades Test", choices = c("quito", "Guayaquil", "Cuenca")),
                                                  htmlOutput("CitiesLoaded"),
                                                  #radioButtons("genero", "Genero", choices = c("Femenino", "Masculino")), 
                                                  dateRangeInput("fromToDate", language = 'es', label = "Rango fecha", separator = "hasta", weekstart = 1),
                                                  p(),
                                                  actionButton("calcSentiment", label = "Análisis")
                                                  
                                             )
                                     ))
                                   
                                   
                                 ),
                                 titlePanel("Análisis general"),
                                 tabsetPanel(type = "tabs", id= "tabsetLocalResult", 
                                             tabPanel(title = "Trazado a nivel de oración", 
                                             wellPanel(
                                               fluidRow(
                                                 
                                                 tableOutput(outputId = "dataTweets"),
                                                 plotOutput(outputId = "scatterplot")
                                               ))),
                                             tabPanel(title = "Diagrama de barras", 
                                                      wellPanel(
                                                        fluidRow(
                                                          plotOutput(outputId = "barPlotPolarity")
                                                        )
                                                      )
                                                      
                                             ), 
                                             tabPanel(title = "Matriz de término de documentos", 
                                                      wellPanel(
                                                        fluidRow(
                                                          plotOutput(outputId = "barPlotTDM")
                                                        ))
                                                      
                                             ), 
                                             tabPanel(title = "Nube de palabras", 
                                                      wellPanel(
                                                        fluidRow(
                                                          plotOutput(outputId = "wordCloudPlot")
                                                        ))
                                                      
                                             ),
                                             tabPanel(title = "Geolocalización", 
                                                      wellPanel(
                                                        fluidRow(
                                                          leafletOutput(outputId = "geoMapLocal")
                                                        ))
                                                      
                                             ),
                                             tabPanel(title = "Mapa de calor", 
                                                      wellPanel(
                                                        fluidRow(
                                                          plotOutput(outputId = "heatMapLocalData")
                                                        ))
                                                      
                                             ),
                                             tabPanel(title = "Grafico Lollipop", 
                                                      wellPanel(
                                                        fluidRow(
                                                          plotOutput(outputId = "lollipopPlot")
                                                        ))
                                                      
                                             ),
                                             tabPanel(title = "Conteo palabras de sentimiento", 
                                                      wellPanel(
                                                        fluidRow(
                                                          plotOutput(outputId = "sentimenWordCountsPlot")
                                                        ))
                                                      
                                             )
                                    )               
                            ),
                           # conditionalPanel( condition="input.showData == 1",
                            #                 wellPanel(
                           tabPanel( title="Datos locales", value = 2,
                                     br(),
                                  
                                     h5(textOutput("descriptionTable")),
                                     br(),
                                     fluidRow(
                                        DT::dataTableOutput(outputId = "twetterDataLocal")
                                       )
                                             
                                     
                           )
                                             #)
                             
                            
            
            )
    ),
    tabItem(tabName = "geolocalizacion",
            titlePanel("Geolocalización Twitter", windowTitle = "Análisis sentimiento Twitter por Geolocalización"),
            tabsetPanel(type = "tabs", id= "tabsetPanel",
                        tabPanel(title ="Geolocalización", 
                                 
                                 wellPanel(
                                   
                                   fluidRow(
                                     column( width = 12,
                                             box( width = NULL,collapsible = TRUE,
                                                  
                                                  textInput("geoSearch", label = "Texto de búsqueda", placeholder = "Ingrese producto o servicio"),
                                                  
                                                  dateRangeInput("geoFromToDate", language = 'es', label = "Rango fecha", separator = "hasta", weekstart = 1),
                                                  numericInput("geoRatio", "Cobertura(km)", min = 0, max = 500,value = 10, width = 200),
                                                  
                                                  actionButton("calcGeoSentiment", label = "Análisis"),
                                                  p(),
                                                  leafletOutput("EcuadorMap")
                                                  
                                             )
                                     )
                                     
                                   )
                                   
                                 ),
                                
                                 titlePanel("Análisis general", windowTitle = "Resultado análisis sentimiento"),
                                 tabsetPanel(type = "tabs", id= "tabsetResult", 
                                             tabPanel(title = "General", 
                                                      
                                                      wellPanel(plotOutput(outputId = "barplot"))
                                                      
                                             ), 
                                             tabPanel(title = "Día", 
                                                      wellPanel(plotOutput(outputId = "dayPlot"))
                                                      
                                             ), 
                                             tabPanel(title = "Género", 
                                                      wellPanel(plotOutput(outputId = "genPolarityPlot"))
                                                      
                                             ), 
                                             tabPanel(title = "Ciudad", 
                                                      wellPanel(plotOutput(outputId = "cityPolarityPlot"))
                                                      
                                             )
                                             
                                 )
                                 
                                
                                 
                        ),
                        
                        tabPanel( title="Datos",
                                  br(),
                                  DT::dataTableOutput(outputId = "twitterData")
                                  
                        )
                        
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

#permite enviar las secciones al estilo dashboard
ui <- dashboardPage(header, sideBar, body, skin = "purple")


