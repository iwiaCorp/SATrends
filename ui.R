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


helpPopup <- function(content, title = NULL) {
  a(href = "#",
    class = "popover-link",
    `data-toggle` = "popover",
    `data-title` = title,
    `data-content` = content,
    `data-html` = "true",
    `data-trigger` = "hover",
    icon("question-circle")
  )
}

# Set up a button to have an animated loading indicator and a checkmark
# for better user experience
# Need to use with the corresponding `withBusyIndicator` server function
withBusyIndicator <- function(button) {
  id <- button[['attribs']][['id']]
  tagList(
    button,
    span(
      class = "btn-loading-container",
      `data-for-btn` = id,
      hidden(
        img(src = "ajax-loader-bar.gif", class = "btn-loading-indicator"),
        icon("check", class = "btn-done-indicator")
      )
    )
  )
}

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
                                   fluidRow( useShinyjs(),
                                     column( width = 12,
                                             box( width = NULL,collapsible = TRUE,
                                                  
                                                  fileInput("fileLoaded", label = "Archivo",
                                                            buttonLabel = "Buscar archivo",
                                                           # div(
                                                          #    helpPopup('Este es un archivo csv con la información descargada de comentarios.')
                                                          #  ),
                                                            
                                                            accept =c("csv",
                                                                      "text/comma-separated-values",
                                                                      ".csv"))
                                                  # withBusyIndicator(
                                                  #   actionButton(
                                                  #     "uploadFilesBtn",
                                                  #     "Upload data",
                                                  #     class = "btn-primary"
                                                  #   )
                                                  # )
                                                  #radioButtons("showData", "Mostrar datos", choices = c("Si"=1, "No"=2), selected = 2)
                                             )#,
                                             # actionButton(
                                             #   "uploadFilesButton",
                                             #   "Cargar datos",
                                             #   class = "btn-primary"
                                             # )
                                             
                                             
                                     )
                                     )
                                   
                                   
                                 ),
                                 wellPanel(
                                   fluidRow(
                                     column( width = 12,
                                             box( width = NULL,collapsible = TRUE, 
                                                  
                                                  #textInput("geoLocalSearch", label = "Texto de búsqueda", placeholder = "Ingrese producto o servicio"),
                                                  
                                                  #selectInput("citySelected", "Ciudades Test", choices = c("quito", "Guayaquil", "Cuenca")),
                                                  htmlOutput("CitiesLoaded"),
                                                  #radioButtons("genero", "Genero", choices = c("Femenino", "Masculino")), 
                                                  dateRangeInput("fromToDate", language = 'es', label = "Rango fecha", separator = "hasta", weekstart = 1),
                                                  p(),
                                                  actionButton("calcSentiment", label = "Análisis")
                                                  
                                             )
                                     ))
                                   
                                   
                                 ),
                                 wellPanel(
                                 
                                   tabsetPanel(type = "tabs", id= "tabsetLocalResult", 
                                               tabPanel(title = "Nivel de polaridad por tweets", icon = icon("chart-line"), 
                                                        wellPanel(
                                                          fluidRow(
                                                            
                                                            tableOutput(outputId = "dataTweets"),
                                                            #plotOutput(outputId = "scatterplot")
                                                            box(
                                                              title = "Tendencia", status = "primary", solidHeader = TRUE,
                                                              collapsible = T,
                                                              plotOutput(outputId = "scatterplot", height = 250)
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
                                                            plotOutput(outputId = "barPlotTDM")
                                                          ))
                                                        
                                               ), 
                                               tabPanel(title = "Nube de palabras", icon  = icon("cloud"),
                                                        wellPanel(
                                                          fluidRow(
                                                            plotOutput(outputId = "wordCloudPlot")
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
                                               
                            ),
                           # conditionalPanel( condition="input.showData == 1",
                            #                 wellPanel(
                           tabPanel( title="Resultados de polaridad", value = 2,
                                     br(),
                                  
                                     h5(textOutput("descriptionTable")),
                                     br(),
                                     downloadButton("saveMetaBtn", "Descargar detalle de archivo."),
                                     br(), br(),
                                     fluidRow(
                                        DT::dataTableOutput(outputId = "twetterDataLocal")
                                       )
                                             
                                     
                           )
                                             #)
                             
                            
            
            )
    ),
    tabItem(tabName = "geolocalizacion",
            titlePanel("Búsqueda de tweets", windowTitle = "Análisis sentimiento Twitter por Geolocalización"),
            tabsetPanel(type = "tabs", id= "tabsetPanel",
                        tabPanel(title ="Geolocalización", 
                                 
                                 wellPanel(
                                   
                                   fluidRow(
                                     column( width = 12,
                                             box( width = NULL,collapsible = TRUE,
                                                  
                                                  textInput("geoSearch", label = "Texto de búsqueda", placeholder = "Ingrese producto o servicio"),
                                                  
                                                  dateRangeInput("geoFromToDate", language = 'es', label = "Rango fecha", separator = "hasta", weekstart = 1),
                                                  numericInput("geoRatio", "Cobertura(km)", min = 0, max = 500,value = 10, width = 200),
                                                  p(),
                                                  leafletOutput("EcuadorMap")
 
                                             )
                                     ),
                                     p(),
                                     actionButton("calcGeoSentiment", label = "Análisis")
                                   )
                                   
                                 ),
                                wellPanel(
                                 titlePanel("Resultados", windowTitle = "Resultado análisis sentimiento"),
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
                                )
                        ),
                        
                        tabPanel( title="Resultados de polaridad",
                                  br(),
                                  DT::dataTableOutput(outputId = "twitterData")
                                  
                        )
                        
            )
            
            
            
            
    ),

    tabItem(tabName = "configuration",
      titlePanel("Configuración de diccionarios", windowTitle = "Configuración de diccionarios"),
        tabsetPanel(type = "tabs", id= "tabsetPanel",
          tabPanel(title ="Diccionario de sentimientos",
                   br(),
                   checkboxInput("showData", "Mostrar datos", value = FALSE),
                   #actionButton("add_btn", "Agregar"),
                   actionButton("newWord", "Agregar"),
                   actionButton("delete_btn", "Eliminar"),
                   #actionButton("save_btn", "Guardar"),
                   span(textOutput("newMappedWord"), style="color:red"),
                   br(),
                   br(),
                   DT::dataTableOutput(output = "dictionary_ec")
                   
          ),
          tabPanel(title = "Diccionario cambio de sentimientos",
                   br(),
                   checkboxInput("showDataShifValence", "Mostrar datos", value = FALSE),
                   actionButton("add_btnShiftValence", "Agregar"),
                   actionButton("delete_btnShiftValence", "Eliminar"),
                   span(textOutput("newMappedWordShif"), style="color:red"),
                  # actionButton("save_btnShiftValence", "Guardar"),
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
ui <- dashboardPage(header, sideBar, body, skin = "purple")
                      


