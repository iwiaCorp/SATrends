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
formatFilePlainDemo <- base64enc::dataURI(file="formatoArchivo.png", mime="image/png")

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
                                                  
                                                  
                                                  #selectInput("citySelected", "Ciudades", choices = c("Quito", "Guayaquil", "Cuenca")),
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
                                                      
                                             )
                                    )               
                            ),
                           # conditionalPanel( condition="input.showData == 1",
                            #                 wellPanel(
                           tabPanel( title="Datos locales", value = 2,
                                     br(),
                                     fluidRow(DT::dataTableOutput(outputId = "twetterDataLocal"))
                                     
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
            titlePanel("Creación archivo plano", windowTitle = "Procedimiento para crear Dataset local"),
            h5(textOutput("description")), # Fifth level header: textOutput("description")
            hr(),
            img(src = formatFilePlainDemo, height = "200px"),
            hr(),
            h5(textOutput("detailsDemo"))
            
    )
    
  )
)

#permite enviar las secciones al estilo dashboard
ui <- dashboardPage(header, sideBar, body, skin = "purple")


