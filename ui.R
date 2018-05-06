library(shiny)
library(leaflet)



fluidPage(
  theme = "bootstrap.css",
  
  tags$head(tags$style(".shiny-plot-output{height:90vh !important;}",
                       "h1{font-size:3rem !important;}",
                       "h2{font-size:2.5rem !important;}",
                       "body{font-size:160% !important;}",
                       ".btn{font-size:110% !important;}",
                       ".form-control{font-size:100% !important;}")),
   
  title = "Cover crop biomass estimates",
  
  tags$h1("Cover crop biomass estimates"),
  tags$hr(),

  column(
    3,
    wellPanel(
      textInput('lastname', label = NULL, 
                placeholder = "Enter your last name"),
      uiOutput('fieldinfo'),
      uiOutput('missingerror'),
      uiOutput('ll_error'),
      leafletOutput("map"),
      tags$small(icon("copyright"), "ESRI,", 
                 icon("copyright"), "OpenStreetMap, Stamen", 
                 icon("creative-commons"), "BY3.0",
                 style = "float: right;"),
      tags$br(),
      actionButton("reset", tags$small(" Reset View"), 
                   icon = icon("refresh", class = "fa-xs"),
                   class = "btn btn-success btn-sm")
    ),
    htmlOutput("freshwt_text"),
    tags$div("See any problems? ", 
             uiOutput("errorreport", inline = T),
             class = "alert alert-secondary"),
    tags$br()
  ),
  
  
  column(3, h2(tags$strong("Dry matter")), htmlOutput("drywt_text")),      
  column(3, plotOutput('dry_plot')),
      
  column(3, h2(tags$strong("Nitrogen")), htmlOutput("nwt_text"))
  
)