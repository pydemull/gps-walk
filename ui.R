
  #######################################################################################################
  # Required packages and functions
  #######################################################################################################
  
  library(shiny)
  library(shinyFeedback)
  library(plotly)
  library(reactable)
  library(tidyverse)
  library(lubridate)
  library(plotKML)
  library(viridis)
  library(leaflet)
  library(patchwork)
  library(withr)

  #######################################################################################################
  # UI
  #######################################################################################################
  
  ui <- fluidPage(
    tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
    shinyFeedback::useShinyFeedback(),
    titlePanel("A shiny app to analyse walking GPS data"),
    fluidRow(wellPanel("The app allows the analysis of GPS data obtained during an outdoor walking session and provides 
    summarized results (eg, number of detected walking bouts, mean speed over the walking bouts, total walking 
    distance over the session, longest distance performed during a walking bout, etc.). The analysis consists of the methodology 
    proposed by Le Faucheur et al. (2007; DOI: 10.1249/mss.0b013e3180cc20c7). Briefly, the user must select a period of time (120 s if possible, or shorter) 
    to compute a representative mean and SD of speed for the session. Then, based on the coefficient of variation (CV) of speed,
    the user must configure the high pass filter (HPF) for the speed data (choose HPF = 2 when the CV is >15%, or HPF = 5 when the CV is <=15%, 
    or even higher than 5 when the CV is very low (eg, <5%); the value to choose for the low pass filter (LPF) is 2 in principle). To determine the period of time to be used to compute the CV of speed
    and then the HPF, the user is helped by a reactive graphic of the coordinates and processed speed that are plotted against time, with the detected walking bouts 
    that are highlighted with colors. The app also provides a map with the positions measured during the session and that are colored in 
    relation to the corresponding detected walking bouts. Be carrefull of the altitude data that may be not accurate with non-differential GPS devices.
    When computing the final results, the user can choose to include or not the last detected walking bout for the calculation of the mean,
             cv, min, and max of speed, walking distance, and walking time over the walking bouts. For now, the app can be used with .gpx DG100 files, 
                       and .csv Qstarz files. Please see", tags$a(href="https://github.com/PYDM1989/gps-walk", "https://github.com/PYDM1989/gps-walk"), "for the full code of the app.
                       For a website allowing faster analysis (without manual analysis) for both GPS and accelerometer data, but withtout context information (map, coordinates), 
                       see the following website:", tags$a(href="https://mapam.ens-rennes.fr/", "https://mapam.ens-rennes.fr."))),
    
    # First layout: Data loading ------------------------------------------------------------------------
    fluidRow(
      column(3,
             fileInput("upload", NULL, placeholder = "Choose file...")
      ),
    ),
    
    # Second layout: Parameters for configurating the analysis of the speed data -------------------------
    fluidRow(
      column(4,
             wellPanel(
               numericInput("FilterStart", "Start of the period to consider to configure the speed filter (s)", value = 0),
               numericInput("FilterEnd", "End of the period to consider to configure the speed filter (s)", value = 120),
               actionButton("update", "View data / Update Period", class = "btn-success", width = "100%")
             ),
             wellPanel(
               textOutput("CV"), style = "padding-left:18px;padding-top:80px;font-size: 40px"
             ),
      ),
      
   # Second layout: Plot for the map -------------------------------------------------------------------
      column(8,
             leafletOutput("map", height = "400px"),
      )
    ),
    
    # Third layout : Parameters for configurating the analysis of the speed data ----------------------
    fluidRow(
      column(4,
             wellPanel(
               sliderInput("HPF", "High Pass Filter (multiple of speed SD)", value = 5, min = 0, max = 20, step = 1),
               sliderInput("LPF", "Low Pass Filter (mulitple of mean speed)", value = 2, min = 1, max = 3, step = 0.5),
               numericInput("min_duration", "Minimum bout duration (s)", value = 15, min = 0, max = 60)
             ),
             wellPanel(
               checkboxInput("IncludeWalk", "Include the last walk in analysis", value = FALSE),
               div(style="display:inline-block",  actionButton("Run", "Run / Update Analysis", class = "btn-success")),
               div(style="display:inline-block", downloadButton("download", label = "Download Final Results", style = "width:100%;"))
             )
      ),

    # Third layout bis: Plots for the coordinates and speed --------------------------------------------
      column(8,
             plotlyOutput("coord", height = "500px")
      ),
    ),
    
    # Fourth layout: Table and plot for the detected periods ---------------------------------------------
    fluidRow(
      column(6,
             reactableOutput("Selection")
      ),
      column(6,
             plotOutput("PlotResults")
      )
    ),
    
    # Fifth layout: Final results -------------------------------------------------------------------------
    fluidRow(
      column(12,
             reactableOutput("TableResults")
      )
    ),
    
    # Sixth layout: Reset ---------------------------------------------------------------------------------
    fluidRow(
      column(4,
             wellPanel(
               actionButton('reset',"Reset App",  class = "btn-danger", width = "100%")
             )
      )
    )
  )

  
  




