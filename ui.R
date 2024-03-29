
#######################################################################################################
# Required packages and functions
#######################################################################################################

library(shiny)
library(shinyFeedback)
library(plotly)
library(patchwork)
library(reactable)
library(tidyverse)
library(lubridate)
library(plotKML)
library(leaflet)
library(viridis)
library(withr)
library(htmltools)
library(Rcpp)
library(shinycssloaders)
sapply(list.files(pattern="[.]R$", path="R/", full.names=TRUE), source)
sourceCpp("R/cppfunc.cpp")

#######################################################################################################
# UI
#######################################################################################################
  
  ui <- fluidPage(
    tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
    shinyFeedback::useShinyFeedback(),
    titlePanel("GPS-WALK"),
    fluidRow(wellPanel("The app allows the analysis of GPS data obtained during an outdoor walking session and provides 
    summarized results (eg, number of detected walking bouts, mean speed over the walking bouts, total walking 
    distance over the session, longest distance performed during a walking bout, etc.). The analysis consists of the methodology 
    proposed by Le Faucheur et al. (2007; DOI: 10.1249/mss.0b013e3180cc20c7). Briefly, the user must select a period of time (120 s if possible, or shorter) 
    to compute a representative mean and SD of speed for the session. Then, based on the coefficient of variation (CV) of speed,
    the user must configure a first filter (HPF) for the speed data (choose HPF = 2 when the CV is >15%, or HPF = 5 when the CV is <=15%, 
    or even higher than 5 when the CV is very low (eg, <5%); the value to choose for the second filter (LPF) is 2 in principle). To determine the period of time to be used to compute the CV of speed
    and then the HPF, the user is helped by an interactive graphic of the coordinates and processed speed that are plotted against time, with the detected walking bouts 
    that are highlighted with colors. The app also provides a map with the positions measured during the session and that are colored in 
    relation to the corresponding detected walking bouts. Be carrefull of the altitude data that may be not accurate with non-differential GPS devices.
    When computing the final results, the user can choose to include or not the last detected walking bout for the calculation of the mean,
             cv, min, and max of speed, walking distance, and walking time over the walking bouts. For now, the app can be used with 1-Hz .gpx DG100 files, .csv Qstarz files, and .CSV V800/M430 files (note: latitude and longitude are not available for .CSV V800/M430 files). 
                       Please reset the app before analysing a new file, and see", tags$a(href="https://github.com/pydemull/gps-walk", "https://github.com/pydemull/gps-walk"), "for the full code of the app.
                       For a website allowing faster analysis (without manual analysis) for both GPS and accelerometer data, but without context information (map, coordinates), 
                       see the following website:", tags$a(href="https://mapam.ens-rennes.fr/", "https://mapam.ens-rennes.fr."))),
    
    # First layout: Data loading ----------------------------------------------------------------------------------------------
    fluidRow(
      column(4,
             wellPanel(
             fileInput("upload", NULL, placeholder = "Choose file..."),
             "Please wait until seeing 'Upload complete'."
             ),

    # Second layout, left: Configuration of the period to use for setting the speed filters------------------------------------
             wellPanel(
               numericInput("FilterStart", "Start of the period to consider to configure the speed filter (s)", value = 0),
               numericInput("FilterEnd", "End of the period to consider to configure the speed filter (s)", value = 120),
             ),
             wellPanel(
               sliderInput("HPF", "HPF (multiple of speed SD)", value = 5, min = 0, max = 20, step = 1),
               sliderInput("LPF", "LPF (mulitple of mean speed)", value = 2, min = 1, max = 3, step = 0.5)
             ),
      ),

   # Second layout, right: Plot for the map --------------------------------------------------------------------------------------
      column(8,
             withSpinner(leafletOutput("map", height = "600px")),
      )
    ),
    
    # Third layout, left : Configuration of the coefficients to use for setting the speed filters and the minimum bout duration ----
    fluidRow(
      column(4,
             wellPanel(
               numericInput("min_duration", "Minimum bout duration (s)", value = 0, min = 0, max = 60)
             ),
             wellPanel(
               textOutput("CV"), style = "padding-left:18px;padding-top:80px;font-size: 30px"
             ),
             wellPanel(
               actionButton("update", "View data / Update Period", class = "btn-success", width = "100%")
             ),
             wellPanel(
               checkboxInput("IncludeWalk", "Include the last walk in analysis", value = FALSE),
               div(style="display:inline-block",  actionButton("Run", "Run / Update Analysis", class = "btn-success")),
               div(style="display:inline-block", downloadButton("downloadRes", label = "Download Final Results", style = "width:100%;")),
               div(style="display:inline-block", downloadButton("downloadPer", label = "Download Periods", style = "width:100%;"))
             )
      ),

    # Third layout, right: Plots for the coordinates and speed -------------------------------------------------------------------
      column(8,
             withSpinner(plotlyOutput("coord", height = "500px"))
      ),
    ),
    
    # Fourth layout: Table and plot for the detected periods --------------------------------------------------------------------
    fluidRow(
      column(6,
             reactableOutput("Selection")
      ),
      column(6,
             plotOutput("PlotResults")
      )
    ),
    
    # Fifth layout: Final results ----------------------------------------------------------------------------------------------
    fluidRow(
      column(12,
             reactableOutput("TableResults")
      )
    ),
    
    # Sixth layout: Reset ------------------------------------------------------------------------------------------------------
    fluidRow(
      column(4,
             wellPanel(
               actionButton('reset',"Reset App",  class = "btn-danger", width = "100%")
             )
      )
    )
  )

  
  
 

