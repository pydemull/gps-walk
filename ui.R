
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
  library(ggmap)
  library(viridis)
  library(patchwork)
  library(withr)

  
  #######################################################################################################
  # UI
  #######################################################################################################
  
  ui <- fluidPage(
    tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
    shinyFeedback::useShinyFeedback(),
    
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
               actionButton("update", "View data / Update Period", class = "btn-success", width = "100%"),
               fluidRow(textOutput("CV"), style = "padding-left:18px;padding-top:10px;")
             ),
             wellPanel(
               sliderInput("HPF", "High Pass Filter (multiple of speed SD)", value = 5, min = 0, max = 20, step = 1),
               sliderInput("LPF", "Low Pass Filter (mulitple of mean speed)", value = 2, min = 1, max = 3, step = 0.5),
               numericInput("min_duration", "Minimum bout duration (s)", value = 15, min = 0, max = 60)
             ),
      ),
      
      # Second layout: Plots for the session data --------------------------------------------------------
      column(8,
             plotlyOutput("map", height = "250px"),
             plotlyOutput("coord", height = "550px")
      )
    ),
    
    # Third layout: Options for the computation of the results and downloading the data -----------------
    fluidRow(
      column(12,
             wellPanel(
               checkboxInput("IncludeWalk", "Include the last walk in analysis", value = FALSE),
               div(style="display:inline-block",  actionButton("Run", "Run / Update Analysis", class = "btn-success")),
               div(style="display:inline-block", downloadButton("download", label = "Download Final Results", style = "width:100%;"))
             )
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
               actionButton("reset", "Reset Input Parameters", class = "btn-danger", width = "100%")
             )
      )
    )
  )
  
  




