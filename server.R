
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
  library(leaflet)
  library(viridis)
  library(patchwork)
  library(withr)
  library(htmltools)
  sapply(list.files(pattern="[.]R$", path="R/", full.names=TRUE), source)

  
  #######################################################################################################
  # Server
  #######################################################################################################
  
  server <- function(input, output, session) {
    
    # Activating the parameters required for processing and plotting the speed data -----------------------
    
    # Getting the data file
    data <- reactive({
      req(input$upload)
      
      if (tools::file_ext(input$upload) == "csv" && sum(str_count(names(read.csv2(file = input$upload$datapath, sep = ",", head = TRUE)), "SPEED")) == 1) {
        read.csv2(file = input$upload$datapath, sep = ",", head = TRUE, dec = ".") %>% gps_file_prep_qstarz()
      
      } else if (tools::file_ext(input$upload) == "CSV" && sum(str_count(names(read.csv2(file = input$upload$datapath, sep = ",", head = TRUE, skip = 2)), "Speed..km.h.")) == 1) {
        read.csv2(file = input$upload$datapath,  sep = ",", head = TRUE, dec = ".", skip = 2) %>% gps_file_prep_polar() 
        
      } else {
      readGPX(input$upload$datapath)$tracks[[1]][[1]] %>% gps_file_prep_dg100()
      }
        
      })
    
    # Setting the time period and the paramaters required for computing the speed data filter
    Filter_Start <- eventReactive(input$update, {
      (input$FilterStart)
    })
    
    Filter_End <- eventReactive(input$update, {
      (input$FilterEnd)
    })
    
    observeEvent(input$update,
                 shinyFeedback::feedbackWarning(
                   "FilterEnd", 
                   round(input$FilterEnd) < round(input$FilterStart),
                   "Please choose a number for the end of the period that is higher than that for the start of the period."
                 )
    )
    
    mean_speed <- eventReactive(input$update, {
      gps_filters(data = data(), filter_start_sec = Filter_Start(), filter_end_sec = Filter_End())$mean_speed
    })
    
    sd_speed <- eventReactive(input$update, {
      gps_filters(data = data(), filter_start_sec = Filter_Start(), filter_end_sec = Filter_End())$sd_speed
    })
    
    # Computing the CV of speed
    output$CV <- renderPrint({
      cat("The CV of speed is", round(sd_speed() / mean_speed() * 100, 2), "%.")
    })
    
    
    # Creating the reactive speed data filter and the reactive plot for the session data ------------------------
    
    # Setting the speed data filter
    LPF <- reactive({
      input$LPF * mean_speed()
    })
    
    HPF <- reactive({
      mean_speed() - input$HPF * sd_speed()
    })
    
    # Getting the reactive dataset with the processed speed
    data_proc <- reactive({
      speed_proc(data = data(), LPF = LPF(), HPF = HPF())
    })
    
    # Getting the reactive plots with the coordinates and the processed speed
    bout_duration <- reactive(input$min_duration)
    
    p <- reactive ({
      req(input$upload)
      p <-  plot_gps_coord(data = data_proc(), mininum_bout_duration_s = bout_duration())
    })
    
    # Displaying the reactive plots with the coordinates and the processed speed
    output$map <- renderLeaflet({
      p()[[1]]
    })
    
    output$coord <- renderPlotly({
      subplot(
        toWebGL(ggplotly(p()[[2]], tooltip = c("Bout", "Seconds", "Altitude"))),
        toWebGL(ggplotly(p()[[3]], tooltip = c("Bout", "Seconds", "Longitude"))),
        toWebGL(ggplotly(p()[[4]], tooltip = c("Bout", "Seconds", "Latitude"))),
        toWebGL(with_options(list(digits = 1), ggplotly(p()[[5]], tooltip = c("Bout", "Seconds", "Speed.proc", "Speed"))) %>% 
          layout(hovermode = "x", spikedistance = 100,
                 xaxis = list(
                   showspikes = TRUE,
                   spikemode  = 'across+toaxis',
                   spikethickness=1, 
                   spikesnap = 'cursor',
                   spikedash = 'solid',
                   showline=TRUE,
                   showgrid=TRUE,
                   spikedistance = -1))), 
        shareX = TRUE, titleY = TRUE, nrows = 4) %>% 
        config(displayModeBar = FALSE) 
    })
    
    
    # Getting the results -----------------------------------------------------------------------------
    
    # Activating the reactive dataset that summarizes the walking and stopping periods
    Periods <- eventReactive(input$Run, {
      get_periods(data = data_proc(), mininum_bout_duration_s = bout_duration())
    })
    
    # Creating the reactive table with the detected walking and stopping bouts
    Selection <- reactive({
      Periods() %>%
        rename(Activity = mark,
               "Bout ID" = bout,
               "Start time" = start_time,
               "End time" = end_time,
               "Duration (s)" = duration,
               "Mean speed (km/h)" = speed,
               "Distance (m)" = distance)
    })
    
    # Displaying the reactive table with the detected walking and stopping bouts
    output$Selection <- renderReactable({
      reactable(Selection(), columns = list(
        Activity = colDef(minWidth = 60),
        "Bout ID" = colDef(minWidth = 60),
        "Mean speed (km/h)" = colDef(minWidth = 150)
      )
      )
    })
    
    # Creating the reactive table of the final results
    include <- reactive(input$IncludeWalk)
    
    results <-  eventReactive(input$Run, {
      if (include() == FALSE) { answer <- "no" 
      } else { answer <- "yes"}
      
      get_results_tab(periods = Periods(), mininum_bout_duration_s = bout_duration(), include_last_walk = answer)
    })
    
    # Displaying the reactive table of the final results
    output$TableResults <- renderReactable({
      reactable(results(), defaultColDef = colDef(minWidth = 220))
    })
    
    # Creating the reactive plot of the final results
    plot <- eventReactive(input$Run, {
      if (include() == FALSE) { answer <- "no" 
      } else { answer <- "yes"}
      
      get_results_plot(data = Periods(), include_last_walk = answer)
      
    })
    
    # Displaying the reactive plot of the final results
    output$PlotResults <- renderPlot({
      plot()
    })  
    
    # Downloading results  
    output$download <- downloadHandler(
      filename = function() {
        paste0(input$upload, "_results.csv")
      },
      content = function(file) {
        write_csv2(results(), file)
      }
    )
    
    
    # Reset app ------------ ------------------------------------------------------------------------
    observeEvent(input$reset,{
      aggg_result = -1
      if(aggg_result == -1)
      {
        session$reload()
        return()
      }
    })
    
  }





