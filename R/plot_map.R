
plotly_map <- function(data) {
  
  df_proc_marked <- data
  types <- filter(df_proc_marked, mark == "walk")$Bout %>% unique
  pal <- leaflet::colorFactor(viridis_pal(option = "D")(nlevels((df_proc_marked$Bout))), domain = types)
  icon_start <- awesomeIcons(
    icon = 'ios-close',
    iconColor = 'black',
    library = 'ion',
    markerColor = "green")
  
  icon_end <- awesomeIcons(
    icon = 'ios-close',
    iconColor = 'black',
    library = 'ion',
    markerColor = "red")
  
  labels <- sprintf(
    "<strong>%s</strong><br/>%g",
    "Seconds:", df_proc_marked$Seconds
  ) %>% lapply(htmltools::HTML)
  
  filter(df_proc_marked) %>% 
    leaflet() %>% 
    addTiles() %>% 
    addCircleMarkers(lng = ~Longitude, lat = ~Latitude, radius =1, color = ~pal(Bout), stroke = TRUE, label = labels) %>%
    addAwesomeMarkers(data = filter(df_proc_marked, mark == "walk")[1, ], lng = ~Longitude, lat = ~Latitude, label = htmlEscape("Start"), 
                      icon = icon_start) %>% 
    addAwesomeMarkers(data = filter(df_proc_marked, mark == "walk")[length(filter(df_proc_marked, mark == "walk")$mark), ], 
                      lng = ~Longitude, lat = ~Latitude, label = htmlEscape("End"), icon = icon_end)

}

    
        
   
    
  