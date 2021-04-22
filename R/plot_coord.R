
plot_coord <- function(data, periods) {
  
  df_proc_marked <- 
    data %>% 
    mutate(Altitude = Altitude - min(Altitude),
           Longitude = Longitude + (0 - min(Longitude)),
           Latitude = Latitude + (0 - min(Latitude)))
  
  Selection <- periods
  
  min_Time <- min(df_proc_marked$Seconds)
  max_Time <- max(df_proc_marked$Seconds)
  min_Alt <- min(df_proc_marked$Altitude)
  max_Alt <- max(df_proc_marked$Altitude)
  min_Lon <- min(df_proc_marked$Longitude)
  max_Lon <- max(df_proc_marked$Longitude)
  min_Lat <- min(df_proc_marked$Latitude)
  max_Lat <- max(df_proc_marked$Latitude)
  min_Speed <- min(df_proc_marked$Speed.proc)
  max_Speed <- max(df_proc_marked$Speed)
  
  col_viridis <- viridis_pal(option = "D")(nlevels(df_proc_marked$Bout))
  
  ele <- 
    plot_ly(df_proc_marked) %>%
    add_lines(x = ~Seconds, y = max_Alt, fill = 'tozeroy', color = ~Bout, colors = col_viridis, showlegend = F, hovertemplate='Bout:') %>%
    add_lines(x = ~Seconds, y = ~Altitude, color = I("black"), fill = 'tozeroy', fillcolor = "grey", showlegend = F, hovertemplate='Seconds: %{x} <br>Altitude: %{y}<extra></extra>') %>%
    add_lines(x = ~Seconds, y = max_Alt, color = I("black"), hoverinfo='skip', showlegend = F) %>%
    add_segments(x = Selection[["start_new"]], xend = Selection[["start_new"]], y = min_Alt, yend = max_Alt, color = I("red"), showlegend = F, hoverinfo='skip') %>%
    add_segments(x = Selection[["end_new"]], xend = Selection[["end_new"]], y = min_Alt, yend = max_Alt, color = I("red"), showlegend = F, hoverinfo='skip') %>%
    layout(xaxis = list(title = "", range = c(min_Time, max_Time), showline = TRUE, showticklabels = FALSE, mirror = "ticks"), 
           yaxis = list(title = "Rescaled Alt. (m)", range = c(min_Alt, max_Alt), showline = TRUE, mirror = "ticks"))
  lon <- 
    plot_ly(df_proc_marked) %>%
    add_lines(x = ~Seconds, y = max_Lon, fill = "tozeroy", color = ~Bout, colors = col_viridis, showlegend = F, hovertemplate='Bout:') %>%
    add_lines(x = ~Seconds, y = ~Longitude, color = I("black"), showlegend = F, hovertemplate='Seconds: %{x}<extra></extra>') %>%
    add_lines(x = ~Seconds, y = max_Lon,  color = I("black"), hoverinfo='skip', showlegend = F) %>%
    add_segments(x = Selection[["start_new"]], xend = Selection[["start_new"]], y = min_Lon, yend = max_Lon, color = I("red"), showlegend = F, hoverinfo='skip') %>%
    add_segments(x = Selection[["end_new"]], xend = Selection[["end_new"]], y = min_Lon, yend = max_Lon, color = I("red"), showlegend = F, hoverinfo='skip') %>%
    layout(xaxis = list(title = "", range = c(min_Time, max_Time), showline = TRUE, showticklabels = FALSE, mirror = "ticks"), 
           yaxis = list(title = "Lon.",  range = c(min_Lon, max_Lon), showline = TRUE, showticklabels = FALSE, mirror = "ticks"))
  lat <- 
    plot_ly(df_proc_marked) %>%
    add_lines(x = ~Seconds, y = max_Lat, fill = 'tozeroy', color = ~Bout, colors = col_viridis, showlegend = F, hovertemplate='Bout:') %>%
    add_lines(x = ~Seconds, y = ~Latitude, color = I("black"), showlegend = F, hovertemplate='Seconds: %{x}<extra></extra>') %>%
    add_lines(x = ~Seconds, y = max_Lat, color = I("black"), hoverinfo='skip', showlegend = F) %>%
    add_segments(x = Selection[["start_new"]], xend = Selection[["start_new"]], y = min_Lat, yend = max_Lat, color = I("red"), showlegend = F, hoverinfo='skip') %>%
    add_segments(x = Selection[["end_new"]], xend = Selection[["end_new"]], y = min_Lat, yend = max_Lat, color = I("red"), showlegend = F, hoverinfo='skip') %>%
    layout(xaxis = list(title = "", range = c(min_Time, max_Time), showline = TRUE, showticklabels = FALSE, mirror = "ticks"), 
           yaxis = list(title = "Lat.",  range = c(min_Lat, max_Lat), showline = TRUE, showticklabels = FALSE, mirror = "ticks"))
  
  speed <-
    plot_ly(df_proc_marked) %>%
    add_lines(x = ~Seconds, y = max_Speed, fill = 'tozeroy', color = ~Bout, colors = col_viridis, showlegend = F, hovertemplate='Bout:') %>%
    add_lines(x = ~Seconds, y = ~Speed, color = I("grey"), showlegend = F, hoverinfo ='skip') %>%
    add_lines(x = ~Seconds, y = ~Speed.proc, color = I("black"), showlegend = F, hovertemplate='Seconds: %{x} <br>Speed.proc: %{y}<extra></extra>') %>%
    add_lines(x = ~Seconds, y = max_Speed, color = I("black"), hoverinfo='skip', showlegend = F) %>%
    add_segments(x = Selection[["start_new"]], xend = Selection[["start_new"]], y = min_Speed, yend = max_Speed, color = I("red"), showlegend = F, hoverinfo='skip') %>%
    add_segments(x = Selection[["end_new"]], xend = Selection[["end_new"]], y = min_Speed, yend = max_Speed, color = I("red"), showlegend = F, hoverinfo='skip') %>%
    layout(xaxis = list(title = "Time (s)", range = c(min_Time, max_Time)), 
           yaxis = list(title = "Speed (km/h)",  range = c(min_Speed, max_Speed), showline = TRUE, mirror = "ticks"))
  
  subplot(
    ele,
    lon,
    lat,
    with_options(list(digits = 1), speed) %>% 
      layout(colorway=col_viridis,
             hovermode = "x", spikedistance = 100,
             xaxis = list(
               showspikes = TRUE,
               spikemode  = 'across+toaxis',
               spikethickness=1, 
               spikesnap = 'cursor',
               spikedash = 'solid',
               showline=TRUE,
               showgrid=TRUE,
               spikedistance = -1,showticklabels = TRUE)), 
    shareX = TRUE, titleY = TRUE, nrows = 4) %>% 
    config(displayModeBar = FALSE)
  
  
}

    
        
   
    
  