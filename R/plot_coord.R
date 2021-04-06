
# Functions for plotting all the GPS data (map, coordinates, speed) 
    
    plot_gps_coord <- function(data, mininum_bout_duration_s = 15) {
          
          
          #####################################################
          # Marking the whole dataset with the walking bout IDs
          #####################################################

          # Marking the bouts as "walk" (speed > 0) or "stop" (speed = 0)
          
          data$mark <- ifelse(data$speed.proc == 0, "stop", "walk")
          data$bout <- vector("double", length(data$mark))
          data[1, "bout"] <- 1
          
          for (i in 2:length(data$bout)) {
            if (data$mark[i] == data$mark[i-1]) {
              data$bout[i] <- data$bout[i-1] 
            } else {
              data$bout[i] <- data$bout[i-1] + 1
            }
          }
          
          # Nesting the dataset and updating the marks for the detected walking and stopping bouts... 
          # ... when considering the minimum bout duration configured in the function
          
          data <- data %>%
            group_by(mark, bout) %>%
            nest() 
          
          data$duration <- vector("double", length = length(data$data))
          
          for (i in 1 : (length(data$data))) {
            data$duration[[i]] <- rep(x = sum(data$data[[i]]$time.interval.sec))
          }
          
          
          for (i in 2:length(data$bout)) {
            if  (data$mark[i] != data$mark[i-1] && data$duration[i] >= mininum_bout_duration_s) {
              data$bout[i] <- data$bout[i-1] + 1
            } else if (data$mark[i] != data$mark[i-1] && data$duration[i] < mininum_bout_duration_s) {
              data$bout[i] <- data$bout[i-1]
              data$mark[i] <- data$mark[i-1] 
            } else if (data$mark[i] == data$mark[i-1]) {
              data$bout[i] <- data$bout[i-1]
            } else {
              "Error"
            }
          }
          
          
          # Processing the summarized dataset to finally get the whole initial dataset with the walking bout marked 
          # ... and adding a new time variable
          
          data <- data %>%
            group_by(mark, bout) %>%
            nest() %>%
            mutate(is_bout = ifelse(mark == "walk", 1, NA))
          
          data$walk_bout_num <- vector("double", length = length(data$mark))
          
          for (i in 1:length(data$mark)) {
            if (data$mark[i] == "walk") {data$walk_bout_num[i] <- sum(data$is_bout[1:i], na.rm = TRUE)
            } else 
            { data$walk_bout_num[i] <- NA}
          }
          

          df_proc_marked <- data %>% 
            unnest(cols = c(data)) %>%
            unnest(cols = c(data)) %>%
            select(mark:time.interval.sec, speed.proc, walk_bout_num, lat, lon) %>%
            ungroup() %>%
            mutate(seconds = cumsum(time.interval.sec),
                   Bout = as.factor(walk_bout_num)) %>%
            rename(Speed = speed, 
                   Speed.proc = speed.proc,
                   Seconds = seconds,
                   Altitude = ele,
                   Longitude = lon,
                   Latitude = lat)
  
                    
          #######################################################################################################################
          # Getting a summarised dataset that will be used to color some graphical elements related to the detected walking bouts
          #######################################################################################################################           
             
          Selection <- data %>% 
            unnest(cols = c(data)) %>%
            unnest(cols = c(data)) %>%
            group_by(mark, bout, walk_bout_num) %>%
            summarize(start_time = min(time),
                      end_time = max(time)) %>%
            arrange(bout) %>% 
            filter(mark == "walk") %>%
            mutate(start = as.numeric(as.duration(hms(start_time))),
                   end = as.numeric(as.duration(hms(end_time))),
                   Bout = as.factor(walk_bout_num))
          
          Selection <- Selection %>% 
            mutate(ref = as.numeric(as.duration(hms(df_proc_marked[1, "time"]$time))),
                   start_new = start - Selection[1, "start"]$start + (Selection[1, "start"]$start - ref),
                   end_new = end - Selection[1, "start"]$start +  (Selection[1, "start"]$start - ref))
          
              
             ############################################
             # Function to extract the legend from a plot
             ############################################
              
              get_legend<-function(myggplot){
                tmp <- ggplot_gtable(ggplot_build(myggplot))
                leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
                legend <- tmp$grobs[[leg]]
                return(legend)
              }          
  
           #################
           # Making the plot
           #################
             
            
           # Getting the map
            
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
            
          p1 <- filter(df_proc_marked) %>% 
            leaflet() %>% 
            addTiles() %>% 
            addCircleMarkers(lng = ~Longitude, lat = ~Latitude, radius =1, color = ~pal(Bout), stroke = TRUE, label = labels) %>%
            addAwesomeMarkers(data = filter(df_proc_marked, mark == "walk")[1, ], lng = ~Longitude, lat = ~Latitude, label = htmlEscape("Start"), 
                                icon = icon_start) %>% 
            addAwesomeMarkers(data = filter(df_proc_marked, mark == "walk")[length(filter(df_proc_marked, mark == "walk")$mark), ], 
                       lng = ~Longitude, lat = ~Latitude, label = htmlEscape("End"), icon = icon_end)
            
            
        
          # Getting the alt, long, lat, and speed data        
                    
          
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
            add_lines(x = ~Seconds, y = min_Alt, color = I("black"), hoverinfo='skip', showlegend = F) %>%
            add_lines(x = min_Time, y = max_Alt, color = I("black"), hoverinfo='skip', showlegend = F) %>%
            add_lines(x = max_Time, y = max_Alt, color = I("black"), hoverinfo='skip', showlegend = F) %>%
            add_segments(x = Selection[["start_new"]], xend = Selection[["start_new"]], y = min_Alt, yend = max_Alt, color = I("red"), showlegend = F, hoverinfo='skip') %>%
            add_segments(x = Selection[["end_new"]], xend = Selection[["end_new"]], y = min_Alt, yend = max_Alt, color = I("red"), showlegend = F, hoverinfo='skip') %>%
            layout(xaxis = list(title = "", range = c(min_Time, max_Time), showline = TRUE, showticklabels = FALSE, mirror = "ticks"), 
                   yaxis = list(title = "Alt. (m)", range = c(min_Alt, max_Alt), showline = TRUE, mirror = "ticks"))
          lon <- 
            plot_ly(df_proc_marked) %>%
            add_lines(x = ~Seconds, y = max_Lon, fill = "tozeroy", color = ~Bout, colors = col_viridis, showlegend = F, hovertemplate='Bout:') %>%
            add_lines(x = ~Seconds, y = min_Lon, fill = "tozeroy", color = ~Bout, colors = col_viridis, showlegend = F, hoverinfo='skip') %>%
            add_lines(x = ~Seconds, y = ~Longitude, color = I("black"), showlegend = F, hovertemplate='Seconds: %{x} <br>Longitude: %{y}<extra></extra>') %>%
            add_lines(x = ~Seconds, y = max_Lon,  color = I("black"), hoverinfo='skip', showlegend = F) %>%
            add_lines(x = ~Seconds, y = min_Lon, color = I("black"), hoverinfo='skip', showlegend = F) %>%
            add_lines(x = min_Time, y = max_Lon, color = I("black"), hoverinfo='skip', showlegend = F) %>%
            add_lines(x = max_Time, y = max_Lon, color = I("black"), hoverinfo='skip', showlegend = F) %>%
            add_segments(x = Selection[["start_new"]], xend = Selection[["start_new"]], y = min_Lon, yend = max_Lon, color = I("red"), showlegend = F, hoverinfo='skip') %>%
            add_segments(x = Selection[["end_new"]], xend = Selection[["end_new"]], y = min_Lon, yend = max_Lon, color = I("red"), showlegend = F, hoverinfo='skip') %>%
            layout(xaxis = list(title = "", range = c(min_Time, max_Time), showline = TRUE, showticklabels = FALSE, mirror = "ticks"), 
                   yaxis = list(title = "Lon.",  range = c(min_Lon, max_Lon), showline = TRUE, mirror = "ticks"))
          lat <- 
            plot_ly(df_proc_marked) %>%
            add_lines(x = ~Seconds, y = max_Lat, fill = 'tozeroy', color = ~Bout, colors = col_viridis, showlegend = F, hovertemplate='Bout:') %>%
            add_lines(x = ~Seconds, y = min_Lat, fill = 'tozeroy', color = ~Bout, colors = col_viridis, showlegend = F, hoverinfo ='skip') %>%
            add_lines(x = ~Seconds, y = ~Latitude, color = I("black"), showlegend = F, hovertemplate='Seconds: %{x} <br>Latitude: %{y}<extra></extra>') %>%
            add_lines(x = ~Seconds, y = max_Lat, color = I("black"), hoverinfo='skip', showlegend = F) %>%
            add_lines(x = ~Seconds, y = min_Lat, color = I("black"), hoverinfo='skip', showlegend = F) %>%
            add_lines(x = min_Time, y = max_Lat, color = I("black"), hoverinfo='skip', showlegend = F) %>%
            add_lines(x = max_Time, y = max_Lat, color = I("black"), hoverinfo='skip', showlegend = F) %>%
            add_segments(x = Selection[["start_new"]], xend = Selection[["start_new"]], y = min_Lat, yend = max_Lat, color = I("red"), showlegend = F, hoverinfo='skip') %>%
            add_segments(x = Selection[["end_new"]], xend = Selection[["end_new"]], y = min_Lat, yend = max_Lat, color = I("red"), showlegend = F, hoverinfo='skip') %>%
            layout(xaxis = list(title = "", range = c(min_Time, max_Time), showline = TRUE, showticklabels = FALSE, mirror = "ticks"), 
                   yaxis = list(title = "Lat.",  range = c(min_Lat, max_Lat), showline = TRUE, mirror = "ticks"))
          
          speed <-
            plot_ly(df_proc_marked) %>%
            add_lines(x = ~Seconds, y = max_Speed, fill = 'tozeroy', color = ~Bout, colors = col_viridis, showlegend = F, hovertemplate='Bout:') %>%
            add_lines(x = ~Seconds, y = ~Speed, color = I("grey"), showlegend = F, hoverinfo ='skip') %>%
            add_lines(x = ~Seconds, y = ~Speed.proc, color = I("black"), showlegend = F, hovertemplate='Seconds: %{x} <br>Speed.proc: %{y}<extra></extra>') %>%
            add_lines(x = ~Seconds, y = max_Speed, color = I("black"), hoverinfo='skip', showlegend = F) %>%
            add_lines(x = ~Seconds, y = min_Speed, color = I("black"), hoverinfo='skip', showlegend = F) %>%
            add_lines(x = min_Time, y = max_Speed, color = I("black"), hoverinfo='skip', showlegend = F) %>%
            add_lines(x = max_Time, y = max_Speed, color = I("black"), hoverinfo='skip', showlegend = F) %>%
            add_segments(x = Selection[["start_new"]], xend = Selection[["start_new"]], y = min_Speed, yend = max_Speed, color = I("red"), showlegend = F, hoverinfo='skip') %>%
            add_segments(x = Selection[["end_new"]], xend = Selection[["end_new"]], y = min_Speed, yend = max_Speed, color = I("red"), showlegend = F, hoverinfo='skip') %>%
            layout(xaxis = list(title = "Time (s)", range = c(min_Time, max_Time)), 
                   yaxis = list(title = "Speed (km/h)",  range = c(min_Speed, max_Speed), showline = TRUE, mirror = "ticks"))
          
          p2 <- subplot(
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
                               spikedistance = -1)), 
            shareX = TRUE, titleY = TRUE, nrows = 4) %>% 
            config(displayModeBar = FALSE)
          
          
          
          
          list(p1, p2)
        }

    
        
   
    
  