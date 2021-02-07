
# Functions for plotting all the GPS data (map, coordinates, speed) 
    
    plot_gps_coord <- function(data, mininum_bout_duration_s = 15, include_map = "no") {
          
          
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
            unnest() %>%
            unnest() %>%
            select(mark:time.interval.sec, speed.proc, walk_bout_num) %>%
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
            unnest() %>%
            unnest() %>%
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
  
             ###################
             # Making the figure
             ###################
             
           if (include_map == "yes"){
            
            
              # Getting the map
            
              map <- get_map(location = c(Longitude = mean(df_proc_marked$Longitude), Latitude = mean(df_proc_marked$Latitude)), zoom = 15, source = "google")
              
              coord <- ggmap(map, extent = "normal") + 
                scale_x_continuous(limits = c(min(df_proc_marked$Longitude), max(df_proc_marked$Longitude))) +
                scale_y_continuous(limits = c(min(df_proc_marked$Latitude), max(df_proc_marked$Latitude))) +
                xlab("longitude") +
                ylab("latitude") +
                geom_point(data = filter(df_proc_marked, mark == "walk"),
                           mapping = aes(x = Longitude, y = Latitude, fill = Bout), 
                           shape = 21, size = 2, alpha = 0.4) +
                scale_fill_viridis(name = "Walking bout", discrete = TRUE, option = "D")  +
                geom_point(data = filter(df_proc_marked, mark == "walk")[1, ], mapping = aes(x = Longitude, y = Latitude), shape = 21, fill = "red", size = 5) +
                geom_point(data = filter(df_proc_marked, mark == "walk")[length(filter(df_proc_marked, mark == "walk")$mark), ], 
                           mapping = aes(x = Longitude, y = Latitude), 
                           shape = 22, fill = "red", size = 5) +
                theme(legend.position = "top",
                      axis.title = element_blank(),
                      axis.text = element_blank(),
                      axis.ticks = element_blank()) +
                guides(fill = guide_legend(override.aes = list(size = 3), ncol = 10))

          } else {
            
            coord <- ggplot() +
              scale_x_continuous(limits = c(min(df_proc_marked$Longitude), max(df_proc_marked$Longitude))) +
              scale_y_continuous(limits = c(min(df_proc_marked$Latitude), max(df_proc_marked$Latitude))) +
              xlab("longitude") +
              ylab("latitude") +
              geom_point(data = filter(df_proc_marked, mark == "walk"),
                         mapping = aes(x = Longitude, y = Latitude, fill = Bout), 
                         shape = 21, size = 2, alpha = 0.4) +
              scale_fill_viridis(name = "Walking bout", discrete = TRUE, option = "D")  +
              geom_point(data = filter(df_proc_marked, mark == "walk")[1, ], mapping = aes(x = Longitude, y = Latitude), shape = 21, fill = "red", size = 5) +
              geom_point(data = filter(df_proc_marked, mark == "walk")[length(filter(df_proc_marked, mark == "walk")$mark), ], 
                         mapping = aes(x = Longitude, y = Latitude), 
                         shape = 22, fill = "red", size = 5) +
              theme(legend.position = "top",
                    axis.title = element_blank(),
                    axis.text = element_blank(),
                    axis.ticks = element_blank()) +
              guides(fill = guide_legend(override.aes = list(size = 3), ncol = 10))
            
            
          }
                    
          ###############   
          point_size <- 1
          ############### 
                    
                    
          ele <- ggplot(data = df_proc_marked) +
            geom_rect(data = Selection, aes(xmin = start_new, xmax =  end_new, 
                                            ymin = min(df_proc_marked$Altitude), 
                                            ymax = max(df_proc_marked$Altitude), 
                                            fill = Bout), alpha = 0.4) +
            geom_point(aes(x = Seconds, y = Altitude), size = point_size) + 
            geom_segment(data = Selection, aes(x = start_new, xend = start_new, 
                                               y = min(df_proc_marked$Altitude), 
                                               yend = max(df_proc_marked$Altitude)),
                         color = "red", size = 0.8) + 
            geom_segment(data = Selection, aes(x = end_new, xend = end_new, y = min(df_proc_marked$Altitude), yend = max(df_proc_marked$Altitude)), 
                         color = "red", size = 0.8) + 
            scale_y_continuous(breaks = seq(min(df_proc_marked$Altitude), max(df_proc_marked$Altitude), (max(df_proc_marked$Altitude) - min(df_proc_marked$Altitude))/4)) +
            scale_fill_viridis(name = "Walking bout", discrete = TRUE, option = "D")  +
            theme_bw() +
            ylab("Alt. (m)") +
            xlab("Time (s)") +
            theme(legend.position = "right",
                  axis.title.x =  element_blank(),
                  axis.text.x = element_blank(),
                  axis.ticks = element_blank()) +
            coord_cartesian(expand = FALSE)  
          
          lon <- ggplot(data = df_proc_marked) +
            geom_rect(data = Selection, aes(xmin = start_new, xmax =  end_new, 
                                            ymin = min(df_proc_marked$Longitude), 
                                            ymax = max(df_proc_marked$Longitude), 
                                            fill = Bout), alpha = 0.4) +
            geom_point(aes(x = Seconds, y = Longitude), size = point_size) + 
            geom_segment(data = Selection, aes(x = start_new, xend = start_new, y = min(df_proc_marked$Longitude), yend = max(df_proc_marked$Longitude)), 
                         color = "red", size = 0.8) + 
            geom_segment(data = Selection, aes(x = end_new, xend = end_new, y = min(df_proc_marked$Longitude), yend = max(df_proc_marked$Longitude)), 
                         color = "red", size = 0.8) + 
            scale_fill_viridis(name = "Walking bout", discrete = TRUE, option = "D")  +
            ylab("Lon.") +
            xlab("Time (s)") +
            theme_bw() +
            theme(legend.position = "right",
                  axis.title.x =  element_blank(),
                  axis.text.x = element_blank(),
                  axis.ticks = element_blank()) +
            coord_cartesian(expand = FALSE)        
          
          lat <- ggplot(data = df_proc_marked) +
            geom_rect(data = Selection, aes(xmin = start_new, xmax =  end_new, 
                                            ymin = min(df_proc_marked$Latitude), 
                                            ymax = max(df_proc_marked$Latitude), 
                                            fill = Bout), alpha = 0.4) +
            geom_point(aes(x = Seconds, y = Latitude), size = point_size) + 
            geom_segment(data = Selection, aes(x = start_new, xend = start_new, y = min(df_proc_marked$Latitude), yend = max(df_proc_marked$Latitude)), 
                         color = "red", size = 0.8) + 
            geom_segment(data = Selection, aes(x = end_new, xend = end_new, y = min(df_proc_marked$Latitude), yend = max(df_proc_marked$Latitude)), 
                         color = "red", size = 0.8) + 
            scale_fill_viridis(name = "Walking bout", discrete = TRUE, option = "D")  +
            ylab("Lat.") +
            xlab("Time (s)") +
            theme_bw() +
            theme(legend.position = "right",
                  axis.title.x =  element_blank(),
                  axis.text.x = element_blank(),
                  axis.ticks = element_blank()) +
            coord_cartesian(expand = FALSE)
          
          speed <- ggplot(data = df_proc_marked) +
            geom_rect(data = Selection, aes(xmin = start_new, xmax =  end_new, ymin = min(df_proc_marked$Speed.proc), ymax = max(df_proc_marked$Speed), 
                                            fill = Bout), alpha = 0.4) +
            geom_line(aes(x = Seconds, y = Speed), color = "#999999", size = 0.8) +
            geom_line(aes(x = Seconds, y = Speed.proc), size = 0.8) +
            geom_segment(data = Selection, aes(x = start_new, xend = start_new, y = min(df_proc_marked$Speed.proc), yend = max(df_proc_marked$Speed)), 
                         color = "red", size = 0.8) + 
            geom_segment(data = Selection, aes(x = end_new, xend = end_new, y = min(df_proc_marked$Speed.proc), yend = max(df_proc_marked$Speed)), 
                         color = "red", size = 0.8) + 
            scale_fill_viridis(name = "Walking bout", discrete = TRUE, option = "D")  +
            xlab("Time (s)") +
            ylab(" Speed (km/h)") +
            theme(legend.position="right") +
            theme_bw() +
            coord_cartesian(expand = FALSE)
          
          ele <- ele + theme(legend.position ="none")
          lon <- lon + theme(legend.position ="none")
          lat <- lat + theme(legend.position ="none")
          speed <- speed + theme(legend.position ="none")
          
          list(coord, ele, lon, lat, speed)

        }

    
        
   
    
  