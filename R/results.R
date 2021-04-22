
# Function for getting a dataset that summarizes the walking and stopping periods
  
    get_periods <- function(data, mininum_bout_duration_s = 15){
              
                data1 <- data
                
                # Marking the bouts
                
                data1$mark <- ifelse(data1$speed.proc == 0, "stop", "walk")
                data1$bout <- vector("double", length(data1$mark))
                data1[1, "bout"] <- 1
                
                for (i in 2:length(data1$bout)) {
                  if (data1$mark[i] == data1$mark[i-1]) {
                    data1$bout[i] <- data1$bout[i-1] 
                  } else {
                    data1$bout[i] <- data1$bout[i-1] + 1
                  }
                }
                
                
                # Getting summarized data for each walking bout and stopping bout
                
                data1 <- 
                  data1 %>%
                  mutate(distance = (time.interval.sec / 3600 * speed.proc * 1000),
                         speed = speed.proc * time.interval.sec) %>%
                  group_by(mark, bout) %>% 
                  summarize(start_time = min(time),
                            end_time = max(time),
                            duration = sum(time.interval.sec),
                            speed = sum(speed) / duration,
                            distance = sum(distance)) %>%
                  arrange(bout)
                
                
                # Updating the summarized data for each walking bout and stopping bout 
                # when considering the minimum bout duration configured in the function
                
                for (i in 2:length(data1$bout)) {
                  if  (data1$mark[i] != data1$mark[i-1] && data1$duration[i] >= mininum_bout_duration_s) {
                    data1$bout[i] <- data1$bout[i-1] + 1
                  } else if (data1$mark[i] != data1$mark[i-1] && data1$duration[i] < mininum_bout_duration_s) {
                    data1$bout[i] <- data1$bout[i-1]
                    data1$mark[i] <- data1$mark[i-1] 
                  } else if (data1$mark[i] == data1$mark[i-1]) {
                    data1$bout[i] <- data1$bout[i-1]
                  } else {
                    "Error"
                  }
                }
                
                data1 <- 
                  data1 %>% 
                  mutate(speed = speed * duration) %>%
                  group_by(mark, bout) %>% 
                  summarize(start_time = min(start_time),
                            end_time = max(end_time),
                            duration = sum(duration),
                            speed = round(sum(speed) / duration, 3),
                            distance = round(sum(distance), 3)) %>%
                  arrange(bout)
                
                
                # Naming the summarized dataset "Periods"
                
                Periods <- data1 
                return(Periods)
              }
    

# Function for getting a table with the final results
    
    get_results_tab <- function(periods, mininum_bout_duration_s = 15, include_last_walk = "no") { 
        
          data <- periods
          
       # Removing the first stopping bout (i.e., the initial resting period) and the last stopping bout where appropriate.
       # Deleting the last walking bout depending on the configuration of the function (with this option, the last walking bout 
       # is kept if it corresponded to the greatest distance performed during a walking bout)

           if(data[1, "mark"] == "stop") data <- data[-1, ] 

           if(data[length(data$mark), "mark"] == "stop") data <- data[-length(data$mark), ]

           if(include_last_walk == "no"  && data[length(data$mark), "distance"] < max(data$distance)) {
             data2 <- data[-length(data$mark), ]
             } else {
             data2 <- data
             }
            
       # Getting the final results
            
          # Calculations performed using the data from the first walking bout to the last walking bout of the session included
            
              session_time <- data %>% ungroup() %>% summarize(session_time = sum(duration) / 60)
              session_walking_time <- data %>% filter(mark == "walk") %>% summarize(session_walking_time = sum(duration) / 60)
              session_distance <- data %>% filter(mark == "walk") %>% summarize(session_distance = sum(distance))
              session_walks_number <- data %>% filter(mark == "walk") %>% summarize(session_walks_number = n())
              
              if (length((data %>% filter(mark == "stop") %>% summarize(session_stops_number = n()))$mark) > 0) {
              session_stops_number <- data %>% filter(mark == "stop") %>% summarize(session_stops_number = n()) 
              } else {
              session_stops_number <- tibble(session_stops_number = NA)
              }

              session_speed_mean <- 
                data %>% 
                filter(mark == "walk") %>% 
                mutate(speed.adj = speed * duration) %>% 
                summarize(session_speed_mean = sum(speed.adj) / sum(duration))
              
           
           # Calculations performed using the data with or without the last walking bout depending on the configuration of the function 
              
              speed_mean_bout  <- data2 %>% filter(mark == "walk") %>% summarize(speed_mean_bout = mean(speed))
              speed_min <- data2 %>% filter(mark == "walk") %>% summarize(speed_min = min(speed))
              speed_max <- data2 %>% filter(mark == "walk") %>% summarize(speed_max = max(speed))
              speed_cv <- data2 %>% filter(mark == "walk") %>% summarize(speed_cv = sd(speed) / mean(speed) * 100)
              
              walking_time_mean <- data2 %>% filter(mark == "walk") %>% summarize(walking_time_mean = mean(duration) / 60)
              walking_time_min <- data2 %>% filter(mark == "walk") %>% summarize(walking_time_min = min(duration) / 60)
              walking_time_max <- data2 %>% filter(mark == "walk") %>% summarize(walking_time_max = max(duration) / 60)
              walking_time_cv <- data2 %>% filter(mark == "walk") %>% summarize(walking_time_cv = sd(duration) / mean(duration) * 100)
              distance_mean <- data2 %>% filter(mark == "walk") %>% summarize(distance_mean = mean(distance))
              distance_min <- data2 %>% filter(mark == "walk") %>% summarize(distance_min = min(distance))
              distance_max <- data2 %>% filter(mark == "walk") %>% summarize(distance_max = max(distance))
              distance_cv <- data2 %>% filter(mark == "walk") %>% summarize(distance_cv = sd(distance) / mean(distance) * 100)
              
              if (length((data2  %>% filter(mark == "stop") %>% summarize(stop_dur_mean = mean(duration) / 60))$mark) > 0) {
              stop_dur_mean <- data2 %>% filter(mark == "stop") %>% summarize(stop_dur_mean = mean(duration) / 60)
              stop_dur_min <- data2 %>% filter(mark == "stop") %>% summarize(stop_dur_min = min(duration) / 60)
              stop_dur_max <- data2 %>% filter(mark == "stop") %>% summarize(stop_dur_max = max(duration) / 60)
              stop_dur_cv <- data2 %>% filter(mark == "stop") %>% summarize(stop_dur_cv = sd(duration) / mean(duration) * 100)
              } else {
              stop_dur_mean <- tibble(stop_dur_mean = NA)
              stop_dur_min <- tibble(stop_dur_min = NA)
              stop_dur_max <- tibble(stop_dur_max = NA)
              stop_dur_cv <- tibble(stop_dur_cv = NA)
              }
              
              max_walk_bout_id <- data2 %>% 
                filter(mark == "walk") %>% 
                arrange(bout) %>% 
                mutate(bout = rep(1:length(mark))) %>% 
                filter(distance == max(distance)) %>% 
                select(bout)
              
           
           # Creating the final table of results
           
              Results <- tibble("Session duration (min)" = round(session_time$session_time, 2),
                                "Session walking time (min)" = round(session_walking_time$session_walking_time, 2),
                                "Session walking distance (m)" = round(session_distance$session_distance, 2),
                                "Session walks number" = session_walks_number$session_walks_number,
                                "Session stops number" = session_stops_number$session_stops_number,
                                "Session mean speed (km/h)" = round(session_speed_mean$session_speed_mean, 2),
                                "Bout mean speed (km/h)" = round(speed_mean_bout$speed_mean_bout, 2),
                                "Bout max speed (km/h)" = round(speed_max$speed_max, 2),
                                "Bout min speed (km/h)" = round(speed_min$speed_min, 2),
                                "Bout speed CV (%)" = round(speed_cv$speed_cv, 2),
                                "Bout mean walking time (min)" = round(walking_time_mean$walking_time_mean, 2),
                                "Bout max walking time (min)" = round(walking_time_max$walking_time_max, 2),
                                "Bout min walking time (min)" = round(walking_time_min$walking_time_min, 2),
                                "Bout walking time CV (%)" = round(walking_time_cv$walking_time_cv, 2),
                                "Bout mean distance (m)" = round(distance_mean$distance_mean, 2),
                                "Bout max distance (m)" = round(distance_max$distance_max, 2),
                                "Bout min distance (m)" = round(distance_min$distance_min, 2),
                                "Bout distance CV (%)" = round(distance_cv$distance_cv, 2),
                                "Bout mean stop time (min)" = round(stop_dur_mean$stop_dur_mean, 2),
                                "Bout max stop time (min)" = round(stop_dur_max$stop_dur_max, 2),
                                "Bout min stop time (min)" = round(stop_dur_min$stop_dur_min, 2),
                                "Bout stop time CV (%)" = round(stop_dur_cv$stop_dur_cv, 2), 
                                "Bout max dist. ID" = max_walk_bout_id$bout,
                                "Bout min duration (s)" = mininum_bout_duration_s)
              return(Results)
        
      }  
  

# Function for plotting distance, mean speed, and stop duration by bout
    
    get_results_plot <- function(data, include_last_walk = "no") {
          
          # Removing the first stopping bout (i.e., the initial resting period) and the last stopping bout where appropriate.
          # Deleting the last walking period depending on the configuration of the function (with this option, the last walking bout 
          # is kept if it corresponded to the greatest distance performed during a walking bout)
          
          if(data[1, "mark"] == "stop") data <- data[-1, ] 
          
          if(data[length(data$mark), "mark"] == "stop") data <- data[-length(data$mark), ]
          
          if(include_last_walk == "no"  && data[length(data$mark), "distance"] < max(data$distance)) {
            data2 <- data[-length(data$mark), ]
          } else {
            data2 <- data
          }
          
          g1 <- data2 %>%
            filter(mark == "walk") %>% arrange(bout) %>% mutate(bout = seq(1:length(mark))) %>% 
            ggplot(aes(x = bout, y = distance)) +
            geom_col() +
            xlab("Walking bout ID") +
            ylab("Distance (m)") +
            theme_classic() +
            scale_x_continuous(breaks = seq(1:length((filter(data2, mark == "walk"))$bout))) +
            theme(axis.title = element_text(size = 15),
                  axis.text = element_text(size = 15))
          
          # Visualizing speed by bout
          g2 <- data2 %>%
            filter(mark == "walk") %>% arrange(bout) %>% mutate(bout = seq(1:length(mark))) %>% 
            ggplot(aes(x = bout, y = speed)) +
            geom_col() +
            xlab("Walking bout ID") +
            ylab("Speed (km/h)") +
            theme_classic() +
            scale_x_continuous(breaks = seq(1:length((filter(data2, mark == "walk"))$bout))) +
            theme(axis.title = element_text(size = 15),
                  axis.text = element_text(size = 15))
          
          # Visualizing stop duration by bout
          if(length((data2 %>%  filter(mark == "stop"))$mark) > 0) 
            g3 <- data %>%
            filter(mark == "stop") %>% arrange(bout) %>% mutate(bout = seq(1:length(mark))) %>% 
            ggplot(aes(x = bout, y = duration)) +
            geom_col() +
            xlab("Stopping bout ID") +
            ylab("Stop duration (s)") +
            theme_classic() +
            scale_x_continuous(breaks = seq(1:length((filter(data2, mark == "stop"))$bout))) +
            theme(axis.title = element_text(size = 15),
                  axis.text = element_text(size = 15))
          
          if(length((data2 %>%  filter(mark == "stop"))$mark) > 0) {
            g1 | g2 | g3 
          } else {
            g1 | g2
          }
        }
    
  