
# Function for formating the original data file
  
   gps_file_prep_dg100 <- function(data) {
     
    
    data$time <- as.POSIXct(data$time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    data$time <- format(data$time, tz = "Europe/Paris")
    data <- separate(data, time, c("date", "time"), sep = " ")
    data <- data %>% mutate(seconds = as.numeric(as.duration(hms(time))),
                            time.interval.sec = seconds - lag(seconds, default = first(seconds)), 
                            time.cum.sec = cumsum(time.interval.sec),
                            ele = as.numeric(ele),
                            speed = round(as.numeric(speed)/1000*3600, digits = 1))
    return(data)
    
   }
   
   gps_file_prep_qstarz <- function(data) {
     
     data <- data %>% 
       rename(speed = SPEED,
              lat = LATITUDE,
              lon = LONGITUDE,
              ele = ALTITUDE,
              time = LOCAL.TIME) %>%
       mutate(time = as.POSIXct(time, format = "%H:%M:%S"))
     data$time <- format(data$time, tz = "Europe/Paris")
     data <- separate(data, time, c("date", "time"), sep = " ")
     data <- data %>% mutate(seconds = as.numeric(as.duration(hms(time))),
                             time.interval.sec = seconds - lag(seconds, default = first(seconds)), 
                             time.cum.sec = cumsum(time.interval.sec),
                             ele = as.numeric(ele),
                             speed = round(speed, digits = 1))
     return(data)

   }
   
   gps_file_prep_polar <- function(data) {
     
     data <- data %>% 
       rename(speed = "Speed..km.h.",
              ele = "Altitude..m.",
              time = Time) %>%
       mutate(time = as.character(time),
              seconds = as.numeric(as.duration(hms(time))),
              time.interval.sec = seconds - lag(seconds, default = first(seconds)), 
              time.cum.sec = cumsum(time.interval.sec),
              lat = 0,
              lon = 0,
              ele = as.numeric(ele),
              speed = round(speed, digits = 1))
     
     return(data)
     
   }
   
  
   
# Function for calculating the mean, the sd, and the cv of speed
  
     gps_filters <- function(data, filter_start_sec = NULL, filter_end_sec = NULL) {

        param <- 
          data.frame(
            mean_speed = (filter(data, time.cum.sec >= filter_start_sec & time.cum.sec <= filter_end_sec) %>%
                            select(speed) %>% 
                            summarise(mean = mean(speed, na.rm = TRUE)))[1,1],
            sd_speed = (filter(data, time.cum.sec >= filter_start_sec & time.cum.sec <= filter_end_sec) %>% 
                          select(speed) %>% 
                          summarise(sd = sd(speed, na.rm = TRUE)))[1,1]
            )
        
        param <- param %>% mutate(cv_speed = sd_speed / mean_speed * 100)
        
        return(param)

     }
 
    
  