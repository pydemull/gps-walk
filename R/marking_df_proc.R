
marking_df_proc <- function(data) {
  
 data <-
   data %>% 
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
 
 return(data)
 
}

    
        
   
    
  