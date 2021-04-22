
marking_periods <- function(data, df_proc_marked) {
  
  Selection <- 
    data %>% 
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


  return(Selection)  
}

    
        
   
    
  