
nesting_data <- function(data, mininum_bout_duration_s = 15) {
  
  # Marking the bouts
  
  data$mark <- ifelse(data$speed.proc == 0, "stop", "walk")
  data$bout <- mark1(data)
  
  # Updating the marks for the detected walking and stopping bouts... 
  # ... when considering the minimum bout duration configured in the function
  
  data <- data %>%
    group_by(mark, bout) %>%
    nest() 
  
  cum <- function(df) {
    with(df, sum(time.interval.sec))
  }
  
  data$duration <- map(data$data, cum) %>% unlist()
  
  ListData <- data["data"]
  data <- mark2(data, mininum_bout_duration_s = mininum_bout_duration_s)
  data["data"] <- ListData
  
  
  # Marking the nested dataset for the walking bouts only
  
  data <- 
    data %>%
    group_by(mark, bout) %>%
    nest() %>%
    mutate(is_bout = ifelse(mark == "walk", 1, NA))
  
  data$walk_bout_num <- vector("double", length = length(data$mark))
  
  for (i in 1:length(data$mark)) {
    if (data$mark[i] == "walk") {data$walk_bout_num[i] <- sum(data$is_bout[1:i], na.rm = TRUE)
    } else 
    { data$walk_bout_num[i] <- NA}
  }
  
  return(data)
}
  
 