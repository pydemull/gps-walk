
# Function for processing the speed data (Le Faucheur et al. 2007; doi: 10.1249/mss.0b013e3180cc20c7)
   
    speed_proc <- function(data, LPF = 2, HPF = 5) {


          # Data transformation #1 (implementation of the low-pass filter)
            
              data$speed.1 <- vector("double", length(data$speed))
              
              for (i in 1:length(data$speed)) {
                if (data$speed[i] > LPF) {
                data$speed.1[i] <- mean(c(data$speed[i+1], data$speed[i+2], data$speed[i+3], data$speed[i+4], data$speed[i+5]))  
                } else {
                data$speed.1[i] <- data$speed[i]
               }
              }
              
              data$speed.1 <- ifelse(is.na(data$speed.1), 0, data$speed.1)
          
          # Data transformation #2 (implementation of the high-pass filter)
            
              data$speed.2 <- ifelse(data$speed.1 < HPF, 0, data$speed.1)
          
          # Data transformation #3 (artifacts removing)
            
              data$speed.3 <- vector("double", length(data$speed))
             
              for (i in 2:length(data$speed)) {
                if (data$speed.2[i-1] == 0 && data$speed.2[i] > 0) {
                  data$speed.3[i] <- mean(c(data$speed.2[i+1], data$speed.2[i+2], data$speed.2[i+3], data$speed.2[i+4], data$speed.2[i+5]))
                } else {
                  data$speed.3[i] <- data$speed.2[i] 
                }
              }
              data$speed.3[1] <- data$speed.3[2]
              data$speed.3 <- ifelse(is.na(data$speed.3), 0, data$speed.3)
            
          # Data transformation #4 (artifacts removing)
            
              data$speed.4 <- vector("double", length(data$speed))
              
              for (i in 3:(length(data$speed) - 3)) {
                if(data$speed.3[i-2] > 0 && data$speed.3[i-1] > 0 && data$speed.3[i] == 0 && data$speed.3[i+2] > 0 && data$speed.3[i+3] > 0) {
                  data$speed.4[i] <- mean(c(data$speed.3[i-2], data$speed.3[i-1], data$speed.3[i+2], data$speed.3[i+3]))
                } else {
                  data$speed.4[i] <- data$speed.3[i] 
                }
              }
              data$speed.4[2] <- data$speed.4[3]
              data$speed.4[1] <- data$speed.4[2]
              data$speed.4[length(data$speed)] <- 0
              data$speed.4[length(data$speed) - 1] <- 0
              data$speed.4[length(data$speed) - 2] <- 0

          # Data transformation #5 (artifacts removing)
            
              data$speed.5 <- vector("double", length(data$speed))
              
              for (i in 3:(length(data$speed) - 3)) {
                if(data$speed.4[i-2] > 0 && data$speed.4[i-1] > 0 && data$speed.4[i] == 0 && data$speed.4[i+2] > 0 && data$speed.4[i+3] > 0) {
                  data$speed.5[i] <- mean(c(data$speed.4[i-2], data$speed.4[i-1], data$speed.4[i+2], data$speed.4[i+3]))
                } else {
                  data$speed.5[i] <- data$speed.4[i] 
                }
              }
              data$speed.5[2] <- data$speed.5[3]
              data$speed.5[1] <- data$speed.5[2]
              data$speed.5[length(data$speed)] <- 0
              data$speed.5[length(data$speed) - 1] <- 0
              data$speed.5[length(data$speed) - 2] <- 0
          
          # Data transformation #6 (artifacts removing)
            
              data$speed.6 <- vector("double", length(data$speed))
              
              for (i in 3:(length(data$speed) - 3)) {
                if(data$speed.5[i-2] == 0 && data$speed.5[i-1] == 0 && data$speed.5[i] > 0 && data$speed.5[i+2] == 0 && data$speed.5[i+3] == 0) {
                  data$speed.6[i] <- mean(c(data$speed.5[i-2], data$speed.5[i-1], data$speed.5[i+2], data$speed.5[i+3]))
                } else {
                  data$speed.6[i] <- data$speed.5[i] 
                }
              }
              data$speed.6[2] <- data$speed.6[3]
              data$speed.6[1] <- data$speed.6[2]
              data$speed.6[length(data$speed)] <- 0
              data$speed.6[length(data$speed) - 1] <- 0
              data$speed.6[length(data$speed) - 2] <- 0
          
          # Data transformation #7 (artifacts removing)
            
              data$speed.proc <- vector("double", length(data$speed))
              
              for (i in 3:(length(data$speed) - 3)) {
                if(data$speed.6[i-2] == 0 && data$speed.6[i-1] == 0 && data$speed.6[i] > 0 && data$speed.6[i+2] == 0 && data$speed.6[i+3] == 0) {
                  data$speed.proc[i] <- mean(c(data$speed.6[i-2], data$speed.6[i-1], data$speed.6[i+2], data$speed.6[i+3]))
                } else {
                  data$speed.proc[i] <- data$speed.6[i] 
                }
              }
              data$speed.proc[2] <- data$speed.proc[3]
              data$speed.proc[1] <- data$speed.proc[2]
              data$speed.proc[length(data$speed)] <- 0
              data$speed.proc[length(data$speed) - 1] <- 0
              data$speed.proc[length(data$speed) - 2] <- 0
              
          return(data)
              
        }

        
