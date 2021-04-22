#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector speed_procCpp(DataFrame data, double LPF, double HPF) {
  
  NumericVector speed = data["speed"];
  int n = speed.size();
  NumericVector speed1(n);
  NumericVector speed2(n);
  NumericVector speed3(n);
  NumericVector speed4(n);
  NumericVector speed5(n);
  NumericVector speed6(n);
  NumericVector speed7(n);
  
  // Filter 1
  
  for (int i = 0; i < n; ++i) {
    if (speed[i] > LPF) {
      speed1[i] = (speed[i+1] + speed[i+2] + speed[i+3] + speed[i+4] + speed[i+5]) / 5;  
    } else {
      speed1[i] = speed[i];
    }
  }
  
  
  // Filter 2
  
  speed2 = ifelse(speed1 < HPF, 0, speed1);
  
  
  // Artifacts removing 1
  
  for (int i = 1; i < n; ++i) {
    if (speed2[i-1] == 0 && speed2[i] > 0) {
      speed3[i] = (speed2[i+1] + speed2[i+2] + speed2[i+3] + speed2[i+4] + speed2[i+5]) / 5;
    } else {
      speed3[i] = speed2[i]; 
    }
  }
  speed3[1] = speed3[2];
  
  
  // Artifacts removing 2
  
  for (int i = 2; i < n; ++i) {
    if(speed3[i-2] > 0 && speed3[i-1] > 0 && speed3[i] == 0 && speed3[i+2] > 0 && speed3[i+3] > 0) {
      speed4[i] = (speed3[i-2] + speed3[i-1] + speed3[i+2] + speed3[i+3]) / 4;
    } else {
      speed4[i] = speed3[i];
    }
  }
  speed4[2] = speed4[3];
  speed4[1] = speed4[2];
  
  
  // Artifacts removing 3
  
  for (int i = 2; i < n; ++i) {
    if(speed4[i-2] > 0 && speed4[i-1] > 0 && speed4[i] == 0 && speed4[i+2] > 0 && speed4[i+3] > 0) {
      speed5[i] = (speed4[i-2] + speed4[i-1] + speed4[i+2] + speed4[i+3]) / 4;
    } else {
      speed5[i] = speed4[i];
    }
  }
  speed5[2] <- speed5[3];
  speed5[1] <- speed5[2];
  
  
  
  // Artifacts removing 4
  
  for (int i = 2; i < n; ++i) {
    if(speed5[i-2] == 0 && speed5[i-1] == 0 && speed5[i] > 0 && speed5[i+2] == 0 && speed5[i+3] == 0) {
      speed6[i] = (speed5[i-2] + speed5[i-1] + speed5[i+2] + speed5[i+3]) / 4;
    } else {
      speed6[i] = speed5[i];
    }
  }
  speed6[2] = speed6[3];
  speed6[1] = speed6[2];
  
  // Artifacts removing 5
  
  for (int i = 2; i < n; ++i) {
    if(speed6[i-2] == 0 && speed6[i-1] == 0 && speed6[i] > 0 && speed6[i+2] == 0 && speed6[i+3] == 0) {
      speed7[i] = (speed6[i-2] + speed6[i-1] + speed6[i+2] + speed6[i+3]) / 4;
    } else {
      speed7[i] = speed6[i];
    }
  }
  speed7[2] = speed7[3];
  speed7[1] = speed7[2];
  
  return speed7;
}


// [[Rcpp::export]]
NumericVector mark1(DataFrame data) {
     
   CharacterVector mark = data["mark"];
   int n = mark.size();
   NumericVector bout(n);
   
   bout[1] = 1;
   
   for (int i = 1; i < n; ++i) {
     if (mark[i] == mark[i-1]) {
       bout[i] = bout[i-1];
     } else {
       bout[i] = bout[i-1] + 1;
     }
   }
   return bout;
}

// [[Rcpp::export]]
DataFrame mark2(DataFrame data, int mininum_bout_duration_s) {
  
  CharacterVector mark = data["mark"];
  NumericVector duration = data["duration"];
  int n = mark.size();
  NumericVector bout(n);
  
  bout[1] = 1;

  for (int i = 1; i < n; ++i) {
    if  (mark[i] != mark[i-1] && duration[i] >= mininum_bout_duration_s) {
      bout[i] = bout[i-1] + 1;
    } else if (mark[i] != mark[i-1] && duration[i] < mininum_bout_duration_s) {
      bout[i] = bout[i-1];
      mark[i] = mark[i-1];
    } else {
      bout[i] = bout[i-1];
    }
  }
  
  return DataFrame::create(_["mark"]= mark, _["bout"]= bout); 
}


  
  