# A shiny app to analyse GPS walking data
## Presentation
The app allows the analysis of GPS data obtained during an outdoor walking session and provides summarized results (eg, number of detected walking bouts, mean speed over the walking bouts, total walking  distance over the session, longest distance performed during a walking bout, etc.). The analysis consists of the methodology  proposed by Le Faucheur et al. (2007; DOI: 10.1249/mss.0b013e3180cc20c7). Briefly, the user must select a period of time (120 s if possible, or shorter)  to compute a representative mean and SD of speed for the session. Then, based on the coefficient of variation (CV) of speed, the user must configure the high pass filter (HPF) for the speed data (choose HPF = 2 when the CV is >15%, or HPF = 5 when the CV is <=15%, or even higher than 5 when the CV is very low (eg, <5%); the value to choose for the low pass filter (LPF) is 2 in principle). To determine the period of time to be used to compute the CV of speed and then the HPF, the user is helped by a reactive graphic of the coordinates and processed speed that are plotted against time, with the detected walking bouts  that are highlighted with colors. The app also provides a map with the positions measured during the session and that are colored in relation to the corresponding detected walking bouts. Be carrefull of the altitude data that may be not accurate with non-differential GPS devices. When computing the final results, the user can choose to include or not the last detected walking bout for the calculation of the mean, cv, min, and max of speed, walking distance, and walking time over the walking bouts. For now, the app can be used with .gpx DG100 files, and .csv Qstarz files, and .CSV V800/M430 files (for now, latitude and longitude are not available for this last type of data files). Please reset the app before analysing a new file. The app is currently hosted at https://pydemull.shinyapps.io/gps-walk. For a website allowing faster analysis (without manual analysis) for both GPS and accelerometer data, but without context information (map, coordinates), see the following website: https://mapam.ens-rennes.fr.

## Acknowledgements
Several great books were particularly useful to produce this app, in particular: Mastering shiny (https://mastering-shiny.org) by Hadley Wickham ; and Interactive web-based data visualization with R, plotly, and shiny (https://plotly-r.com) by Carson Sievert. Lots of thanks also to the "https://stackoverflow.com" community who produced some pieces of code that can be found in the final code of this app.

Thanks to Alexis Le Faucheur who gave me the original Excel file related to his 2007 publication (DOI: 10.1249/mss.0b013e3180cc20c7) that allowed me to understand the details of the computation of the speed data filter that is implemented in this app.