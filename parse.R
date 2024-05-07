parseData <- function(data, firstcolumn, noRuns){
  col <- firstcolumn
  
  allstats <- (ncol(data)-1)/noRuns   #how many stats were collected. Omit the first column (Generations)
  cols <- seq(col,noRuns*allstats, by=allstats)
  subdata <- data[,cols]
  noGens <- nrow(data)
  pdata <- matrix(nrow = noGens, ncol = 3)
  for (i in 1:noGens){
    pdata[i,1] = i
    pdata[i,2] = mean(subdata[i,])
    pdata[i,3] = 1.96*sd((subdata[i,]))/sqrt(noRuns)   #compute the length of error bar. 
  }
  # Assign column names
  colnames(pdata) <- c("generation", "best_mean", "std_deviations")
  return (pdata)
}

parsed_data1 <- parseData(results1, 2, 30) #15
parsed_data2 <- parseData(results2, 2, 30) #30
parsed_data3 <- parseData(results3, 2, 30) #50