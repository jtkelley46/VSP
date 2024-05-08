library(readxl)
library(viridisLite)
raw_data <- as.data.frame(read_excel(file.choose())) # Prompts the user to select an excel file to read from, the file data is stored in raw_data.
tracelist <- list() # List of data frames each containing time and dF/F
for (i in 1:13) { # Saves the kinetic data for each trace
  tracelist[[i]] <- data.frame("Time (s)" = raw_data[, 1], "dFF" = (raw_data[,(3*i+i+52)] - mean(raw_data[18000:20000, (3*i+i+52)]))/mean(raw_data[18000:20000, (3*i+i+52)])) # dF/F data using the time before pulse as the baseline
}
ymax = max(tracelist[[2]][20000:40000,2]) + .005 # y bounds for the kinetic data [SCALE]
ymin = -0.05 # tracelist[[2]][(stime+25000),2] - 0.03
pal <- viridis(13) # Creates palette used for lines
titles <- c("-100mV", "180 mV", "160 mV", "140 mV", "120 mV", "100 mV", "80 mV", "60 mV", "40 mV", "20 mV", "0 mV", "-20 mV", "-60 mV") # Creates list of titles to use for each graph
for (i in 1:13) {
  plot(tracelist[[i]], type="l", ylim=c(ymin, ymax), col = pal[i], main=titles[i], xlab="Time (s)", ylab=expression(paste(Delta, "F/F"))) # Plots each trace individually
}
