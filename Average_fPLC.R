library(openxlsx)
library(VSP)
filename = file.choose() # Prompts user for FV file
wb <- loadWorkbook(file = filename) # Loads the voltage dependence workbook
sheets <- length(names(wb)) # Number of data sets
totaltrace <- list() # Empty list used to hold all the FV data
for (i in 1:sheets) { # List to hold all the FV data sets
  totaltrace[[i]] <- as.data.frame(readWorkbook(wb, sheet = i)) # Each dataset is added to the list as one element (data frame) per set
}
ave_data <- Reduce("+", totaltrace)/length(totaltrace) # Takes the average of the whole workbook, voltage and dF/F
for (i in 1:13) {
  ave_data[i,3] <-  sd(as.numeric(lapply(totaltrace, "[", i, 2))) / sqrt(sheets) # Loops through and calculates std error
  colnames(ave_data)[3] <- "SEM"
}
par <- fit_sigmoid(ave_data, "fplc") # Tries to fit the FV data to a sigmoid curve and retrieve the parameters
cansig <- par[1] != FALSE # "cansig" is FALSE if R can't fit a curve to the averaged dataset
if (cansig) {y2 <- sigmoid(par,-100:200)} # If R can fit the curve, creates a line
title= readline(prompt="Title for the graphs? ") # Prompts user for title
write.table(ave_data, paste0(title, "_FV", ".txt"), sep = "\t", row.names = FALSE)
ymax<- max(ave_data[,2]) + 0.005 # Vertical bounds for FV data
ymin<- min(ave_data[,2]) - 0.01 # SCALE
plot(ave_data[, 1], ave_data[, 2], type="p", ylim = c(ymin,ymax),main=title, xlab="Voltage (mV)", ylab=expression(paste(Delta, "F/F"))) # Plots the data points
arrows(x0=ave_data[,1], y0=ave_data[,2]-ave_data[,3], x1=ave_data[,1], y1=ave_data[,2]+ave_data[,3], code=3, angle=90, length=0.1) # Adds the error bars
if(cansig) {points (-100:200, y2, type="l")} # If a sigmoid fits, the curve is added
#above for FV
filename = file.choose() # Prompts for kinetic data file
wb <- loadWorkbook(file = filename) # Loads the dF/F over time workbook
sheets <- length(names(wb)) # Number of data sets
totaltrace <- list() # Empties previous list, so I could copy/paste
for (i in 1:sheets) {
  totaltrace[[i]] <- as.data.frame(readWorkbook(wb, sheet=i)) # Each dataset is added to the list as one element (data frame) per set
}
ave_data <- Reduce("+", totaltrace)/length(totaltrace) # Takes the average of the entire dataset and stores it in one data frame
write.table(ave_data, paste0(title, "_kinetic", ".txt"), sep = "\t", row.names = FALSE)
ymax = max(ave_data[10000:15000,5]) + .01 # Y bounds for the kinetic data
ymin = min(ave_data[10000:15000,5]) - 0.02 # SCALE
resp <- readline("Would you like to plot the voltage step graph? (will take a minute) (y/n) \n") # Voltage-time plot prompt
legg <- c("180 mV", "160 mV", "140 mV", "120 mV", "100 mV", "80 mV", "60 mV", "40 mV", "20 mV", "0 mV", "-20 mV", "-60 mV", "-100 mV") # Legend text
plot(x=ave_data[,1], y=ave_data[,3], type="l", lwd=0.4, ylim=c(ymin, ymax), main=title, xlab="Time (s)", ylab=expression(paste(Delta, "F/F"))) # Plots the first trace and gives proper labels and bounds
pal <- c("#A20DFF", "#49FF26", "#FF1AC1", "#FFDB11", "#1600FF", "#FA0E0C", "#00F9F8", "#8F3907", "#FFF901", "#FE8C00", "#0CADFA", "#0DFF5D") # Creates palette used for line colors
for (j in 2:13) {
  points(x=ave_data[,1],y=ave_data[,(2*(15-j)+1)], type="l", lwd=0.4, col=pal[14-j]) # Adds each trace to the graph in reverse order, so that the +180mV trace is on the top layer
}
legend("bottomleft", legend=legg, lty=1, lwd = 5, col=c(pal, "black"), cex=0.8) # Adds the legend
if(resp=="y") { # This code block plots the ever-so-sluggish voltage over time graph. It's your funeral
  plot(x=ave_data[,1], y=ave_data[,2], type="l", ylim=c(-100, 220), main=title, xlab="Time (s)", ylab="Voltage (mV)", lwd=2) # Plots first trace with labels and bounds
  for (j in 2:13) {
    points(x=ave_data[20000:42000,1], y=ave_data[20000:42000,(2*j)], type="l", col=pal[j-1], lwd=2) # Adds each trace to the plot
  }
  legend("topleft", legend=legg, lty=1, lwd = 5, col=c(pal, "black"), cex=0.7) # Adds legend
}
if(cansig) {writeLines(paste("Parameters for the sigmoid curve:", "\nbase: ", par[1], "\nmax: ", par[2], "\nvhalf: ", par[3], "\nrate: ", par[4]))} # Spits out the sigmoid equation parameters in the console
