library(openxlsx)
library(VSP)
filename = file.choose() # Prompts user to select the FV file
wb <- loadWorkbook(file = filename) # Loads the voltage dependence workbook
sheets <- length(names(wb)) # Number of data sets
totaltrace <- list() # List to hold all the FV data sets
for (i in 1:sheets) { # Runs through each data set, each sheet in the excel file
  totaltrace[[i]] <- as.data.frame(readWorkbook(wb, sheet = i)) # Each dataset is added to the list as one element (data frame) per set
}
ave_data <- Reduce("+", totaltrace)/length(totaltrace) # Takes the average of the whole workbook, voltage and dF/F
for (i in 1:13) {
  ave_data[i,3] <-  sd(as.numeric(lapply(totaltrace, "[", i, 2))) / sqrt(sheets) # Loops through and calculates std error of the mean
  colnames(ave_data)[3] <- "SEM"
}
par <- fit_sigmoid(ave_data, "gplc") # Tries to fit the FV data to a sigmoid curve and retrieve the parameters
cansig <- par[1] != FALSE # "cansig" is FALSE if R can't fit a curve to the averaged dataset
if (cansig) {y2 <- sigmoid(par,-100:200)} # If R can fit the curve, creates a line
title= readline(prompt="Title for the graphs? ") # Prompts user for title
write.table(ave_data, paste0(title, "_FV", ".txt"), sep = "\t", row.names = FALSE)
ymin = min(ave_data[,2]) - 0.01 # Min and max y values for the FV plot [SCALE]
ymax = max(ave_data[,2]) + 0.01 # Default values are based on the min and max FV +/- 0.01
plot(ave_data[, 1], ave_data[, 2], type="p", ylim = c(ymin,ymax),main=title, xlab="Voltage (mV)", ylab=expression(paste(Delta, "F/F"))) # Plots points
arrows(x0=ave_data[,1], y0=ave_data[,2]-ave_data[,3], x1=ave_data[,1], y1=ave_data[,2]+ave_data[,3], code=3, angle=90, length=0.1) # Adds error bars
if(cansig) {points (-100:200, y2, type="l")} # If a curve fits, adds it
#above for FV
filename = file.choose() # Prompts user for kinetic data excel file
wb <- loadWorkbook(file = filename) # Loads the dF/F over time workbook
sheets <- length(names(wb)) # Number of datasets
totaltrace <- list() # Empties the "totaltrace" list so I could just copy/paste code lol
for (i in 1:sheets) {
  totaltrace[[i]] <- as.data.frame(readWorkbook(wb, sheet=i)) # Each dataset is an element (a data frame) in the list "totaltrace"
}
ave_data <- Reduce("+", totaltrace)/length(totaltrace) # Takes the average of the entire dataset and stores it in one data frame, overwriting the previous ave_data
write.table(ave_data, paste0(title, "_kinetic", ".txt"), sep = "\t", row.names = FALSE)
ymax = max(ave_data[,5]) + .001 # Vertical bounds for kinetic data
ymin = 0-ymax-0.01 # SCALE
resp <- readline("Would you like to plot the voltage step graph? (will take a minute) (y/n) \n") # Voltage-time prompt
plot(x=ave_data[,1], y=ave_data[,3], type="l", ylim=c(ymin, ymax), main=title, xlab="Time (s)", ylab=expression(paste(Delta, "F/F"))) # Plots the first trace and gives proper labels and bounds
pal <- c("#A20DFF", "#49FF26", "#FF1AC1", "#FFDB11", "#1600FF", "#FA0E0C", "#00F9F8", "#8F3907", "#FFF901", "#FE8C00", "#0CADFA", "#0DFF5D") # Creates palette used for line colors
legg <- c("180 mV", "160 mV", "140 mV", "120 mV", "100 mV", "80 mV", "60 mV", "40 mV", "20 mV", "0 mV", "-20 mV", "-60 mV", "-100 mV") # Creates text list used in the legends
for (j in 2:13) {
  points(x=ave_data[,1],y=ave_data[,(2*j+1)], type="l", col=pal[j-1]) # Adds each trace to the plot
}
legend("bottomleft", legend=legg, lty=1, lwd = 5, col=c(pal, "black"), cex=0.8) # Adds legend
if(resp=="y") { # Plotting the voltage-time plot
  plot(x=ave_data[,1], y=ave_data[,2], type="l", ylim=c(-100, 220), main=title, xlab="Time (s)", ylab="Voltage (mV)", lwd=2) # Plots first trace with bounds and labels
  for (j in 2:13) {
    points(x=ave_data[,1], y=ave_data[,(2*j)], type="l", col=pal[j-1], lwd=2) # Adds each trace
  }
  legend("topleft", legend=legg, lty=1, lwd = 5, col=c(pal, "black"), cex=0.8) # Adds legend
}
if(cansig) {writeLines(paste("Parameters for the sigmoid curve:", "\nbase: ", par[1], "\nmax: ", par[2], "\nvhalf: ", par[3], "\nrate: ", par[4]))} # Spits out sigmoid curve parameters in th console
