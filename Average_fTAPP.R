library (openxlsx)
library(VSP)
filename = file.choose() # Prompts user to select the FV file
wb <- loadWorkbook(file = filename) # Loads the voltage dependence workbook
sheets <- length(names(wb)) # Number of data sets
totaltrace <- list() # List to hold all the FV data sets
for (i in 1:sheets) { # Runs through each data set, each sheet in the excel file
  totaltrace[[i]] <- as.data.frame(readWorkbook(wb, sheet = i)) # Each dataset is added to the list as one element (data frame) per set
}
ave_sig <- Reduce("+", totaltrace)/length(totaltrace) # Takes the average of the whole workbook, voltage and dF/F
for (i in 1:13) { # Loops through and calculates std error data for both dF/F values
  ave_sig[i,4] <-  sd(as.numeric(lapply(totaltrace, "[", i, 2))) / sqrt(sheets)
  colnames(ave_sig)[4] <- "SEM1"
  ave_sig[i,5] <-  sd(as.numeric(lapply(totaltrace, "[", i, 3))) / sqrt(sheets)
  colnames(ave_sig)[5] <- "SEM2"
}
# Above for loading FV data
filename = file.choose() # Prompts for kinetic data file
wb <- loadWorkbook(file = filename) # Loads the dF/F over time workbook
sheets <- length(names(wb)) # Number of data sets
totaltrace <- list() # Empties the list so I can copy/paste
for (i in 1:sheets) {
  totaltrace[[i]] <- as.data.frame(readWorkbook(wb, sheet = i)) # Each dataset is an element in the list "totaltrace"
}
ave_dff <- Reduce("+", totaltrace)/length(totaltrace) # Average of all kinetic data, dF/F and voltage over time
title <- readline(prompt="Title of the graphs? ") # Title prompt
write.table(ave_dff, paste0(title, "_kinetic", ".txt"), sep = "\t", row.names = FALSE)
# Above for loading kinetic data
par <- fit_sigmoid(ave_sig, "ftapp") # Tries to fit the FV data to a sigmoid curve and retrieve the parameters
cansig1 <- class(par[[1]]) != FALSE # "cansig" is FALSE if R can't fit a curve to the averaged dataset
cansig2 <- class(par[[2]]) != FALSE # "cansig" is TRUE if R can fit a curve to the averaged dataset
if (cansig1) {y3 <- sigmoid(par[[1]],-100:200)} # If R can fit a curve to the "up" component data, makes a line with those parameters
if (cansig2) {y4 <- sigmoid(par[[2]],-100:200)} # "down" component line
write.table(ave_sig, paste0(title, "_FV", ".txt"), sep = "\t", row.names = FALSE)
resp <- readline("Would you like to plot the voltage step graph? (will take a minute) (y/n) ") # voltage-time prompt. ya'ilahi it's slow
ymax <- max(ave_sig[,2]) + 0.01 # Y bounds for FV data
ymin = -1.4*ymax # [SCALE]
plot(ave_sig[,1], ave_sig[,2], type="p", col= "purple", pch= 16, ylim = c(ymin, ymax),  main=title, xlab="Voltage (mV)", ylab=expression(paste(Delta, "F/F"))) # Plots averaged "up" component points
points(ave_sig[,1], ave_sig[,3], type="p", col="forestgreen", pch=16) # Adds "down" component points
if (cansig1) {points(-100:200, y3, type="l", col="purple")} # Adds "up" component curve"
if (cansig2) {points(-100:200, y4, type="l", col="forestgreen")} # Adds "down" component curve
arrows(x0=ave_sig[,1], y0=ave_sig[,2]-ave_sig[,4], x1=ave_sig[,1], y1=ave_sig[,2]+ave_sig[,4], code=3, angle=90, length=0.1, col = "purple") # Adds error bars
arrows(x0=ave_sig[,1], y0=ave_sig[,3]-ave_sig[,5], x1=ave_sig[,1], y1=ave_sig[,3]+ave_sig[,5], code=3, angle=90, length=0.1, col = "forestgreen")
# Above for plotting FV data
ymax = max(ave_dff[20000:50000,5]) + .001 # y bounds for the dF/F over time graph, change appropriately
ymin = 0-ymax+0.005 # [SCALE]
plot(x=ave_dff[,1], y=ave_dff[,3], type="l", ylim=c(ymin, ymax), lwd=0.35, main=title, xlab="Time (s)", ylab=expression(paste(Delta, "F/F"))) # Plots the first trace and gives proper labels and bounds
pal <- c("#A20DFF", "#49FF26", "#FF1AC1", "#FFDB11", "#1600FF", "#FA0E0C", "#00F9F8", "#8F3907", "#FFF901", "#FE8C00", "#0CADFA", "#0DFF5D") # Creates palette used for line colors
legg <- c("180 mV", "160 mV", "140 mV", "120 mV", "100 mV", "80 mV", "60 mV", "40 mV", "20 mV", "0 mV", "-20 mV", "-60 mV", "-100 mV") # Creates legend text, for clarity
for (j in 2:13) {
  points(x=ave_dff[,1],y=ave_dff[,(31-(2*j))], type="l", lwd=0.35, col=pal[14-j]) # Adds each trace to the plot
}
legend("bottomleft", legend=legg, lty=1, lwd = 5, col=c(pal, "black"), cex=0.6) # Adds the legend
if(resp=="y") { # Voltage-time plot
  plot(x=ave_dff[,1], y=ave_dff[,2], type="l", ylim=c(-100, 220), main=title, xlab="Time (s)", ylab="Voltage (mV)", lwd=2) # Plots first trace with bounds and labels
  for (j in 2:13) {
    points(x=ave_dff[,1], y=ave_dff[,(2*j)], type="l", col=pal[j-1], lwd=2) # Adds each trace
  }
  legend("topleft", legend=legg, lty=1, lwd = 5, col=c(pal, "black"), cex=0.8) # Adds legend
}
if (cansig1) {writeLines(paste("Parameters for the purple sigmoid curve:\n", "base: ", par[[1]][1], "\nmax: ", par[[1]][2], "\nvhalf: ", par[[1]][3], "\nrate: ", par[[1]][4]))}
if (cansig2) {writeLines(paste("Parameters for the green sigmoid curve:\n", "base: ", par[[2]][1], "\nmax: ", par[[2]][2], "\nvhalf: ", par[[2]][3], "\nrate: ", par[[2]][4]))}
