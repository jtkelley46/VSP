library(readxl)
library(openxlsx)
library(VSP)
raw_data <- as.data.frame(read_excel(file.choose())) # Prompts the user to select an excel file to read from, the file data is stored in raw_data.
trace_data <- data.frame(Voltage = double(), dFF = double())  # creates empty data frame for all data
stime <- as.numeric(readline(prompt = "How many seconds before voltage step? ")) * 10000 # Converts time from seconds
endtime <- -1
j <- stime+15000
while (j < length(raw_data[,1])) { # Scans whole pulse
  volt <- raw_data[j+5, 7]
  if (volt < 150) { # When  the voltage dips below 150 (indicating the end of the pulse), records the time
    endtime = j
    break # Search ends
  }
  j = j + 5 # If the pulse hasn't ended, program scans forward
}
for (i in 1:13) {   # Runs through each trace to collect voltage and dFF data
  f1 <- mean(raw_data[(stime-2000):(stime), (3*i+i+52)]) # Baseline fluorescence
  f2 <- mean(raw_data[(endtime-500):endtime,(3*i+i+52) ]) # Fluorescence at end of pulse
  trace_data[i, 1] <- mean(raw_data[(endtime-600):(endtime-100),(3*i+i-1)]) # Voltage right before end of pulse
  trace_data[i, 2] <- (f2 - f1) / f1 # dFF
}
par <- fit_sigmoid(trace_data, "fplc") # Fits the FV data to a sigmoid regression
cansig <- par[1] != FALSE # If R can't fit a sigmoid, it won't throw an error and won't plot a curve
if (cansig) { y2 <- sigmoid(par,-100:200)} # If R can fit a sigmoid, it will plot it
title <- readline(prompt="Title of the graphs? ") # Prompts user to title graph, eg. "210531_HG_fP_1"
ymin <- min(trace_data[,2]) - 0.01 # This defines the minimum y value for the FV plot [SCALE]
ymax <- max(trace_data[,2]) + 0.01 # You can change the bounds by changing what ymin and ymax are assigned to
plot(trace_data[,1], trace_data[,2], type="p", ylim = c(ymin, ymax),  main=title, xlab="Voltage (mV)", ylab=expression(paste(Delta, "F/F"))) # Plots the actual collected data points
if (cansig) {points(-100:200, y2, type="l")} # Plots the sigmoid regression curve
# above for FV
resp <- readline("Would you like to plot the voltage step graph? (will take a minute) (y/n) \n") # It will take a minute, by God it will
export <- readline("Would you like to export these data to an Excel file (e), pdf (p), both (b) or neither (n)? \n") # Export prompt
volttrace <- list() #A list of data frames containing time and voltage data
tracelist <- list() #A list of data frames containing voltage and dF/F data
for (i in 1:13) {
  tracelist[[i]] <- data.frame("Time (s)" = raw_data[, 1], "dFF" = (raw_data[,(3*i+i+52)] - mean(raw_data[(stime-2000):stime, (3*i+i+52)]))/mean(raw_data[(stime-2000):stime, (3*i+i+52)]))
  volttrace[[i]] <- data.frame("Time (s)" = raw_data[,1], "Voltage (mV)" = raw_data[, (3*i+(i-1))])
}
ymax = max(tracelist[[2]][20000:40000,2]) + .01 # y bounds for the kinetic data [SCALE]
ymin = -.15 # min(tracelist[[2]][20000:40000,2]) - 0.01 #tracelist[[2]][(stime+25000),2] - 0.05
plot(tracelist[[1]], type="l", ylim=c(ymin, ymax), main=title, xlab="Time (s)", ylab=expression(paste(Delta, "F/F")), lwd=0.4) # Plots first trace of kinetic data with proper bounds and labels
pal <- c("#A20DFF", "#49FF26", "#FF1AC1", "#FFDB11", "#1600FF", "#FA0E0C", "#00F9F8", "#8F3907", "#FFF901", "#FE8C00", "#0CADFA", "#0DFF5D") # Creates palette used for line colors
legg = c("180 mV", "160 mV", "140 mV", "120 mV", "100 mV", "80 mV", "60 mV", "40 mV", "20 mV", "0 mV", "-20 mV", "-60 mV", "-100 mV") # Creates a list for the legends later, purely for clarity of code
for (j in 2:13) { # Adds each trace to the plot
  points(tracelist[[15-j]], type="l", col=pal[14-j], lwd=0.4)
}
legend("bottomleft", legend=legg, lty=1, lwd = 5, col=c(pal, "black"), cex=0.8) # Adds legend
if(resp=="y"){ # If you're crazy enough to want the voltage step graph, here you go
  plot(volttrace[[1]], type="l", ylim=c(-100, 220), main=title, xlab="Time (s)", ylab="Voltage (mV)", lwd=1.5)
  for (j in 2:13) {
    points(volttrace[[j]], type="l", col=pal[j-1], lwd=1.5)
  }
  legend("topleft", legend=legg, lty=1, lwd = 5, col=c(pal, "black"), cex=0.7)
}
if (export == "e" || export == "b") {export(trace_data, volttrace, tracelist)} # Export to excel
if (cansig) {writeLines(paste("Parameters for the curve:", "\nbase: ", par[1], "\nmax: ", par[2], "\nvhalf: ", par[3], "\nrate: ", par[4]))} # Spits out the sigmoig equation parameters
if (export == "p" || export == "b") {vsp_pdf(sig = trace_data, parm = par, trc = tracelist, volt = volttrace, title = title, scale = 2.3)} # Exports plots to a pdf file -- works faster than plotting voltage-time
