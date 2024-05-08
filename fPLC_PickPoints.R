library(readxl)
library(openxlsx)
library(VSP)
raw_data <- as.data.frame(read_excel(file.choose())) #Prompts the user to select an excel file to read from, the file data is stored in raw_data.
trace_data <- data.frame(Voltage = double(), dFF = double())  #creates empty data frame for all data
reg_data <- data.frame(Voltage=double(), dFF = double())  #Creates empty data frame for data used in the regression
stime <- as.numeric(readline(prompt = "How many seconds before voltage step? ")) * 10000
crt <- 0 #The "correction" factor, this allows the program to appropriately enter data into reg_data
exclude <- readline("Would you like to exclude any points from the regression? (y/n) ") == "y"
canc <- double() #The numeric vector containing the indices of traces to exclude from the regression
if (exclude) {
  ans <- readline("Which points would you like to exclude? \n")
  i <- 1
  while (ans!="") { #Enter the indices of traces (e.g. 2 for the +180 mV trace, then hit enter/return to submit it) to exclude, when finished, hit "enter/return" again
    canc[i] <- as.numeric(ans)
    ans <- readline()
    i=i+1
  }
}
for (i in 1:13) {   #Runs through each trace to collect voltage and dFF data
  f1 <- mean(raw_data[(stime-2000):(stime), (3*i+i+52)])
  j <- stime+15000
  f2 <- 0
  while(i!=1 && j < stime +40000) { #This loop captures the average voltage in a 50ms range at the end of the voltage step
    volt2 <- raw_data[(j+5), (3*i+i-1)]
    if (volt2 < -65) {
      f2 <- mean(raw_data[(j-500):j,(3*i+i+52) ])
      break
    }
    j = j+5
  }
  if(i==1) {
    f2 <- mean(raw_data[(j-500):j,56])
  }
  if (!(i %in% canc)) { #If trace "i" is not excluded, it is added to reg_data
    reg_data[(i-crt), 1] <- mean(raw_data[(j-600):(j-100),(3*i+i-1)])
    reg_data[(i-crt), 2] <- (f2 - f1) / f1
  } else {crt=crt+1} #If trace "i" is to be excluded, crt is increased by 1
  trace_data[i, 1] <- mean(raw_data[(j-600):(j-100),(3*i+i-1)])
  trace_data[i, 2] <- (f2 - f1) / f1
}
par <- fit_sigmoid(reg_data, "fplc", exclude = FALSE)
cansig <- par[1] != FALSE #If R can't fit a sigmoid, it won't throw an error and won't plot a curve
if (cansig) { #If R can fit a sigmoid, it will plot it
  y2 <- sigmoid(par,-100:200)
}
title <- readline(prompt="Title of the graph? ") #Prompts user to title graph, eg. "200611_HG_gP_1"
ymin <- min(trace_data[,2]) - 0.01 #This defines the minimum y value, "ymax" can be added, or ymin can be altered to your liking, just remember to change the "ylim=" parameter below accordingly
plot(trace_data[,1], trace_data[,2], type="p", ylim = c(ymin, (ymin*(-0.25))),  main=title, xlab="Voltage (mV)", ylab=expression(paste(Delta, "F/F"))) #Plots the actual collected data points
if (cansig) {points(-100:200, y2, type="l")} #Plots the sigmoid regression curve
#above for sigmoid
resp <- readline("Would you like to plot the voltage step graph? (will take a minute) (y/n) \n") #It will take a minute, by God it will
export <- readline("Would you like to export these data to excel? (y/n) \n") #Indicate whether you'd like to export the data, all data points will be exported
volttrace <- list() #A list of data frames containing time and voltage data
tracelist <- list() #A list of data frames containing voltage and dF/F data
for (i in 1:13) {
  tracelist[[i]] <- data.frame("Time (s)" = raw_data[, 1], "dFF" = (raw_data[,(3*i+i+52)] - mean(raw_data[(stime-2000):stime, (3*i+i+52)]))/mean(raw_data[(stime-2000):stime, (3*i+i+52)]))
  volttrace[[i]] <- data.frame("Time (s)" = raw_data[,1], "Voltage (mV)" = raw_data[, (3*i+(i-1))])
}
ymax = max(tracelist[[2]][20000:40000,2]) + .01 #These numbers added/subtracted from ymax or ymin may be changed
ymin = tracelist[[2]][(stime+25000),2] - 0.05
plot(tracelist[[1]], type="l", ylim=c(ymin, ymax), main=title, xlab="Time (s)", ylab=expression(paste(Delta, "F/F")), lwd=0.4)
pal <- c("#A20DFF", "#49FF26", "#FF1AC1", "#FFDB11", "#1600FF", "#FA0E0C", "#00F9F8", "#8F3907", "#FFF901", "#FE8C00", "#0CADFA", "#0DFF5D") #Creates palette used for line colors
legg = c("180 mV", "160 mV", "140 mV", "120 mV", "100 mV", "80 mV", "60 mV", "40 mV", "20 mV", "0 mV", "-20 mV", "-60 mV", "-100 mV") #Creates a list for the legends later, purely for clarity of code
for (j in 2:13) {
  points(tracelist[[15-j]], type="l", col=pal[14-j], lwd=0.4)
}
legend("bottomleft", legend=legg, lty=1, lwd = 5, col=c(pal, "black"), cex=0.8)
if(resp=="y"){
  plot(volttrace[[1]], type="l", ylim=c(-100, 220), main=title, xlab="Time (s)", ylab="Voltage (mV)", lwd=1.5)
  for (j in 2:13) {
    points(volttrace[[j]][stime:(stime+22000),], type="l", col=pal[j-1], lwd=1.5)
  }
  legend("topleft", legend=legg, lty=1, lwd = 5, col=c(pal, "black"), cex=0.8)
}
if (export == "y") {export(trace_data, volttrace, tracelist)}
if (cansig) {writeLines(paste("Parameters for the curve:\n", "base: ", par[1], "\nmax: ", par[2], "\nvhalf: ", par[3], "\nrate: ", par[4]))} #Spits out the sigmoig equation parameters
