library(readxl)
library(openxlsx)
library(VSP)
num_traces = 9 # The number of traces to do peak-searching from. Default is 9 => top 8 voltages. Change as you like. 9 works for most with signal, 0 or 1 indicate no peak-searching
raw_data <- as.data.frame(read_excel(file.choose())) # Prompts the user to select an excel file to read from, the file data is stored in raw_data.
trace_data <- data.frame(Voltage = double(), dFF1 = double(), dFF2 = double())  # creates empty data frame
stime <- as.numeric(readline(prompt = "How many seconds before voltage step? ")) * 10000 # Converts time in seconds to tenths of milliseconds (the sample rate of the protocol)
endtime <- -1
j <- stime+15000
while (j < length(raw_data[,1])) { # Scans through whole pulse to find the end, used later in finding FV data
  volt <- raw_data[j+5, 7]
  if (volt < -70) { # When voltage dips below 150 (indicating end of pulse), records time
    endtime = j
    break # ends the search
  }
  j = j + 5 # If end not yet reached, moves forward
}
for (i in 1:13) {   # Runs through each trace to collect voltage and dFF data
  f1 <- mean(raw_data[(stime-2000):(stime), (3*i+i+52)]) # The baseline F
  max_ave <- 0 #The peak F
  f3 <- -1  #The "down" component
  if (i != 1 && i <= num_traces) { # Peak-searching for higher voltages
    posj <- 0
    avec <- vector()
    for (k in 1:10) { # Starts the scan from 10 different positions, think open reading frames
      j <- stime + (10 * k)
      while (j<(stime+30001)) {  # Runs through the data range of the voltage step finding the highest average range. Window moves by 5 ms
        new_ave <- mean(raw_data[j:j+1000, (3*i+i+52)]) # The moving average
        if(new_ave > max_ave) { # If the moving average is higher than the current maximum, replace max
          max_ave <- new_ave
          posj = j # Record the time that the max is found
          volt <- mean(raw_data[j:j+500, (3*i+i-1)]) # Record voltage at this point
        }
        else if(new_ave + 0.1 < max_ave) { # If the moving average is much lower than the max (indicating a down component) the search stops
          break
        }
        j = j+100 # Window moves 5 ms
      }
      avec[k] <- posj # Vector of times where max is found
      max_ave = 0
    }
    avec = avec[avec != min(avec)] # Depending on the voltage, the range where dFF is taken changes
    avec = avec[avec != min(avec)]
    avec = avec[avec != max(avec)]
    if (i > 3) {
      avec = avec[avec != min(avec)]
      avec = avec[avec != max(avec)]
      if (i > 4) {
        avec = avec[avec != min(avec)]
        if (i > 5) {
          avec = avec[avec != min(avec)]
          avec = avec[avec != min(avec)]
        }
      }
    }
    max_ave = mean(raw_data[min(avec):max(avec)+500, (3*i+i+52)]) # The max is taken from within the shortened range of where roughly the max is
  } else { # dFF snd voltage taken from end of pulse for lower voltages
    max_ave <- mean(raw_data[(endtime-1000):endtime, (3*i+i+52)])
    volt <- mean(raw_data[(endtime-500):endtime, (3*i+i-1)])
  }

  f3 <- mean(raw_data[(endtime-1000):endtime, (3*i+i+52)])
  trace_data[i, 1] <- volt # Voltage as recorded at the up component
  trace_data[i, 2] <- (max_ave - f1) / f1  # dFF at the up component
  trace_data[i, 3] <- (f3-max_ave) / max_ave # dFF at the down component
}
par <- fit_sigmoid(trace_data, "ftapp") # fits a regression curve to the up and down component data
cansig1 <- class(par[[1]]) != "logical" #cansig is TRUE when R can fit a sigmoid curve
cansig2 <- class(par[[2]]) != "logical" #cansig is FALSE when R cannot
if (cansig1) {y3 <- sigmoid(par[[1]],-100:200)} # If R can fit a curve, it makes a line for it
if (cansig2) {y4 <- sigmoid(par[[2]],-100:200)}
title <- readline(prompt="Title of the graphs? ") # Prompts user to title graph, eg. "210531_HG_fT_1"
ymax <- max(trace_data[,2]) + 0.01 # The upper bound for the y axis of the FV plot
ymin = -1.4*ymax # Lower bound [SCALE]
plot(trace_data[,1], trace_data[,2], type="p", col= "purple", pch= 16, ylim = c(ymin, ymax), main=title, xlab="Voltage (mV)", ylab=expression(paste(Delta, "F/F"))) # Plots the actual collected data points
points(trace_data[,1], trace_data[,3], type="p", col="forestgreen", pch=16) # Adds the down component points
if (cansig1) {points(-100:200, y3, type="l", col="purple")} # Adds the regression curves, if possible
if(cansig2) {points(-100:200, y4, type="l", col="forestgreen")}
#above for sigmoid
resp <- readline("Would you like to plot the voltage step graph? (will take a minute) (y/n) \n") # It will. Save yourself.
export <- readline("Would you like to export these data to an Excel file (e), pdf (p), both (b) or neither (n)? \n") # Export prompt
volttrace <- list() # List of data frames each containing time and voltage
tracelist <- list() # List of data frames each containing time and dF/F
for (i in 1:13) { # Saves the kinetic and voltage data for each trace in two lists
  tracelist[[i]] <- data.frame("Time (s)" = raw_data[, 1], "dFF" = (raw_data[,(3*i+i+52)] - mean(raw_data[18000:20000, (3*i+i+52)]))/mean(raw_data[18000:20000, (3*i+i+52)])) # dF/F data using the time before pulse as the baseline
  volttrace[[i]] <- data.frame("Time (s)" = raw_data[,1], "Voltage (mV)" = raw_data[, (3*i+(i-1))])
}
ymax = max(tracelist[[2]][20000:40000,2]) + .005 # y bounds for the kinetic data [SCALE]
ymin = -0.08
plot(tracelist[[1]], type="l", ylim=c(ymin, ymax), main=title, xlab="Time (s)", ylab=expression(paste(Delta, "F/F")), lwd=0.4) #Plots first trace, setting the proper labels, bounds etc
pal <- c("#A20DFF", "#49FF26", "#FF1AC1", "#FFDB11", "#1600FF", "#FA0E0C", "#00F9F8", "#8F3907", "#FFF901", "#FE8C00", "#0CADFA", "#0DFF5D") #  palette used for line colors
legg <- c("180 mV", "160 mV", "140 mV", "120 mV", "100 mV", "80 mV", "60 mV", "40 mV", "20 mV", "0 mV", "-20 mV", "-60 mV", "-100 mV") # Creates legend text
for (j in 2:13) {
  points(tracelist[[15-j]], type="l", col=pal[14-j], lwd=0.2) # Adds each trace
}
legend("bottomleft", legend=legg, lty=1, lwd = 5, col=c(pal, "black"), cex=0.6) # Adds legend
if(resp=="y"){ # Plots voltage step graph
  plot(volttrace[[1]], type="l", ylim=c(-100, 220), main=title, xlab="Time (s)", ylab="Voltage (mV)", lwd=1.5)
  for (j in 2:13) {
    points(volttrace[[j]], type="l", col=pal[j-1], lwd=1.5)
  }
  legend("topleft", legend=legg, lty=1, lwd = 5, col=c(pal,"black"), cex=0.8)
}
if (export == "e" || export == "b") {export(trace_data, volttrace, tracelist)} # Exports to excel
if (cansig1) {writeLines(paste("Parameters for the purple curve:", "\nbase: ", par[[1]][1], "\nmax: ", par[[1]][2], "\nvhalf: ", par[[1]][3], "\nrate: ", par[[1]][4]))} #If a sigmoid can fit the data, R spits out the parameters for each regression in the console at the end of the program
if (cansig2) {writeLines(paste("Parameters for the green curve:", "\nbase: ", par[[2]][1], "\nmax: ", par[[2]][2], "\nvhalf: ", par[[2]][3], "\nrate: ", par[[2]][4]))}
if (export == "p" || export == "b") {vsp_pdf(sig = trace_data, parm = par[[1]], parm2 = par[[2]], trc = tracelist, volt = volttrace, title = title, sigbound = c(-0.06, 0.04), kinbound = c(ymin, ymax))} # Exports plots to a pdf file -- works faster than plotting voltage-time
# Y bounds for the pdf export graphs can be changed by changing the values of "sigbound" and "kinbound" in the function above
