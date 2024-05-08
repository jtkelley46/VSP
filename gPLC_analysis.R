library(readxl)
library(openxlsx)
library(VSP)
library(viridisLite)
raw_data <- as.data.frame(read_excel(file.choose())) #Prompts the user to select an excel file to read from, the file data is stored in raw_data.
trace_data <- data.frame(Voltage = double(), dFF = double())  # Creates empty data frame to read in FV data
atype <- readline("Type A (peak finding) or Type B (record from end of pulse) analysis? (a/b) \n") == "a" # Prompts user to indicate how to retrieve FV data.
#Type A finds the peak for the top 8 voltages and records the FV at the end of the pulse for the rest. Type B records FV at the end of the pulse for all voltages.
stime <- as.numeric(readline(prompt = "How many seconds before voltage step? \n")) * 10000 # Prompts user for the approximate time the voltage pulse begins.
endtime <- -1
j <- stime+14000
while (j < length(raw_data[,1])) { # Scans through whole pulse to find the end
  volt <- raw_data[j+5, 6]
  if (volt < 150) { # When voltage dips below 150 (indicating end of pulse), records time
    endtime = j
    break # ends the search
  }
  j = j + 5 # If end not yet reached, moves forward
} # endtime is the time for the end of the pulse, used later when collecting FV data
volttrace <- list() # Empty list for voltage-time data
tracelist <- list() # Empty list for dFF-time data
for (i in 1:13) {
  tracelist[[i]] <- data.frame("Time (s)" = raw_data[, 1], "dFF" = ((raw_data[,(3*i+1)]))) # Reads in dFF vs time data for Trace i
  volttrace[[i]] <- data.frame("Time (s)" = raw_data[,1], "Voltage (mV)" = raw_data[, (2*i+i)]) # Reads in voltage vs time data for Trace i
}
bleach <- readline("Would you like to correct for bleaching? (y/n) \n")=="y" # Prompts user if they wish to correct for bleaching
if(bleach) {
  tracelist <- bleach_c(tracelist, 3.1, 1) # If you wish to change the bleach correction linear model interval, edit the numbers. First is starting time, second is length, both measured in seconds
}
for (i in 1:13) {
  f1 <- mean(tracelist[[i]][(stime-2000):(stime),2]) # Sets the baseline fluorescence
  tracelist[[i]][,2] <- ((tracelist[[i]][,2] - f1)/f1) # Changes the data stored to deltaF/F
}
if (atype) { # Type A data analysis
  for (i in 1:13) {   #Runs through each trace to collect voltage and dFF data
    max_ave <- -1
    tempvolt <- -100
    volt <- -300
    j <- stime + 1000
    volt2 <- 300
    if (i != 1 && i < 10) { # Takes data by moving average for the top 8 voltages (Trace i is -100mV)
      while (volt2 > -70) {  # Runs through the data range of the voltage step finding the highest average range. Window moves by 10 ms
        new_ave <- mean(tracelist[[i]][j:j+600, 2])
        tempvolt <- mean(raw_data[j:j+500, (2*i+i)])
        if(new_ave > max_ave) { # If the new moving average is greater--save it as max_ave
          volt <- tempvolt
          max_ave <- new_ave
        }
        else if(new_ave + 0.5 < max_ave) { # If the running average is much lower than the max (indicating a down component) the search ends
          break
        }
        j = j+100
        volt2 <- mean(raw_data[j:j+500, (2*i+i)])
      }
    } else { # Takes data at the end of the pulse for lower voltages
      volt <- mean(raw_data[(endtime-510):(endtime-10), 2*i+i])
      max_ave <- mean(tracelist[[i]][(endtime-500):endtime, 2])
    }
    trace_data[i, 1] <- volt # FV data collected above is stored in trace_data
    trace_data[i, 2] <- max_ave
  }
} else { # Type B
  for (i in 1:13) {   #Runs through each trace to collect voltage and dFF data at end of pulse
    volt <- mean(raw_data[(endtime-510):(endtime-10), 2*i+i])
    max_ave <- mean(tracelist[[i]][(endtime-500):endtime, 2])
    trace_data[i, 1] <- volt
    trace_data[i, 2] <- max_ave # FV data collected above is stored in trace_data
  }
}
par <- fit_sigmoid(trace_data, "gplc") # Takes parameters from the sigmoid curve fit function
cansig <- par[1] != FALSE # If there are no parameters (AKA no sigmoid could be fit) then cansig is FALSE
if (cansig) {y2 <- sigmoid(par,-100:200)} # If a sigmoid can be fit, the curve for -100mV to 200 mV is generated
title <- readline(prompt="Title of the graphs? ") # Prompts user to title graph, eg. "200611_HG_gP_1"
ymin = min(trace_data[,2]) - 0.01 # Min and max y values for the FV plot [SCALE]
ymax = max(trace_data[,2]) + 0.01 # Default values are based on the min and max FV +/- 0.01
plot(trace_data[, 1], trace_data[, 2], type="p", main=title, ylim=c(ymin, ymax), xlab="Voltage (mV)", ylab=expression(paste(Delta, "F/F"))) #Plots the actual collected data points
if (cansig) {points (-100:200, y2, type="l")} # Adds the regression curve to the graph
#The above creates plot 1, parameters are printed at the end of the program
exportresp <- readline("Would you like to export these data to an Excel file (e), pdf (p), both (b) or neither (n)? \n") # Export prompt
vresp <- readline("Would you like to plot the voltage step graph? (will take a minute) (y/n) \n") # voltage vs time prompt
ymax = max(c(tracelist[[2]][,2], tracelist[[1]][,2])) + .005 # Min and max y values for dF/F vs time data
ymin = 0-ymax-0.03 # [SCALE]
# pal <- viridis(12) # You may choose to use the Viridis palette, which is more red-green colorblind friendly
pal <- c("#A20DFF", "#49FF26", "#FF1AC1", "#FFDB11", "#1600FF", "#FA0E0C", "#00F9F8", "#8F3907", "#FFF901", "#FE8C00", "#0CADFA", "#0DFF5D") # Creates palette used for line colors
legg <- c("180 mV", "160 mV", "140 mV", "120 mV", "100 mV", "80 mV", "60 mV", "40 mV", "20 mV", "0 mV", "-20 mV", "-60 mV", "-100mV") # Creates the legend, so the later code is more compact
plot(tracelist[[1]], type="l", ylim=c(ymin, ymax), col = "black", main=title, xlab="Time (s)", ylab=expression(paste(Delta, "F/F"))) # Plots the first trace and gives proper labels and bounds
for (j in 2:13) { # Runs through each trace and adds it to the plot
  points(tracelist[[j]], type="l", col=pal[j-1])
}
legend("bottomleft", legend=legg, lty=1, lwd = 5, col=c(pal,"black"), cex=0.8) # Adds legend
if(vresp=="y") { # If you're crazy enough to want Voltage vs time data, this block of code plots it
  plot(volttrace[[1]], type="l", ylim=c(-120, 220), main=title, xlab="Time (s)", ylab="Voltage (mV)", lwd=1) # Plots first line with proper labels and bounds
  for (j in 2:13) { # Each trace is added to the plot
    points(volttrace[[j]][stime:(stime+30000),], type="l", col=pal[j-1], lwd=1) # This line restricts the data plotted so as to not take a year
    # "stime:stime+30000" means the interval from start time (e.g. 4) plus 3 seconds. You may change this, but it's your funeral
  }
  legend("topleft", legend=legg, lty=1, lwd = 5, col=c(pal,"black"), cex=0.8) # Adds legend
}
if(exportresp == "e" || exportresp == "b") {export(trace_data, volttrace, tracelist)} # Calls export function to save data in two excel files. First for FV data, second for voltage, dF/F vs time
if(cansig) {writeLines(paste("Parameters for the sigmoid curve:", "\nbase: ", par[1], "\nmax: ", par[2], "\nvhalf: ", par[3], "\nrate: ", par[4])) # Prints the parameters a, b, c & d
} else {print("Sigmoid curve could not be generated")}
if (exportresp == "p" || exportresp == "b") {vsp_pdf(sig = trace_data, parm = par, trc = tracelist, volt = volttrace, title = title)} # Exports plots to a pdf file -- works faster than plotting voltage-time
