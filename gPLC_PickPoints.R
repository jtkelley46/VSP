library(readxl)
library(VSP)
raw_data <- as.data.frame(read_excel(file.choose())) #Prompts the user to select an excel file to read from, the file data is stored in raw_data.
trace_data <- data.frame(Voltage = double(), dFF = double())  #creates empty data frame
stime <- as.numeric(readline(prompt = "How many seconds before voltage step?")) * 10000
cond <- readline("Would you like to include/exclude traces from the regression? (answer inc, exc or n ) ") #Prompts user to choose if they wish to list out traces to include or exclude
if(cond=="inc") {
  i<-1
  canc <- double()
  print("Which data points would you like to include?\n")
  answ <- readline()
  while (answ!="") { #Enter the indices of traces (e.g. 2 for the +180 mV trace, then hit enter/return to submit it) to exclude, when finished, hit "enter/return" again
    canc[i] <- as.numeric(answ)
    answ <- readline()
    i = i+1
  }
  crt <- 0 #"correction" factor
  all_data <- data.frame(Voltage = double(), dFF = double())
  for (i in 1:13) {   #Runs through each trace to collect voltage and dFF data
    f1 <- mean(raw_data[(stime-2000):(stime), (3*i+1)])
    max_ave <- 0
    j <- stime + 1000
    while (j<(stime+20001)) {  #Runs through the data range of the voltage step finding the highest average range. Window moves by 10 ms
      new_ave <- mean(raw_data[j:j+500, (3*i+1)])
      tempvolt <- mean(raw_data[j:j+500, (2*i+i)])
      if (tempvolt > volt + 1) {
        volt <- tempvolt
      }
      if(new_ave > max_ave) {
        max_ave <- new_ave
      }
      else if(new_ave + 0.1 < max_ave){
        break
      }
      j = j+100
    }
    all_data[i, 1] <- volt
    all_data[i, 2] <- (max_ave - f1) / f1
    if (i%in%canc) { #If trace "i" is to be included in regression
      trace_data[(i-crt), 1] <- volt
      trace_data[(i-crt), 2] <- (max_ave - f1) / f1
    }
    else {
      crt = crt+1
    }
  }
} else if(cond=="exc") {
  canc <- double()
  print("Which data points would you like to exclude?\n")
  answ <- readline()
  i <- 1
  while (answ!="") {
    canc[i] <- as.numeric(answ)
    answ <- readline()
    i = i+1
  }
  crt <- 0
  all_data <- data.frame(Voltage = double(), dFF = double())
  for (i in 1:13) {   #Runs through each trace to collect voltage and dFF data
    f1 <- mean(raw_data[(stime-2000):(stime), (3*i+1)])
    max_ave <- 0
    j <- stime + 1000
    volt <- -300
    while (j<(stime+15001)) {  #Runs through the data range of the voltage step finding the highest average range. Window moves by 10 ms
      new_ave <- mean(raw_data[j:j+500, (3*i+1)])
      tempvolt <- mean(raw_data[j:j+500, (2*i+i)])
      if (tempvolt > volt + 1) {
        volt <- tempvolt
      }
      if(new_ave > max_ave) {
        max_ave <- new_ave
      }
      else if(new_ave + 0.1 < max_ave){
        break
      }
      j = j+100
    }
    all_data[i, 1] <- volt
    all_data[i, 2] <- (max_ave - f1) / f1
    if (!(i%in%canc)) {
      trace_data[(i-crt), 1] <- volt
      trace_data[(i-crt), 2] <- (max_ave - f1) / f1
    }
    else {
      crt = crt+1
    }
  }
} else { #If no inclusion or exclusion, just a safety block
  for(i in 1:13) {   #Runs through each trace to collect voltage and dFF data
    f1 <- mean(raw_data[(stime-2000):(stime), (3*i+1)])
    max_ave <- 0
    j <- stime + 1000
    while (j<(stime+15001)) {  #Runs through the data range of the voltage step finding the highest average range. Window moves by 10 ms
      new_ave <- mean(raw_data[j:j+500, (3*i+1)])
      tempvolt <- mean(raw_data[j:j+500, (2*i+i)])
      if (tempvolt > volt + 1) {
        volt <- tempvolt
      }
      if(new_ave > max_ave) {
        max_ave <- new_ave
      }
      else if(new_ave + 0.1 < max_ave){
        break
      }
      j = j+100
    }
    trace_data[(i), 1] <- volt
    trace_data[(i), 2] <- (max_ave - f1) / f1
  }
  all_data <- trace_data
}
y <- trace_data[,2]
funct <- y ~ a + b/(1+exp((c-trace_data[,1])/d)) #Creates a formula "funct" which describes the relation between V and dF/F with equation parameters a (base), b (max), c (xhalf), d (rate).
fitmodel <- nls(funct, start=list(a=-0.1, b=0.048, c=29, d=5), control= nls.control(minFactor=1/4096, tol=0.1)) #Creates a model to fit the data using "funct" and three arbitrary starting points for a, b, c.
par <- coef(fitmodel) #Pulls the equation parameters a, b, c & d from fitmodel
y2 <- sigmoid(par,-100:200)
title <- readline(prompt="Title of the sigmoid graph?")
plot(all_data[,1], all_data[,2], type="p", main=title, xlab="Voltage (mV)", ylab=expression(paste(Delta, "F/F"))) #Plots the actual collected data points
points (-100:200, y2, type="l")
writeLines(paste("Parameters for the sigmoid curve:\n", "base: ", par[1], "\nmax: ", par[2], "\nvhalf: ", par[3], "\nrate: ", par[4]))
