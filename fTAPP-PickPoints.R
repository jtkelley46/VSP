library(readxl)
library(VSP)
raw_data <- as.data.frame(read_excel(file.choose())) #Prompts the user to select an excel file to read from, the file data is stored in raw_data.
trace_data <- data.frame(Voltage = double(), dFF1 = double(), dFF2 = double())  #creates empty data frame
stime <- as.numeric(readline(prompt = "How many seconds before voltage step? ")) * 10000
symcond <- readline("Would you like to include/exclude the same points from both curves? (y/n) \n")
if (symcond=="y") {
  cond <- readline("Would you like to include (a) or exclude (b) points? \n")
  if (cond == "a") {
    canc <- double()
    i <- 1
    print("Which data points would you like to include? \n")
    answ <- readline()
    while (answ!="") {
      canc[i] <- parse_number(answ)
      answ <- readline()
      i = i+1
    }
    crt<-0
    pdata <- data.frame(V = double(), dFF1 = double(), dFF2 = double())
    for (i in 1:13) {   #Runs through each trace to collect voltage and dFF data
      f1 <- mean(raw_data[(stime-2000):(stime), (3*i+i+52)])
      max_ave <- 0
      f3 <- -1
      j <- stime + 1000
      while (j<(stime+20001)) {  #Runs through the data range of the voltage step finding the highest average range. Window moves by 10 ms
        new_ave <- mean(raw_data[j:j+500, (3*i+i+52)])
        if(new_ave > max_ave) {
          max_ave <- new_ave
          volt <- mean(raw_data[j:j+500, (3*i+i-1)])
        }
        else if(new_ave + 0.1 < max_ave){
          break
        }
        j = j+100
      }
      j <- stime+15000
      while(i!=1 & j < stime +40000) {
        volt2 <- raw_data[(j+5), (3*i+i-1)]
        if (volt2 < -70) {
          f3 <- mean(raw_data[(j-500):j,(3*i+i+52) ])
          break
        }
        j = j+5
      }
      if(i==1) {
        f3 <- mean(raw_data[(j-500):j,56])
      }
      pdata[i, 1] <- volt
      pdata[i, 2] <- (max_ave - f1) / f1
      pdata[i, 3] <- (f3-max_ave) / max_ave
      if (i%in%canc) {
        trace_data[(i-crt),1] <- volt
        trace_data[(i-crt),2] <- (max_ave - f1) / f1
        trace_data[(i-crt),3] <- (f3-max_ave) / max_ave
      } else {
        crt=crt+1
      }
    }
  } else if (cond == "b") {
    canc <- double()
    i <- 1
    print("Which data points would you like to exclude? \n")
    answ <- readline()
    while (answ!="") {
      canc[i] <- parse_number(answ)
      answ <- readline()
      i = i+1
    }
    crt<-0
    pdata <- data.frame(V = double(), dFF1 = double(), dFF2 = double())
    for (i in 1:13) {   #Runs through each trace to collect voltage and dFF data
      f1 <- mean(raw_data[(stime-2000):(stime), (3*i+i+52)])
      max_ave <- 0
      f3 <- -1
      j <- stime + 1000
      while (j<(stime+20001)) {  #Runs through the data range of the voltage step finding the highest average range. Window moves by 10 ms
        new_ave <- mean(raw_data[j:j+500, (3*i+i+52)])
        if(new_ave > max_ave) {
          max_ave <- new_ave
          volt <- mean(raw_data[j:j+500, (3*i+i-1)])
        }
        else if(new_ave + 0.1 < max_ave){
          break
        }
        j = j+100
      }
      j <- stime+15000
      while(i!=1 & j < stime +40000) {
        volt2 <- raw_data[(j+5), (3*i+i-1)]
        if (volt2 < -70) {
          f3 <- mean(raw_data[(j-500):j,(3*i+i+52) ])
          break
        }
        j = j+5
      }
      if(i==1) {
        f3 <- mean(raw_data[(j-500):j,56])
      }
      pdata[i, 1] <- volt
      pdata[i, 2] <- (max_ave - f1) / f1
      pdata[i, 3] <- (f3-max_ave) / max_ave
      if (!(i%in%canc)) {
        trace_data[(i-crt),1] <- volt
        trace_data[(i-crt),2] <- (max_ave - f1) / f1
        trace_data[(i-crt),3] <- (f3-max_ave) / max_ave
      } else {
        crt=crt+1
      }
    }
  }
  y <- trace_data[,2]
  y2 <- trace_data[,3]
  funct <- y ~ a + b/(1+exp((c-trace_data[,1])/d))
  funct2 <- y2 ~ a + b/(1+exp((c-trace_data[,1])/d))
} else if(symcond=="n") {
  reg_data1 <- data.frame(V=double(), dFF=double())
  reg_data2 <- data.frame(V=double(), dFF=double())
  condx <- readline("Include (a) or exclude (b) from top curve? \n") == "a"
  condy <- readline("Include (a) or exclude (b) from bottom curve? \n") == "a"
  cancx <- double()
  cancy <- double()
  if (condx) {
    i <- 1
    print("Which data points would you like to include from the top curve? \n")
    answ <- readline()
    while (answ!="") {
      cancx[i] <- parse_number(answ)
      answ <- readline()
      i = i+1
    }
  } else {
    i <- 1
    print("Which data points would you like to exclude from the top curve? \n")
    answ <- readline()
    while (answ!="") {
      cancx[i] <- parse_number(answ)
      answ <- readline()
      i = i+1
    }
  }
  if (condy) {
    i <- 1
    print("Which data points would you like to include from the bottom curve? \n")
    answ <- readline()
    while (answ!="") {
      cancy[i] <- parse_number(answ)
      answ <- readline()
      i = i+1
    }
  } else {
    i <- 1
    print("Which data points would you like to exclude from the bottom curve? \n")
    answ <- readline()
    while (answ!="") {
      cancy[i] <- parse_number(answ)
      answ <- readline()
      i = i+1
    }
  }
  crtx<-0
  crty<-0
  pdata <- data.frame(V = double(), dFF1 = double(), dFF2 = double())
  for (i in 1:13) {   #Runs through each trace to collect voltage and dFF data
    f1 <- mean(raw_data[(stime-2000):(stime), (3*i+i+52)])
    max_ave <- 0
    f3 <- -1
    j <- stime + 1000
    while (j<(stime+20001)) {  #Runs through the data range of the voltage step finding the highest average range. Window moves by 10 ms
      new_ave <- mean(raw_data[j:j+500, (3*i+i+52)])
      if(new_ave > max_ave) {
        max_ave <- new_ave
        volt <- mean(raw_data[j:j+500, (3*i+i-1)])
      }
      else if(new_ave + 0.1 < max_ave){
        break
      }
      j = j+100
    }
    j <- stime+15000
    while(i!=1 & j < stime +40000) {
      volt2 <- raw_data[(j+5), (3*i+i-1)]
      if (volt2 < -70) {
        f3 <- mean(raw_data[(j-500):j,(3*i+i+52) ])
        break
      }
      j = j+5
    }
    if(i==1) {
      f3 <- mean(raw_data[(j-500):j,56])
    }
    pdata[i, 1] <- volt
    pdata[i, 2] <- (max_ave - f1) / f1
    pdata[i, 3] <- (f3-max_ave) / max_ave
    if (condx && i%in%cancx) {
      reg_data1[(i-crtx),1] <- volt
      reg_data1[(i-crtx),2] <- (max_ave - f1) / f1
    } else if(!condx && !(i%in%cancx)) {
      reg_data1[(i-crtx),1] <- volt
      reg_data1[(i-crtx),2] <- (max_ave - f1) / f1
    } else {
      crtx=crtx+1
    }
    if (condy && i%in%cancy) {
      reg_data2[(i-crty),1] <- volt
      reg_data2[(i-crty),2] <- (f3-max_ave) / max_ave
    } else if(!condy && !(i%in%cancy)) {
      reg_data2[(i-crty),1] <- volt
      reg_data2[(i-crty),2] <- (f3-max_ave) / max_ave
    } else {
      crty=crty+1
    }
  }
  y <- reg_data1[,2]
  y2 <- reg_data2[,2]
  funct <- y ~ a + b/(1+exp((c-reg_data1[,1])/d))
  funct2 <- y2 ~ a + b/(1+exp((c-reg_data2[,1])/d))
}
fitmodel <- try(nls(funct, start=list(a=-0.1, b=0.048, c=29, d=5), control= nls.control(minFactor=1/4096, tol=0.1))) #Creates a model to fit the data using "funct" and three arbitrary starting points for a, b, c.
fitmodel2 <- try(nls(funct2, start=list(a=-0.1, b=0.048, c=29, d=-5), control= nls.control(minFactor=1/4096, tol=0.1)))
cansig1 <- class(fitmodel) != "try-error"
cansig2 <- class(fitmodel) != "try-error"
if (cansig1) {
  par <- coef(fitmodel)
  y3 <- sigmoid(par,-100:200)
}
if (cansig2) {
  par2 <- coef(fitmodel2)
  y4 <- sigmoid(par2,-100:200)
}
title <- readline(prompt="Title of the sigmoid graph? ")
ymax <- max(pdata[,2]) + 0.01
plot(pdata[,1], pdata[,2], type="p", col= "purple", pch= 16, ylim = c((-1.5*ymax), ymax),  main=title, xlab="Voltage (mV)", ylab=expression(paste(Delta, "F/F"))) #Plots the actual collected data points
points(pdata[,1], pdata[,3], type="p", col="forestgreen", pch=16)
if (cansig1) {
  points(-100:200, y3, type="l", col="purple")
  writeLines(paste("Parameters for the purple sigmoid curve:\n", "base: ", par[1], "\nmax: ", par[2], "\nvhalf: ", par[3], "\nrate: ", par[4]))
  }
if(cansig2) {
  points(-100:200, y4, type="l", col="forestgreen")
  writeLines(paste("Parameters for the green sigmoid curve:\n", "base: ", par2[1], "\nmax: ", par2[2], "\nvhalf: ", par2[3], "\nrate: ", par2[4]))
  }
