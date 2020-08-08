library(stringr)
library(ggplot2)

drawArrivalAvg <- function(folderpath, drawLargeReq, addToPlot){
  tracename <- "trace_file.txt"
  largeSize = 240000
  smallSize = 5
  
  trace <- read.csv(paste(folderpath,"/", tracename, sep = ""))
  traceSplit <- str_split_fixed(trace[,1], ";;;", n= 5)
  
  
  interArrivalTime <- diff(as.numeric(traceSplit[,1]), differences = 1)
  
  # plot(x = c(1:length(interArrivalTime)), y = interArrivalTime/1000000000, type = "p")
  
  traceSplit[,1] <- ceiling(as.numeric(traceSplit[,1])/1000000000)
  reqParam <- str_split_fixed(traceSplit[,5], ";", n= 3)
  
  n_large <- length(which(nchar(reqParam[,2]) == largeSize))
  
  n_small <- length(which(nchar(reqParam[,2]) == smallSize))
  
  # print(which(nchar(reqParam[,2]) == largeSize)+1)
  
  ratio <- n_large/(n_small+n_large)*100
  print(paste(n_large, " + ", n_small , ": ", n_large/(n_small+n_large)*100))
  
  
  queue = matrix(nrow = as.numeric(traceSplit[nrow(traceSplit)]), ncol= 1)
  queueLarge = matrix(nrow = as.numeric(traceSplit[nrow(traceSplit)]), ncol= 1)
  queueSmall = matrix(nrow = as.numeric(traceSplit[nrow(traceSplit)]), ncol= 1)
  queueDummy = matrix(nrow = as.numeric(traceSplit[nrow(traceSplit)]), ncol= 1)
  
  queue[,1] = 0
  queueLarge[,1] = 0
  queueSmall[,1] = 0
  queueDummy[,1] = 1
  
  # print(nrow(traceSplit))
  
  for(i in 1:nrow(traceSplit))
  {
    ####if condition for large request
    if(nchar(reqParam[i,2]) == largeSize){
      queueLarge[as.numeric(traceSplit[i, 1])] <- queueLarge[as.numeric(traceSplit[i, 1])] + 1
    }
    else{
      queueSmall[as.numeric(traceSplit[i, 1])] <- queueSmall[as.numeric(traceSplit[i, 1])] + 1
    }
  
    queue[as.numeric(traceSplit[i, 1])] <- queue[as.numeric(traceSplit[i, 1])] + 1
  
    
    # print(paste("queue[", as.numeric(traceSplit[i, 1]), "]=", queue[as.numeric(traceSplit[i, 1])]))
  }
  print(paste("Large", queueLarge[13,1], "Small", queueSmall[13,1], "Other", queueDummy[13,1]))
  data <- data.frame(proportion = c(rbind(queueSmall, queueLarge, queueDummy)), race = c("Small", "Large", "Other"))
  data$index <- seq(1,nrow(queue), 1)
  
  # print(sum(as.numeric(queue[,1])))
  
  pdf(paste(folderpath, "/", str_split_fixed(tracename, "\\.", n=2)[,1], "",".pdf", sep = ""))
  par(mar=c(5+2, 4+2, 4, 2))

  # if(addToPlot == 1)
  # {
  #   plot(x = c(1:nrow(queue)), y = as.numeric(queue[,1]), type = "l", xlab = "Time, hour", ylab = "# of requests", col = addToPlot)
  # }
  # else{
  #   lines(x = c(1:nrow(queue)), y = as.numeric(queue[,1]), type = "l", xlab = "Time, hour", ylab = "# of requests", col = addToPlot)
  # }
  barplot( as.numeric(queue[,1]), names.arg = c(1:nrow(queue)), ylab = "# of requests", xlim=c(1,60), ylim=c(1,100),width = 1, cex.names = 2, cex.axis = 2, cex.lab = 3)
  title( xlab = "Time (hr)", cex.lab = 3, line = 4)
    # Title not given main = str_split_fixed(folderpath, "/", n=8)[8],
  
  # myplot<- ggplot(data, aes(fill=data$race, y=data$proportion, x=data$index)) + geom_bar(position="stack", stat="identity") + ylab("Percentage") +xlab("Time")
  # xtick<-seq(0, 170, by=12)
  # text(x=xtick,  par("usr")[3], labels = xtick, srt = 45, pos = 1, xpd = TRUE)
  # print(myplot)
  dev.off()
  
  excelData <- matrix(nrow = nrow(queue), ncol = 3)
  excelData[,1] <- queue[,1]
  excelData[,2] <- queueSmall[,1]
  excelData[,3] <- queueLarge[,1]
  
  latencystatsresult <- tibble::rownames_to_column(as.data.frame(excelData), "VALUE")
  # write_xlsx(as.data.frame(latencystatsresult), paste(folderpath, "/", str_split_fixed(tracename, "\\.", n=2)[,1], ".xlsx", sep = ""))
  
  return(data)
}

path <- "/Volumes/YES NAME/trace_FILES/msTrace/trial5"
directories <- list.dirs(path = path ) #first one is the parent directory
addToPlot = 1
# pdf(paste(path, "/",  "line",".pdf", sep = ""))
for(dir in directories[-1]) #all elements except first one
{
  print(dir)
  data<- drawArrivalAvg(dir, drawLargeReq = 0, addToPlot)
  addToPlot = addToPlot + 1
}
# dev.off()



############## test ###############

a <- matrix(nrow = 10, ncol = 3)
a[1:5,1] <- seq(5,5,0)
a[6:10,1] <- seq(2,2,0)
a[1:10,2] <- c(15:24)
a[1:10,3] <- seq(2,20,2)

a.df <- as.data.frame(a)

b <- aggregate(a.df[c("V2", "V3")], a.df["V1"], FUN = mean)

print(b$V1)
