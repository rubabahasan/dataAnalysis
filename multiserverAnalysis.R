library(stringr)  #str_split_fixed
library(moments)  #skewness, kurtosis
library(sjstats)  #cv
library(writexl)     #write_xlsx
library(dplyr) #rite rownames as column


drawScatterGraphTLresponse <- function(tracelineRsplit, latency, reqResponse, folderPath){
  jpeg(paste(folderPath, "traceresgraph.jpg", sep = ""))
  # plot(NA, xlim=c(0,80), ylim=c(0,6000), xlab="x", ylab="y")
  # segments((as.numeric(tracelineRsplit[,1])/1000000000), 1:length(latency), (as.numeric(tracelineRsplit[,1])/1000000000), 1:length(latency))

  #plots latencies of login and post vs request index, giving an idea on how latency changes with time
  
  ##### old
  plot(1:length(latency[,1]), y = (latency[,1]/1000000000), ylim = c(0, 1), type = "p", col= ifelse(str_detect(tracelineRsplit[,5], "POST_SELF_WALL"), "red", ifelse(str_detect(tracelineRsplit[,5], "LOG_IN"),"blue", "black")), legend = c("login", "post"))
  # points(1:length(latency[,2]), y = (latency[,2]/1000000000), type = "p", col= ifelse(str_detect(tracelineRsplit[,5], "POST_SELF_WALL"), "green4", ifelse(str_detect(tracelineRsplit[,5], "LOG_IN"),"purple", "black")), legend = c("login delay", "post delay"))
  # points(1:length(latency[,3]), y = (latency[,3]/1000000000), type = "p", col= ifelse(str_detect(tracelineRsplit[,5], "POST_SELF_WALL"), "yellow", ifelse(str_detect(tracelineRsplit[,5], "LOG_IN"),"deeppink", "black")), legend = c("login duration", "post duration"))
  
  # plot(1:length(latency), y = (latency/1000000000), type = "p", col= ifelse(str_detect(tracelineRsplit[,4], "POST_SELF_WALL"), "red", ifelse(str_detect(tracelineRsplit[,4], "LOG_IN"),"blue", "black")), legend = c("login", "post"))
  
  # points(1:nrow(reqResponse), y = reqResponse[, 2]/1000000000, col = "green")
  
  ##### old
  # legend(x=max(latency[,1]/1000000000),legend=c("post", "login", "req response", "login delay", "post delay", "login duration", "post duration") ,col=c("red", "blue", "green", "green4", "purple", "yellow", "deeppink"), pch=1)
  legend(x=5,legend=c("post", "login", "req response") ,col=c("red", "blue", "green"), pch=1)
  
  dev.off()
}

drawAvgScatterGraphTLresponse <- function(tracelineRsplit, latency, reqResponse, folderPath, n){
  avgMean <- colMeans(matrix(latency[,1], nrow=n))
  # print(paste(dim(latency), length(avg)))
  jpeg(paste(folderPath, "traceresAvggraph.jpg", sep = ""))
  barplot((avgMean/1000000000), names.arg = 1:length(avgMean), xlab = "Request count", ylab = "latency, sec", main = str_split_fixed(folderPath, "/", n=9)[9], ylim = c(0, 0.7))
  dev.off()
}

#function gets matrix of traceline values and returns matrix of queuelength in each time segment  ##### old structure
queuelength <- function(tracelineRsplit, N){
  numberOfSegment = N
  start = as.numeric(tracelineRsplit[1,1])
  end = as.numeric(tracelineRsplit[nrow(tracelineRsplit), 3])
  
  segmentSize = (end-start)/numberOfSegment
  
  queue <- matrix(nrow = numberOfSegment, ncol = 3)
  
  queue[1,1] <- start
  queue[1,2] <- queue[1,1] + segmentSize
  queue[1,3] <- 0
  
  for(i in 2:numberOfSegment)
  {
    queue[i, 1] <- queue[i-1, 2]
    queue[i, 2] <- queue[i, 1] + segmentSize
    queue[i, 3] <- 0
  }
  
  #print(queue)
  
  queue[numberOfSegment, 2] <- end
  
  for(i in 1:nrow(tracelineRsplit))
  {
    startidx = ceiling((as.numeric(tracelineRsplit[i, 1])-start)/segmentSize)
    if(startidx < 1){
      startidx = 1
    }
    
    for(j in startidx:nrow(queue))
    {
      #print(paste("#################################", startidx))
      #print(queue)
      #print(paste(tracelineRsplit[i,1], tracelineRsplit[i,3], " vs ", queue[j,1], queue[j,2]))
      if(as.numeric(tracelineRsplit[i,3]) < as.numeric(queue[j,1])){
        break
      }else if(as.numeric(tracelineRsplit[i,1]) > queue[j,2]){
        #do nothing
      }else{
        queue[j,3] = queue[j,3] + 1
      }
    }
  }

  queue[,1] <- queue[,1]/1000000000
  queue[,2] <- queue[,2]/1000000000
  jpeg(paste(folderPath, "queuegraph.jpg", sep = ""))
  plot(1:nrow(queue), y = queue[,3], type = "p")
  dev.off()

  return(queue)
}

analyzeReqResponse <- function(reqResponse, path){
  reqResponseDelays <- matrix(nrow = nrow(reqResponse), ncol = 5)
  reqResponseDelays[,1] <- as.character(reqResponse[,1])
  reqResponseDelays[,2] <- as.numeric(reqResponse[,2])
  reqResponseDelays[,3] <- as.numeric(reqResponse[,5]) - as.numeric(reqResponse[,4])
  reqResponseDelays[,4] <- as.numeric(reqResponse[,6]) - as.numeric(reqResponse[,5])
  reqResponseDelays[,5] <- as.numeric(reqResponse[,7]) - as.numeric(reqResponse[,6])
  
  # print(reqResponseDelays)
  # par(mfrow=c(2,2))
  jpeg(paste(path, "/reqResponse_latencygraph.jpg", sep = ""))
  plot(1:nrow(reqResponseDelays), y = (as.numeric(reqResponseDelays[,2])/1000000000), ylim = c(0, 3), xlab = "Request index", ylab = "Latency, sec", type = "p", col= ifelse(str_detect(reqResponseDelays[,1], "POST_SELF_WALL"), "red", ifelse(str_detect(reqResponseDelays[,1], "LOG_IN"),"blue", "black")))
  # plot(1:15, y = (as.numeric(reqResponseDelays[1:15,2])/1000000000), xlab = "Request index", ylab = "Latency, sec", type = "p", col= ifelse(str_detect(reqResponseDelays[,1], "POST_SELF_WALL"), "red", ifelse(str_detect(reqResponseDelays[,1], "LOG_IN"),"blue", "black")))
  # legend(x=max(as.numeric(reqResponseDelays[,2])/1000000000),legend=c("post", "login", "req response", "login delay", "post delay", "login duration", "post duration") ,col=c("red", "blue", "green", "green4", "purple", "yellow", "deeppink"), pch=1)
  
  dev.off()
  
  jpeg(paste(path, "/reqResponse_delay1graph.jpg", sep = ""))
  plot(1:nrow(reqResponseDelays), y = (as.numeric(reqResponseDelays[,3])/1000000000), ylim = c(0, 2.5), xlab = "Request index", ylab = "Latency, sec", type = "p", col= ifelse(str_detect(reqResponseDelays[,1], "POST_SELF_WALL"), "red", ifelse(str_detect(reqResponseDelays[,1], "LOG_IN"),"blue", "black")))
  # plot(1:15, y = (as.numeric(reqResponseDelays[1:15,3])/1000000000), xlab = "Request index", ylab = "Latency, sec", type = "p", col= ifelse(str_detect(reqResponseDelays[,1], "POST_SELF_WALL"), "red", ifelse(str_detect(reqResponseDelays[,1], "LOG_IN"),"blue", "black")))
  
  dev.off()
  
  jpeg(paste(path, "/reqResponse_delay2graph.jpg", sep = ""))
  plot(1:nrow(reqResponseDelays), y = (as.numeric(reqResponseDelays[,4])/1000000000), ylim = c(0, 2), xlab = "Request index", ylab = "Latency, sec", type = "p", col= ifelse(str_detect(reqResponseDelays[,1], "POST_SELF_WALL"), "red", ifelse(str_detect(reqResponseDelays[,1], "LOG_IN"),"blue", "black")))
  # plot(1:15, y = (as.numeric(reqResponseDelays[1:15,4])/1000000000),  xlab = "Request index", ylab = "Latency, sec", type = "p", col= ifelse(str_detect(reqResponseDelays[,1], "POST_SELF_WALL"), "red", ifelse(str_detect(reqResponseDelays[,1], "LOG_IN"),"blue", "black")))
  dev.off()
  
  jpeg(paste(path, "/reqResponse_delay3graph.jpg", sep = ""))
  plot(1:nrow(reqResponseDelays), y = (as.numeric(reqResponseDelays[,5])/1000000000), ylim = c(0, 2), xlab = "Request index", ylab = "Latency, sec", type = "p", col= ifelse(str_detect(reqResponseDelays[,1], "POST_SELF_WALL"), "red", ifelse(str_detect(reqResponseDelays[,1], "LOG_IN"),"blue", "black")))
  # plot(1:15, y = (as.numeric(reqResponseDelays[1:15,5])/1000000000), xlab = "Request index", ylab = "Latency, sec", type = "p", col= ifelse(str_detect(reqResponseDelays[,1], "POST_SELF_WALL"), "red", ifelse(str_detect(reqResponseDelays[,1], "LOG_IN"),"blue", "black")))
  dev.off()
  
  
  return(reqResponseDelays)
}

calculateThroughput<- function(tracelineRsplit){
  startTime = as.numeric(tracelineRsplit[1,1])
  endTime = as.numeric(tracelineRsplit[nrow(tracelineRsplit),4])
  throughput = nrow(tracelineRsplit)*1000000000/(endTime-startTime)
  return(throughput)
}

statsFromFolder <- function(folderPath){
  #define stats length here
  
  traceName <- unlist(strsplit(folderPath, "/"))[8]
  #stats[0] <- traceName

  ############################ Read all filenames from folder ##############################
  web_serverFile <- list.files(path = folderPath , pattern = "^web+.*csv$")
  memcacheDFile <- list.files(path = folderPath , pattern = "^memcache+.*csv$")
  mysqlFile <- list.files(path = folderPath , pattern = "^mysql+.*csv$")
  reqResponseFile <- list.files(path = folderPath , pattern = "^request+.*csv$")
  tacelineResponseFile <- list.files(path = folderPath , pattern = "^traceline+.*csv$")
  traceFile <- list.files(path = folderPath , pattern = "^trace+.*txt$")

  # print(web_serverFile)
  # print(memcacheDFile)
  # print(mysqlFile)
  # print(reqResponseFile)
  # print(tacelineResponseFile)
  # print(traceFile)
  
  ############################# Read csv contents of all files #############################
  if(length(web_serverFile) > 1){ #multiple server running
    print("Many servers")
    serverfilename = paste(folderPath, slash, web_serverFile, sep = "")
    web_server <- lapply(serverfilename,read.csv)
  }else{ #only one server running
    print(paste("One server", web_serverFile))
    if(is.null(web_serverFile)){
      web_server <- read.csv(paste(folderPath, slash, web_serverFile, sep = ""))
    }else
    {
      web_server <- NULL
    }
   
  }
  # memcache <- read.csv(paste(folderPath, slash, memcacheDFile, sep = ""))
  
  # mysql <- read.csv(paste(folderPath, slash, mysqlFile, sep = ""))

  reqResponse <- read.csv(paste(folderPath, slash, reqResponseFile, sep = ""))
  # reqResponse <- reqResponse[-2895,]
  
  tracelineResponse <- read.csv(paste(folderPath, slash, tacelineResponseFile, sep = ""))
  tracelineRsplit <- str_split_fixed(tracelineResponse[,1], ";;;", n= 6)
  
### latency matrix[total duration, delay from start time to actial start, actual duration of run]#####
  ##### old
  latency <- matrix(nrow = nrow(tracelineRsplit), ncol = 3)
  latency[,1] <- as.numeric(tracelineRsplit[,4]) - as.numeric(tracelineRsplit[,1])
  latency[,2] <- as.numeric(tracelineRsplit[,2]) - as.numeric(tracelineRsplit[,1])
  latency[,3] <- as.numeric(tracelineRsplit[,4]) - as.numeric(tracelineRsplit[,2])
  # latency <- as.numeric(tracelineRsplit[,3]) - as.numeric(tracelineRsplit[,1])
  
   
  #print(paste(folderPath, slash, traceFile, sep = ""))
  # trace <- read.csv(paste(folderPath, slash, traceFile, sep = ""))
  # #print("Read trace")
  # traceSplit <- str_split_fixed(trace[,1], ";;;", n= 5)
  # 
  # interArrivalTime <- diff(as.numeric(traceSplit[,1]), differences = 1)

  for(i in (1:length(web_server)))
  {
    # web_server[[i]][2:dim(web_server[[i]])[1],6] <- diff(as.numeric(web_server[[i]][,6]), differences = 1)
    # web_server[[i]][2:dim(web_server[[i]])[1],8] <- diff(as.numeric(web_server[[i]][,8]), differences = 1)

  }

  # memcache[2:dim(memcache)[1],6] <- diff(as.numeric(memcache[,6]), differences = 1)
  # memcache[2:dim(memcache)[1],8] <- diff(as.numeric(memcache[,8]), differences = 1)
  # 
  # mysql[2:dim(mysql)[1],6] <- diff(as.numeric(mysql[,6]), differences = 1)
  # mysql[2:dim(mysql)[1],8] <- diff(as.numeric(mysql[,8]), differences = 1)

  ############################# Calculate Stats #############################


  if(length(web_serverFile) > 1)
  {
    #multiple web server
    print("Here")
    web_serverStats = matrix(nrow = 1, ncol = length(web_server)*5*4) # 5 features, 4 statistics for each

    for(serveridx in (1:length(web_server)))
    {
      # Want to keep one mean for all servers
      # # meanlist <- (1:length(web_server))
      # # skewlist <- (1:length(web_server))
      # # kurtlist <- (1:length(web_server))
      # # cvlist <- (1:length(web_server))
      offset = (serveridx-1) * 5 * 4
      i=0
      for(j in c(2,4,5,6,8))
      {
        # Want to keep one mean for all servers
        # # meanlist[serveridx] = mean(web_server[[serveridx]][,j])
        # # skewlist[serveridx] = skewness(web_server[[serveridx]][,j])
        # # kurtlist[serveridx] = kurtosis(web_server[[serveridx]][,j])
        # # cvlist[serveridx] = cv(web_server[[serveridx]][,j])
        #print(meanlist)

        web_serverStats[1,offset + (i*4)+1] <- mean(web_server[[serveridx]][,j])
        web_serverStats[1,offset + (i*4)+2] <- skewness(web_server[[serveridx]][,j])
        web_serverStats[1,offset + (i*4)+3] <- kurtosis(web_server[[serveridx]][,j])
        web_serverStats[1,offset + (i*4)+4] <- cv(web_server[[serveridx]][,j])

        i = i+1
      }
      #  keep only one mean for all servers . mean of each of mean, skew, kurtosis and cv for for each server
      # # web_serverStats[1,(i*4)+1] <- mean(meanlist)
      # # web_serverStats[1,(i*4)+2] <- mean(web_server[[1]][,j]) # web memory use
      # # web_serverStats[1,(i*4)+3] <- mean(web_server[[1]][,j]) # web memory use
      # # web_serverStats[1,(i*4)+4] <- mean(web_server[[1]][,j]) # web memory use
      # # print("Printing")
      # # print(i*5 +1)
      # # print(j)
      # i = i+1
    }
   }else{
    #single web server
    web_serverStats = matrix(nrow = 1, ncol = 20)
    i = 0
    for(j in c(2,4,5,6,8))
    {
      # web_serverStats[1,(i*4)+1] <- mean(web_server[,j]) # web memory use
      # web_serverStats[1,(i*4)+2] <- skewness(web_server[,j]) # web memory use
      # web_serverStats[1,(i*4)+3] <- kurtosis(web_server[,j]) # web memory use
      # web_serverStats[1,(i*4)+4] <- cv(web_server[,j]) # web memory use
      # # print("Printing")
      # # print(i*5 +1)
      # # print(j)
      # 
      # i = i + 1
    }
   }

  datastats = matrix(nrow = 1, ncol = 40)
  # i = 0
  # for(j in c(2,4,5,6,8))
  # {
  #   datastats[1,(i*4)+1] <- mean(memcache[,j]) # memcache
  #   datastats[1,(i*4)+2] <- skewness(memcache[,j]) # memcache
  #   datastats[1,(i*4)+3] <- kurtosis(memcache[,j]) # memcache
  #   datastats[1,(i*4)+4] <- cv(memcache[,j]) # memcache
  #   # print("Printing")
  #   # print(i*5 +1)
  #   # print(j)
  # 
  #   i = i + 1
  # }
  # 
  # #mysql
  # #i = 10
  # for(j in c(2,4,5,6,8))
  # {
  #   datastats[1,(i*4)+1] <- mean(mysql[,j]) # mysql
  #   datastats[1,(i*4)+2] <- skewness(mysql[,j]) # mysql
  #   datastats[1,(i*4)+3] <- kurtosis(mysql[,j]) # mysql
  #   datastats[1,(i*4)+4] <- cv(mysql[,j]) # mysql
  #   # print("Printing")
  #   # print(i*5 +1)
  #   # print(j)
  # 
  #   i = i + 1
  # }
  
  #### old
  latencystats = matrix(nrow = 1, ncol = 32)
  # latencystats = matrix(nrow = 1, ncol = 16)
  
  # print(paste("tracelineRsplit 4 ", tracelineRsplit[,4]))
  
  # drawScatterGraphTLresponse(tracelineRsplit, latency, reqResponse, folderPath)
  drawAvgScatterGraphTLresponse(tracelineRsplit, latency, reqResponse, folderPath, 10)

  print(paste("plotted points", folderPath))
  ### old
  latencystats[1, 1] <- mean(latency[,1])/1000000000
  latencystats[1, 2] <- skewness(latency[,1])
  latencystats[1, 3] <- kurtosis(latency[,1])
  latencystats[1, 4] <- cv(latency[,1])
  latencystats[1, 5:8] <- quantile(latency[,1], c(0.5, 0.9, 0.95, 0.99))/1000000000

  latencystats[1, 9] <- mean(latency[,2])/1000000000
  latencystats[1, 10] <- skewness(latency[,2])
  latencystats[1, 11] <- kurtosis(latency[,2])
  latencystats[1, 12] <- cv(latency[,2])
  latencystats[1, 13:16] <- quantile(latency[,2], c(0.5, 0.9, 0.95, 0.99))/1000000000

  latencystats[1, 17] <- mean(latency[,3])/1000000000
  latencystats[1, 18] <- skewness(latency[,3])
  latencystats[1, 19] <- kurtosis(latency[,3])
  latencystats[1, 20] <- cv(latency[,3])
  latencystats[1, 21:24] <- quantile(latency[,3], c(0.5, 0.9, 0.95, 0.99))/1000000000

  latencystats[1, 25] <- mean(reqResponse[,2])/1000000000
  latencystats[1, 26] <- skewness(reqResponse[,2])
  latencystats[1, 27] <- kurtosis(reqResponse[,2])
  latencystats[1, 28] <- cv(reqResponse[,2])
  latencystats[1, 29:32] <- quantile(reqResponse[,2], c(0.5, 0.9, 0.95, 0.99))/1000000000
  # latencystats[1, 1] <- mean(latency)/1000000000
  # latencystats[1, 2] <- skewness(latency)
  # latencystats[1, 3] <- kurtosis(latency)
  # latencystats[1, 4] <- cv(latency)
  # latencystats[1, 5:8] <- quantile(latency, c(0.5, 0.9, 0.95, 0.99))/1000000000
  # 
  # latencystats[1, 9] <- mean(reqResponse[,2])/1000000000
  # latencystats[1, 10] <- skewness(reqResponse[,2])
  # latencystats[1, 11] <- kurtosis(reqResponse[,2])
  # latencystats[1, 12] <- cv(reqResponse[,2])
  # latencystats[1, 13:16] <-quantile(reqResponse[,2], c(0.5, 0.9, 0.95, 0.99))/1000000000

  # delays <- analyzeReqResponse(reqResponse, folderPath)
  # print(delays)
  
  queue = matrix(nrow = nrow(tracelineRsplit), ncol = 2)
  # queue = queuelength(tracelineRsplit, 1000)

  throughput <- 0
  # throughput<- calculateThroughput(tracelineRsplit)
  #return(list(web_server, web_serverStats, datastats))
  return(list(web_serverStats, datastats, latencystats, latency, reqResponse[,2], queue, throughput))
}


#rootpathDir = "~/Documents/Research/Data/TracePoisson/orig"
#rootpathDir = "/Users/rxh655/Documents/Research/Data/throughput_calc/trace8WebLogin0"
# rootpathDir = "/Users/rxh655/Documents/Research/Data/debug/comparison_login_mix"
# rootpathDir = "/Users/rxh655/The Pennsylvania State University/Sajal, Sultan Mahmud - Research/TraceDownscaler/experiment-11/results/aggregate_folder"
# rootpathDir = "/Users/rxh655/Documents/Research/Data/throughput_calc/login_1"
rootpathDir = "/Users/rxh655/Documents/Research/Data/large_small/run4/output/block1/aggregate_folder"
slash = "/"
#result <- statsFromFolder(rootpathDir)

directories <- list.dirs(path = rootpathDir ) #first one is the parent directory
nRow <- length(directories)-1 #number of rows of data

result <- list()
webserverstatsresult <- matrix(nrow = length(directories)-1, ncol = 160)

###old
latencystatsresult <- matrix(nrow = length(directories)-1, ncol = 33)
# latencystatsresult <- matrix(nrow = length(directories)-1, ncol = 16)

colnames(latencystatsresult) <- c("tracelineResponseMean", "tracelineResponseSkew", "tracelineResponseKurt", "tracelineResponseCV", "tracelineResponse50", "tracelineResponse90", "tracelineResponse95" , "tracelineResponse99", "delayMean", "delaySkew", "delayKurt", "delayCV", "delay50", "delay90", "delay95" , "delay99", "durationMean", "durationSkew", "durationKurt", "durationCV", "duration50", "duration90", "duration95" , "duration99","reqResponseMean", "reqResponseSkew", "reqResponseKurt", "reqResponseCV", "reqResponse50", "reqResponse90", "reqResponse95", "reqResponse99", "throughput")
rownames(latencystatsresult) <- seq(1,nRow,1)
idx = 1


for(dir in directories[-1]) #all elements except first one
{
  print(dir)
  traceName <- unlist(strsplit(dir, "/"))[12]
  rownames(latencystatsresult)[idx] <- traceName
  
  #webserverstatsresult[idx,] <-  statsFromFolder(dir)[[1]]
  res <- statsFromFolder(dir)
  latencystatsresult[idx, 1:32] <- res[[3]]#statsFromFolder(dir)[[3]]
  latencystatsresult[idx, 33] <- res[[7]]
  
  queue <- res[[6]]
  
  
  latency <- res[[4]]
  reqres <- res[[5]]
  diff <- as.numeric(latency) - as.numeric(reqres[2:3000])
  idx = idx+1
}
latencystatsresult <- tibble::rownames_to_column(as.data.frame(latencystatsresult), "VALUE")
write_xlsx(as.data.frame(latencystatsresult), paste(rootpathDir,"/", unlist(strsplit(rootpathDir, "/"))[6], unlist(strsplit(rootpathDir, "/"))[7], ".xlsx", sep = ""))
# write_xlsx(as.data.frame(latencystatsresult), paste("/Users/rxh655/The Pennsylvania State University/Sajal, Sultan Mahmud - Research/TraceDownscaler/experiment-11/results/aggregate_folder/", unlist(strsplit(rootpathDir, "/"))[6], unlist(strsplit(rootpathDir, "/"))[7], ".xlsx", sep = ""))
