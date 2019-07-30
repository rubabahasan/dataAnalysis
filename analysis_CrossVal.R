library(moments)
library(sjstats)
library(pracma)
library(quantmod)
library(stringr)
library(combinat)
library(outliers)
library(EnvStats)
library(caret)  #cross-validation function

#define folders

repeatationVal = 2
rootpathDir = "~/Documents/Research/Data/run13"
graphFolder = "/Users/rxh655/Documents/Research/Data/graph13/scatter"

#define cross-validation parameters
kfold <- 10


########################## Read from file ###################################

#read all the directories like trace1-1, trace 2-1 etc.
slash = "/"

directories <- list.dirs(path = rootpathDir ) #first one is the parent directory
nRow <- length(directories)-1 #number of rows of data

#create data matrix for keeping all the data
datamatrix <- matrix(nrow = nRow, ncol = 72)
#datamatrix.colnames <- c("web_s_mem_use_mean", "web_s_mem_use_var", "web_s_mem_use_skew", "web_s_mem_use_kurt", "web_s_mem_use_coeff_var", "web_s_mem_pcnt_mean", "web_s_mem_pcnt_var", "web_s_mem_pcnt_skew", "web_s_mem_pcnt_kurt", "web_s_mem_pcnt_coeff_var" ,"web_s_cpu_pcnt_mean", "web_s_cpu_pcnt_var", "web_s_cpu_pcnt_skew", "web_s_cpu_pcnt_kurt", "web_s_cpu_pcnt_coeff_var", "web_s_d_sent_mean", "web_s_d_sent_var", "web_s_d_sent_skew", "web_s_d_sent_kurt", "web_s_d_sent_coeff_var", "web_s_d_recv_mean", "web_s_d_recv_var", "web_s_d_recv_skew", "web_s_d_recv_kurt", "web_s_d_recv_coeff_var", "cache_mem_use_mean", "cache_mem_use_var", "cache_mem_use_skew", "cache_mem_use_kurt", "cache_mem_use_coeff_var", "cache_mem_pcnt_mean", "cache_mem_pcnt_var", "cache_mem_pcnt_skew", "cache_mem_pcnt_kurt", "cache_mem_pcnt_coeff_var ", "cache_cpu_pcnt_mean", "cache_cpu_pcnt_var", "cache_cpu_pcnt_skew", "cache_cpu_pcnt_kurt", "cache_cpu_pcnt_coeff_var", "cache_d_sent_mean", "cache_d_sent_var", "cache_d_sent_skew", "cache_d_sent_kurt", "cache_d_sent_coeff_var", "cache_d_recv_mean", "cache_d_recv_var", "cache_d_recv_skew", "cache_d_recv_kurt", "cache_d_recv_coeff_var", "mysql_mem_use_mean", "mysql_mem_use_var", "mysql_mem_use_skew", "mysql_mem_use_kurt", "mysql_mem_use_coeff_var", "mysql_mem_pcnt_mean", "mysql_mem_pcnt_var", "mysql_mem_pcnt_skew", "mysql_mem_pcnt_kurt", "mysql_mem_pcnt_coeff_var ", "mysql_cpu_pcnt_mean", "mysql_cpu_pcnt_var", "mysql_cpu_pcnt_skew", "mysql_cpu_pcnt_kurt", "mysql_cpu_pcnt_coeff_var", "mysql_d_sent_mean", "mysql_d_sent_var", "mysql_d_sent_skew", "mysql_d_sent_kurt", "mysql_d_sent_coeff_var", "mysql_d_recv_mean", "mysql_d_recv_var", "mysql_d_recv_skew", "mysql_d_recv_kurt", "mysql_d_recv_coeff_var", "req_res_latency_mean", "req_res_latency_var", "req_res_latency_skew", "req_res_latency_kurt", "req_res_latency_coeff_var", "traceline_res_latency_mean", "traceline_res_latency_var", "traceline_res_latency_skew", "traceline_res_latency_kurt", "traceline_res_latency_coeff_var", "interArrivalT_latency_mean", "interArrivalT_latency_var", "interArrivalT_latency_skew", "interArrivalT_latency_kurt", "interArrivalT_latency_coeff_var")
datamatrix.colnames <- c("web_s_mem_use_mean", "web_s_mem_use_skew", "web_s_mem_use_kurt", "web_s_mem_use_coeff_var", "web_s_mem_pcnt_mean", "web_s_mem_pcnt_skew", "web_s_mem_pcnt_kurt", "web_s_mem_pcnt_coeff_var" ,"web_s_cpu_pcnt_mean", "web_s_cpu_pcnt_skew", "web_s_cpu_pcnt_kurt", "web_s_cpu_pcnt_coeff_var", "web_s_d_sent_mean", "web_s_d_sent_skew", "web_s_d_sent_kurt", "web_s_d_sent_coeff_var", "web_s_d_recv_mean", "web_s_d_recv_skew", "web_s_d_recv_kurt", "web_s_d_recv_coeff_var", "cache_mem_use_mean", "cache_mem_use_skew", "cache_mem_use_kurt", "cache_mem_use_coeff_var", "cache_mem_pcnt_mean", "cache_mem_pcnt_skew", "cache_mem_pcnt_kurt", "cache_mem_pcnt_coeff_var ", "cache_cpu_pcnt_mean", "cache_cpu_pcnt_skew", "cache_cpu_pcnt_kurt", "cache_cpu_pcnt_coeff_var", "cache_d_sent_mean", "cache_d_sent_skew", "cache_d_sent_kurt", "cache_d_sent_coeff_var", "cache_d_recv_mean", "cache_d_recv_skew", "cache_d_recv_kurt", "cache_d_recv_coeff_var", "mysql_mem_use_mean", "mysql_mem_use_skew", "mysql_mem_use_kurt", "mysql_mem_use_coeff_var", "mysql_mem_pcnt_mean", "mysql_mem_pcnt_skew", "mysql_mem_pcnt_kurt", "mysql_mem_pcnt_coeff_var ", "mysql_cpu_pcnt_mean", "mysql_cpu_pcnt_skew", "mysql_cpu_pcnt_kurt", "mysql_cpu_pcnt_coeff_var", "mysql_d_sent_mean", "mysql_d_sent_skew", "mysql_d_sent_kurt", "mysql_d_sent_coeff_var", "mysql_d_recv_mean", "mysql_d_recv_skew", "mysql_d_recv_kurt", "mysql_d_recv_coeff_var", "req_res_latency_mean", "req_res_latency_skew", "req_res_latency_kurt", "req_res_latency_coeff_var", "traceline_res_latency_mean", "traceline_res_latency_skew", "traceline_res_latency_kurt", "traceline_res_latency_coeff_var", "interArrivalT_latency_mean", "interArrivalT_latency_skew", "interArrivalT_latency_kurt", "interArrivalT_latency_coeff_var")
length(datamatrix.colnames)
colnames(datamatrix) <- datamatrix.colnames
rownames(datamatrix) <- seq(1,nRow,1)

# rootpath = "~/Documents/Research/Code/Analysis/sample_trace" 

#read data, find mean, variance etc. and put them in the data matrix
#todo: data_sent and data_recv cumulative, make them non-cumulative
idx = 1
for(dir in directories[-1]) #all elements except first one
{
  
  traceName <- unlist(strsplit(dir, "/"))[8]
  rownames(datamatrix)[idx] <- traceName
  
  rootpath = dir
  print(rootpath)
  
  web_serverFile <- list.files(path = rootpath , pattern = "^web+.*csv$")
  memcacheDFile <- list.files(path = rootpath , pattern = "^memcache+.*csv$")
  mysqlFile <- list.files(path = rootpath , pattern = "^mysql+.*csv$")
  reqResponseFile <- list.files(path = rootpath , pattern = "^request+.*csv$")
  tacelineResponseFile <- list.files(path = rootpath , pattern = "^traceline+.*csv$")
  traceFile <- list.files(path = rootpath , pattern = "^trace+.*txt$")
  
  web_server <- read.csv(paste(rootpath, slash, web_serverFile, sep = ""))
  memcache <- read.csv(paste(rootpath, slash, memcacheDFile, sep = ""))
  mysql <- read.csv(paste(rootpath, slash, mysqlFile, sep = ""))
  reqResponse <- read.csv(paste(rootpath, slash, reqResponseFile, sep = ""))
  # reqResponse <- reqResponse[-2895,]
  
  tracelineResponse <- read.csv(paste(rootpath, slash, tacelineResponseFile, sep = ""))
  
  trace <- read.csv(paste(rootpath, slash, traceFile, sep = ""))
  
  #adjusting tracelineResponse and trace
  #tracelineRsplit <- unlist(strsplit(as.character(tracelineResponse$X14018557...28...1561326863888000000...REGISTER.dcVF3E2dak.3372551038), ";;;"))
  tracelineRsplit <- str_split_fixed(tracelineResponse[,1], ";;;", n= 5)
  
  latency <- as.numeric(tracelineRsplit[,3]) - as.numeric(tracelineRsplit[,1])
  #print(latency)
  print(paste("0 values", idx, length(which(latency < 0))))
  
  latency <- latency[!(latency %in% latency[which(latency < 0)])]
  
  traceSplit <- str_split_fixed(trace[,1], ";;;", n= 5)
  
  interArrivalTime <- diff(as.numeric(traceSplit[,1]), differences = 1)
  
  web_server[2:dim(web_server)[1],6] <- diff(as.numeric(web_server[,6]), differences = 1)
  web_server[2:dim(web_server)[1],8] <- diff(as.numeric(web_server[,8]), differences = 1)
  
  memcache[2:dim(memcache)[1],6] <- diff(as.numeric(memcache[,6]), differences = 1)
  memcache[2:dim(memcache)[1],8] <- diff(as.numeric(memcache[,8]), differences = 1)
  
  mysql[2:dim(mysql)[1],6] <- diff(as.numeric(mysql[,6]), differences = 1)
  mysql[2:dim(mysql)[1],8] <- diff(as.numeric(mysql[,8]), differences = 1)
  # dim(traceSplit)
  # 
  # length(difftrace)
  # #25
  # web_s_mem_use_mean, web_s_mem_use_var, web_s_mem_use_skew, web_s_mem_use_kurt, web_s_mem_use_coeff_var, web_s_mem_pcnt_mean, web_s_mem_pcnt_var, web_s_mem_pcnt_skew, web_s_mem_pcnt_kurt, web_s_mem_pcnt_coeff_var , web_s_cpu_pcnt_mean, web_s_cpu_pcnt_var, web_s_cpu_pcnt_skew, web_s_cpu_pcnt_kurt, web_s_cpu_pcnt_coeff_var, web_s_d_sent_mean, web_s_d_sent_var, web_s_d_sent_skew, web_s_d_sent_kurt, web_s_d_sent_coeff_var, web_s_d_recv_mean, web_s_d_recv_var, web_s_d_recv_skew, web_s_d_recv_kurt, web_s_d_recv_coeff_var
  # cache_mem_use_mean, cache_mem_use_var, cache_mem_use_skew, cache_mem_use_kurt, cache_mem_use_coeff_var, cache_mem_pcnt_mean, cache_mem_pcnt_var, cache_mem_pcnt_skew, cache_mem_pcnt_kurt, cache_mem_pcnt_coeff_var , cache_cpu_pcnt_mean, cache_cpu_pcnt_var, cache_cpu_pcnt_skew, cache_cpu_pcnt_kurt, cache_cpu_pcnt_coeff_var, cache_d_sent_mean, cache_d_sent_var, cache_d_sent_skew, cache_d_sent_kurt, cache_d_sent_coeff_var, cache_d_recv_mean, cache_d_recv_var, cache_d_recv_skew, cache_d_recv_kurt, cache_d_recv_coeff_var
  # mysql_mem_use_mean, mysql_mem_use_var, mysql_mem_use_skew, mysql_mem_use_kurt, mysql_mem_use_coeff_var, mysql_mem_pcnt_mean, mysql_mem_pcnt_var, mysql_mem_pcnt_skew, mysql_mem_pcnt_kurt, mysql_mem_pcnt_coeff_var , mysql_cpu_pcnt_mean, mysql_cpu_pcnt_var, mysql_cpu_pcnt_skew, mysql_cpu_pcnt_kurt, mysql_cpu_pcnt_coeff_var, mysql_d_sent_mean, mysql_d_sent_var, mysql_d_sent_skew, mysql_d_sent_kurt, mysql_d_sent_coeff_var, mysql_d_recv_mean, mysql_d_recv_var, mysql_d_recv_skew, mysql_d_recv_kurt, mysql_d_recv_coeff_var
  
  #5
  # req_res_latency_mean, req_res_latency_var, req_res_latency_skew, req_res_latency_kurt, req_res_latency_coeff_var
  # traceline_res_latency_mean, traceline_res_latency_var, traceline_res_latency_skew, traceline_res_latency_kurt, traceline_res_latency_coeff_var ##################### one cell extraction
  
  
  #web server
  i = 0
  for(j in c(2,4,5,6,8))
  {
    datamatrix[idx,(i*4)+1] <- mean(web_server[,j]) # web memory use
    #datamatrix[idx,(i*4)+2] <- var(web_server[,j]) # web memory use
    datamatrix[idx,(i*4)+2] <- skewness(web_server[,j]) # web memory use
    datamatrix[idx,(i*4)+3] <- kurtosis(web_server[,j]) # web memory use
    datamatrix[idx,(i*4)+4] <- cv(web_server[,j]) # web memory use
    # print("Printing")
    # print(i*5 +1)
    # print(j)
    
    i = i + 1
  }
  
  
  #memcached
  i = 5
  for(j in c(2,4,5,6,8))
  {
    datamatrix[idx,(i*4)+1] <- mean(memcache[,j]) # memcache
    #datamatrix[idx,(i*4)+2] <- var(memcache[,j]) # memcache
    datamatrix[idx,(i*4)+2] <- skewness(memcache[,j]) # memcache
    datamatrix[idx,(i*4)+3] <- kurtosis(memcache[,j]) # memcache
    datamatrix[idx,(i*4)+4] <- cv(memcache[,j]) # memcache
    # print("Printing")
    # print(i*5 +1)
    # print(j)
    
    i = i + 1
  }
  
  #mysql
  i = 10
  for(j in c(2,4,5,6,8))
  {
    datamatrix[idx,(i*4)+1] <- mean(mysql[,j]) # mysql
    #datamatrix[idx,(i*4)+2] <- var(mysql[,j]) # mysql
    datamatrix[idx,(i*4)+2] <- skewness(mysql[,j]) # mysql
    datamatrix[idx,(i*4)+3] <- kurtosis(mysql[,j]) # mysql
    datamatrix[idx,(i*4)+4] <- cv(mysql[,j]) # mysql
    # print("Printing")
    # print(i*5 +1)
    # print(j)
    
    i = i + 1
  }
  
  #req-response latency
  i = 15
  for(j in c(2))
  {
    delResponseCount = 0
    while(kurtosis(reqResponse[,j]) > 100)
    {
      outlier <- max(reqResponse[,j])
      reqResponse <- reqResponse[!(reqResponse[,j] %in% outlier),]
      delResponseCount = delResponseCount + 1
    }
    print(paste("deleted response count = ", delResponseCount))
    
    datamatrix[idx,(i*4)+1] <- mean(reqResponse[,j]) # req response
    #datamatrix[idx,(i*4)+2] <- var(reqResponse[,j]) # req response
    datamatrix[idx,(i*4)+2] <- skewness(reqResponse[,j]) # req response
    datamatrix[idx,(i*4)+3] <- kurtosis(reqResponse[,j]) # req response
    datamatrix[idx,(i*4)+4] <- cv(reqResponse[,j]) # req response
    # print("Printing")
    # print(i*5 +1)
    # print(j)
    
    i = i + 1
  }
  
  #traceline response
  i = 16
  for(j in c(2))
  {
    delResponseCount = 0
    while(kurtosis(latency) > 100)
    {
      outlier <- max(latency)
      latency <- latency[!(latency %in% outlier)]
      delResponseCount = delResponseCount + 1
    }
    print(paste("deleted traceline response count = ", delResponseCount))
    
    
    datamatrix[idx,(i*4)+1] <- mean(latency) # traceline response
    #datamatrix[idx,(i*4)+2] <- var(latency) # traceline response
    datamatrix[idx,(i*4)+2] <- skewness(latency) # traceline response
    datamatrix[idx,(i*4)+3] <- kurtosis(latency) # traceline response
    if(is.na(mean(latency)))
    {
      datamatrix[idx,(i*4)+4] <- NA # traceline response
    }else{
      datamatrix[idx,(i*4)+4] <- cv(latency) # traceline response
    }
    
    # print("Printing")
    # print(i*5 +1)
    # print(j)
    
    i = i + 1
  }
  # trace response
  i = 17
  for(j in c(2))
  {
    datamatrix[idx,(i*4)+1] <- mean(interArrivalTime) # interarrival
    #datamatrix[idx,(i*4)+2] <- var(interArrivalTime) # interarrival
    datamatrix[idx,(i*4)+2] <- skewness(interArrivalTime) # interarrival
    datamatrix[idx,(i*4)+3] <- kurtosis(interArrivalTime) # interarrival
    datamatrix[idx,(i*4)+4] <- cv(interArrivalTime) # interarrival
    
    # print("Printing")
    # print(i*5 +1)
    # print(j)
    
    i = i + 1
  }
  
  idx = idx + 1
}


####################### Cross Validation ##########################


groupSize <- nRow/kfold
# 
testData <- sample(nRow)
# testData

accuracy = 0
for(loop in seq(1, kfold, 1))
{
  
  datamatrix.test <- datamatrix[c(testData[seq((i-1)*groupSize+1,i*groupSize,1)]),]
  datamatrix.train <- datamatrix[c(testData[-seq((i-1)*groupSize+1,i*groupSize,1)]),]
  
  ######################## Create train difference matrix #########################
 
  nRowTrain <- dim(datamatrix.train)[1]
  nRowDiff <- choose(nRowTrain, 2)
  print(paste("nRowDiff", nRowDiff))
  diffmatrix.train <- matrix(nrow = nRowDiff, ncol = 74)  #1275 for 51 #903 for 43
  #diffmatrix.colnames <- c("web_s_mem_use_mean", "web_s_mem_use_var", "web_s_mem_use_skew", "web_s_mem_use_kurt", "web_s_mem_use_coeff_var", "web_s_mem_pcnt_mean", "web_s_mem_pcnt_var", "web_s_mem_pcnt_skew", "web_s_mem_pcnt_kurt", "web_s_mem_pcnt_coeff_var" ,"web_s_cpu_pcnt_mean", "web_s_cpu_pcnt_var", "web_s_cpu_pcnt_skew", "web_s_cpu_pcnt_kurt", "web_s_cpu_pcnt_coeff_var", "web_s_d_sent_mean", "web_s_d_sent_var", "web_s_d_sent_skew", "web_s_d_sent_kurt", "web_s_d_sent_coeff_var", "web_s_d_recv_mean", "web_s_d_recv_var", "web_s_d_recv_skew", "web_s_d_recv_kurt", "web_s_d_recv_coeff_var", "cache_mem_use_mean", "cache_mem_use_var", "cache_mem_use_skew", "cache_mem_use_kurt", "cache_mem_use_coeff_var", "cache_mem_pcnt_mean", "cache_mem_pcnt_var", "cache_mem_pcnt_skew", "cache_mem_pcnt_kurt", "cache_mem_pcnt_coeff_var ", "cache_cpu_pcnt_mean", "cache_cpu_pcnt_var", "cache_cpu_pcnt_skew", "cache_cpu_pcnt_kurt", "cache_cpu_pcnt_coeff_var", "cache_d_sent_mean", "cache_d_sent_var", "cache_d_sent_skew", "cache_d_sent_kurt", "cache_d_sent_coeff_var", "cache_d_recv_mean", "cache_d_recv_var", "cache_d_recv_skew", "cache_d_recv_kurt", "cache_d_recv_coeff_var", "mysql_mem_use_mean", "mysql_mem_use_var", "mysql_mem_use_skew", "mysql_mem_use_kurt", "mysql_mem_use_coeff_var", "mysql_mem_pcnt_mean", "mysql_mem_pcnt_var", "mysql_mem_pcnt_skew", "mysql_mem_pcnt_kurt", "mysql_mem_pcnt_coeff_var ", "mysql_cpu_pcnt_mean", "mysql_cpu_pcnt_var", "mysql_cpu_pcnt_skew", "mysql_cpu_pcnt_kurt", "mysql_cpu_pcnt_coeff_var", "mysql_d_sent_mean", "mysql_d_sent_var", "mysql_d_sent_skew", "mysql_d_sent_kurt", "mysql_d_sent_coeff_var", "mysql_d_recv_mean", "mysql_d_recv_var", "mysql_d_recv_skew", "mysql_d_recv_kurt", "mysql_d_recv_coeff_var", "req_res_latency_mean", "req_res_latency_var", "req_res_latency_skew", "req_res_latency_kurt", "req_res_latency_coeff_var", "traceline_res_latency_mean", "traceline_res_latency_var", "traceline_res_latency_skew", "traceline_res_latency_kurt", "traceline_res_latency_coeff_var", "interArrivalT_latency_mean", "interArrivalT_latency_var", "interArrivalT_latency_skew", "interArrivalT_latency_kurt", "interArrivalT_latency_coeff_var", "DiffVal")
  diffmatrix.colnames <- c("web_s_mem_use_mean", "web_s_mem_use_skew", "web_s_mem_use_kurt", "web_s_mem_use_coeff_var", "web_s_mem_pcnt_mean", "web_s_mem_pcnt_skew", "web_s_mem_pcnt_kurt", "web_s_mem_pcnt_coeff_var" ,"web_s_cpu_pcnt_mean", "web_s_cpu_pcnt_skew", "web_s_cpu_pcnt_kurt", "web_s_cpu_pcnt_coeff_var", "web_s_d_sent_mean", "web_s_d_sent_skew", "web_s_d_sent_kurt", "web_s_d_sent_coeff_var", "web_s_d_recv_mean", "web_s_d_recv_skew", "web_s_d_recv_kurt", "web_s_d_recv_coeff_var", "cache_mem_use_mean", "cache_mem_use_skew", "cache_mem_use_kurt", "cache_mem_use_coeff_var", "cache_mem_pcnt_mean", "cache_mem_pcnt_skew", "cache_mem_pcnt_kurt", "cache_mem_pcnt_coeff_var ", "cache_cpu_pcnt_mean", "cache_cpu_pcnt_skew", "cache_cpu_pcnt_kurt", "cache_cpu_pcnt_coeff_var", "cache_d_sent_mean", "cache_d_sent_skew", "cache_d_sent_kurt", "cache_d_sent_coeff_var", "cache_d_recv_mean", "cache_d_recv_skew", "cache_d_recv_kurt", "cache_d_recv_coeff_var", "mysql_mem_use_mean", "mysql_mem_use_skew", "mysql_mem_use_kurt", "mysql_mem_use_coeff_var", "mysql_mem_pcnt_mean", "mysql_mem_pcnt_skew", "mysql_mem_pcnt_kurt", "mysql_mem_pcnt_coeff_var ", "mysql_cpu_pcnt_mean", "mysql_cpu_pcnt_skew", "mysql_cpu_pcnt_kurt", "mysql_cpu_pcnt_coeff_var", "mysql_d_sent_mean", "mysql_d_sent_skew", "mysql_d_sent_kurt", "mysql_d_sent_coeff_var", "mysql_d_recv_mean", "mysql_d_recv_skew", "mysql_d_recv_kurt", "mysql_d_recv_coeff_var", "req_res_latency_mean", "req_res_latency_skew", "req_res_latency_kurt", "req_res_latency_coeff_var", "traceline_res_latency_mean", "traceline_res_latency_skew", "traceline_res_latency_kurt", "traceline_res_latency_coeff_var", "interArrivalT_latency_mean", "interArrivalT_latency_skew", "interArrivalT_latency_kurt", "interArrivalT_latency_coeff_var", "DiffVal", "Label")
  
  colnames(diffmatrix.train) <- diffmatrix.colnames
  rownames(diffmatrix.train) <- seq(1,nRowDiff,1)
  
  k = 1
  for( i in seq(1, (nRowTrain-1), 1))
  {
    for( j in seq((i+1), nRowTrain, 1))
    {
      
      rownames(diffmatrix.train)[k] <- paste(rownames(datamatrix.train)[i], "vs", rownames(datamatrix.train)[j])
      # diffmatrix[k,1:90] =  abs(datamatrix[i,] - datamatrix[j,])/max(abs(datamatrix[i,]), abs(datamatrix[j,]))*100
      
      for(colIdx in seq(1,72,4))
      {
        #mean uses average difference
        diffmatrix.train[k,colIdx] = abs(datamatrix.train[i,colIdx] - datamatrix.train[j,colIdx])/max(abs(datamatrix.train[i,colIdx]), abs(datamatrix.train[j,colIdx]))*100
      }
      for(colIdx in seq(4,72,4))
      {
        #coeffecient of variance uses only difference
        diffmatrix.train[k,colIdx] = abs(datamatrix.train[i,colIdx] - datamatrix.train[j,colIdx])
      }
      for(colIdx in seq(2,72,4))
      {
        #skewness uses only difference
        # print(paste(i, colIdx, as.numeric(datamatrix[i,(colIdx+2)])))
        if(as.numeric(datamatrix.train[i,(colIdx+2)]) == 0) # i cv = 0
        {
          if(as.numeric(datamatrix.train[j,(colIdx+2)]) == 0) # j cv = 0
          {
            # both coeffecient of variance is 0. meaning, difference of skewness should be 0
            diffmatrix.train[k,colIdx] = 0
          }
          else{
            # both cv != 0, i is 0
            # diffmatrix[k,colIdx] = abs(datamatrix[j,colIdx])
            diffmatrix.train[k,colIdx] = 100
          }
          
        }
        else{
          if (as.numeric(datamatrix.train[j,(colIdx+2)]) == 0) # both cv != 0, any one is 0
          {
            # both cv != 0, j is 0
            # diffmatrix[k,colIdx] = abs(datamatrix[i,colIdx])
            diffmatrix.train[k,colIdx] = 100
          }
          else{
            # both coeffecient of variance is nonzero
            diffmatrix.train[k,colIdx] = abs(datamatrix.train[i,colIdx] - datamatrix.train[j,colIdx]) 
          }
        }
      }
      for(colIdx in seq(3,72,4))
      {
        #kurtosis uses only difference
        
        if((as.numeric(datamatrix.train[i,(colIdx+1)]) == 0)) # i cv = 0
        {
          if((as.numeric(datamatrix.train[j,(colIdx+1)]) == 0)) # j cv = 0
          {
            # both coeffecient of variance is 0. meaning, difference of kurtosis should be 0
            diffmatrix.train[k,colIdx] = 0
          }
          else{
            # both cv != 0, i is 0
            # diffmatrix[k,colIdx] = abs(datamatrix[j,colIdx])
            diffmatrix.train[k,colIdx] = 100
          }
          
        }
        else{
          if (as.numeric(datamatrix.train[j,(colIdx+1)]) == 0) # both cv != 0, any one is 0
          {
            # both cv != 0, j is 0
            # diffmatrix[k,colIdx] = abs(datamatrix[i,colIdx])
            diffmatrix.train[k,colIdx] = 100
          }
          else{
            # both coeffecient of variance is nonzero
            diffmatrix.train[k,colIdx] = abs(datamatrix.train[i,colIdx] - datamatrix.train[j,colIdx]) 
          }
        }
      }
      
      k = k + 1
    }
  }
  
  ######################## Create test difference matrix #########################
  
  nRowTest <- dim(datamatrix.test)[1]
  nRowDiff <- choose(nRowTest, 2) + (dim(datamatrix.test)[1] * dim(datamatrix.train)[1])
  print(paste("nRowDiff", nRowDiff))
  diffmatrix.test <- matrix(nrow = nRowDiff, ncol = 74)  #1275 for 51 #903 for 43
  #diffmatrix.colnames <- c("web_s_mem_use_mean", "web_s_mem_use_var", "web_s_mem_use_skew", "web_s_mem_use_kurt", "web_s_mem_use_coeff_var", "web_s_mem_pcnt_mean", "web_s_mem_pcnt_var", "web_s_mem_pcnt_skew", "web_s_mem_pcnt_kurt", "web_s_mem_pcnt_coeff_var" ,"web_s_cpu_pcnt_mean", "web_s_cpu_pcnt_var", "web_s_cpu_pcnt_skew", "web_s_cpu_pcnt_kurt", "web_s_cpu_pcnt_coeff_var", "web_s_d_sent_mean", "web_s_d_sent_var", "web_s_d_sent_skew", "web_s_d_sent_kurt", "web_s_d_sent_coeff_var", "web_s_d_recv_mean", "web_s_d_recv_var", "web_s_d_recv_skew", "web_s_d_recv_kurt", "web_s_d_recv_coeff_var", "cache_mem_use_mean", "cache_mem_use_var", "cache_mem_use_skew", "cache_mem_use_kurt", "cache_mem_use_coeff_var", "cache_mem_pcnt_mean", "cache_mem_pcnt_var", "cache_mem_pcnt_skew", "cache_mem_pcnt_kurt", "cache_mem_pcnt_coeff_var ", "cache_cpu_pcnt_mean", "cache_cpu_pcnt_var", "cache_cpu_pcnt_skew", "cache_cpu_pcnt_kurt", "cache_cpu_pcnt_coeff_var", "cache_d_sent_mean", "cache_d_sent_var", "cache_d_sent_skew", "cache_d_sent_kurt", "cache_d_sent_coeff_var", "cache_d_recv_mean", "cache_d_recv_var", "cache_d_recv_skew", "cache_d_recv_kurt", "cache_d_recv_coeff_var", "mysql_mem_use_mean", "mysql_mem_use_var", "mysql_mem_use_skew", "mysql_mem_use_kurt", "mysql_mem_use_coeff_var", "mysql_mem_pcnt_mean", "mysql_mem_pcnt_var", "mysql_mem_pcnt_skew", "mysql_mem_pcnt_kurt", "mysql_mem_pcnt_coeff_var ", "mysql_cpu_pcnt_mean", "mysql_cpu_pcnt_var", "mysql_cpu_pcnt_skew", "mysql_cpu_pcnt_kurt", "mysql_cpu_pcnt_coeff_var", "mysql_d_sent_mean", "mysql_d_sent_var", "mysql_d_sent_skew", "mysql_d_sent_kurt", "mysql_d_sent_coeff_var", "mysql_d_recv_mean", "mysql_d_recv_var", "mysql_d_recv_skew", "mysql_d_recv_kurt", "mysql_d_recv_coeff_var", "req_res_latency_mean", "req_res_latency_var", "req_res_latency_skew", "req_res_latency_kurt", "req_res_latency_coeff_var", "traceline_res_latency_mean", "traceline_res_latency_var", "traceline_res_latency_skew", "traceline_res_latency_kurt", "traceline_res_latency_coeff_var", "interArrivalT_latency_mean", "interArrivalT_latency_var", "interArrivalT_latency_skew", "interArrivalT_latency_kurt", "interArrivalT_latency_coeff_var", "DiffVal")
  diffmatrix.colnames <- c("web_s_mem_use_mean", "web_s_mem_use_skew", "web_s_mem_use_kurt", "web_s_mem_use_coeff_var", "web_s_mem_pcnt_mean", "web_s_mem_pcnt_skew", "web_s_mem_pcnt_kurt", "web_s_mem_pcnt_coeff_var" ,"web_s_cpu_pcnt_mean", "web_s_cpu_pcnt_skew", "web_s_cpu_pcnt_kurt", "web_s_cpu_pcnt_coeff_var", "web_s_d_sent_mean", "web_s_d_sent_skew", "web_s_d_sent_kurt", "web_s_d_sent_coeff_var", "web_s_d_recv_mean", "web_s_d_recv_skew", "web_s_d_recv_kurt", "web_s_d_recv_coeff_var", "cache_mem_use_mean", "cache_mem_use_skew", "cache_mem_use_kurt", "cache_mem_use_coeff_var", "cache_mem_pcnt_mean", "cache_mem_pcnt_skew", "cache_mem_pcnt_kurt", "cache_mem_pcnt_coeff_var ", "cache_cpu_pcnt_mean", "cache_cpu_pcnt_skew", "cache_cpu_pcnt_kurt", "cache_cpu_pcnt_coeff_var", "cache_d_sent_mean", "cache_d_sent_skew", "cache_d_sent_kurt", "cache_d_sent_coeff_var", "cache_d_recv_mean", "cache_d_recv_skew", "cache_d_recv_kurt", "cache_d_recv_coeff_var", "mysql_mem_use_mean", "mysql_mem_use_skew", "mysql_mem_use_kurt", "mysql_mem_use_coeff_var", "mysql_mem_pcnt_mean", "mysql_mem_pcnt_skew", "mysql_mem_pcnt_kurt", "mysql_mem_pcnt_coeff_var ", "mysql_cpu_pcnt_mean", "mysql_cpu_pcnt_skew", "mysql_cpu_pcnt_kurt", "mysql_cpu_pcnt_coeff_var", "mysql_d_sent_mean", "mysql_d_sent_skew", "mysql_d_sent_kurt", "mysql_d_sent_coeff_var", "mysql_d_recv_mean", "mysql_d_recv_skew", "mysql_d_recv_kurt", "mysql_d_recv_coeff_var", "req_res_latency_mean", "req_res_latency_skew", "req_res_latency_kurt", "req_res_latency_coeff_var", "traceline_res_latency_mean", "traceline_res_latency_skew", "traceline_res_latency_kurt", "traceline_res_latency_coeff_var", "interArrivalT_latency_mean", "interArrivalT_latency_skew", "interArrivalT_latency_kurt", "interArrivalT_latency_coeff_var", "DiffVal", "Label")
  
  colnames(diffmatrix.test) <- diffmatrix.colnames
  rownames(diffmatrix.test) <- seq(1,nRowDiff,1)
  
  k = 1
  for( i in seq(1, (nRowTest-1), 1))
  {
    for( j in seq((i+1), nRowTest, 1))
    {
      
      rownames(diffmatrix.test)[k] <- paste(rownames(datamatrix.test)[i], "vs", rownames(datamatrix.test)[j])
      # diffmatrix[k,1:90] =  abs(datamatrix[i,] - datamatrix[j,])/max(abs(datamatrix[i,]), abs(datamatrix[j,]))*100
      
      for(colIdx in seq(1,72,4))
      {
        #mean uses average difference
        diffmatrix.test[k,colIdx] = abs(datamatrix.test[i,colIdx] - datamatrix.test[j,colIdx])/max(abs(datamatrix.test[i,colIdx]), abs(datamatrix.test[j,colIdx]))*100
      }
      for(colIdx in seq(4,72,4))
      {
        #coeffecient of variance uses only difference
        diffmatrix.test[k,colIdx] = abs(datamatrix.test[i,colIdx] - datamatrix.test[j,colIdx])
      }
      for(colIdx in seq(2,72,4))
      {
        #skewness uses only difference
        # print(paste(i, colIdx, as.numeric(datamatrix[i,(colIdx+2)])))
        if(as.numeric(datamatrix.test[i,(colIdx+2)]) == 0) # i cv = 0
        {
          if(as.numeric(datamatrix.test[j,(colIdx+2)]) == 0) # j cv = 0
          {
            # both coeffecient of variance is 0. meaning, difference of skewness should be 0
            diffmatrix.test[k,colIdx] = 0
          }
          else{
            # both cv != 0, i is 0
            # diffmatrix[k,colIdx] = abs(datamatrix[j,colIdx])
            diffmatrix.test[k,colIdx] = 100
          }
          
        }
        else{
          if (as.numeric(datamatrix.test[j,(colIdx+2)]) == 0) # both cv != 0, any one is 0
          {
            # both cv != 0, j is 0
            # diffmatrix[k,colIdx] = abs(datamatrix[i,colIdx])
            diffmatrix.test[k,colIdx] = 100
          }
          else{
            # both coeffecient of variance is nonzero
            diffmatrix.test[k,colIdx] = abs(datamatrix.test[i,colIdx] - datamatrix.test[j,colIdx]) 
          }
        }
      }
      for(colIdx in seq(3,72,4))
      {
        #kurtosis uses only difference
        
        if((as.numeric(datamatrix.test[i,(colIdx+1)]) == 0)) # i cv = 0
        {
          if((as.numeric(datamatrix.test[j,(colIdx+1)]) == 0)) # j cv = 0
          {
            # both coeffecient of variance is 0. meaning, difference of kurtosis should be 0
            diffmatrix.test[k,colIdx] = 0
          }
          else{
            # both cv != 0, i is 0
            # diffmatrix[k,colIdx] = abs(datamatrix[j,colIdx])
            diffmatrix.test[k,colIdx] = 100
          }
          
        }
        else{
          if (as.numeric(datamatrix.test[j,(colIdx+1)]) == 0) # both cv != 0, any one is 0
          {
            # both cv != 0, j is 0
            # diffmatrix[k,colIdx] = abs(datamatrix[i,colIdx])
            diffmatrix.test[k,colIdx] = 100
          }
          else{
            # both coeffecient of variance is nonzero
            diffmatrix.test[k,colIdx] = abs(datamatrix.test[i,colIdx] - datamatrix.test[j,colIdx]) 
          }
        }
      }
      
      k = k + 1
    }
  }
  
  for( i in seq(1, dim(datamatrix.test)[1], 1))
  {
    for( j in seq(1, dim(datamatrix.train)[1], 1))
    {
      
      rownames(diffmatrix.test)[k] <- paste(rownames(datamatrix.test)[i], "vs", rownames(datamatrix.train)[j])
      # diffmatrix[k,1:90] =  abs(datamatrix[i,] - datamatrix[j,])/max(abs(datamatrix[i,]), abs(datamatrix[j,]))*100
      
      for(colIdx in seq(1,72,4))
      {
        #mean uses average difference
        diffmatrix.test[k,colIdx] = abs(datamatrix.test[i,colIdx] - datamatrix.train[j,colIdx])/max(abs(datamatrix.test[i,colIdx]), abs(datamatrix.train[j,colIdx]))*100
      }
      for(colIdx in seq(4,72,4))
      {
        #coeffecient of variance uses only difference
        diffmatrix.test[k,colIdx] = abs(datamatrix.test[i,colIdx] - datamatrix.train[j,colIdx])
      }
      for(colIdx in seq(2,72,4))
      {
        #skewness uses only difference
        # print(paste(i, colIdx, as.numeric(datamatrix[i,(colIdx+2)])))
        if(as.numeric(datamatrix.test[i,(colIdx+2)]) == 0) # i cv = 0
        {
          if(as.numeric(datamatrix.train[j,(colIdx+2)]) == 0) # j cv = 0
          {
            # both coeffecient of variance is 0. meaning, difference of skewness should be 0
            diffmatrix.test[k,colIdx] = 0
          }
          else{
            # both cv != 0, i is 0
            # diffmatrix[k,colIdx] = abs(datamatrix[j,colIdx])
            diffmatrix.test[k,colIdx] = 100
          }
          
        }
        else{
          if (as.numeric(datamatrix.train[j,(colIdx+2)]) == 0) # both cv != 0, any one is 0
          {
            # both cv != 0, j is 0
            # diffmatrix[k,colIdx] = abs(datamatrix[i,colIdx])
            diffmatrix.test[k,colIdx] = 100
          }
          else{
            # both coeffecient of variance is nonzero
            diffmatrix.test[k,colIdx] = abs(datamatrix.test[i,colIdx] - datamatrix.train[j,colIdx]) 
          }
        }
      }
      for(colIdx in seq(3,72,4))
      {
        #kurtosis uses only difference
        
        if((as.numeric(datamatrix.test[i,(colIdx+1)]) == 0)) # i cv = 0
        {
          if((as.numeric(datamatrix.train[j,(colIdx+1)]) == 0)) # j cv = 0
          {
            # both coeffecient of variance is 0. meaning, difference of kurtosis should be 0
            diffmatrix.test[k,colIdx] = 0
          }
          else{
            # both cv != 0, i is 0
            # diffmatrix[k,colIdx] = abs(datamatrix[j,colIdx])
            diffmatrix.test[k,colIdx] = 100
          }
          
        }
        else{
          if (as.numeric(datamatrix.train[j,(colIdx+1)]) == 0) # both cv != 0, any one is 0
          {
            # both cv != 0, j is 0
            # diffmatrix[k,colIdx] = abs(datamatrix[i,colIdx])
            diffmatrix.test[k,colIdx] = 100
          }
          else{
            # both coeffecient of variance is nonzero
            diffmatrix.test[k,colIdx] = abs(datamatrix.test[i,colIdx] - datamatrix.train[j,colIdx]) 
          }
        }
      }
      
      k = k + 1
    }
  }
  
  
  
  ##################### Define Label ########################
  
  # Diffmatrix.train
  for(row in seq(1, dim(diffmatrix.train)[1], 1))
  {
    rowname <- rownames(diffmatrix.train)[row]
    test <- str_split_fixed(rowname, "vs", n = 2)
    
    test1 <- str_split_fixed(test[1], "e", n = 2)
    test2 <- str_split_fixed(test[2], "e", n = 2)
    
    val1 <- str_split_fixed(test1[2], "-", n = 2)
    val2 <- str_split_fixed(test2[2], "-", n = 2)
    
    if(as.numeric(val1[1]) == as.numeric(val2[1]))
    {
      # print(paste("SAME", row, ":", col, diffmatrix[row,col]))
      diffmatrix.train[row, 74] = 1
    }
    else{
      if(floor((as.numeric(val1[1])-1)/repeatationVal) == floor((as.numeric(val2[1])-1)/repeatationVal))
      {
        # print(paste("SIMILAR", row, ":", col, diffmatrix[row,col]))
        diffmatrix.train[row, 74] = 1
      }
      else{
        # print(paste("DIFFERENT", row, ":", col, diffmatrix[row,col]))
        diffmatrix.train[row, 74] = 0
      }
    }
  }
  
  # Diffmatrix.test
  for(row in seq(1, dim(diffmatrix.test)[1], 1))
  {
    rowname <- rownames(diffmatrix.test)[row]
    test <- str_split_fixed(rowname, "vs", n = 2)
    
    test1 <- str_split_fixed(test[1], "e", n = 2)
    test2 <- str_split_fixed(test[2], "e", n = 2)
    
    val1 <- str_split_fixed(test1[2], "-", n = 2)
    val2 <- str_split_fixed(test2[2], "-", n = 2)
    
    if(as.numeric(val1[1]) == as.numeric(val2[1]))
    {
      # print(paste("SAME", row, ":", col, diffmatrix[row,col]))
      diffmatrix.test[row, 74] = 1
    }
    else{
      if(floor((as.numeric(val1[1])-1)/repeatationVal) == floor((as.numeric(val2[1])-1)/repeatationVal))
      {
        # print(paste("SIMILAR", row, ":", col, diffmatrix[row,col]))
        diffmatrix.test[row, 74] = 1
      }
      else{
        # print(paste("DIFFERENT", row, ":", col, diffmatrix[row,col]))
        diffmatrix.test[row, 74] = 0
      }
    }
  }
  
  
  ######################## Define Similarity Threshold ########################
  
  diffMatrix2 <- diffmatrix.train[which(diffmatrix.train[,74]==1),]
  # simThreshold <- as.list(0, dim(datamatrix)[2]))
  simThreshold <- matrix(rep(0, (dim(datamatrix.train)[2] * 2)), nrow = 2, ncol = dim(datamatrix)[2])
  print(simThreshold[[2]])
  for(col in seq(1, dim(simThreshold)[2], 1))
  {
    simThreshold[1, col] <- max(diffMatrix2[,col])
    simThreshold[2, col] <- min(diffMatrix2[,col])
  }
  
  
  ##################### Determine Similarity with Threshold values ########################
  
  #Train Data set
  for(row in seq(1,dim(diffmatrix.train)[1],1))
  {
    diffmatrix.train[row,73] = 1
    for(col in seq(1, 64, 1))
    {
      if(as.numeric(diffmatrix.train[row, col]) > simThreshold[1, col])
      {
        diffmatrix.train[row, 73] = 0
        # print(paste(col, " = ", diffmatrix[row, col], ">", simThreshold[1, col], "NO\n"))
        break
      }
    }
    
  }
  
  trainAccuracy = (nrow(diffmatrix.train)-sum(abs(as.numeric(diffmatrix.train[,73]) - as.numeric(diffmatrix.train[,74]))))/nrow(diffmatrix.train)*100
  print(paste("Train accuracy", loop, trainAccuracy))
  #Test Data set
  for(row in seq(1,dim(diffmatrix.test)[1],1))
  {
    diffmatrix.test[row,73] = 1
    for(col in seq(1, 64, 1))
    {
      if(as.numeric(diffmatrix.test[row, col]) > simThreshold[1, col])
      {
        diffmatrix.test[row, 73] = 0
        # print(paste(col, " = ", diffmatrix[row, col], ">", simThreshold[1, col], "NO\n"))
        break
      }
    }
    
  }
  
  # (nrow(diffmatrix.test)-sum(abs(as.numeric(diffmatrix.test[,73]) - as.numeric(diffmatrix.test[,74]))))/nrow(diffmatrix.test)*100
  
  accuracy = accuracy + (nrow(diffmatrix.test)-sum(abs(as.numeric(diffmatrix.test[,73]) - as.numeric(diffmatrix.test[,74]))))/nrow(diffmatrix.test)*100
  

}

accuracy = accuracy / kfold
  
  
  
  
  
  