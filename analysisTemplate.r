library(moments)
library(sjstats)
library(pracma)
library(quantmod)
library(stringr)
library(combinat)
library(outliers)
library(EnvStats)
getwd()
# 
# setwd("~/Documents/Research/Code/Analysis/sample_trace")
# getwd()

repeatationVal = 1

#read all the directories like trace1-1, trace 2-1 etc.
slash = "/"

# rootpathDir = "~/Documents/Research/Data/aggregate_folder4"
rootpathDir = "~/Documents/Research/Data/run11"

directories <- list.dirs(path = rootpathDir ) #first one is the parent directory
nRow <- length(directories)-1

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
  mean(web_server$web_server_memory_usage)
  
  
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
    datamatrix[idx,(i*4)+1] <- mean(memcache[,j]) # web memory use
    #datamatrix[idx,(i*4)+2] <- var(memcache[,j]) # web memory use
    datamatrix[idx,(i*4)+2] <- skewness(memcache[,j]) # web memory use
    datamatrix[idx,(i*4)+3] <- kurtosis(memcache[,j]) # web memory use
    datamatrix[idx,(i*4)+4] <- cv(memcache[,j]) # web memory use
    # print("Printing")
    # print(i*5 +1)
    # print(j)
    
    i = i + 1
  }
  
  #mysql
  i = 10
  for(j in c(2,4,5,6,8))
  {
    datamatrix[idx,(i*4)+1] <- mean(mysql[,j]) # web memory use
    #datamatrix[idx,(i*4)+2] <- var(mysql[,j]) # web memory use
    datamatrix[idx,(i*4)+2] <- skewness(mysql[,j]) # web memory use
    datamatrix[idx,(i*4)+3] <- kurtosis(mysql[,j]) # web memory use
    datamatrix[idx,(i*4)+4] <- cv(mysql[,j]) # web memory use
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
    
    datamatrix[idx,(i*4)+1] <- mean(reqResponse[,j]) # web memory use
    #datamatrix[idx,(i*4)+2] <- var(reqResponse[,j]) # web memory use
    datamatrix[idx,(i*4)+2] <- skewness(reqResponse[,j]) # web memory use
    datamatrix[idx,(i*4)+3] <- kurtosis(reqResponse[,j]) # web memory use
    datamatrix[idx,(i*4)+4] <- cv(reqResponse[,j]) # web memory use
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
    
    
    datamatrix[idx,(i*4)+1] <- mean(latency) # web memory use
    #datamatrix[idx,(i*4)+2] <- var(latency) # web memory use
    datamatrix[idx,(i*4)+2] <- skewness(latency) # web memory use
    datamatrix[idx,(i*4)+3] <- kurtosis(latency) # web memory use
    if(is.na(mean(latency)))
    {
      datamatrix[idx,(i*4)+4] <- NA # web memory use
    }else{
      datamatrix[idx,(i*4)+4] <- cv(latency) # web memory use
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
    datamatrix[idx,(i*4)+1] <- mean(interArrivalTime) # web memory use
    #datamatrix[idx,(i*4)+2] <- var(interArrivalTime) # web memory use
    datamatrix[idx,(i*4)+2] <- skewness(interArrivalTime) # web memory use
    datamatrix[idx,(i*4)+3] <- kurtosis(interArrivalTime) # web memory use
    datamatrix[idx,(i*4)+4] <- cv(interArrivalTime) # web memory use
    
    # print("Printing")
    # print(i*5 +1)
    # print(j)
    
    i = i + 1
  }
  
  idx = idx + 1
}
# 
# smalldm <- datamatrix[,68:72]
# smalldm[,4] <- smalldm[,4] -3
#  
# directories[2]
# traceName <- unlist(strsplit(directories[2], "/"))[8]

# Create difference matrix
dim(datamatrix)
nRowDiff <- choose(nRow, 2)
print(paste("nRowDiff", nRowDiff))
diffmatrix <- matrix(nrow = nRowDiff, ncol = 73)  #1275 for 51 #903 for 43
#diffmatrix.colnames <- c("web_s_mem_use_mean", "web_s_mem_use_var", "web_s_mem_use_skew", "web_s_mem_use_kurt", "web_s_mem_use_coeff_var", "web_s_mem_pcnt_mean", "web_s_mem_pcnt_var", "web_s_mem_pcnt_skew", "web_s_mem_pcnt_kurt", "web_s_mem_pcnt_coeff_var" ,"web_s_cpu_pcnt_mean", "web_s_cpu_pcnt_var", "web_s_cpu_pcnt_skew", "web_s_cpu_pcnt_kurt", "web_s_cpu_pcnt_coeff_var", "web_s_d_sent_mean", "web_s_d_sent_var", "web_s_d_sent_skew", "web_s_d_sent_kurt", "web_s_d_sent_coeff_var", "web_s_d_recv_mean", "web_s_d_recv_var", "web_s_d_recv_skew", "web_s_d_recv_kurt", "web_s_d_recv_coeff_var", "cache_mem_use_mean", "cache_mem_use_var", "cache_mem_use_skew", "cache_mem_use_kurt", "cache_mem_use_coeff_var", "cache_mem_pcnt_mean", "cache_mem_pcnt_var", "cache_mem_pcnt_skew", "cache_mem_pcnt_kurt", "cache_mem_pcnt_coeff_var ", "cache_cpu_pcnt_mean", "cache_cpu_pcnt_var", "cache_cpu_pcnt_skew", "cache_cpu_pcnt_kurt", "cache_cpu_pcnt_coeff_var", "cache_d_sent_mean", "cache_d_sent_var", "cache_d_sent_skew", "cache_d_sent_kurt", "cache_d_sent_coeff_var", "cache_d_recv_mean", "cache_d_recv_var", "cache_d_recv_skew", "cache_d_recv_kurt", "cache_d_recv_coeff_var", "mysql_mem_use_mean", "mysql_mem_use_var", "mysql_mem_use_skew", "mysql_mem_use_kurt", "mysql_mem_use_coeff_var", "mysql_mem_pcnt_mean", "mysql_mem_pcnt_var", "mysql_mem_pcnt_skew", "mysql_mem_pcnt_kurt", "mysql_mem_pcnt_coeff_var ", "mysql_cpu_pcnt_mean", "mysql_cpu_pcnt_var", "mysql_cpu_pcnt_skew", "mysql_cpu_pcnt_kurt", "mysql_cpu_pcnt_coeff_var", "mysql_d_sent_mean", "mysql_d_sent_var", "mysql_d_sent_skew", "mysql_d_sent_kurt", "mysql_d_sent_coeff_var", "mysql_d_recv_mean", "mysql_d_recv_var", "mysql_d_recv_skew", "mysql_d_recv_kurt", "mysql_d_recv_coeff_var", "req_res_latency_mean", "req_res_latency_var", "req_res_latency_skew", "req_res_latency_kurt", "req_res_latency_coeff_var", "traceline_res_latency_mean", "traceline_res_latency_var", "traceline_res_latency_skew", "traceline_res_latency_kurt", "traceline_res_latency_coeff_var", "interArrivalT_latency_mean", "interArrivalT_latency_var", "interArrivalT_latency_skew", "interArrivalT_latency_kurt", "interArrivalT_latency_coeff_var", "DiffVal")
diffmatrix.colnames <- c("web_s_mem_use_mean", "web_s_mem_use_skew", "web_s_mem_use_kurt", "web_s_mem_use_coeff_var", "web_s_mem_pcnt_mean", "web_s_mem_pcnt_skew", "web_s_mem_pcnt_kurt", "web_s_mem_pcnt_coeff_var" ,"web_s_cpu_pcnt_mean", "web_s_cpu_pcnt_skew", "web_s_cpu_pcnt_kurt", "web_s_cpu_pcnt_coeff_var", "web_s_d_sent_mean", "web_s_d_sent_skew", "web_s_d_sent_kurt", "web_s_d_sent_coeff_var", "web_s_d_recv_mean", "web_s_d_recv_skew", "web_s_d_recv_kurt", "web_s_d_recv_coeff_var", "cache_mem_use_mean", "cache_mem_use_skew", "cache_mem_use_kurt", "cache_mem_use_coeff_var", "cache_mem_pcnt_mean", "cache_mem_pcnt_skew", "cache_mem_pcnt_kurt", "cache_mem_pcnt_coeff_var ", "cache_cpu_pcnt_mean", "cache_cpu_pcnt_skew", "cache_cpu_pcnt_kurt", "cache_cpu_pcnt_coeff_var", "cache_d_sent_mean", "cache_d_sent_skew", "cache_d_sent_kurt", "cache_d_sent_coeff_var", "cache_d_recv_mean", "cache_d_recv_skew", "cache_d_recv_kurt", "cache_d_recv_coeff_var", "mysql_mem_use_mean", "mysql_mem_use_skew", "mysql_mem_use_kurt", "mysql_mem_use_coeff_var", "mysql_mem_pcnt_mean", "mysql_mem_pcnt_skew", "mysql_mem_pcnt_kurt", "mysql_mem_pcnt_coeff_var ", "mysql_cpu_pcnt_mean", "mysql_cpu_pcnt_skew", "mysql_cpu_pcnt_kurt", "mysql_cpu_pcnt_coeff_var", "mysql_d_sent_mean", "mysql_d_sent_skew", "mysql_d_sent_kurt", "mysql_d_sent_coeff_var", "mysql_d_recv_mean", "mysql_d_recv_skew", "mysql_d_recv_kurt", "mysql_d_recv_coeff_var", "req_res_latency_mean", "req_res_latency_skew", "req_res_latency_kurt", "req_res_latency_coeff_var", "traceline_res_latency_mean", "traceline_res_latency_skew", "traceline_res_latency_kurt", "traceline_res_latency_coeff_var", "interArrivalT_latency_mean", "interArrivalT_latency_skew", "interArrivalT_latency_kurt", "interArrivalT_latency_coeff_var", "DiffVal")

colnames(diffmatrix) <- diffmatrix.colnames
rownames(diffmatrix) <- seq(1,nRowDiff,1)

#diffmatrix[1,1:85] = datamatrix[1,] - datamatrix[2,]
rownames(diffmatrix)[10]
k = 1
for( i in seq(1, (nRow-1), 1))
{
  for( j in seq((i+1), nRow, 1))
  {
    
    rownames(diffmatrix)[k] <- paste(rownames(datamatrix)[i], "vs", rownames(datamatrix)[j])
    # diffmatrix[k,1:90] =  abs(datamatrix[i,] - datamatrix[j,])/max(abs(datamatrix[i,]), abs(datamatrix[j,]))*100
  
    for(colIdx in seq(1,72,4))
    {
      #mean uses average difference
      diffmatrix[k,colIdx] = abs(datamatrix[i,colIdx] - datamatrix[j,colIdx])/max(abs(datamatrix[i,colIdx]), abs(datamatrix[j,colIdx]))*100
    }
    for(colIdx in seq(2,72,2))
    {
      #skewness, coeffecient of variance uses only difference
      diffmatrix[k,colIdx] = abs(datamatrix[i,colIdx] - datamatrix[j,colIdx])
    }
    for(colIdx in seq(3,72,4))
    {
      #kurtosis uses only difference
      diffmatrix[k,colIdx] = abs(datamatrix[i,colIdx] - datamatrix[j,colIdx])
    }
    
    # print(rownames(diffmatrix)[k])
    # print(datamatrix[i,1])
    # print(datamatrix[j,1])
    # print(datamatrix[i,1] - datamatrix[j,1])
    # print(abs(datamatrix[i,1] - datamatrix[j,1]))
    # print(max(abs(datamatrix[i,1]), abs(datamatrix[j,1])))
    # print(abs(datamatrix[i,1] - datamatrix[j,1])/max(abs(datamatrix[i,1]), abs(datamatrix[j,1]))*100)
    # print(diffmatrix[k,1])
    
  
    # trace1 <- unlist(strsplit(rownames(datamatrix)[i], "-"))[1]
    # trace2 <- unlist(strsplit(rownames(datamatrix)[j], "-"))[1]
    # 
    # if(strcmp(trace1,trace2))
    # {
    #   diffmatrix[k,86] = 0.1
    # }
    # 
    # if(i == j)
    # {
    #   diffmatrix[k,86] = 0
    # }
    
    k = k + 1
  }
}



######################## draw graph ########################
print(rownames(diffmatrix)[2])
dim(diffmatrix)[1]
max(as.numeric(diffmatrix[,68]))
?max
#split rownames


for(col in seq(48, dim(diffmatrix)[2]-1, 1))
{
  jpeg(paste("/Users/rxh655/Documents/Research/Data/graph11/scatter",col,"graph.jpg"))
  plot(y=0, x=0, xlim = c(1,dim(diffmatrix)[1]), ylim = c(min(diffmatrix[,col]), max(diffmatrix[,col])), xlab = "Index", ylab = colnames(diffmatrix)[col], col= "white")
  for(row in seq(1, dim(diffmatrix)[1], 1))
  {
    rowname <- rownames(diffmatrix)[row]
    test <- str_split_fixed(rowname, "vs", n = 2)
    
    test1 <- str_split_fixed(test[1], "e", n = 2)
    test2 <- str_split_fixed(test[2], "e", n = 2)
    
    val1 <- str_split_fixed(test1[2], "-", n = 2)
    val2 <- str_split_fixed(test2[2], "-", n = 2)
    
    if(as.numeric(val1[1]) == as.numeric(val2[1]))
    {
      # print(paste("SAME", row, ":", col, diffmatrix[row,col]))
      points(y=diffmatrix[row,col], x = row, col= "red")
    }
    else{
      if(floor((as.numeric(val1[1])-1)/repeatationVal) == floor((as.numeric(val2[1])-1)/repeatationVal))
      {
        # print(paste("SIMILAR", row, ":", col, diffmatrix[row,col]))
        points(y=diffmatrix[row,col], x = row, col= "green")
      }
      else{
        # print(paste("DIFFERENT", row, ":", col, diffmatrix[row,col]))
        points(y=diffmatrix[row,col], x = row, col= "blue")
      }
    }
  }
  dev.off()
}

##################### Define Similarity ########################


s <- .8
for(row in seq(1,nRowDiff,1))
{
  # for(col in seq(61, 72, 1))
  # {
  #   if(as.numeric(diffmatrix[row, col]) > s)
  #   {
  #     diffmatrix[row, 73] = "NO"
  #     #(paste(col, " = ", diffmatrix[row, col], "NO\n"))
  #     break
  #   }else
  #   {
  #     if(col == 72)
  #     {
  #       diffmatrix[row,73] = "YES"
  #       #print(paste(col, " = ", diffmatrix[row, col], "YES\n"))
  #     }
  #   }
  # }
  
  diffmatrix[row,73] = "YES"
  if(as.numeric(diffmatrix[row, 61]) > 0.6)
  {
    diffmatrix[row, 73] = "NO"
    print(paste(61, " = ", diffmatrix[row, 61], "NO\n"))
  }
  
  if(as.numeric(diffmatrix[row, 62]) > 2.5)
  {
    diffmatrix[row, 73] = "NO"
    print(paste(62, " = ", diffmatrix[row, 62], "NO\n"))
  }
  
  if(as.numeric(diffmatrix[row, 63]) > 30)
  {
    diffmatrix[row, 73] = "NO"
    print(paste(63, " = ", diffmatrix[row, 63], "NO\n"))
  }
  
  if(as.numeric(diffmatrix[row, 64]) > 0.01)
  {
    diffmatrix[row, 73] = "NO"
    print(paste(64, " = ", diffmatrix[row, 64], "NO\n"))
  }
  
}


smalldm <- diffmatrix[, 61:73]
smalldm <- subset(diffmatrix)
?points

plot(diffmatrix[,61])

mobs <- diffmatrix[,76:91]
#take subset of datamatrix

smalldm <- subset(diffmatrix, select = c("web_s_mem_use_mean", "web_s_mem_use_var", "web_s_mem_use_skew", "web_s_mem_use_kurt", "web_s_mem_use_coeff_var", "DiffVal"))
smalldm <- smalldm[order(smalldm[,6]),]
smalldm <- smalldm[1:28, ]

smalldm <- as.data.frame(smalldm)
linearMod <- lm(smalldm$DiffVal ~ smalldm$web_s_mem_use_mean+smalldm$web_s_mem_use_var+smalldm$web_s_mem_use_skew+smalldm$web_s_mem_use_kurt+smalldm$web_s_mem_use_coeff_var, data=smalldm)  # build linear regression model on full data
print(linearMod)        

summary(linearMod)

names(linearMod)
coef(linearMod)



############################ Rough ######################################
?Delt
Delt(80,90)
Delt(90,80)

test1 <- "742209389;;;33;;;1561326866860000000;;;CLEAN_UP;YlEIUNk4sU;290538380"
test2 <- "874993841;;;61;;;1561326864824000000;;;LOG_IN;UcpC47DJPe;1534517702"
test <- data.frame(c(test1,test2))
temp <- unlist(strsplit(as.character(test$c.test1..test2.), ";;;"))

str_split_fixed(test$c.test1..test2., ";;;", n = 5)


rowname = "trace1-1 vs trace2-2"
test <- str_split_fixed(rowname, "vs", n = 2)

test1 <- str_split_fixed(test[1], "e", n = 2)
test2 <- str_split_fixed(test[2], "e", n = 2)

val1 <- str_split_fixed(test1[2], "-", n = 2)
val2 <- str_split_fixed(test2[2], "-", n = 2)
a = floor((as.numeric(val1[1])-1)/repeatationVal)
b = floor((as.numeric(val2[1])-1)/repeatationVal)
if((as.numeric(val1[1])-1)/repeatationVal == (as.numeric(val2[1])-1)/repeatationVal)

?diff

?abs
abs(0)

write(diffmatrix, file = "/Users/rxh655/Documents/Research/Data/diffmatrix.csv")
print(seq(2, 20, 2))


##################### Testing if moments of Exp distribution converge ##################
meanList <- list()
skewList <- list()
kurtList <- list()
coefList <- list()

# val = 1000
# mul = 1
Nlist <- c(1000, 10000, 100000, 1000000, 10000000)
for(loop in seq(1,5,1))
{
  # N = mul*val
  meanSum = 0
  skewSum = 0
  kurtSum = 0
  coefSum = 0
  # print(N)
  for (i in seq(1,10,1)) {
    data = rexp(n=Nlist[loop], rate = 0.4)
    # hist(data, plot=TRUE)
    # print(mean(data))
    # print(skewness(data))
    # print(kurtosis(data))
    meanSum = meanSum + mean(data)
    skewSum = skewSum + skewness(data)
    kurtSum = kurtSum + (kurtosis(data)-3)
    coefSum = coefSum + cv(data)
  }
  # mul = mul * 10
  meanSum = meanSum/10
  skewSum = skewSum/10
  kurtSum = kurtSum/10
  coefSum = coefSum/10
  
  meanList <- c(meanList, meanSum)
  skewList <- c(skewList, skewSum)
  kurtList <- c(kurtList, kurtSum)
  coefList <- c(coefList, coefSum)
}

print(paste("Mean", meanList))
print(paste("Skew", skewList))
print(paste("Kurt", kurtList))
print(paste("Coef", coefList))


attach(mtcars)
par(mfrow=c(2,2))
plot(y = meanList, x = log10(Nlist), ylab = "Mean", xlab = "log(Number of samples)", main = "Expected mean = 2.5")
plot(y = skewList, x = log10(Nlist), ylab = "Skewness", xlab = "log(Number of samples)", main = "Expected skewness = 2")
plot(y = kurtList, x = log10(Nlist), ylab = "Kurtosis", xlab = "log(Number of samples)", main = "Expected kurtosis = 6")
plot(y = coefList, x = log10(Nlist), ylab = "Coefficient of variation", xlab = "log(Number of samples)", main = "Expected coefficient of variation = 1")

##################################################################


data = rexp(n=4000, rate = 0.4)
hist(data, plot=TRUE)
print(mean(data))
print(skewness(data))
print(kurtosis(data))
?plot
?kurtosis

print(floor(3/3))

s1 = "trace13-1"
s2 = "trace1-1"
s3 = "trace13-2"

strcmp(s1,"trace13-1")

choose(5,5)

?choose


test <- read.csv("/Users/rxh655/Documents/Research/Data/param_fit/trace1-1/requestResponseTimes__trace1-1_.csv")
test <- read.csv("/Users/rxh655/Documents/Research/Data/run9/trace7-2/requestResponseTimes__trace7-2_.csv")

while(kurtosis(test$latency) > 100)
{
  outlier <- max(test$latency)
  test <- test[!(test$latency %in% outlier),]
}

kurtosis(test$latency)
hist(test$latency, plot=TRUE, breaks = 100)
length(test$latency)
# plot(y = test$latency, x = seq(1,10000,1), ylim = c(30000000, 50000000))

plot(y = test$latency, x = seq(1,length(test$latency),1))

outlier<- boxplot.stats(test$latency)$out

boxplot(test$latency)
test.noOutlier <- test$latency[!(test$latency %in% outlier)]
length(test.noOutlier)
boxplot(test.noOutlier)
?hist
?boxplot.stats

outlier(test$latency, opposite = TRUE)

sum(scores(test$latency, type= "chisq", prob = 0.9))
sum(scores(test$latency, type= "t", prob = 0.9))
sum(scores(test$latency, type= "z", prob = 0.9))
sum(scores(test$latency, type= "iqr", prob = 0.9))
sum(scores(test$latency, type= "mad", prob = 0.9))

x <- c(4,3,1,7,8,4,5,9)
?scores
?rosnerTest
ou <- rosnerTest(test$latency, k=2)
outVal <- ou$all.stats[ou$all.stats$Outlier == TRUE,]
print(outVal$Value)
?plot
