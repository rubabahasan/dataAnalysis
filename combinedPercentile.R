library(stringr)  #str_split_fixed
library(moments)  #skewness, kurtosis
library(sjstats)  #cv
library(writexl)     #write_xlsx
library(dplyr) #rite rownames as column


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
  
  
  throughput <- 0
  throughput<- calculateThroughput(tracelineRsplit)
  #return(list(web_server, web_serverStats, datastats))
  print("*********************  Done  ************************")
  return(list(web_serverStats, datastats, latencystats, latency, reqResponse[,2], queue, throughput))
}

rootpathDir = "/Volumes/YES NAME/trace_data/exp-44/aggregate_folder"
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

cdfVal <- matrix(data = NA, nrow = 8000, ncol = length(directories)-1)
m0 <- matrix(NA, ncol = length(directories)-1, nrow = 1)
colnames(cdfVal) <- m0


for(dir in directories[-1]) #all elements except first one
{
  print(dir)
  traceName <- unlist(strsplit(dir, "/"))[7]
  rownames(latencystatsresult)[idx] <- traceName
  
  #webserverstatsresult[idx,] <-  statsFromFolder(dir)[[1]]
  res <- statsFromFolder(dir)
  latencystatsresult[idx, 1:32] <- res[[3]]#statsFromFolder(dir)[[3]]
  latencystatsresult[idx, 33] <- res[[7]]
  
  queue <- res[[6]]
  
  cdfVal[1:nrow(res[[4]]),idx] <- res[[4]][,1]
  colnames(cdfVal)[idx] <- traceName
  
  latency <- res[[4]]
  reqres <- res[[5]]
  diff <- as.numeric(latency) - as.numeric(reqres[2:3000])
  idx = idx+1
}

# drawMultipleCDF(cdfVal, rootpathDir)
latencystatsresult <- tibble::rownames_to_column(as.data.frame(latencystatsresult), "VALUE")
write_xlsx(as.data.frame(latencystatsresult), paste(rootpathDir,"/", unlist(strsplit(rootpathDir, "/"))[6], unlist(strsplit(rootpathDir, "/"))[7], ".xlsx", sep = ""))
# write_xlsx(as.data.frame(latencystatsresult), paste("/Users/rxh655/The Pennsylvania State University/Sajal, Sultan Mahmud - Research/TraceDownscaler/experiment-11/results/aggregate_folder/", unlist(strsplit(rootpathDir, "/"))[6], unlist(strsplit(rootpathDir, "/"))[7], ".xlsx", sep = ""))
