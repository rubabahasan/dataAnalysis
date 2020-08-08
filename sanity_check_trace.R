library(stringr)
library(writexl)     #write_xlsx

drawBigReq <- function(folderpath, idx){
  tracename <- "block1_v1rand"
  # tracename <- "newFile"
  extension <- ".txt"
  
  largeSize = 60000
  
  trace <- read.csv(paste(folderpath,"/", tracename, extension, sep = ""))
  traceSplit <- str_split_fixed(trace[,1], ";;;", n= 5)
  
  interArrivalTime <- diff(as.numeric(traceSplit[,1]), differences = 1)
  
  # plot(x = c(1:length(interArrivalTime)), y = interArrivalTime/1000000000, type = "p")
  
  traceSplit[,1] <- ceiling(as.numeric(traceSplit[,1])/1000000000)
  # 
  # a.df <- as.data.frame(traceSplit)
  # 
  # b <- aggregate(a.df[c("V2", "V3", "V4", "V5")], a.df["V1"], FUN = length)
  
  reqParam <- str_split_fixed(traceSplit[,5], ";", n= 3)
  
  n_large <- length(which(nchar(reqParam[,2]) == largeSize))
  
  n_small <- length(which(nchar(reqParam[,2]) == 10000))
  
  # print(which(nchar(reqParam[,2]) == largeSize)+1)
  
  ratio <- n_large/(n_small+n_large)*100
  print(paste(n_large, " + ", n_small , ": ", n_large/(n_small+n_large)*100))
############### *************************************************################################
  queue = matrix(nrow = as.numeric(traceSplit[nrow(traceSplit)]), ncol= 1)
  queueLarge = matrix(nrow = as.numeric(traceSplit[nrow(traceSplit)]), ncol= 1)
  queueSmall = matrix(nrow = as.numeric(traceSplit[nrow(traceSplit)]), ncol= 1)
  queueDummy = matrix(nrow = as.numeric(traceSplit[nrow(traceSplit)]), ncol= 1)
  
  queue[,1] = 0
  queueLarge[,1] = 0
  queueSmall[,1] = 0
  queueDummy[,1] = 0
  
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
############### *************************************************################################
  
  if(idx == 1){
    # plot(x = c(1:nrow(traceSplit)), y = ifelse((nchar(reqParam[,2]) == largeSize), 0.7, -1), ylim = c(0.15,0.75), yaxt = "n", xaxt = "n", col=1, xlab = "Request index", ylab = "", pch = 16, cex=2, cex.lab = 2)
    plot(x = c(1:nrow(queueLarge)), y = queueLarge*(7-idx), ylim = c(0.5,6.5), yaxt = "n", xaxt = "n", col=1, xlab = "Time (s)", ylab = "", pch = 16, cex=2, cex.lab = 2)
    
    # axis(2, at=seq(0.2, 0.7, 0.1), labels=c("Run 1", "Run 2", "Run 3", "Run 4", "Run 5", "Original"), cex.axis = 2, las = 2)
    axis(2, at=seq(1, 6, 1), labels=c("Run 1", "Run 2", "Run 3", "Run 4", "Run 5", "Original"), cex.axis = 2, las = 2)
    axis(3, at=seq(0, 100, 10), cex.axis = 2)
    axis(1, at=seq(0, 100, 10), labels = seq(0, 100, 10), cex.axis = 2)
  }
  else{
    # points(x = seq(1,nrow(traceSplit)*2, 2), y = ifelse((nchar(reqParam[,2]) == largeSize), idx/10, -1), ylim = c(0,1), col=1, pch = idx, cex = 2)
    points(x = c(1:nrow(queueLarge)), y = queueLarge*(7-idx), ylim = c(0.5,6.5), col=1, pch = idx, cex = 2)
  }

  print(paste(folderpath, "/", unlist(strsplit(folderpath, "/"))[7], tracename, "traceStructure.jpg", sep = ""))
  
  # jpeg(paste(folderpath, "/", unlist(strsplit(folderpath, "/"))[7], tracename, "traceStructure.jpg", sep = ""))
  # barplot((ifelse((nchar(reqParam[,2]) == largeSize), 1, 0)), names.arg = 1:nrow(traceSplit),  xlab = "Request index", ylab = "", ylim = c(0, 1), cex.lab = 2, cex.main = 3)
  # 
  # # points((ifelse((nchar(reqParam[,2]) == largeSize), 1, 0)), names.arg = 1:nrow(traceSplit),  xlab = "Request index", ylab = "", ylim = c(0, 1), cex.lab = 2, cex.main = 3)
  # 
  # dev.off()
  

  return(ratio)
}


path <- "/Users/rxh655/Documents/Research/Final Graph Data/Rare"
directories <- list.dirs(path = path ) #first one is the parent directory

ratioOfLarge <- matrix(nrow = length(directories)-1, ncol = 1)
m0 <- matrix(NA, nrow = length(directories)-1, ncol = 1)
rownames(ratioOfLarge) <- m0

pdf(paste(path, "/traceTime.pdf", sep = ""))
par(mar=c(5, 4+3, 4, 2))
idx = 1
for(dir in directories[-1]) #all elements except first one
{
  print(dir)
  traceName <- unlist(strsplit(dir, "/"))[8]
  rownames(ratioOfLarge)[idx] <- traceName

  ratioOfLarge[idx, 1]<- drawBigReq(dir, idx)
  idx <- idx + 1
}

abline(h=1.5)
abline(h=2.5)
abline(h=3.5)
abline(h=4.5)
abline(h=5.5)

dev.off()
# 
# ratioOfLarge <- tibble::rownames_to_column(as.data.frame(ratioOfLarge), "NAME")
# write_xlsx(as.data.frame(ratioOfLarge), paste(path, ".xlsx", sep = ""))
# 
# path <- "/Users/rxh655/OneDrive - The Pennsylvania State University/Research/Data/test"
# 
# drawBigReq(path)
# 
