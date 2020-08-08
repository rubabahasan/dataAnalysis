library(stringr)  #str_split_fixed

drawRateGraph <- function(filepath, filename){
  traceName <- unlist(strsplit(filename, ".", fixed = TRUE))[1]
  print(paste("name ", traceName))
  
  trace <- read.csv(paste(filepath,"/", filename, sep = ""))
  # traceSplit <- str_split_fixed(trace, ",", n= 2)
  
  pdf(paste(filepath, "/", filename, "graph.pdf", sep = ""))
  
  plot(x = seq(1,nrow(trace),1), y = trace[,2] , type= "l", main = paste("graph"))
  # mapply(axis, side = 1, at = axTicks(1), labels = axTicks(1))
  lines(x = trace[,1], y = trace[,2])
  dev.off()
  return(trace)
  
}

path <- "/Volumes/YES NAME/trace_FILES/systor/arrival_times"
files <- list.files(path = path )
for(f in files)
{
  
  if(traceName <- unlist(strsplit(f, ".", fixed = TRUE))[2] == "csv"){
    print(f)
    data<- drawRateGraph(path, f)
  }
  
}
