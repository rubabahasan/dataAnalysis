library(stringr)

path <- "/Users/rxh655/Documents/Research/Data/large_small/run4/output/ratio2/aggregate_folder/Ratio2/"
tracename <- "trace_file.txt"

trace <- read.csv(paste(path, tracename, sep = ""))
traceSplit <- str_split_fixed(trace[,1], ";;;", n= 5)

interArrivalTime <- diff(as.numeric(traceSplit[,1]), differences = 1)

plot(x = c(1:length(interArrivalTime)), y = interArrivalTime/1000000000, type = "p")

traceSplit[,1] <- ceiling(as.numeric(traceSplit[,1])/1000000000)

queue = matrix(nrow = as.numeric(traceSplit[nrow(traceSplit)]), ncol= 1)

queue[,1] = 0

print(nrow(traceSplit))

for(i in 1:nrow(traceSplit))
{
  queue[as.numeric(traceSplit[i, 1])] <- queue[as.numeric(traceSplit[i, 1])] + 1
  
  # print(paste("queue[", as.numeric(traceSplit[i, 1]), "]=", queue[as.numeric(traceSplit[i, 1])]))
}

print(sum(as.numeric(queue[,1])))

jpeg(paste(path, str_split_fixed(tracename, "\\.", n=2)[,1], "graphp.jpg", sep = ""))

barplot( as.numeric(queue[,1]), names.arg = c(1:nrow(queue)), type = "p", xlab = "Time, sec", ylab = "# of requests", width = 1)
dev.off()

