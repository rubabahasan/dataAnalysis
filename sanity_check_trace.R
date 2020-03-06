library(stringr)

path <- "/Users/rxh655/Documents/Research/Data/large_small/run3/ratio1/trace_Ratio1rand/"
tracename <- "trace_file"
extension <- ".txt"

largeSize = 60000

trace <- read.csv(paste(path, tracename, extension, sep = ""))
traceSplit <- str_split_fixed(trace[,1], ";;;", n= 5)

interArrivalTime <- diff(as.numeric(traceSplit[,1]), differences = 1)

# plot(x = c(1:length(interArrivalTime)), y = interArrivalTime/1000000000, type = "p")

traceSplit[,1] <- ceiling(as.numeric(traceSplit[,1])/1000000000)

reqParam <- str_split_fixed(traceSplit[,5], ";", n= 3)

n_large <- length(which(nchar(reqParam[,2]) == largeSize))

n_small <- length(which(nchar(reqParam[,2]) == 10000))

print(paste(n_large, " + ", n_small , ": ", n_large/(n_small+n_large)*100))

plot(x = c(1:nrow(traceSplit)), y = ifelse((nchar(reqParam[,2]) == largeSize), 1, 0))

jpeg(paste(path, tracename, "traceStructure.jpg", sep = ""))
barplot((ifelse((nchar(reqParam[,2]) == largeSize), 1, 0)), names.arg = 1:nrow(traceSplit), xlab = "Request count", ylab = "",ylim = c(0, 1))
dev.off()

