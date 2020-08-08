library(writexl)     #write_xlsx

idx = "50-10"
filename <- "/Volumes/YES NAME/trace_FILES/wiki/first305659Good.txt"
file <- paste("/Volumes/YES NAME/trace_FILES/wiki/sample",idx,".txt",sep = "")


mydata <- read.delim(filename, sep = " ")

mydata[152842, 4]

sampleSize = floor(50/100*nrow(mydata))

sample1 <- mydata[sample(nrow(mydata), sampleSize, replace = FALSE),]

write.table(sample1, file, append = FALSE, sep = " ", dec = ".",row.names = FALSE, col.names = FALSE)
# 
# latencystatsresult <- tibble::rownames_to_column(as.data.frame(sample1), "VALUE")
# write_xlsx(as.data.frame(latencystatsresult), paste("sample",idx,".txt",sep = ""))
