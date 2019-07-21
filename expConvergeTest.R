library(moments)
library(sjstats)

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
