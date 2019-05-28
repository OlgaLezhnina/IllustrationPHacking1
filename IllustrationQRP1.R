#set wd
#get a package for correlations results
library(Hmisc)

#set seed to have results fixed (should be removed to get different random datasets )
set.seed(100)

#generate random data with normally distributed columns
#1000 observations, 100 variables
datamatrix <- matrix(rnorm(1000*100),ncol = 100)
mydata <- data.frame(datamatrix)

#see correlations
results <- Hmisc::rcorr(as.matrix(mydata))
#how many of them are above .1, with diagonal values excluded
(sum(results$r > 0.1) - 100)/2
#which exactly
coeff <- which(results$r > 0.1, arr.ind = T)
coeff <- as.data.frame(coeff)
which(coeff$row != coeff$col)
#View(coeff) #as the names of variables are different than indices in the output
#choose one pair of variables as an example
rcorr(mydata$X16, mydata$X55)# p = .002, r = .10

# decrease the sample size by selecting 100 random cases
set.seed(100)
data1 <- mydata[sample(nrow(mydata), 100), ]
#see correlations
results1 <- rcorr(as.matrix(data1))
(sum(results1$r > 0.1) - 100)/2
#look for higher correlations
(sum(results1$r > 0.4) - 100)/2
coeff1 <- which(results1$r > 0.4, arr.ind = T)
coeff1 <- as.data.frame(coeff1)
which(coeff1$row != coeff1$col)
#View(coeff1)
rcorr(data1$X5, data1$X34)# p < .001, r = .41

#get a sample size even smaller by selecting 20 random cases
set.seed(100)
data2 <- mydata[sample(nrow(mydata), 20), ]
results2 <- rcorr(as.matrix(data2))
#look for even higher correlations
(sum(results2$r > 0.7) - 100)/2
coeff2 <- which(results2$r > 0.7, arr.ind = T)
coeff2 <- as.data.frame(coeff2)
which(coeff2$row != coeff2$col)
#View(coeff2)
rcorr(data2$X4, data2$X57) #p<.001, r = .71

#now get fewer variables, only ten, for analysis
set.seed(100)
datamatrix3 <- matrix(rnorm(1000*10),ncol = 10)
mydata3 <- data.frame(datamatrix3)
results3 <- rcorr(as.matrix(mydata3))
(sum(results3$r > 0.1) - 10)/2
#with a large sample size and few variables we do not have false positives - at least, in this sample


