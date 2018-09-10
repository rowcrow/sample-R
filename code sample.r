# sample-R

##################################################################################
## Simulating data from a Weibull distribution - continuous probability distribution
## and sample several times with different sample sizes 10, 20, 50 and 100.
## to see the sampling distributions of the mean and variance              

## Simulate the population data
set.seed(465) ## setting the seed for the random numbers
N1 <- 2000 ## size of the population
## population values
x1 <- rweibull(N1, shape=1.5, scale = 1)

summary(x1)

##########################################################################################
## Distribution of the sample mean with the different sample sizes 10, 20, 50 and 100.
##########################################################################################


#################################################################################
## function dist.x.bar
## to calculate a vector of M sample means of sample with sample size n1
## giving a vector of sample means=x.b
## 
dist.x.bar <- function(n1=50){
  M <- 1000 ## number of samples drawn to approximate the probability distribution
  ## vector of sample means
  x.b <- rep(NA,M)
  ## getting samples from the population
  for (j in 1:M){
    r1 <- runif(N1)
    data <- data.frame(x1,r1)
    ## sort the data from the smallest random number
    datar <- data[order(data$r1),]
    ## select the first n1 records from the population
    dat.sample <- datar[1:n1,]
    ## sample mean for sample j
    x.b[j] <- mean(dat.sample$x1)
  }
  return(x.b)
}
## end of function
#################################################################################


## Get the sample means for different sample sizes
set.seed(465) ## setting the seed for the random numbers
x.b10 <- dist.x.bar(n1=10)
x.b20 <- dist.x.bar(n1=20)
x.b50 <- dist.x.bar(n1=50)
x.b100 <- dist.x.bar(n1=100)

## plotting the histograms of true x1 and the sampling distribution of x.bar using the same range 
## in the x values
xlim1 <- c(0.2,1.7) 
par(mfrow=c(5,1),oma=c(0,0,0,0),mar=c(3,3,1,1))
hist(x1,main=paste("Weibull distribution - population=2000"))
hist(x.b10,xlim=xlim1,main=paste("Distribution of sample means - n=10"))
hist(x.b20,xlim=xlim1,main=paste("Distribution of sample means - n=20"))
hist(x.b50,xlim=xlim1,main=paste("Distribution of sample means - n=50"))
hist(x.b100,xlim=xlim1,main=paste("Distribution of sample means - n=100"))

#######
## empirical mean
empmean.bar10 <- format(mean(x.b10), digits = 3)
empmean.bar20 <- format(mean(x.b20), digits = 3)
empmean.bar50 <- format(mean(x.b50), digits = 3)
empmean.bar100 <- format(mean(x.b100), digits = 3)
## true mean
truemean <- format(mean(x1), digits = 3)

## empirical standard deviation 
empsd.b10 <- format(sd(x.b10), digits = 3)
empsd.b20 <- format(sd(x.b20), digits = 3)
empsd.b50 <- format(sd(x.b50), digits = 3)
empsd.b100 <- format(sd(x.b100), digits = 3)
## true sd
truesd.bar10 <- format(sd(x1)/sqrt(10), digits = 3)
truesd.bar20 <- format(sd(x1)/sqrt(20), digits = 3)
truesd.bar50 <- format(sd(x1)/sqrt(50), digits = 3)
truesd.bar100 <- format(sd(x1)/sqrt(100), digits = 3)

## Sampling distribution of the mean
res <- matrix(c(10,20,50,100,truemean,truemean,truemean,truemean,empmean.bar10,empmean.bar20,empmean.bar50,empmean.bar100,truesd.bar10,truesd.bar20,truesd.bar50,truesd.bar100,empsd.b10,empsd.b20,empsd.b50,empsd.b100), ncol=5)
colnames(res) <- c('Sample Size','True Mean','Empirical Mean','True SD','Empirical SD')
rownames(res) <- c('', '','','')
res.table <- as.table(res)
## Sampling distribution of the mean
res.table 




##########################################################################################
## Distribution of the sample variance with the different sample sizes 10, 20, 50 and 100.
##########################################################################################

##########################################################################################
## function dist.var
## calculate a vector sample variances=x.b of M sample variances of sample with size n1
dist.var <- function(n1=50){
  M <- 2000 ## number of samples drawn to approximate the probability distribution
  ## vector of sample means
  x.b <- rep(NA,M)
  ## getting samples from the population
  for (j in 1:M){
    r1 <- runif(N1)
    data <- data.frame(x1,r1)
    ## sorting the data from the smallest random number
    datar <- data[order(data$r1),]
    ## selecting the first n1 records from the population
    dat.sample <- datar[1:n1,]
    ## sample mean for sample j
    x.b[j] <- var(dat.sample$x1)
  }
  return(x.b)
}
## end of function
##########################################################################################


## Get the sample variances for different sample sizes
set.seed(465) ## setting the seed for the random numbers
xv.b10 <- dist.var(n1=10)
xv.b20 <- dist.var(n1=20)
xv.b50 <- dist.var(n1=50)
xv.b100 <- dist.var(n1=100)

## plot the histograms using the same range in the x values
xlim1 <- c(0.2,1.7) 
par(mfrow=c(5,1),oma=c(0,0,0,0),mar=c(3,3,1,1))
hist(x1,main=paste("Weibull distribution - population=2000"))
hist(xv.b10,xlim=xlim1,main=paste("Distribution of sample variances - n=10"))
hist(xv.b20,xlim=xlim1,main=paste("Distribution of sample variances - n=20"))
hist(xv.b50,xlim=xlim1,main=paste("Distribution of sample variances - n=50"))
hist(xv.b100,xlim=xlim1,main=paste("Distribution of sample variances - n=100"))

## true variance
truevar <-format(var(x1), digits = 3)

## empirical mean of s^2
empmeanv.b10 <- format(mean(xv.b10), digits = 3)
empmeanv.b20 <- format(mean(xv.b20), digits = 3) 
empmeanv.b50 <- format(mean(xv.b50), digits = 3)
empmeanv.b100 <- format(mean(xv.b100), digits = 3)


## empirical sd
empsd.b10 <-format(sd(xv.b10), digits = 3)
empsd.b20 <-format(sd(xv.b20), digits = 3)
empsd.b50 <-format(sd(xv.b50), digits = 3)
empsd.b100 <-format(sd(xv.b100), digits = 3)
##(f1 <- format(z, digits = 2))
## true sd
truesd.bar10 <- format(sqrt(2)*var(x1)/sqrt(9), digits = 3)
truesd.bar20 <- format(sqrt(2)*var(x1)/sqrt(19), digits = 3)
truesd.bar50 <- format(sqrt(2)*var(x1)/sqrt(49), digits = 3)
truesd.bar100 <- format(sqrt(2)*var(x1)/sqrt(99), digits = 3)

res2 <- matrix(c(10,20,50,100,truevar,truevar,truevar,truevar,empmeanv.b10,empmeanv.b20,empmeanv.b50,empmeanv.b100,truesd.bar10,truesd.bar20,truesd.bar50,truesd.bar100,empsd.b10,empsd.b20,empsd.b50,empsd.b100), ncol=5)
colnames(res2) <- c('Sample Size','True Mean','Empirical Mean','True SD','Empirical SD')
rownames(res2) <- c('', '','','')
res2.table <- as.table(res2)
## Sampling distribution of the variance
res2.table 


