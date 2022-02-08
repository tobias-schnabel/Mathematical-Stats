fib <- c(0, 1, 1, 2, 3, 5, 8, 13, 21)
fib[1]
fib[5] 
fib[c(1, 3, 5)] 
fib[10] 
length(fib)

u <- 1:5 
u 
v <- -3:3 
v 
rep(5, 10) 
rep(u, times = 2) 
rep(u, each = 2) 
seq(0, 1, by = 0.1) 
seq(0, 2, length.out = 5)

u - 1 
v^2 
for (i in 1:7) { 
  print(v[i]^2)
}


#Construct a vector v of length 10 for 
#which v i= 2i + i^2, with and without 
# using a for loop.

#with
v <- 1:10
for (i in 1:10) {
  v[i] <- 2*i + i^2
}
v

#without
oneToTen <- 1:10
x <- 2*oneToTen + oneToTen^2
x

# Construct a vector v of length 10 for which v i= i + 2 i, 
#with and without using a for loop.

#with
a <- 1:10
for (i in 1:10) {
  a[i] <- i + 2^i
}
a

#without

b <- oneToTen + 2^oneToTen
b

#Write a for loop that generates the ﬁrst k 
#numbers of the Fibonacci sequence and stores them in a vector

k <- 10
x <- 0
y <- 1
fibVec <- c()
for (i in 1:k) {
  x <- x + y
  y <- x + y
  fibVec = c(fibVec, x,y)
}
fibVec
  
#############################

M <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2) 
M[1] 
M[2]

M[1, ] 
M[, 2]

df <- data.frame(1:4, 2:5) 
colnames(df) <- c("A", "B") 
df 
df$A 
df$B 
df$B[5] 
df$A[2:3] 
df[3, ]

#
rm (list = ls ())
#

qnbinom(0.9, 5, 0.7, lower.tail = TRUE, log.p = FALSE)

1- ppois(4, 2, lower.tail = TRUE)

dexp(0.7, rate = 0.5)

pnorm(5, 10, 4, lower.tail = TRUE) #16 is variance, not sd

dcauchy(0, 0, 1) #ratio of 2 normals is cauchy bc 0 mean

dchisq(1, 2,0) #2 normals squared and added is chi sq with 2 df

plnorm(5, 1, 2, lower.tail = FALSE) #4 is variance not sd, exp(normal) = lognormal

######4

set.seed(2022) # set the seed for reproducibility 
k <- 1000 # number of replications 
n <- 100 # sample size 
means <- rep(0, k) # create a container for the output 
for (i in 1:k) {
  x <- rchisq(n, 6)
  means[i] <- mean(x) 
  }

# Now plot the histogram of means and 
# add the theoretical density as a line in red

xvals <- seq(0, 10, by = 0.01) 
hist(means, probability = TRUE, ylim = c(0, 1)) 
lines(xvals, dnorm(xvals, 6, sqrt(12/n)), col = "red")

# Now plot the empirical cumulative distribution function and 
# add the theoretical distribution as a line in red

xvals <- seq(0, 10, by = 0.01) 
plot(ecdf(means)) 
lines(xvals, pnorm(xvals, 6, sqrt(12/n)), col = "red")


###REPEAT FOR X ~ EXP(1)
set.seed(2022) # set the seed for reproducibility 
k <- 1000 # number of replications 
n <- 100 # sample size 
meansNew <- rep(0, k) # create a container for the output 

for (i in 1:k) {
  x <- rexp(n, 1)
  meansNew[i] <- mean(x) 
}


xvals <- seq(0, 10, by = 0.01) 
hist(meansNew)


######5


x <- rnorm(100) 
y <- rnorm(100)

# plot the vector x against the index: 
plot(x)

# plot the vector y against x: 
plot(x, y)

# illustrate some of the additional options:

plot(x, y, main = "scatter plot of y against x", col = "red",
     xlim = c(-5, 5), ylim = c(-5, 5))

points(x = c(-3, 2), y = c(-4, 4))
abline(h = 1)

#####

x <- rnorm(1000)

# make a histogram of x hist(x)
hist(x)
# make a boxplot of x boxplot(x)
boxplot(x)

a <- rt(1000, 2)
hist(a)
boxplot(a)
