set.seed(467312)

n <- 100							# Sample size
mu <- 5								# True mean
X <- rnorm(n, mean = mu, sd = 1)	# Draw a sample of size n from the N(mu,1) distribution

mu.0 <- 5							# We test H.0: mu <= mu.0 vs H.1: mu > mu.0

X.bar <- mean(X) 					# Obtain sample mean
S <- sd(X)							# Obtain sample standard deviation
Q <- sqrt(n) * (X.bar - mu.0) / S	# Obtain the t-statistic, store as Q
print(paste("The test statistic has a value of", Q))	# Print the t-statistic on screen

B <- 500							# Choose how many bootstrap replications B (I should take 499, but then the final plot doesn't show)
Q.star <- rep(NA, B)				# Create an 'empty' vector of size B to store bootstrap statistics

for (b in 1:B) {											# Start bootstrap loop
	Sys.sleep(.01)
	J <- sample.int(n,size = n, replace = TRUE)				# Draw the indices of the bootstrap sample
	X.star <- X[J]											# Construct the bootstrap sample
	X.bar.star <- mean(X.star) 								# Obtain bootstrap sample mean
	S.star <- sd(X.star)									# Obtain bootstrap sample standard deviation
	Q.star[b] <- sqrt(n) * (X.bar.star - X.bar) / S.star	# Obtain bootstrap t-statistic
	
	# The next code only serves to draw pretty graphs, so you may ignore it.
	left.int <- seq(-4, 3.5, 0.5)
	right.int <- seq(-3.5, 4, 0.5)
	mid.int <- (left.int + right.int) / 2
	freq <- colSums(outer(Q.star[1:b], right.int, "<=") - outer(Q.star[1:b], left.int, "<="))/B
	if (b %% 10 == 0){
		plot(mid.int, freq, type = "h", col = "blue", lend = "butt", lwd = 34, xlab = "", ylab = "Frequency", ylim = c(0, 0.25))
		title("Bootstrap Distribution")
		lines(Q * c(1, 1), c(0, 0.25), lwd = 3, col = "red")
		mtext("Q", side = 3, line = 0, at = Q, col = "red")
		points(Q.star[(b - 9):b], rep(0.24, 10), col = "blue", pch = 20)
		Sys.sleep(0)
	}
}

alpha <- 0.05															# Choose a significance level alpha
c.alpha.star <- quantile(Q.star, probs = 1 - alpha)						# Get the bootstrap critical value
print(paste("The bootstrap critical value is", round(c.alpha.star, 3)))	# Show the value on screen

plot(mid.int, freq, type = "h", col = "blue", lend = "butt", lwd = 34, xlab = "", ylab = "Frequency", ylim = c(0, 0.25))
title("Bootstrap Distribution & Critical Value")
lines(Q * c(1, 1), c(0, 0.25), lwd = 3, col = "red")
mtext("Q", side = 3, line = 0, at = Q, col = "red")
lines(c.alpha.star * c(1, 1), c(0, 0.25), lwd = 3, col = "darkgreen")
mtext(expression(paste(c[alpha],"*")), side = 3, line = 0, at = c.alpha.star, col = "darkgreen")

p.value <- mean(Q.star > Q)										# Get the bootstrap p-value
print(paste("The bootstrap p-value is", round(p.value, 3)))		# Show the value on screen

larger.than.Q <- sum(left.int > Q)
plot(mid.int, freq, type = "h", col = c(rep("blue", 16 - larger.than.Q), rep("green", larger.than.Q)) , lend = "butt", lwd = 34, xlab = "", ylab = "Frequency", ylim = c(0, 0.25))
title("Bootstrap Distribution & p-Value")
lines(Q * c(1, 1), c(0, 0.25), lwd = 3, col = "red")
mtext("Q", side = 3, line = 0, at = Q, col = "red")
rect(Q + 0.02, 0, right.int[16 - larger.than.Q] - 0.02, freq[16 - larger.than.Q], col = "green", border = NA)
