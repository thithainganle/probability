##Given a vector x, we can define a function for computing the CDF of x using:
  
  F <- function(a) mean(x <= a)
  1 - F(70)    # probability of male taller than 70 inches

  # plot distribution of exact heights in data
  plot(prop.table(table(x)), xlab = "a = Height in inches", ylab = "Pr(x = a)")
  
  # probabilities in actual data over length 1 ranges containing an integer
  mean(x <= 68.5) - mean(x <= 67.5)
  mean(x <= 69.5) - mean(x <= 68.5)
  mean(x <= 70.5) - mean(x <= 69.5)
  
  # probabilities in normal approximation match well
  pnorm(68.5, mean(x), sd(x)) - pnorm(67.5, mean(x), sd(x))
  pnorm(69.5, mean(x), sd(x)) - pnorm(68.5, mean(x), sd(x))
  pnorm(70.5, mean(x), sd(x)) - pnorm(69.5, mean(x), sd(x))
  
  # probabilities in actual data over other ranges don't match normal approx as well
  mean(x <= 70.9) - mean(x <= 70.1)
  pnorm(70.9, mean(x), sd(x)) - pnorm(70.1, mean(x), sd(x))
  
#First, we generate a series of z-scores covering the typical range of the normal distribution. Since we know 99.7% of observations will be within  ???3???z???3 , we can use a value of  z  slightly larger than 3 and this will cover most likely values of the normal distribution. Then, we calculate  f(z) , which is dnorm() of the series of z-scores. Last, we plot  z  against  f(z) .
  
library(tidyverse)
x <- seq(-4, 4, length = 100)
data.frame(x, f = dnorm(x)) %>%
    ggplot(aes(x, f)) +
    geom_line()


## define x as male heights from dslabs data
library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)

# generate simulated height data using normal distribution - both datasets should have n observations
n <- length(x)
avg <- mean(x)
s <- sd(x)
simulated_heights <- rnorm(n, avg, s)

# plot distribution of simulated_heights
data.frame(simulated_heights = simulated_heights) %>%
  ggplot(aes(simulated_heights)) +
  geom_histogram(color="black", binwidth = 2)

##Code: Monte Carlo simulation of tallest person over 7 feet
B <- 10000
tallest <- replicate(B, {
  simulated_data <- rnorm(800, avg, s)    # generate 800 normally distributed random heights
  max(simulated_data)    # determine the tallest height
})
mean(tallest >= 7*12)    # proportion of times that tallest person exceeded 7 feet (84 inches)



####
set.seed(16)
act_scores <- rnorm(10000, 20.9, 5.7)
mean(act_scores)
sd(act_scores)
sum(act_scores >= 36)
mean(act_scores <= 10)

x<- seq(1,36,1)
f_x <- pnorm(x, 20.9, 5.7)
data.frame(x, f_x) %>% filter(x >= 0.95) %>% summarise(f_x)

z <- (act_scores - mean(act_scores))/ sd(act_scores)
mean(z>2)
dnorm(2,mean(z), sd(z))
2*sd(act_scores) + mean(act_scores)
qnorm(0.95, 20.9, 5.7)

p <- seq(0.01, 0.99, 0.01)
sample_quantiles <- quantile(act_scores,p)
sample_quantiles

theoretical_quantile <- qnorm(p, 20.9, 5.7)
plot(theoretical_quantile, sample_quantiles)


##Monte Carlo simulation: Chance of casino losing money on roulette
#We build a sampling model for the random variable  S  that represents the casino's total winnings. 

# sampling model 1: define urn, then sample
color <- rep(c("Black", "Red", "Green"), c(18, 18, 2)) # define the urn for the sampling model
n <- 1000
X <- sample(ifelse(color == "Red", -1, 1), n, replace = TRUE)
X[1:10]

# sampling model 2: define urn inside sample function by noting probabilities
x <- sample(c(-1, 1), n, replace = TRUE, prob = c(9/19, 10/19))    # 1000 independent draws
S <- sum(x)    # total winnings = sum of draws
S
#We use the sampling model to run a Monte Carlo simulation and use the results to estimate the probability of the casino losing money.

n <- 1000    # number of roulette players
B <- 10000    # number of Monte Carlo experiments
S <- replicate(B, {
    X <- sample(c(-1,1), n, replace = TRUE, prob = c(9/19, 10/19))    # simulate 1000 spins
    sum(X)    # determine total profit
})

mean(S < 0)    # probability of the casino losing money
We can plot a histogram of the observed values of S as well as the normal density curve based on the mean and standard deviation of S.

library(tidyverse)
s <- seq(min(S), max(S), length = 100)    # sequence of 100 values across range of S
normal_density <- data.frame(s = s, f = dnorm(s, mean(S), sd(S))) # generate normal density for S
data.frame (S = S) %>%    # make data frame of S for histogram
    ggplot(aes(S, ..density..)) +
    geom_histogram(color = "black", binwidth = 10) +
    ylab("Probability") +
    geom_line(data = normal_density, mapping = aes(s, f), color = "blue")


#Expected value of a random variable: 
  a*p + b*(1???p) 
#Expected value of the sum of n draws of a random variable: 
  n*(a*p + b*(1???p)) 
#Standard deviation of an urn with two values: 
  abs(b-a)*sqrt(p*(1-p))

#Standard error of the sum of n draws of a random variable:
  sqrt(n)*abs(b-a)*sqrt(p*(1-p))
  
#The expected value of the sum of  n  draws of a random variable is  n  times its original expected value:
  
  E[nX]=n?? 
#The standard error of the sum of  n  draws of random variable is  n?????????  times its original standard error:
    
    SE[nX]=sqrt(n)*standard eror
  
  # We defined standard error using this equation
  se <- 1/sqrt(n) * (17 - -1)*sqrt(p_green*p_not_green)
#or se<- (17 - -1)*sqrt(p_green*p_not_green)/sqrt(n)
  

####
p <- 1/5 # one correct choice of 5 options
a <- 1
b <- -0.25
mu <- 44*(a*p + b*(1-p))

se <-abs(-0.25-1)*sqrt(0.2*0.8)*sqrt(44)
1-pnorm(8, mu, se)

set.seed(21)
a<- replicate(10000, {
  funct<- sample(c(1, -0.25), 44, replace = TRUE, prob= c(0.2, 0.8))
  sum(funct)
})
  mean(a>=8)


  p <- seq(0.25, 0.95, 0.05)
  exp_val <- sapply(p, function(x){
    mu <- n * a*x + b*(1-x)
    sigma <- sqrt(n) * abs(b-a) * sqrt(x*(1-x))
    1-pnorm(35, mu, sigma)
  })
  
  min(p[which(exp_val > 0.8)])
set.seed(1)
b<- replicate(500,{
  funct<- sample(c(-1,6), 38, replace= TRUE, prob = c(1-p, p))
  
  })
mean(b)
mean(funct)*500

##What is the expected value of the average payout over 500 bets? = mean of 1 bet
((p*6) - (1-p))

#What is the standard error of the average payout over 500 bets?#
sqrt(p*(1-p))*abs(-1-6)/sqrt(500)

p<- 5/38
mu<- ((p*6) - (1-p))*500
se<- sqrt(p*(1-p))*abs(-1-6)*sqrt(500)
pnorm(0, mu,se)




####THE BIG SHORT
Code: Interest rate sampling model
n <- 1000
loss_per_foreclosure <- -200000
p <- 0.02
defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE)
sum(defaults * loss_per_foreclosure)
Code: Interest rate Monte Carlo simulation
B <- 10000
losses <- replicate(B, {
  defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE) 
  sum(defaults * loss_per_foreclosure)
})
Code: Plotting expected losses
library(tidyverse)
data.frame(losses_in_millions = losses/10^6) %>%
  ggplot(aes(losses_in_millions)) +
  geom_histogram(binwidth = 0.6, col = "black")

##Code: Expected value and standard error of the sum of 1,000 loans
n*(p*loss_per_foreclosure + (1-p)*0)    # expected value 
sqrt(n)*abs(loss_per_foreclosure)*sqrt(p*(1-p))    # standard error

##Code: Calculating interest rates for expected value of 0
We can calculate the amount  x  to add to each loan so that the expected value is 0 using the equation  lp+x(1???p)=0 . Note that this equation is the definition of expected value given a loss per foreclosure  l  with foreclosure probability  p  and profit  x  if there is no foreclosure (probability  1???p ).

We solve for  x=???lp1???p  and calculate  x :
  
  x = - loss_per_foreclosure*p/(1-p)
x
On a $180,000 loan, this equals an interest rate of:
  
  x/180000
Equations: Calculating interest rate for 1% probability of losing money
We want to calculate the value of  x  for which  Pr(S<0)=0.01 . The expected value  E[S]  of the sum of  n=1000  loans given our definitions of  x ,  l  and  p  is:
  
  ??S=(lp+x(1???p))???n 
And the standard error of the sum of  n  loans,  SE[S] , is:
  
  ??S=???x???l???np(1???p)??????????????????????????? 
Because we know the definition of a Z-score is  Z=x??????? , we know that  Pr(S<0)=Pr(Z<???????) . Thus,  Pr(S<0)=0.01  equals:
  
  Pr(Z<???{lp+x(1???p)}n(x???l)np(1???p)???????????????????????????)=0.01 
z<-qnorm(0.01) gives us the value of  z  for which  Pr(Z???z)=0.01 , meaning:
  
  z=???{lp+x(1???p)}n(x???l)np(1???p)??????????????????????????? 
Solving for  x  gives:
  
  x=???lnp???znp(1???p)???????????????????????????n(1???p)+znp(1???p)??????????????????????????? 
Code: Calculating interest rate for 1% probability of losing money
l <- loss_per_foreclosure
z <- qnorm(0.01)
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))
x/180000    # interest rate
loss_per_foreclosure*p + x*(1-p)    # expected value of the profit per loan
n*(loss_per_foreclosure*p + x*(1-p)) # expected value of the profit over n loans

#Code: Monte Carlo simulation for 1% probability of losing money
#Note that your results will vary from the video because the seed is not set.

B <- 100000
profit <- replicate(B, {
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-p, p), replace = TRUE) 
  sum(draws)
})
mean(profit)    # expected value of the profit over n loans
mean(profit<0)    # probability of losing money

##https://courses.edx.org/courses/course-v1:HarvardX+PH125.3x+1T2020/courseware/261991f34f234bdb91acfbc3842e6a11/b724e248134849859d3941a9982d6630/?child=first

Code: Calculating number of loans for desired probability of losing money
The number of loans required is:
  
  z <- qnorm(0.01)
l <- loss_per_foreclosure
n <- ceiling((z^2*(x-l)^2*p*(1-p))/(l*p + x*(1-p))^2)
n    # number of loans required
n*(loss_per_foreclosure*p + x * (1-p))    # expected profit over n loans
Code: Monte Carlo simulation with known default probability
This Monte Carlo simulation estimates the expected profit given a known probability of default  p=0.04 . Note that your results will differ from the video because the seed is not set.

B <- 10000
p <- 0.04
x <- 0.05 * 180000
profit <- replicate(B, {
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-p, p), replace = TRUE) 
  sum(draws)
})
mean(profit)
Code: Monte Carlo simulation with unknown default probability
This Monte Carlo simulation estimates the expected profit given an unknown probability of default  0.03???p???0.05 , modeling the situation where an event changes the probability of default for all borrowers simultaneously. Note that your results will differ from the video because the seed is not set.

p <- 0.04
x <- 0.05*180000
profit <- replicate(B, {
  new_p <- 0.04 + sample(seq(-0.01, 0.01, length = 100), 1)
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-new_p, new_p), replace = TRUE)
  sum(draws)
})
mean(profit)    # expected profit
mean(profit < 0)    # probability of losing money
mean(profit < -10000000)    # probability of losing over $10 million

#https://courses.edx.org/courses/course-v1:HarvardX+PH125.3x+1T2020/courseware/261991f34f234bdb91acfbc3842e6a11/b724e248134849859d3941a9982d6630/?child=first



#######INSURANCE RATES
install.packages("dslabs")
library(dslabs)
head(death_prob)
df <- death_prob
install.packages("tidyverse")
library(tidyverse)

p <- df %>% filter(age == "50" & sex =="Female") %>% pull(prob)
loss <- -150000
gain <- 1150
mu <- (p*loss + (1-p)*gain)*1000
se <- abs(loss-gain)*sqrt(p*(1-p))*sqrt(1000)
n<- 1000
profit <- sample(c(loss,gain), n, prob= c(p,1-p), replace= TRUE)
mean(profit<0)
sum(profit)
#Use the Central Limit Theorem to calculate the probability that the insurance company loses money on this set of 1,000 policies.
pnorm(0, mu, se)


p <- df %>% filter(age == "50" & sex =="Male") %>% pull(prob)
loss <- -150000
gain <- (700000/1000 -(-150000*p))/(1-p)
se <- abs(loss-gain)*sqrt(p*(1-p))*sqrt(1000)
mu <- (p*loss + (1-p)*gain)*1000
pnorm(0,mu,se)

p<- 0.015
gain <- 1150
loss <- -150000
n<- 1000
mu <- (gain*(1-p)+loss*p)*n
se <- abs(loss-gain)*sqrt(p*(1-p))*sqrt(n)
pnorm(0,mu, se)
pnorm(-1000000,mu, se)

p <- seq(.01, .03, .001)
exp_val <- sapply(p, function(x){
  mu <- n * (loss*x + gain*(1-x))
  sigma <- sqrt(n) * abs(gain-loss) * sqrt(x*(1-x))
  pnorm(0, mu, sigma)
})
min(p[which(exp_val > 0.9)])

p <- seq(.01, .03, .0025)
exp_val <- sapply(p, function(x){
  mu <- n * (loss*x + gain*(1-x))
  sigma <- sqrt(n) * abs(gain-loss) * sqrt(x*(1-x))
  pnorm(-1000000, mu, sigma)
})
min(p[which(exp_val > 0.9)])


n<- 1000
p_loss <- 0.015
loss <- -150000
gain <- 1150
set.seed(25)
profit<- sample(c(loss, gain), n, prob = c(p_loss, 1-p_loss), replace= TRUE)
sum(profit)/10^6
profit

set.seed(27)
B<- 10000
profit <- replicate(B, {
  draws <- sample(c(loss,gain), n, prob = c(p_loss, 1-p_loss), replace = TRUE)
  sum(draws)
})
mean(profit <= -1000000)

n<-1000
p <- 0.015
l <- -150000
z <- qnorm(0.05)
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))
l*p + x*(1-p)

set.seed(28)
B<-10000
profit <- replicate(B, {
  draws <- sample( c(x, l), n, 
                   prob=c(1-p, p), replace = TRUE) 
  sum(draws)
})
mean(profit<0)


set.seed(29)
profit <- replicate(B, {
  new_p <- p+ sample(seq(-0.01, 0.01, length = 100), 1)
  draws <- sample( c(x, l), n, 
                   prob=c(1-new_p, new_p), replace = TRUE)
  sum(draws)
})
mean(profit)    # expected profit
mean(profit <= -1000000)
mean(profit < 0)
