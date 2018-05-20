#Monte Carlo Approach: invoking large numbers to approximate expected value via simulation
# --> Simulating in R is inexpensive and easy
set.seed(12344)

#Birthday Problem
#p(n) = 365*364*...*(365-n+1)/365^n
1-(prod(365:343)/365^23)

#Simulation:
#(1) Pick 23 random #'s from 1 to 365 where each number represents a birthday in the given year
#(2) Check to see if any are equal -- any 2 people have the same birthday
#(3) Repeat 10000 times
#(4) Find fraction of trials that have matching birthdays

#Number of trials
indexes <- 1:10000
#Number of people per trial
n <- 23

sim <- unlist(lapply(indexes, function(i){
  sim.i <- anyDuplicated(sample(1:365, n, TRUE))
}))
sum(sim > 0)/length(sim)
#51% probability that 2 people have the same birthday in a class of 23 people 

#To understand the distribution or set of outcomes -- based on simulating a number of paths through a process

#e.g. Tossing a coin multiple times
#Using a random uniform distribution and transformed into real output set {-1,1}
#Can use a sample function instead

#Random Uniform Distribution 
r <- runif(1000)
#If r > .5, then head = 1, else tails = -1
toss <- ifelse(r > 0.5, 1, -1)
plot(cumsum(toss), type = 'l')
outcomes <- sapply(1:1000, function(i) sum(ifelse(runif(1000)>.5,1,-1)))
hist(outcomes)

#Bernoulli random variables
# A Bernoulli trial is an experiment in which there are only 2 possible outcomes
# A student who guesses on a multiple choice test question which has 5 options; 
# the student may guess correctly with probability = 0.2 and incorrectly with 0.8
# Suppose we wanted to know how well 150 students would do on a multiple choice test with 20Q's
# Each question corresponds to an independent Bernoulli trial
# We can simulate the correctness of the student for each question by generating an indep. unirom random number
# If this number < 0.2 --> Correct; Otherwise --> Incorrect
# Probability that a uniform random variable is < 0.2 is exactly 0.2, which is the same as prob of guessing correctly

indexes <- 1:150
class <- unlist(lapply(indexes, function(i){
  guess <- runif(20)
  correct.answers <- sum(guess < 0.2)/20
}))
hist(class)
mean(class)
sd(class)


#E.g.2 Suppose you want to simulate a random sample from a distribution with density fx(x) = 3x^2, 0<x<1
#Fx(x) = x^3 for 0<x<1 and F^-1(u) = u^1/3. If we generate n random uniform numbers as vectors u, 
#then u^(1/3) is a vector of length n containing the sample x1...xn

n = 1000
u <- runif(n)
head(u)
x <- u^(1/3)
head(x)
hist(x, prob = TRUE, main = expression(f(x) == 3*x^2))
y <- seq(0,1,.01)
lines(y, 3*y^2) #density curve

#e.g.3 estimates
#Samples
population <- c(rep(1,45), rep(0,55))
samp <- sample(population, 1500, replace = TRUE)
est_p <- sum(samp)/length(samp)

#Repeat many times to see long-run behavior
population <- c(rep(1,45), rep(0,55))
samples <- c()
for(i in 1:1000){
  samp = sample(population, 1500, replace=TRUE)
  est_p = sum(samp)/length(samp)
  samples <- c(samples, est_p)
}
hist(samples)
mean(samples)
sd(samples)
summary(samples)

#E.g.4 Estimate of slope
model.y <- function(x, intercept, slope, sigma){
  y <- intercept + slope*x + rnorm(length(x), 0, sigma)
}
x <- -10:10
y <- model.y(x, 10, -12, 3)
plot(x,y)

#Repeat N times to see long-run behavior
simulation <- function(N, x, intercept, slope, sigma){
  slopes = c()
  for(i in 1:N){
    y_values <- model.y(x, intercept, slope, sigma)
    fit <- lm(y_values~x)
    slopes <- c(slopes, fit$coefficients[2])
  }
  slopes
}
run_sim <- simulation(1000, x, 10, -12, 3)
hist(run_sim)
mean(run_sim)
sd(run_sim)

#Estimates of slope are ~N(-12, 3/sqrt(SXX))
SXX <- sum((x-mean(x))^2)
SE_slope <- 3/sqrt(SXX)
