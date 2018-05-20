#Adjust parmameters below
m_1 <- 3.47
m_2 <- 1.28
n_1 = 1400
n_2 = 1400
s_traffic <- 1

#Adjust width of plot, higer values to increase width 
window <- 5

#Simulate Plots 
d <- m_1 - m_2
d_traffic <- 1.96*(sqrt(1/n_1 + 1/n_2)*s_traffic)
n_p <- (n_1 + n_2)/2
set.seed(100)
x = rnorm(n_p, mean = 0, sd= s_traffic)
dens <- density(x)
plot(dens, col=rgb(0,0,1,1), xlim = c(min((-d)-window,(-d_traffic)-window),max(d+window,d_traffic+window)), main="", lwd=3, xlab="")
title(main = "Distribution of Difference Between Control and Treatment", xlab="Difference")
x1max <- max(which(dens$x >= quantile(x, .975)))  
x1 <- min(which(dens$x >= quantile(x, .975)))  
x2 <- max(which(dens$x <=  quantile(x, .025)))
with(dens, polygon(x=c(x[c(0,0:x2,x2)]), y= c(y[0:x2],0), col="blue"))
with(dens, polygon(x=c(x[c(x1,x1:x1max)]), y= c(y[x1max], y[x1:x1max]), col="blue"))
temp <- mapply(function(x,y,c) lines(c(x,x), c(0,y), lwd=3, col=c), c(d,-d), c(dens$y[length(dens$x[dens$x < d])], dens$y[length(dens$x[dens$x < -d])] ), c("Red","Red"))
