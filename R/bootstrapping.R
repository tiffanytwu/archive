#Bootstrapping (Resampling)
#A resampling method that provides a measure of confidence on a statistic
#Q: What would I get if I generated my sample again?
#Given that the population distribution is unknown (can't take another sample), 
#use the sample distribution to construct a model of the population
#Therefore, it produces inference by resampling the data / a function of the data

##################### UDFs ####################

#x is a single quantitative sample
#B is the desired number of bootstrap samples to take
#binwidth is passed on the geom_histogram()

boot.mean <- function(x, B, binwidth=NULL){
    n <- length(x)
    boot.samples <- matrix(sample(x, size=n*B, replace=TRUE), B, n)
    boot.statistics <- apply(boot.samples, 1, mean)
    se = sd(boot.statistics)
    
    require(ggplot2)
    if (is.null(binwidth)){
        binwidth <- diff(range(boot.statistics))/30
    }
    p <- ggplot(data.frame(x=boot.statistics), aes(x=x)) +
        geom_histogram(aes(y=..density..), binwidth = binwidth) +
        geom_density(color='red')
    plot(p)
    
    interval <- mean(x) + c(-1, 1)*2*se
    print(interval)
    return(list(boot.statistics = boot.statistics,
               interval=interval,
               se=se,
               plot=p))
}

#95% CI for proportions
data <- c(rep(1, 11), rep(0, 19))
data.boot <- boot.mean(data, 1000, binwidth = 1/30)
