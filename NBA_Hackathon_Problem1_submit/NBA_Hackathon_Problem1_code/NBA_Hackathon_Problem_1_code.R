# Problem 1
# (a)
# Approach 1: Exact answer
no_closs <- function(win){
    ans <- choose((win + 1), (82 - win) ) * 0.8^win * 0.2^(82 - win)
    return(ans) }
### Probability of no 2 consecutive losses
x <- sum(sapply(41:82, no_closs))
x

# Approach 2: Simulation
season_simulation <- function(n = 1000){
    # simulate 900 82-game seasons
    sample <- 900     
    # create a container for storing experiment outcomes
    container <- matrix(NA, ncol = sample, nrow = n) 
    
    for(j in 1:n){
        for(i in 1:sample){
            # set-up
            sample.space <- c(0,1) # win -- 1; loss -- 0
            theta <- 0.8 # Warriors have 80% chance of winning each game
            N <- 82 # 82 games
            # Similar to biased coin flips
            flips <- rbinom(n = N, size = 1, prob = theta)
            # next check for consecutive losses (consecutive 0s)
            r0 <- rle(flips) # list running lengths 
            x <- r0$length[r0$values == 0] # count running length of 0s
            boo <- sum(x != 1) == 0 # consecutive occurrence of 0 or not; 
            # FALSE : there are consecutive loss in a simulated 82-game season
            # TRUE : there are no consecutive loss in a simulated 82-game season
            container[j,i] <- boo
        }
    }
    
    return(container)
}
    
# (a)
set.seed(123456789) # set starting point
sim <- season_simulation(n = 1000)
y <- rowSums(sim)/ncol(sim) # Prob(no consecutive losses)
p <- mean(y) 
se <- sd(y)/sqrt(900)
p
se
    
hist(y,
     main = "Histogram of the simulated probabilities", 
     xlab = "Prob(Warriors does not lose consecutive games 
         at any point in an game season)")

#(b)    
# 95% confidence interval
upp <- p + 1.96 * se
low <- p - 1.96 * se
upp_per <- upp* 100
low_per <- low* 100
    
# (c)
# At least game win percentage
least_per <- function(percentage){
    ans <- 0
    for(i in 41 :82){
        ans <- ans + choose((i + 1), (82 - i) ) * percentage^i * 
            (1-percentage)^(82-i)
    }
    ans - 0.5
}
z <- uniroot(least_per,c(0,1))
z <- z$root
z_per <- z*100
### At least winning percentage 
### 0.903776165, this is tested by a lot of numbers to get different answer from
### the uniroot function