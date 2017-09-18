# (c)
source("Problem2b_simulator.R")
set.seed(1234)
# dat <- game_simulation() 

# total revenue distributions
# parallel computing
# library(foreach)
# library(parallel)
# library(doParallel)
# Calculate the number of cores
no_cores <- detectCores() - 1
# Initiate cluster
cl <- makeCluster(no_cores)
registerDoParallel(cl)
# parallel computing reference: http://garthtarr.com/parallel-computation-in-r/
N = 10000
revv <- foreach(i = 1:N, 
                .combine = 'rbind')  %dopar%  {
                    df <- game_simulation()
                    c(total = sum(df$total_revenues), df$each_round_revenue)
                }
stopCluster(cl)

revv <- as.data.frame(revv)

summary <- function(x){
    funs <- c(mean, sd)
    sapply(funs,function(f) f(x, na.rm = TRUE))
}

# Create the mean and the standard deviation for each round and total
summary_revenue <- sapply(revv, summary)
rownames(summary_revenue) <- c("mean", "sd")

# Preparing for using ggplot
mapping <- list(total = aes(x = total),
                first = aes(x = first_round), 
                second = aes(x = second_round), 
                third = aes(x = third_round), 
                fourth = aes(x = fourth_round))
ggplot_revenue <- lapply(mapping, ggplot, data = revv)

barfill <- "#4271AE"
barlines <- "#1F3552"
ggplot_total <- ggplot_revenue[[1]] + 
    geom_histogram(binwidth = 5, colour = barlines, fill = barfill) +
    xlab("Total Gate Revenue($MM)")
ggplot_first <- ggplot_revenue[[2]] + 
    geom_histogram(binwidth = 5, colour = barlines, fill = barfill) +
    xlab("First Round Gate Revenue($MM)")
ggplot_second <- ggplot_revenue[[3]] + 
    geom_histogram(binwidth = 5, colour = barlines, fill = barfill) + 
    xlab("Second Round Gate Revenue($MM)")
ggplot_third <- ggplot_revenue[[4]] + 
    geom_histogram(binwidth = 5, colour = barlines, fill = barfill) +
    xlab("Third Round Gate Revenue($MM)")
ggplot_fourth <- ggplot_revenue[[5]] + 
    geom_histogram(binwidth = 5, colour = barlines, fill = barfill) +
    xlab("Fourth Round Gate Revenue($MM)")

# plots produced
ggplot_total
grid.arrange(ggplot_first, ggplot_second, ggplot_third, ggplot_fourth)
