# (d)
set.seed(1234)
no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)
registerDoParallel(cl)
# reference: http://garthtarr.com/parallel-computation-in-r/

N = 100*100
ptm <- proc.time()
winn <- foreach(i = 1:N,
                .combine = "rbind")  %dopar%  {
                    df <- game_simulation()
                    c(t(df$winners[[1]]), # row vector of 8
                      t(df$winners[[2]]), # row vector of 4
                      t(df$winners[[3]]), # row vector of 2 
                      sum(df$revenues)) # numeric vector of 1
                }
proc.time() - ptm
stopCluster(cl)
head(winn)

# turn big matrix into a list of length 100, each with a matrix of 15* 1000
lst <- vector("list", length = 100)
sq <- seq(from = 1, to = N+100, by = 100)
i = 1
while(i <= 100){
    lst[[i]] <- winn[sq[i]:(sq[i+1]-1),]
    i = i + 1
}


# if west1 meets east1 in final
# probability("east1-west1")
p <- sapply(lst, function(e) sum(e[,13] == "East1" & e[,14] == "West1")/ nrow(e))
mean(p)
sd(p)
# expected total revenue if west1 meets east 1 in final (law of large numbers)
t <- sapply(lst, 
            function(e) mean(as.numeric(e[e[,13] == "East1" & e[,14] == "West1",15])))
mean(t)
sd(t)

# if west1 does not meet east1 in final
# probability("east1 not against west1")
p <- sapply(lst, function(e) sum(!(e[,13] == "East1" & e[,14] == "West1"))/ nrow(e))
mean(p)
sd(p)
# expected total revenue if west1 does not meet east 1 in final
t <- sapply(lst,
            function(e) mean(as.numeric(e[!(e[,13] == "East1" & e[,14] == "West1"),15])))
mean(t)
sd(t)


# Conditional on the fact that east5 defeats east4 in round1
# if west1 meets east1 in final given east5 defeats east4
# probability("east1-west1"| east5 defeats east4)
# # probability("east1-west1" & east5 defeats east4)
p <- sapply(lst, function(e) sum(e[,2] == "East5" 
                                 & (e[,13] == "East1" 
                                    & e[,14] == "West1"))/nrow(e))
mean(p)
sd(p)
# expected total revenue
t <- sapply(lst, function(e) mean(as.numeric(e[(e[,2] == "East5" & 
                                                    (e[,13] == "East1" & 
                                                         e[,14] == "West1")),15])))
mean(t)
sd(t)

# if west1 does not meets east1 in final given east5 defeats east 4
# probability("east1 not against west1"| east5 defeats east4)
# # probability("east1 not against west1" & east5 defeats east4)
p <- sapply(lst, function(e) sum(e[,2] == "East5" 
                                 & !((e[,13] == "East1" 
                                      & e[,14] == "West1")))/nrow(e))
mean(p)
sd(p)
# expected total revenue 
t <- sapply(lst, function(e) mean(as.numeric(e[(e[,2] == "East5" & 
                                                    !((e[,13] == "East1" & 
                                                           e[,14] == "West1"))),15])))
mean(t)
sd(t)
