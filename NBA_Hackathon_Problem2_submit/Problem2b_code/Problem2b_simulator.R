# Problem 2
# (b)
list.of.packages <- c("ggplot2", 
                      "gridExtra", 
                      "readxl", 
                      "stringr", 
                      "foreach", 
                      "doParallel")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load required packages
library(ggplot2)
library(gridExtra)
library(readxl)
library(stringr)
library(foreach)
library(doParallel)

# load data gate revenues function
load_rev <- function(){
    
    gate_rev <- read_excel ("Business-Track-Application-Datasets.xlsx", sheet = 1)
    gate_rev <- as.data.frame(gate_rev)
    gate_rev[,3] <- round(gate_rev[,3])
    gate_names <- gate_rev[,1]
    gate_rev <- gate_rev[,-1]
    gate_rev <- gate_rev/(10^6)
    row.names(gate_rev) <- gate_names
    return(gate_rev)
}
# gate_rev <- load_rev()

# load data win probabilities function
load_prob <- function(){
    win_probs <- read_excel("win_probabilities.xlsx", sheet = 1)
    win_probs <- as.data.frame(win_probs)
    return(win_probs)
}
# win_probs <- load_prob()

# setup vectors function
load_setups <- function(){
    # game format 2-2-1-1-1
    gameformat <- c("H", "H", "A", "A", "H", "A", "H")
    # round1 fixed pairs
    eastround1 <- matrix(paste0(rep("East", 8), c(1,8,4,5,3,6,2,7)), ncol= 2, byrow = T)
    westround1 <- matrix(paste0(rep("West", 8), c(1,8,4,5,3,6,2,7)), ncol=2, byrow = T)
    round1pairs <- rbind(eastround1, westround1)
    
    return(list(gameformat = gameformat, round1pairs = round1pairs))
}
# gameformat <- load_setups()$gameformat
# round1pairs <- load_setups()$round1pairs

# a helper function to extract win probabilities 
HAgameoutcome <- function(game, homewinprob, awaywinprob){
    if(game == "H"){
        rbinom(n = 1, size = 1, prob = homewinprob)
    }else{
        rbinom(n = 1, size = 1, prob = awaywinprob)
    }
}

# best four of seven round simulator function
bestfourofseven <- function(round, roundpairs){
    # winner tracking container 
    roundwinners <- matrix(NA, nrow = nrow(roundpairs))
    # revenue tracking container
    rev_container <- matrix(NA, nrow = 16)
    row.names(rev_container) <- row.names(gate_rev)
    
    for(pair in 1:nrow(roundpairs)){ # eg. 8 pairs of teams --> 8 winners
        # teamA win count
        i = 0  
        # teamB win count
        j = 0 
        # counter for tracking home/away
        c <- 0 
        # round1 pair win prob information
        p <- win_probs[(win_probs$Team1 %in% roundpairs[pair, 1]|win_probs$Team2 %in% roundpairs[pair, 1]) &
                           (win_probs$Team2 %in% roundpairs[pair, 2]|win_probs$Team1 %in% roundpairs[pair, 2]),]
        while(i < 4 & j < 4){
            gf <- gameformat[c+1] # Home game or Away game
            out <- HAgameoutcome(gf, p$Prob1WinsHome, p$Prob1WinsAway) # outcome
            # print(out)
            ifelse(out == 1, i <- i+1, i <- i) # teamA wins
            ifelse(out == 0, j <- j+1, j <- j) # teamB wins
            c <- c + 1  # counter for tracking home/away
        }
        # track winner
        if(i == 4){
            roundwinners[pair] <- p[,1] # teamA wins round1
        }else{
            roundwinners[pair] <- p[,2] # teamB wins round1
        }
        # track revenue gain in round 1
        # 1 extract team per-game per round gate revenue
        tmA <- row.names(gate_rev) == p[,1]
        tmB <- row.names(gate_rev) == p[,2]
        # 2 extract N-series
        gm <- gameformat[1:c]   # N-series
        homes <- sum(gm == "H") # number of home games
        aways <- sum(gm == "A") # number of away games
        # 3 determine who gets home court
        tmA_rk <- stringr::str_extract(roundpairs[pair,1], "[1-8]")
        tmB_rk <- stringr::str_extract(roundpairs[pair,2], "[1-8]")
        
        if(tmA_rk == tmB_rk){
            rev_container[tmA, ] <- aways* (gate_rev[tmA, round]) # teamA revenue
            rev_container[tmB, ] <- homes* (gate_rev[tmB, round]) # teamB revenue
        }else{
            rev_container[tmA, ] <- homes* (gate_rev[tmA, round]) # teamA revenue
            rev_container[tmB, ] <- aways* (gate_rev[tmB, round]) # teamB revenue
        }
    }
    
    return(list(winners = roundwinners, revenues = rev_container))
}
# bestfourofseven(round = 1, roundpairs = round1pairs)

# one posssible playoff outcome simulation function
game_simulation <- function(){
    # round1 
    round1 <- bestfourofseven(roundpairs = round1pairs, round = 1)
    # round2
    round2pairs <- matrix(round1$winners, ncol = 2, byrow = T)
    round2 <- bestfourofseven(roundpairs = round2pairs, round = 2)
    # round3
    round3pairs <- matrix(round2$winners, ncol = 2, byrow = T)
    round3 <- bestfourofseven(roundpairs = round3pairs, round = 3)
    # round4 
    round4pairs <- matrix(round3$winners, ncol = 2, byrow = T)
    round4 <- bestfourofseven(roundpairs = round4pairs, round = 4)
    
    # winners 
    winners <- list(first_round_winner = round1$winners, 
                    second_round_winner = round2$winners, 
                    third_round_winner = round3$winners, 
                    fourth_round_winner = round4$winners)
    # revenues 
    revenues <- cbind(cbind(round1$revenues,round2$revenues),  
                      cbind(round3$revenues,round4$revenues))
    colnames(revenues) <- c("first_round", "second_round", 
                            "third_round", "fourth_round")
    revenues[is.na(revenues)] <- 0
    
    # Calculate revenue for each round
    each_round_revenue <- apply(revenues, 2, sum, na.rm = T)
    total_revenues <- rowSums(revenues, na.rm = T)
    
    # return(revenues)
    return(list(revenues = revenues, 
                each_round_revenue = each_round_revenue , 
                winners = winners, 
                total_revenues = total_revenues))
}
# game_simulation()

# run simulation
# set.seed(1234)
# load win probabilities
win_probs <- load_prob()
# load hypothetical gate revenues
gate_rev <- load_rev()
# load game format 2-2-1-1-1
gameformat <- load_setups()$gameformat
# load round1 brackets
round1pairs <- load_setups()$round1pairs
# game_simulation()

