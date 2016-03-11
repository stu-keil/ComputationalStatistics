###Sumulate the game of throwing sixes with five dice
library(ggplo2)
set.seed(110143)
simulate.game <- function(...){ # 1) Simulate one run of the game
  continue <- TRUE # Dummy to know if we must throw again
  win <- TRUE # Dummmy that will change to false if one throws no 6 at any point
  dice.left <- 6 # Dice to throw at next round
  while(continue){
    throw <- sample.int(6, dice.left, replace=TRUE)
    hits <- sum(throw == 6)
    if(hits==0){
      win <- FALSE
      continue <- FALSE
    } else{
      dice.left <- dice.left - hits
      if(dice.left==0) continue <- FALSE
    }
  }
  return(win)
}
compute.prob <- function(nsim){ # 2) Run several times and average
  results <- sapply(1:nsim, FUN=simulate.game)
  return(sum(results)/nsim)
}
prob <- sapply(seq(10,10000,100),compute.prob)
prob
qplot(1:length(prob),prob) + geom_smooth()
