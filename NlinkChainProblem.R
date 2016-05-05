# You have a chain with links numbered through . Every minute, you draw a random link from a bag, and connect it to any other consecutively­numbered link that you drew before. For example, if you drew , you would end up with three subchains: . You keep on drawing until you have drawn all links and connected them into a single chain of length . Let be the maximum number of subchains in this process.

# What is the mean and standard deviation of the distribution of if = 8 or 16 or 32 

# TO display results till 10 decimal points
options(digits=10)
# function that returns # maximum number of subchains
MaximumNumberOfSubchains <- function(N){
  if (N==1) {return(1)}
  else{
    links <- sample(N) # draw N random links from a bag
    chains <- list(links[1]) # list which will contain all the subchains
    
    for(l in 2:length(links))
    {
      added <- FALSE
      chainLength <- length(chains)
      for(c in 1:chainLength)
      {
        # find the biggest number in this subchain
        maxLink = max(chains[[c]])
        # find the smallest number in this subchain
        minLink = min(chains[[c]])
        # if the new link is consequtive to biggest or smallest number, add it to this subchain
        if(abs(maxLink - links[l])==1 | abs(minLink - links[l])==1) 
        {
          chains[[c]] <- c(chains[[c]], links[l])
          added <- TRUE
          break
        }
      }
      
      # create a new sub chain
      if(added==FALSE)
      {
        chains[[chainLength+1]] <- links[l]
      }
    }
    # maximum number of subchains
    M <- length(chains)
    return(M)  
  }  
}

# set of N values given in question 
N_values <- c(8,16,32) 
# for reproducible results, set seed
set.seed(1)
# repeat this experiment 100,000 times and calculate the mean and standard deviation of the distribution
sapply(N_values,function(i) {
  monte_carlo <- replicate(100000,MaximumNumberOfSubchains(i))
  # plot a histogram
  hist(monte_carlo)
  return(c(mean(monte_carlo),sd(monte_carlo)))
})
