


reset <- function () {
counter <- 0
}

----------------------------

counter <- 0
vector <- c(0,1,2,3,4,5,6,0,0,4,5,6,0)


for (i in vector) {

  if ("0" %in% i == "TRUE") {
  counter = counter + 1
  }

  return(counter)
}

counter


# Question 2

vector <- c(0,1,2,3,4,5,6,0,0,4,5,6,0)

countzeroes <- function(vector){
  return(sum(vector==0))
}

countzeroes(vector)


# Question 3


getdata <- function(row,column) {
  results <- matrix(nrow=row,ncol=column)

  for (i in 1:row) {
    for (j in 1:column) {
      results[i,j] <- i*j
    }
  }
  return(results)
}

row <- 3
column <- 4
data <- getdata(row,column)
matrix <- matrix(data=data,nrow=row,ncol=column)
matrix










# Question 4

n <- 4
means <- c(10,20,30)
group <- rep(1:3, each=n)
response <- c(rnorm(n,mean=means[1]),
              rnorm(n,mean=means[2]),
              rnorm(n,mean=means[3]))

data <- data.frame(group,response)
data


# Question 4b

# reshuffle + get means!
reshuffle <- function(data) {

  for (i in data) {
    shuffleresponse <- sample(data[,2])
    shuffleresponse

    # new data frame using the shuffled responses
    data2 <- data.frame(group,shuffleresponse)
    data2

    # get means of each group
    group1mean <- mean(data2[1:n,2])
    group2mean <- mean(data2[n:(2*n),2])
    group3mean <- mean(data2[(2*n):(3*n),2])
  }
newmeans <- data.frame(group1mean,group2mean,group3mean)
return(newmeans)
}

# test that it works
reshuffle(data)


# Question 4c

library(tidyverse)

runagain <- function(data) {

  # make an empty data frame
  results <- data.frame(matrix(nrow=n,ncol=4))
  colnames(results) <- c("replicate","group1mean","group2mean","group3mean")

  # running it n times!
  for (i in 1:n) {
    newmeans <- reshuffle(data)
    results[i,1:4] <- c(i,newmeans)
  }

  # gimme my results
  return(results)
}

n=100
runagain(data)




