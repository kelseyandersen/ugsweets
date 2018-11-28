

#' A disease spread function
#'
#' This function allows you to simulate disease spread through a network that combines two types of data- observed transaction data and modeled spread data. Spread modeled using the inverse power law model.
#' @param transmat your transaction matrix which is a full network matrix of 1 and 0s
#' @param d matrix of geographic distances between all nodes, scale does not matter
#' @param startnode single node number that introduces the pathogen into the network
#' @param p probability of infection on a link in each timestep
#' @param Beta spread parameter beta that will be used in the inverse power law model to model link formation between locations- higher number means fewer links
#' @keywords network spread
#' @examples
#' DiseaseSpreadPWR()
#' @importFrom dplyr group_by summarise as_tibble
#' @export


DiseaseSpreadPWR <- function(transmat, d, startnode, p, Beta){

  nodenum <- nrow(transmat)
  pwrD <- d^(-Beta)
  diag(pwrD) <- 1
  nn <- 124*124
  k <- as.matrix(transmat)
  diag(k) <-1
  vec <- rep(0, 124)
  vec[c(startnode)] <- 1
  NodesNew <- rep(1:124,20*100) # make a dataframe large enough for 20 times steps, 100 simulations
  Vec <- matrix(rep(vec,22),22,byrow = T) #vector of infection states after each time step
  Status <- NULL
  Step <- NULL
  Sim <- NULL

  for(j in 1:100){ ## number of simulations
    Sim <- c(Sim,rep(j,nodenum*20)) ## number of time steps

    for(i in 1:20){
      nn1 <- 97*97 ## 99 villages total
      nn2 <- matrix(runif(nn1), ncol =97, nrow = 97)
      nn3 <- pwrD > nn2 ## compare kernal to uniform dist.
      nn4 <- nn3*1
      k[28:124, 28:124] <- nn4[1:97, 1:97]
      MatrixRun <- matrix(runif(nn)< p, ncol =length(vec), nrow = length(vec))
      MatrixRun <- MatrixRun*1
      MatrixRun <- MatrixRun*pwr3si
      diag(MatrixRun) <- 1
      Vec[i+1,] <- Vec[i,] %*% MatrixRun
      Vec[i+1,] <- as.numeric(Vec[i+1,] > 0)
      Status <- c(Status ,Vec[i+1,])
      Step <- c(Step, rep(i, 124))
    }
  }
  temp <- cbind(NodesNew,Sim, Step, Status)
  temp <- dplyr::as_tibble(temp)
  out <- temp %>%
    group_by(Sim, Step) %>%
    summarise(Number_of_Nodes = sum(Status))
  return(out)
}
