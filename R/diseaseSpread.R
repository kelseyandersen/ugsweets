

#' A disease spread function
#'
#' This function allows you to simulate disease spread through a network.
#' @param mat adjacency matrix of 0 and 1s representing potential links which will transmit infection from an infected to an uninfected node at probability described in p
#' @param startnode the node where infection will start
#' @param nodenum total number of nodes in the network
#' @param p probability of infection
#' @param timesteps number of timesteps per simulation
#' @param sims number of simulations
#' @keywords network spread
#' @examples
#' diseasespread()
#' @importFrom dplyr group_by summarise as_tibble
#' @export


diseaseSpread <- function(mat, startnode, p, timesteps, sims){
  nodnum <- nrow(mat)
  nn <- nodenum*nodenum
  mat <-  as.matrix(mat)
  diag(mat) <-1
  vec <- rep(0, nodenum)
  vec[c(startnode)] <- 1
  NodesNew <- rep(1:126,timesteps*sims)
  Vec <- matrix(rep(vec,12),12,byrow = T)
  Status <- NULL
  Step <- NULL
  Sim <- NULL

  for(j in 1:sims){
    Sim <- c(Sim,rep(j,nodenum*timesteps))

    for(i in 1:timesteps){
      MatrixRun <- matrix(runif(nn)< p, ncol =length(vec), nrow = length(vec))
      MatrixRun <- MatrixRun*1
      MatrixRun <- MatrixRun*mat
      diag(MatrixRun) <- 1
      Vec[i+1,] <- Vec[i,] %*% MatrixRun
      Vec[i+1,] <- as.numeric(Vec[i+1,] > 0)
      Status <- c(Status ,Vec[i+1,])
      Step <- c(Step, rep(i, 126))
    }
  }
  temp <- cbind(NodesNew,Sim, Step, Status)
  temp <- as_tibble(temp)
  out <- temp %>%
    group_by(Sim, Step) %>%
    summarise(Number_of_Nodes = sum(Status))
  return(out)
}

