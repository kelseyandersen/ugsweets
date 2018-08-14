

#' A disease spread function
#'
#' This function allows you to simulate disease spread through a network.
#' @param mat adjacency matrix
#' @param startnode the node where infection will start
#' @param nodenum total number of nodes in the network
#' @param p probability of infection
#' @keywords network spread
#' @examples
#' diseasespread()
#' @importFrom dplyr group_by summarise
#' @importFrom tibble as_tibble
#' @export


diseaseSpread <- function(mat, startnode, nodenum, p){
  nn <- nodenum*nodenum
  mat <-  as.matrix(mat)
  diag(mat) <-1
  vec <- rep(0, nodenum)
  vec[c(startnode)] <- 1
  NodesNew <- rep(1:126,10*10)
  Vec <- matrix(rep(vec,12),12,byrow = T)
  Status <- NULL
  Step <- NULL
  Sim <- NULL

  for(j in 1:10){
    Sim <- c(Sim,rep(j,nodenum*10))

    for(i in 1:10){
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

