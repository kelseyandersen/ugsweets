
#' An AUDPC function, calculating across many time-series simulations
#'
#' This function allows you to calculate AUDPC across many reps of a dataframe generated from the function diseaseSpread, or DiseaseSpreadPWR
#' @param reps number of simulations
#' @param df dataframe with Rep #, "Number_of_Nodes" and "Step"
#' @keywords network spread audpc
#' @examples
#' AUDPCfun
#' @importFrom agricolae audpc
#' @export



AUDPCfun <- function(reps, df){
  e <- c(1:reps)
  out <- NULL
  df <- as_tibble(df)
  for (i in seq(e)){
    temp <- filter(df, Rep == e[i])
    out[[i]] <- agricolae::audpc(temp$Number_of_Nodes, temp$Step, type = "absolute")
    temp <- mean(out)
  }
  return(temp)
}
