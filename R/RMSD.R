#' This function calculates root mean squared  difference (RMSD) function as introduced in Wyse & McBride (2021)
#'
#' @param estimated.theta A data matrix that has the provisional ability estimates upon administering a sequence of items. In this input, rows are individuals taking the CAT. The columns are the items administered to individuals. The values in the cells are the provisional ability estimates after each item administration.
#' @param items.administered A data matrix that has the set of item items administered to individuals. This input assumes that every row in the data frame corresponds to the set of item names/identifiers administered to an individual.
#' @param bank A data matrix that have item parameters in the following order: discrimination, difficulty, guessing and slipping.
#'
#' @importFrom catR Ii
#' @return Returns a matrix for RMSD in which every row is the RMSD for each test taker
#' @export
#' @references
#' Wyse, A. E., & McBride, J. R. (2021). A Framework for Measuring the Amount of Adaptation of Rasch‐based Computerized Adaptive Tests. Journal of Educational Measurement, 58(1), 83-103.
#' @examples
#' library(catR)
#'N=1000 #number of students
#'bank=250 #number of items
#'items=45
#'theta=rnorm(N,0,1) #level of trait
#'model="2PL" #IRT model to use
#'start <- list(theta = -1:1, randomesque = 1)
#'stop <- list(rule = c( "length"), thr = items)
#'final <- list(method = "ML")
#'
#'test=list(method = "ML", itemSelect = "MFI")
#'bank=genDichoMatrix(items =bank, cbControl = NULL,
#'                    model = model)
#'
#'res <-simulateRespondents(thetas = theta, bank,
#'                           start = start, test = test, stop = stop,
#'                           final = final, model = NULL)
#'
#'estimated.theta=res$responses.df[,grepl("estimated.theta",names( res$responses.df ) ) ]
#'items.administered=res$responses.df[,grepl("items.administrated",names( res$responses.df ) ) ]
#'RMSD(estimated.theta, items.administered, bank)

RMSD <- function(estimated.theta, items.administered, bank) {
  require(catR)
  estimated.theta <- as.matrix(estimated.theta)
  RMSD<- matrix(NA,nrow = nrow(estimated.theta))
  for (i in 1:nrow(estimated.theta)) {
    L <- length(items.administered[i,])
    dif <- bank[unlist(items.administered[i,]),2]
    pay<- sum(
      (dif-(as.vector(estimated.theta[i,])))^2
              )
    payda<- L
    RMSD[i,] <- sqrt(pay/payda)
  }
  return(RMSD)
}
