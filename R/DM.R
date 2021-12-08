#' This function calculates difficulty mismatch (DM ) function as introduced in Wise, Kingsbury & Webb (2015)
#'
#' @param estimated.theta A data matrix that has the provisional ability estimates upon administering a sequence of items. In this input, rows are individuals taking the CAT. The columns are the items administered to individuals. The values in the cells are the provisional ability estimates after each item administration.
#' @param items.administered A data matrix that has the set of item items administered to individuals. This input assumes that every row in the data frame corresponds to the set of item names/identifiers administered to an individual.
#' @param bank A data matrix that have item parameters in the following order: discrimination, difficulty, guessing and slipping.
#'
#' @importFrom catR Ii
#' @importFrom stats sd
#' @return Returns a matrix for DM in which every row is the DM for each test taker
#' @export
#' @references
#' Wise, S. L., Kingsbury, G. G., & Webb, N. L. (2015). Evaluating content alignment in computerized adaptive testing. Educational Measurement: Issues and Practice, 34(4), 41-48.
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
#'res <- simulateRespondents(thetas = theta, bank,
#'                           start = start, test = test, stop = stop,
#'                           final = final, model = NULL)
#'
#'estimated.theta=res$responses.df[,grepl("estimated.theta",names( res$responses.df ) ) ]
#'items.administered=res$responses.df[,grepl("items.administrated",names( res$responses.df ) ) ]
#'DM(estimated.theta, items.administered, bank)

DM <- function(estimated.theta, items.administered, bank) {
  require(catR)
  estimated.theta <- as.matrix(estimated.theta)
  DM<- matrix(NA,nrow = nrow(estimated.theta))
  for (i in 1:nrow(estimated.theta)) {
    L <- length(items.administered[i,])
    dif <- bank[unlist(items.administered[i,]),2]
    pay<- sum((abs(as.vector(estimated.theta[i,])-dif)[2:L]))
    payda<- sd(estimated.theta[i,])*(L-1)
    DM[i,] <- pay/payda
  }
  return(DM)
}

