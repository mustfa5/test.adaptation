
#' This function calculates the proportion reduction in variance (PRV) index as described in Reckase, Ju, and Kim (2019)
#'
#' @param diff a data frame in which rows represent test takers while columns represent the difficulty level of items that were administered to that test taker
#' @param N total number of test takers who participated in a CAT that uses a specific CAT pool
#' @param n.items test length
#' @param item.diff item difficulties administered to a test taker during a computerized test administration
#'
#' @return This function returns numeric value
#' @export
#' @importFrom catR Ii
#' @references
#' Reckase, M. D., Ju, U., & Kim, S. (2019). How adaptive is an adaptive test: Are all adaptive tests adaptive?. Journal of Computerized Adaptive Testing, 7(1), 1-14.
#' @examples
#' library(catR)
#' N=1000 #number of students
#' bank=250 #number of items
#' items=45
#' theta=rnorm(N,0,1) #level of trait
#' model="2PL" #IRT model to use
#' start <- list(theta = -1:1, randomesque = 1)
#' stop <- list(rule = c( "length"), thr = items)
#' final <- list(method = "ML")
#'
#' test=list(method = "ML", itemSelect = "MFI")
#' bank=genDichoMatrix(items =bank, cbControl = NULL,
#'                     model = model)
#'
#' res <- simulateRespondents(thetas = theta, bank,
#'                            start = start, test = test, stop = stop,
#'                            final = final, model = NULL)
#' t.hat=res$final.values.df$estimated.theta
#'
#' items.administered=res$responses.df[,grepl("items.administrated",
#'                                            names( res$responses.df ) ) ]
#' colnames(items.administered)=NULL
#' item.diff=matrix(ncol = ncol(items.administered),nrow = nrow(items.administered))
#' for (k in 1:nrow(items.administered)) {
#'   xx= as.numeric(items.administered[k,])
#'   item.diff[k,]=bank[xx,2]
#' }
#' PRV(diff = item.diff, N=N, n.items = items, item.diff = bank$b )



PRV <- function(diff,N,n.items, item.diff) {
  require(catR)
  #s2b the variance of the  difficulties of the items in the item bank\
#diff a data frame that contains the difficulty levels of items administred to each individual
#diff should be a data frame where columns represents items, rows represent individuals
#item.diff is the difficulty levels of items in the item bank

  s2b=var(item.diff)
  pooled=sum(apply(diff,1,var)*(n.items-1))/((n.items-1)*N)
  PRV=(s2b-pooled)/s2b
  return(PRV)
}

