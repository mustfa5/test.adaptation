#' This function calculates the correlation index as described in Reckase, Ju, and Kim (2019)
#'
#' @param item.diff average item difficulties administered to a test taker during a computerized test administration
#' @param theta.hat estimated ability level of a group of individuals who were measured using a CAT environment
#'
#' @return This function returns a correlation value
#' @export
#' @importFrom catR Ii
#' @importFrom stats cor
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
#' theta.hat=res$final.values.df$estimated.theta
#'
#' items.administered=res$responses.df[,grepl("items.administrated",
#'                                            names( res$responses.df ) ) ]
#' colnames(items.administered)=NULL
#' diff=matrix(ncol = ncol(items.administered),nrow = nrow(items.administered))
#' for (k in 1:nrow(items.administered)) {
#'   xx= as.numeric(items.administered[k,])
#'   diff[k,]=bank[xx,2]
#' }
#' bhat=rowMeans(diff)
#' r.index(bhat, t.hat)
#'
r.index <- function(item.diff,theta.hat) {
  require(catR)
  r.index <- cor(item.diff,theta.hat)
  return(r.index)
}
