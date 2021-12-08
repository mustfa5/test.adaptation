
#' This function calculates the conditional proportion reduction in variance (PRV) index as described in Ju and Reckase (2019)
#'
#' @param diff a data frame in which rows represent test takers while columns represent the difficulty level of items that were administered to that test taker
#' @param bank A data matrix that have item parameters in the following order: discrimination, difficulty, guessing and slipping.#'
#' @return This function returns a matrix of CPRV values in numeric form
#' @export
#' @importFrom catR Ii
#' @importFrom stats var
#' @references
#' Ju, U., & Reckase, M. D. (2019). New conditional measures of the amount of adaptation of adaptive tests. Paper presented at the annual meeting of the National Council on Measurement in Education, Toronto, Canada.
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
#' diff=matrix(ncol = ncol(items.administered),nrow = nrow(items.administered))
#' for (k in 1:nrow(items.administered)) {
#'   xx= as.numeric(items.administered[k,])
#'   diff[k,]=bank[xx,2]
#' }
#' CPRV(diff = diff, bank=bank )

CPRV <- function(diff, bank) {
  require(catR)
  require(stats)
  s2b=var(bank[,2])
  s2bj=apply(diff,1,var)
  CPRV=(s2b-s2bj)/s2b

  return(CPRV)
}

