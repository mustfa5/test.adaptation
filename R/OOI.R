#'  This function calculates operational optimal information (OOI ) function as introduced in Kingsbury & Wise (2020)
#'
#' @param theta a numeric vector containing the true ability level of students
#' @param t.hat a numeric vector containing the estimated ability level of students
#' @param items.administered A data matrix that has the set of item items administered to individuals. This input assumes that every row in the data frame corresponds to the set of item names administered to an individual.
#' @param bank A data matrix that have item parameters in the following order: discrimination, difficulty, guessing and slipping.
#'
#' @importFrom catR Ii
#' @importFrom dplyr arrange
#' @return Returns a single numeric value for OOI
#' @export
#' @references
#' Kingsbury, G. G., & Wise, S. L. (2020). Three Measures of Test Adaptation Based on Optimal Test Information. Journal of Computerized Adaptive Testing, 8(1), 1-19.
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
#' OOI(theta, t.hat, items.administered, bank)

OOI <- function(theta,t.hat, items.administered, bank) {
  require(catR)
  res=matrix(ncol = 1,nrow = length(theta))
  for (i in 1:length(theta)) {

    items.administered.individual=as.numeric(items.administered[i,])
    IAj=sum(Ii(t.hat[i],bank[items.administered.individual,],)$Ii)
    inf.items=bank[items.administered.individual,]
    inf.items[,2]=t.hat[i]
    bank.sort=bank
    bank.sort$id=rownames(bank.sort)
    bank.sort$e=abs(bank$b-theta[i])
    bank.sorted=arrange(bank.sort,bank.sort$e)[1:(dim(bank)[1]),]
    res[i,1]=IAj/sum(Ii(t.hat[i],bank.sorted[,1:4]) $Ii)

  }
  OOI=mean(res)
  return(noquote(paste("OOI=",round(OOI,digits = 2))))}


