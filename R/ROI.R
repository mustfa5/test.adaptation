
#' This function calculates the ratio of information (ROI) index as described in Ju and Reckase (2019)
#'
#' @param estimated.theta A data matrix that has the provisional ability estimates upon administering a sequence of items. In this input, rows are individuals taking the CAT. The columns are the items administered to individuals. The values in the cells are the provisional ability estimates after each item administration.
#' @param items.administered A data matrix that has the set of item items administered to individuals. This input assumes that every row in the data frame corresponds to the set of item names/identifiers administered to an individual.
#' @param bank A data matrix that have item parameters in the following order: discrimination, difficulty, guessing and slipping.
#' @return This function returns a list with two outputs. (1) average ROI for the entire CAT administration and (2) ROI at individual level
#' @export
#' @importFrom catR Ii
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
#'res <- simulateRespondents(thetas = theta, bank,
#'                           start = start, test = test, stop = stop,
#'                           final = final, model = NULL)
#'
#'estimated.theta=res$responses.df[,grepl("estimated.theta",names( res$responses.df ) ) ]
#'items.administered=res$responses.df[,grepl("items.administrated",names( res$responses.df ) ) ]
#' ROI(estimated.theta,items.administered, bank)


ROI<- function(estimated.theta,items.administered, bank){
  require(catR)
  for(i in 1:length(estimated.theta[i,])) {
  #test length
 nj <- length(estimated.theta[i,])
  #item characteristics of items administered
 ichars <- bank[unlist(items.administered[i,]),]
  #provisional ability estimates
 provisional <- unlist(estimated.theta[i,])
 maxinfo <- Ii(ichars[,2],ichars[,])$Ii
 provinfo <- Ii(provisional[],ichars[,])$Ii
 ROI.individual<-provinfo/maxinfo
 names(ROI.individual) <- paste("ROI.for.item.",1:nj,sep = "")
 ROI.average<-mean(provinfo/maxinfo)

 ROI= list(ROI.individual=ROI.individual,ROI.average=ROI.average)
return(ROI)
}}
