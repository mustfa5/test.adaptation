
#' This function calculates the devation of difficulty (DOD) index as described in Ju and Reckase (2019)
#'
#' @param estimated.theta A data matrix that has the provisional ability estimates upon administering a sequence of items. In this input, rows are individuals taking the CAT. The columns are the items administered to individuals. The values in the cells are the provisional ability estimates after each item administration.
#' @param items.administered A data matrix that has the set of item items administered to individuals. This input assumes that every row in the data frame corresponds to the set of item names/identifiers administered to an individual.
#' @param bank A data matrix that have item parameters in the following order: discrimination, difficulty, guessing and slipping.
#' @param ni total number of items that were used to estimate initial ability level.
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
#' DOD(estimated.theta,items.administered, bank)



DOD <- function(estimated.theta,bank,items.administered,ni) {
  require(catR)
  if(missing(ni)) {
    warning("The ni was not provided hence a default value of 1 was used")}

  if(missing(ni))
    ni<-NULL
  if(is.null(ni)) {
    ni<-1
  } else {
    ni
  }
  ni <- 1
  for (i in 1:dim(estimated.theta)[2]) {
    Lj=dim(estimated.theta)[2]
    out=matrix(NA,nrow  = Lj-1)
    for (k in 2:Lj) {
      c1=1/(Lj-ni)
      uygulanan.madde.gucluk=bank[unlist(items.administered[i,1:k]),2]
      uygulanmayan.madde.gucluk=bank[-c(unlist(items.administered[i,1:k])),2]
      num=mean(abs(uygulanan.madde.gucluk-estimated.theta[i,k-1]))
      denom=mean(abs(uygulanmayan.madde.gucluk-estimated.theta[i,k-1]))
      out[k-1,]= c1*(1-num/denom)
      DOD=mean(out)
    }
    print(DOD)

  }
}
