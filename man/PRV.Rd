% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PRV.R
\name{PRV}
\alias{PRV}
\title{This function calculates the proportion reduction in variance (PRV) index as described in Reckase, Ju, and Kim (2019)}
\usage{
PRV(diff, N, n.items, item.diff)
}
\arguments{
\item{diff}{a data frame in which rows represent test takers while columns represent the difficulty level of items that were administered to that test taker}

\item{N}{total number of test takers who participated in a CAT that uses a specific CAT pool}

\item{n.items}{test length}

\item{item.diff}{item difficulties administered to a test taker during a computerized test administration}
}
\value{
This function returns numeric value
}
\description{
This function calculates the proportion reduction in variance (PRV) index as described in Reckase, Ju, and Kim (2019)
}
\examples{
library(catR)
N=1000 #number of students
bank=250 #number of items
items=45
theta=rnorm(N,0,1) #level of trait
model="2PL" #IRT model to use
start <- list(theta = -1:1, randomesque = 1)
stop <- list(rule = c( "length"), thr = items)
final <- list(method = "ML")

test=list(method = "ML", itemSelect = "MFI")
bank=genDichoMatrix(items =bank, cbControl = NULL,
                    model = model)

res <- simulateRespondents(thetas = theta, bank,
                           start = start, test = test, stop = stop,
                           final = final, model = NULL)
t.hat=res$final.values.df$estimated.theta

items.administered=res$responses.df[,grepl("items.administrated",
                                           names( res$responses.df ) ) ]
colnames(items.administered)=NULL
item.diff=matrix(ncol = ncol(items.administered),nrow = nrow(items.administered))
for (k in 1:nrow(items.administered)) {
  xx= as.numeric(items.administered[k,])
  item.diff[k,]=bank[xx,2]
}
PRV(diff = item.diff, N=N, n.items = items, item.diff = bank$b )
}
\references{
Reckase, M. D., Ju, U., & Kim, S. (2019). How adaptive is an adaptive test: Are all adaptive tests adaptive?. Journal of Computerized Adaptive Testing, 7(1), 1-14.
}
