\name{se}
\alias{se}

\title{Standard Error}
\description{This function computes the standard error of the values in x. If na.rm is TRUE then missing values are removed before computation proceeds.}

\usage{se(x, na.rm=FALSE)}

\arguments{
  \item{x }{A numeric vector.}
  \item{na.rm }{A logical. Should missing values be removed? See details for implementation.}
}
\details{
If \code{na.rm=TRUE} then \code{na.rm=TRUE} is passed to \code{\link{sd}} and \emph{n} is calculated as \code{length(na.omit(x))}.  

Note this might have unexpected consequences for matrixes or dataframes, which is why \code{x} MUST be a vector.    
}
\author{
Daniel Pritchard
}

\seealso{
\code{\link{sd}}, \code{\link{na.omit}}. \code{\link{length}}.
}

\examples{
dat <- c(1,2,3,4,5)
se(dat) # 0.7071068

dat_na <- c(1,2,3,4,NA)
se(dat_na) # NA
se(dat_na, na.rm=TRUE) # 0.6454972
}
