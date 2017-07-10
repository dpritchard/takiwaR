\name{error_bar}
\alias{error_bar}

\title{Draw error bars}
\description{Draw error bars on an existing plot}
\usage{
error_bar(x, y, upper, lower=upper, length=0.05, dir='y',...)
}

\arguments{
  \item{x }{A numeric. The x position}
  \item{y }{A numeric. The y position}
  \item{upper, lower }{A numeric. The value to add (upper) and subtract (lower) to y (or x, depending on the direction).}
  \item{length }{A numeric. The length of the edges of the arrowhead (as defined by \code{\link{arrows}})}
  \item{dir }{A string. The direction of the error bars (either 'x' or 'y')}
  \item{\ldots }{Additional arguments passed to \code{\link{arrows}}}
}

\details{
Note that \code{upper} and \code{lower} are added / substracted from y (or x, depending on direction). So a smart call to error_bar might be:

error_bar(x,y,sd(y))
}

\references{
Adapted from here: \url{http://monkeysuncle.stanford.edu/?p=485}
}
\author{
Daniel Pritchard
}