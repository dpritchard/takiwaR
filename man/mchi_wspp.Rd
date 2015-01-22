\name{mchi_wspp}
\alias{mchi_wspp}

\title{MCHI Weighted Species}
\description{Provides a weighting vector for species in MCHI calculation.}
\usage{
mchi_wspp(nspp)
}

\arguments{
  \item{nspp }{An integer. The number of species.}
}

\details{
At the moment this is a simple exponential decay, such that each sequential species has a 20 percent reduction in relative importance over the previous one.
}

\value{
Returns a vector of length \code{nspp} describing the relative importance of each species.  
}

\author{
Daniel Pritchard
}

\seealso{
\code{\link{mchi}}
}
\examples{
# An example with five species, the current MCHI limit
fivespp <- mchi_wspp(5)
plot(1:5, fivespp, ylab="Relative Importance", xlab="Species Rank", ylim=c(0,1))

# An exmaple with fifty species, for illustration
manyspp <- mchi_wspp(50)
plot(1:50, manyspp, ylab="Relative Importance", xlab="Species Rank", ylim=c(0,1))
}
