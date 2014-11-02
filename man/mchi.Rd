\name{mchi}
\alias{mchi}

\title{Marine Cultural Health Index}
\description{A description of the MCHI function}
\usage{
mchi(safe, spp, habitat)
mchi_spp_weights(nspp)
print.mchi(x, ...)
}


\arguments{
  \item{safe }{A numeric vector of length one.}
  \item{spp }{A numeric matrix with 3 columns, or a numeric vector of length 3.}
  \item{habitat }{A numeric matrix with 6 columns, or a numeric vector of length 6.}
  \item{nspp }{The number of species.}
  \item{... }{Other agruments passed to print}
}
\details{
Details here...
}
\value{
mchi returns a list with the following items: spp_health, spp_health_ratio, spp_with_site, spp_with_site_ratio, final_score, final_score_ratio

}
\references{
\url{http://www.takiwa.org.nz}
}
\author{
Daniel Pritchard
Henrick Moller
}
\examples{
result <- mchi(4, c(4,4,4), c(4,4,4,4,4,4))
print(result)
}