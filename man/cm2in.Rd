\name{cm2in}
\alias{cm2in}
\title{A function to convert centimeters to inches}
\description{
A utility function to convert centimeters to inches, mostly used for plotting.  
}
\usage{
cm2in(x)
}

\arguments{
  \item{x }{A size in cm, to be converted to inches}
}
\details{
}
\value{Returns a numeric vector (possibly a single scalar)}
\author{
Daniel Pritchard
}
\seealso{
Many of the base plotting functions require inches, see \code{\link{par}}
}
\examples{
cm2in(19) # Should be approx 7.5 inches
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
