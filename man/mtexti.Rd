\name{mtexti}
\alias{mtexti}

\title{Descending text in margin}
\description{Functions similar to mtext, but allows rotation.}

\usage{
mtexti(text, side, off = 0.25, 
        srt = if(side == 2) 90 else if(side == 4) 270 else 0,
        ...)
}

\arguments{
  \item{text }{A character or \code{\link{expression}} vector specifying the text to be written. See \code{\link{mtext}}}
  \item{side }{An integer. Which side to plot on. See \code{\link{mtext}}}
  \item{off }{A numeric. The offset.}
  \item{srt }{A numeric. String rotation in degrees. See \code{\link{par}}}
  \item{\ldots}{Other arguments passed to \code{\link{text}}}
}
\details{
TODO
}
\value{
TODO
}
\author{
Orignal version from here: \url{http://biostatmatt.com/archives/2522}. 

Originally included in dgmisc, and reproduced in takiwaR for convenience.  
}

\seealso{
\code{\link{mtext}}, \code{\link{text}} 
}
\examples{
\dontrun{

#TODO
}
}
