\name{rnt}

\title{Generate a random number table}
\description{
Generate a matrix of random numbers, with optional row and column name prefixes
}
\usage{
rnt(min = 0, max = 30, by = 1, 
    nrow = 10, ncol = 10, 
    rowpref = "T", colpref = "Q")
}

\arguments{
  \item{min }{The minimum random number}
  \item{max }{The maximum random number}
  \item{by }{The "step size" when generating random numbers}
  \item{nrow }{Number of rows}
  \item{ncol }{Number of columns}
  \item{rowpref }{Row prefix. "T" for "Transect" by default.}
  \item{colpref }{Column prefix. "Q" for "Quadrat" by defualt.}
}
\details{
<TODO DOCS>
}
\value{
Returns a matrix, with helpfully labelled columns and rows.
}

\author{
Daniel Pritchard
}

\examples{
rnt()
}
