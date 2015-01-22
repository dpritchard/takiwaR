\name{mchi}
\alias{mchi}
\alias{print.mchi}

\title{Marine Cultural Health Index}
\description{Calculates an MCHI Score based on the initial implementation provided by Professor Henrick Moller}
\usage{
mchi(safe, spp, habitat, names = NULL)

\method{print}{mchi}(x, ...)
}

\arguments{
  \item{safe }{A numeric vector of length one. The site safety (0 - 4).}
  \item{spp }{A numeric matrix with 3 columns, or a numeric vector of length 3. See \emph{Details}.}
  \item{habitat }{A numeric vector of length 6. See \emph{Details}.}
  \item{names }{A character vector providing species names. See \emph{Details}.}
  
  \item{x }{An object of class mchi.}
  \item{... }{Other arguments passed to print.}
}

\details{
\code{mchi} provides a simple and reproducible interface for calculating and displaying a Marine Cultural Health Index (MCHI) score. The MCHI itself is described in detail in Schweikert et al. (2012) and users are directed there for further information regarding the calculation of the score itself. References to "forms" here relate to the original survey forms presented in Schweikert et al. (2012). 

\code{safe} is the food safety score (Form D). It must be an numeric value between 0 and 4, where 0 is highly contaminated and 4 is clean, uncontaminated food.  

\code{spp} are answers (for each species surveyed) to the following three questions:

\itemize{
    \item Is the kai replacing itself? (Form E)
    \item Can you get a feed easily enough? (Form F) and
    \item What is the taste and condition of the kai? (Form G)
}

\code{spp} can be a (potentially named) numeric vector with three numbers (for a single species) or a matrix (potentially with \code{\link{rownames}}) with three columns, with species values as rows. Numeric values must be between 0 and 4.

\code{habitat} must be a numeric vector with 6 values representing the answers to the questions in forms H and I. Answers to the questions should be between 0 and 4, but might also be -1 (Not Sure) or -2 (Not Applicable). Answers less than zero are treated as \code{NA} for the purposes of calculating the MCHI score.
}

\value{
\code{mchi} returns a list with the following items: 

\itemize{
\item \code{spp_names}: A character vector. The supplied species names.  
\item \code{spp_health}: A numeric vector. Species health, not including the influence of site.  
\item \code{spp_health_ratio}: A numeric vector. \code{spp_health} as a ratio of the maximum.  
\item \code{spp_with_site}: A numeric vector. Species health, including the influence of site saftey.  
\item \code{spp_with_site_ratio}: A numeric vector. \code{spp_with_site} as a ratio of the maximum.  
\item \code{final_score}: A single numeric value. The overall site score.  
\item \code{final_score_ratio}: A single numeric value. \code{final_score} as a ratio of the maximum.  
}

}
\references{
Schweikert K., McCarthy, A., Akins, A., Scott, N., Moller, H., Hepburn, C., and Landesberger, F. 2012. \emph{A Marine Cultural Health Index for sustainable management of mahinga kai in Aotearoa - New Zealand}. He Kohinga Rangahau No. 15. 88 pp. University of Otago, Dunedin. \url{http://hdl.handle.net/10523/5367}

\url{http://www.takiwa.org.nz}
}

\author{
Daniel Pritchard and Henrick Moller  
}

\examples{
# A simple single species example
res1 <- mchi(4, c(4,4,4), c(4,4,4,4,4,4))
print(res1)

# A more complex example, with species names
spp_mat <- matrix(c(4,4,4,4,4,4,4,4,2), ncol=3)
rownames(spp_mat) <- c('Paua', 'Kina', 'Tuaki')
res2 <- mchi(3, spp_mat, habitat=c(4,4,4,4,4,4))
print(res2)

# If names are provided, they will override rownames
res3 <- mchi(3, spp_mat, habitat=c(4,4,4,4,4,4), 
             names=c("Abalone", "Sea Urchin", "Cockle"))
print(res3)
}