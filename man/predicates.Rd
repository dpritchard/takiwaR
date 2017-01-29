\name{predicates}
\alias{all_is_na}
\alias{has_empty_colnames}
\alias{has_empty_names}
\alias{has_empty_rownames}
\alias{has_na_names}
\alias{has_specified_named_object}
\alias{has_takRvalidators}
\alias{has_unique_colnames}
\alias{has_unique_names}
\alias{has_unique_rownames}
\alias{is_default_block_name}
\alias{is_default_str_in}
\alias{is_default_str_out}
\alias{is_readdef}
\alias{is_zero}

\title{NEED THIS}
\description{NEED THIS}
\usage{
all_is_na(x)
all_is_na(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x }{Something to test}
}
\details{NEED THIS}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...

}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
x <- c("Test", NA)
all_is_na(x)
x <- c(NA, NA, NA)
all_is_na(x)


##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x) 
{
    attr(x, "takRpredicates")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
