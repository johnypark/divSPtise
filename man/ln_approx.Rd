\name{ln_approx}
\alias{ln_approx}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{wrapper function of approx
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ln_approx(df, x.name, y.name, xout)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
%%     ~~Describe \code{df} here~~
}
  \item{x.name}{
%%     ~~Describe \code{x.name} here~~
}
  \item{y.name}{
%%     ~~Describe \code{y.name} here~~
}
  \item{xout}{
%%     ~~Describe \code{xout} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (df, x.name, y.name, xout)
{
    if (missing(xout)) {
        output <- approx(x = df[[x.name]], y = df[[y.name]])
        xout = 0
    }
    else {
        output <- approx(x = df[[x.name]], y = df[[y.name]],
            xout)
    }
    return(output)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
