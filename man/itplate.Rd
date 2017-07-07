\name{itplate}
\alias{itplate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
itplate(Dat_Frm, Data_Type, IntrPol_Var, Time_Var = NULL, Prime_Key = NULL, Secnd_Key = NULL, Time_Full = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Dat_Frm}{
%%     ~~Describe \code{Dat_Frm} here~~
}
  \item{Data_Type}{
%%     ~~Describe \code{Data_Type} here~~
}
  \item{IntrPol_Var}{
%%     ~~Describe \code{IntrPol_Var} here~~
}
  \item{Time_Var}{
%%     ~~Describe \code{Time_Var} here~~
}
  \item{Prime_Key}{
%%     ~~Describe \code{Prime_Key} here~~
}
  \item{Secnd_Key}{
%%     ~~Describe \code{Secnd_Key} here~~
}
  \item{Time_Full}{
%%     ~~Describe \code{Time_Full} here~~
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
function (Dat_Frm, Data_Type, IntrPol_Var, Time_Var = NULL, Prime_Key = NULL, 
    Secnd_Key = NULL, Time_Full = NULL) 
{
    if (is.null(Prime_Key)) 
        Prime_Key <- "tag"
    if (is.null(Secnd_Key)) 
        Secnd_Key <- "spcode"
    if (is.null(Time_Var)) 
        Time_Var <- "date"
    if (is.null(Time_Full)) 
        Time_Full <- Dat_Frm[[Time_Var]] \%>\% as.Date \%>\% unique
    Dat_Frm.index.na <- (Dat_Frm[[IntrPol_Var]] \%>\% is.na)
    Dat_Frm \%<>\% .[!Dat_Frm.index.na, ]
    Dat_Frm[[Time_Var]] \%<>\% as.Date
    Dat_Frm[["QC"]] <- Data_Type
    Dat_Frm.array.KeyMatch <- Dat_Frm[[Prime_Key]] \%>\% unique
    Dat_Frm \%<>\% filter(!is.na(Prime_Key))
    Dat_Frm.itrpl.LIST = list()
    for (i in 1:length(Dat_Frm.array.KeyMatch)) {
        sbst.Dat_Frm <- Dat_Frm \%>\% filter(tag == Dat_Frm.array.KeyMatch[i])
        sbst.Dat_Frm$Dcount <- sbst.Dat_Frm[[Time_Var]] \%>\% as.numeric
        index.MssngPnts <- (!(Time_Full) \%in\% sbst.Dat_Frm[[Time_Var]]) \%>\% 
            which
        if (length(index.MssngPnts) > 0) {
            itrpl.leaf <- sbst.Dat_Frm \%>\% ln_approx("Dcount", 
                IntrPol_Var, xout <- (Time_Full \%>\% as.numeric)[index.MssngPnts])
            Dat_Frm.itrpl <- data.frame(as.Date(itrpl.leaf[["x"]], 
                origin = "1970-1-1"), itrpl.leaf[["y"]], QC = "interpolation")
            colnames(Dat_Frm.itrpl)[1:2] <- c(Time_Var, IntrPol_Var)
            Dat_Frm.itrpl[[Prime_Key]] <- Dat_Frm.array.KeyMatch[i]
            Dat_Frm.itrpl[[Secnd_Key]] <- (sbst.Dat_Frm[[Secnd_Key]])[1]
            index.neg <- which(Dat_Frm.itrpl[[IntrPol_Var]] < 
                0)
            Dat_Frm.itrpl[index.neg, IntrPol_Var] <- 0
            Dat_Frm.itrpl.LIST[[i]] <- Dat_Frm.itrpl
        }
        else {
        }
    }
    sbst.Dat_Frm <- do.call("rbind", Dat_Frm.itrpl.LIST)
    return(sbst.Dat_Frm)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
