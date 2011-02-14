\name{ICCest}
\alias{ICCest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{A function to estimate the Intraclass Correlation Coefficient}
\description{Estimates the ICC and confidence interval using a one-way anova.}
\usage{
ICCest(x, y, data = data, alpha = 0.05)
}
\arguments{
  \item{x}{column name indicating individual or group id in the dataframe \code{data}}
  \item{y}{column name indicating measurements in the dataframe \code{data}}
  \item{data}{a dataframe containing \code{x} and \code{y}}
  \item{alpha}{the alpha level to use when estimating the confidence interval.  Default is 0.05.}
}

\value{
  \item{ICC}{the intraclass correlation coefficient}
  \item{LowerCI}{the lower confidence interval limit, where the confidence level is set by \code{alpha}}
  \item{UpperCI}{the upper confidence interval limit, where the confidence level is set by \code{alpha}}
  \item{N}{the total number of individuals or groups used in the analysis}
  \item{k}{the number of measurements per individual or group.  In an unbalanced design, k is always less than the mean number of measurements per individual/group and is calculated using the equation in Lessells and Boag (1987).}
  \item{varw}{the within individual or group variance}
  \item{vara}{the among individual or group variance}

}
\references{C.M. Lessells and P.T. Boag. 1987. The Auk, 104(1):116-121.}

\author{Matthew Wolak \email{matthewwolak@gmail.com}}

\seealso{\code{\link{ICCbare}}}
\examples{
data(ChickWeight)
ICCest(Chick, weight, data=ChickWeight)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }
