\name{ICCbare}
\alias{ICCbare}
\title{Simple function to estimate the Intraclass Correlation Coefficient}
\description{This function only estimates the Intraclass Correlation Coefficient (ICC).  It is meant to be as simple and fast as possible for use in Monte Carlo simulations or bootstrapping.  Calculates the ANOVA 'by hand', instead of using the aov() function, to obtain variance components.}
\usage{
ICCbare(x, y, data)
}
\arguments{
  \item{x}{column name indicating individual or group id in the dataframe \code{data}}
  \item{y}{column name indicating measurements in the dataframe \code{data}}
  \item{data}{a dataframe containing \code{x} and \code{y}}
}

\details{\code{ICCbare} can only be used on balanced datasets without NAs
}

\value{
  \item{ICC}{the intraclass correlation coefficient}

}
\author{Matthew Wolak \email{matthewwolak@gmail.com}}
\seealso{\code{\link{ICCest}}}
