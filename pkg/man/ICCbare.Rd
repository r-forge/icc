\name{ICCbare}
\alias{ICCbare}
\title{Simple function to just estimate the Intraclass Correlation Coefficient}
\description{This function only estimates the Intraclass Correlation Coefficient (ICC).  It is meant to be as simple and fast as possible for use in Monte Carlo simulations or bootstrapping}
\usage{
ICCbare(x, y, data)
}
\arguments{
  \item{x}{column name indicating individual or group id in the dataframe \code{data}}
  \item{y}{column name indicating measurements in the dataframe \code{data}}
  \item{data}{a dataframe containing \code{x} and \code{y}}
}
\details{\code{ICCbareF} is distinguished from \code{ICCbare}, in that \code{ICCbare} is more flexible and can handle missing values and unbalanced datasets.  \code{ICCbareF} cannot and can only be used on balanced datasets without any NAs.
}

\value{
  \item{ICC}{the intraclass correlation coefficient}
}
\author{Matthew Wolak \email{matthewwolak@gmail.com}}
\seealso{\code{\link{ICCest}}, \code{\link{ICCbareF}},
}
