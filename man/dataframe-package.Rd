\name{dataframe-package}
\alias{dataframe-package}
\alias{dataframe}
\docType{package}
\title{
Fast data frames
}
\description{
This contains versions of standard data frame functions in R,
modified to avoid making extra copies of inputs.  This is faster, particularly
for large data.
}
\details{
\tabular{ll}{
Package: \tab dataframe\cr
Type: \tab Package\cr
Version: \tab 2.5\cr
Date: \tab 2012-05-20\cr
License: \tab GPL-2\cr
LazyLoad: \tab no\cr
}
Load this package in the usual way using \code{library(dataframe)},
then use R as usual.

Most of the changes here relate to speed and memory.
There are tests in the \code{tests} directory, and examples of
memory and speed improvements in the \code{inst/doc} or \code{doc} directory.

One change is substantive:  \code{Math.data.frame} now skips
non-numerical variables instead of failing.
This is useful for printing a rounded version of the data frame,
as \code{\link{round}} and \code{\link{trunc}} are part
of \code{\link{Math}}.
There is a warning, which may be removed in the future.
}
\author{
Maintainer:
Tim Hesterberg <rocket@google.com>
}
\keyword{array}
\keyword{classes}
\keyword{methods}
