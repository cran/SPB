% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods.R
\name{summary,simple.progress.bar-method}
\alias{summary,simple.progress.bar-method}
\title{Summary method for simple progress bars}
\usage{
\S4method{summary}{simple.progress.bar}(object)
}
\arguments{
\item{object}{A simple progress bar}
}
\value{
A summary including:
\item{message}{The message printed before the bar (if any)}
\item{status}{The advancement status of the bar}
\item{max}{The length of the bar}
\item{percentage}{The advancement status of the bar in percentage points}
The summary is also printed to the console as a kable (if \code{knitr} is installed). Otherwise it prints out as a normal table.
}
\description{
Summary method for simple progress bars
}
\examples{

# Example without custom message
pb<-create_pb(length=10, print=FALSE)
summary(pb)

# Example with a custom message
pb<-create_pb(length=43, print=FALSE,custom.message='Custom pb')
summary(pb)

# Example with a custom message and an updated value
pb<-create_pb(length=11, print=FALSE,custom.message='A new value:')
pb<-update_pb(pb,6)
summary(pb)

}
\author{
\enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
}
