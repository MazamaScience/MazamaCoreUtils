% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-logging.R
\name{logger.setLevel}
\alias{logger.setLevel}
\title{Set console log level}
\usage{
logger.setLevel(level)
}
\arguments{
\item{level}{Threshold level.}
}
\value{
No return value.
}
\description{
By default, the logger threshold is set to \code{FATAL} so that the console
will typically receive no log messages. By setting the level to one of the
other log levels: \code{TRACE, DEBUG, INFO, WARN, ERROR} users can see
logging messages while running commands at the command line.
}
\note{
All functionality is built on top of the excellent \pkg{futile.logger}
  package.
}
\examples{
\dontrun{
# Set up console logging only
logger.setup()
logger.setLevel(DEBUG)
}

}
\seealso{
\code{\link{logger.setup}}
}
