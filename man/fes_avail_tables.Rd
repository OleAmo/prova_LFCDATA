% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lfcFES.R
\name{fes_avail_tables}
\alias{fes_avail_tables}
\title{Get the available tables in FES db}
\usage{
fes_avail_tables(object)
}
\arguments{
\item{object}{\code{lfcFES} object, as created by \code{\link{fes}}}
}
\value{
A character vector with the table names
}
\description{
\code{fes_avail_tables} is a wrapper for the \code{$avail_tables}
  method of \code{lfcFES} objects. See \code{\link{fes}}.
}
\examples{
if (interactive()) {
  fesdb <- fes()
  fes_avail_tables(fesdb)

  # fesdb is an R6 object, so the previous example is the same as:
  fesdb$avail_tables()
}

}
\seealso{
Other FES functions: 
\code{\link{fes_describe_var}()},
\code{\link{fes_get_data}()},
\code{\link{fes}()}
}
\concept{FES functions}
