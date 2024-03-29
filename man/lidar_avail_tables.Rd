% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lfcLiDAR.R
\name{lidar_avail_tables}
\alias{lidar_avail_tables}
\title{Get the available tables in LiDAR db}
\usage{
lidar_avail_tables(object)
}
\arguments{
\item{object}{\code{lfcLiDAR} object, as created by \code{\link{lidar}}}
}
\value{
A character vector with the table names
}
\description{
\code{lidar_avail_tables} is a wrapper for the \code{$avail_tables} method
  of \code{lfcLiDAR} objects. See \code{\link{lidar}}.
}
\examples{
if (interactive()) {
  lidardb <- lidar()
  lidar_avail_tables(lidardb)

  # lidardb is an R6 object, so the previous example is the same as:
  lidardb$avail_tables()
}

}
\seealso{
Other LiDAR functions: 
\code{\link{lidar_clip_and_stats}()},
\code{\link{lidar_describe_var}()},
\code{\link{lidar_get_data}()},
\code{\link{lidar_get_lowres_raster}()},
\code{\link{lidar_point_value}()},
\code{\link{lidar}()}
}
\concept{LiDAR functions}
