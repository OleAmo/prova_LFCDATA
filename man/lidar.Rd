% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lfcLiDAR.R
\name{lidar}
\alias{lidar}
\title{lfcLiDAR class}
\usage{
lidar()
}
\value{
An \code{lfcLiDAR} class object (inherits from \code{\link[R6]{R6Class}}),
  with methods to access the data. See Methods section.
}
\description{
\code{lidar()} creates an object to access the LiDAR database.
}
\section{Methods}{

  \code{lfcLiDAR} objects has the following methods available:
  \itemize{
    \item{\code{$get_data}: Retrieve aggregated precalculated data for administrative
          divisions and natural areas. See \code{\link{lidar_get_data}} for more
          details}
    \item{\code{$get_lowres_raster}: Retrieve and collect LiDAR database rasters. See
          \code{\link{lidar_get_lowres_raster}} for more details}
    \item{\code{$avail_tables}: List all the tables that can be consulted. See
          \code{\link{lidar_avail_tables}} for more details}
    \item{\code{$describe_var}: Describe the variables, with their units and details.
          See \code{\link{lidar_describe_var}} for more details}
    \item{\code{$clip_and_stats}: Clip the specified variables with the provided set of
          polygons and calculate the raw raster statistics for each polygon. See
          \code{\link{lidar_clip_and_stats}} for more details}
    \item{\code{$point_value}: Extract values from the raw raster for the desired
          points. See \code{\link{lidar_point_value}} for more details}
  }
}

\examples{
lidardb <- lidar()
lidardb
}
\seealso{
Other LiDAR functions: 
\code{\link{lidar_avail_tables}()},
\code{\link{lidar_clip_and_stats}()},
\code{\link{lidar_describe_var}()},
\code{\link{lidar_get_data}()},
\code{\link{lidar_get_lowres_raster}()},
\code{\link{lidar_point_value}()}
}
\concept{LiDAR functions}
