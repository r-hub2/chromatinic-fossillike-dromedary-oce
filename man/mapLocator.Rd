% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/map.R
\name{mapLocator}
\alias{mapLocator}
\title{Locate Points on a Map}
\usage{
mapLocator(n = 512, type = "n", ...)
}
\arguments{
\item{n}{number of points to locate; see \code{\link[=locator]{locator()}}.}

\item{type}{type of connector for the points; see \code{\link[=locator]{locator()}}.}

\item{\dots}{extra arguments passed to \code{\link[=locator]{locator()}} (and either
\code{\link[=mapPoints]{mapPoints()}} or \code{\link[=mapLines]{mapLines()}}, if appropriate) if
\code{type} is not \code{'n'}.}
}
\description{
Locate points on an existing map.
This uses \code{\link[=map2lonlat]{map2lonlat()}} to infer the location in
geographical space, so it suffers the same
limitations as that function.
}
\seealso{
A map must first have been created with \code{\link[=mapPlot]{mapPlot()}}.

Other functions related to maps: 
\code{\link{formatPosition}()},
\code{\link{lonlat2map}()},
\code{\link{lonlat2utm}()},
\code{\link{map2lonlat}()},
\code{\link{mapArrows}()},
\code{\link{mapAxis}()},
\code{\link{mapContour}()},
\code{\link{mapCoordinateSystem}()},
\code{\link{mapDirectionField}()},
\code{\link{mapGrid}()},
\code{\link{mapImage}()},
\code{\link{mapLines}()},
\code{\link{mapLongitudeLatitudeXY}()},
\code{\link{mapPlot}()},
\code{\link{mapPoints}()},
\code{\link{mapPolygon}()},
\code{\link{mapScalebar}()},
\code{\link{mapText}()},
\code{\link{mapTissot}()},
\code{\link{oceCRS}()},
\code{\link{oceProject}()},
\code{\link{shiftLongitude}()},
\code{\link{usrLonLat}()},
\code{\link{utm2lonlat}()}
}
\author{
Dan Kelley
}
\concept{functions related to maps}
