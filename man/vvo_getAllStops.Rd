% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/VVOLib.R
\name{vvo_getAllStops}
\alias{vvo_getAllStops}
\title{Get all Stops (daily updated)}
\source{
https://www.vvo-online.de/open_data/VVO_STOPS.JSON
}
\usage{
vvo_getAllStops()
}
\value{
Tibble with all Stops in VVO, fetched from an open_data-JSON. There might be duplicates.
}
\description{
Get all Stops (daily updated)
}
