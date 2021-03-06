% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_clusters_other.R
\name{create_clusters_other}
\alias{create_clusters_other}
\title{Create other clusters}
\usage{
create_clusters_other(
  planning,
  infos,
  hypothesis = NULL,
  start_date = NULL,
  area_name = NULL,
  constraints = NULL,
  opts = simOptions()
)
}
\arguments{
\item{planning}{Calendar data read with \code{\link{read_calendar}}.}

\item{infos}{Info about clusters read with \code{\link{read_info}}.}

\item{hypothesis}{Kp coefficients read with \code{\link{read_kp_edf}}. If not \code{NULL}, used to compute FO rate.}

\item{start_date}{Starting date of the study, if \code{NULL} (default),
the date will be retrieve from the Antares study.}

\item{area_name}{Name of the area where to create clusters.}

\item{constraints}{Stretch/Zircaloy constraints read with
\code{\link{read_constraints}}. Defaults to NULL.}

\item{opts}{List of simulation parameters returned by the function
\code{setSimulationPath}}
}
\description{
Create other clusters
}
