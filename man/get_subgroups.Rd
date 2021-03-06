% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GetInformation.R
\name{get_subgroups}
\alias{get_subgroups}
\title{Get information about available sub-group-types (SubGroupTypeID)}
\usage{
get_subgroups(save_file = FALSE, ...)
}
\arguments{
\item{save_file}{Logical. Choose whether or not to save the data in an
external \code{.Rdata} file in the working directory. Default: \code{FALSE}.}

\item{...}{Other arguments that might define the path to data. All arguments
accept a numeric code which is interpreted as the code of the specific
product requested. Alternatively, you can supply the equivalent product
name as a string which is case insensitive (see examples). Handle with
care, this is important! For a list of all options available, see the
parameters for each endpoint at https://popdiv.dfs.un.org/Demodata/swagger/ui/index#/}
}
\description{
Get information about available sub-group-types (SubGroupTypeID)
}
\examples{

\dontrun{
# Check what subgroups are available for:
S <- get_subgroups(
  indicatorTypeIds = 8, # Population by age and sex indicator;
  locIds = 818, # Egypt
  isComplete = 0
)
S
}
}
