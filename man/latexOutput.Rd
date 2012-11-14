\name{latexOutput}
\alias{latexOutput}
\title{Generate a LaTeX table from the table layout.}
\usage{
  latexOutput(suffix, tab, tab.lay, header = NULL,
    units = NULL, pos = NULL, dname = NULL,
    preamble = NULL)
}
\arguments{
  \item{suffix}{Suffix of the file name.}

  \item{tab}{Data to be written to LaTeX files.}

  \item{tab.lay}{Table layout computed with
  \code{\link{tableLayout}}.}

  \item{Header}{of each column. If not specified, the names
  of each column is used.}

  \item{units}{An optional second line in the header
  specifying units or some other info.}

  \item{pos}{LaTeX tabular position specifier for each
  variable.}

  \item{dname}{Directory name where the file should be
  written.}

  \item{preamble}{Some message that should be written
  before the header.}
}
\description{
  After generating a layout with \code{\link{tableLayout}}
  this function generates a set of LaTeX files that contain
  the data in table format.
}

