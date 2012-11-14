\name{convertFile}
\alias{convertFile}
\title{Converts an image from a color keymap to grey scale.}
\usage{
  convertFile(ipng, opng, tol = 20, clen = 100,
    color.palette = cfd.colors, glim = c(0.02, 0.96))
}
\arguments{
  \item{ipng}{Input png file.}

  \item{opng}{Output png file.}

  \item{tol}{Tolerance in color interpolation.}

  \item{clen}{Number of color subdivisions that should be
  used.}

  \item{color.palette}{Color scale.}

  \item{glim}{Gray scale that is used when generating
  output.}
}
\description{
  An utility function used when converting some figure that
  has a color map from a color scale to a grey scale.
}
\details{
  If a pixel has o color too far away from the color scale,
  the pixel is left without modification since it probably
  belongs to annotations on the figure.
}

