\name{col2gray}
\alias{col2gray}
\title{Converts an image from a color keymap to grey scale.}
\usage{
  col2gray(img, tol = 20, clen = 100,
    myrain = col2rgb(cfd.colors(clen)))
}
\arguments{
  \item{img}{Array containing the image.}

  \item{tol}{Tolerance in color interpolation.}

  \item{clen}{Number of color subdivisions that should be
  used.}

  \item{myrain}{Color scale.}
}
\value{
  Index characterizing the level of each pixel.
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

