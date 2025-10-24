\name{extract_labels}
\alias{extract_labels}
\title{ Extract labelled variable attributes }
\description{
   \code{extract_labels} is used to extract variable label, value label, and data label from a data frame and variables in it.

   These three data attributes are stored in a list with three elements named \code{varlabels}, \code{vallabels}, and \code{datalabel}, respectively.
}
\usage{
extract_labels(
  dat,
  cols=names(dat)
)
}
\arguments{
  \item{dat}{ A data frame or data frame extension (e.g. a data.table, tibble, etc.). }
  \item{cols}{ The columns whose attributes (labels) need to be extracted. By default, all columns are considered. It can be character vector of variables names or a logical/numeric vector specifying the indices of the relevant variables.}
}
\details{
  This function is designed to easy the preparation of the argument \code{list_labels} of the function \code{\link{add_labels}}.

  The data, \code{dat}, is expected to be read into R using \code{\link[haven:read_dta]{haven::read_*()}}.
}
\value{
    A named list containing the three types of labels extracted: data label (\code{datalabel}), variable label (\code{varlabels}), and value label  (\code{vallabels}). If a given label is missing, the corresponding element is \code{NULL}.
}
\seealso{
 \code{\link{add_labels}}
}
\examples{
dat = tibble::tibble(
id = 1:40,
x = haven::labelled(rep(0:1, 20), labels=c(No=0, Yes=1), label="Are you happy?"),
y = haven::labelled(rep(c(1:3, 99), 10), labels=c(Small=1, Medium=2, Large=3, Unkown=99), label="Land size")
)

extract_labels(dat)

}
