\name{add_labels}
\alias{add_labels}
\title{Convert factor variables to labelled variables}
\description{
   \code{add_labels} is used to convert factor variables to \code{haven_labelled} variables.

   Variables of class \code{haven_labelled} are usually created using \code{\link[haven:read_dta]{haven::read_*()}} family of functions.
}
\usage{
add_labels(
  dat,
  list_labels,
  skip_absent=FALSE,
  levels=c("labels", "values"),
  droplevels=c("none", "unused")
)
}
\arguments{
  \item{dat}{ A data frame or data frame extension (e.g. a data.table, tibble, etc.). }

  \item{list_labels}{
    A named list of label type. Three types of labels are supported: data label, variable label, and value label. These three types of labels are expected to be named \code{varlabels}, \code{vallabels}, and \code{datalabel} respectively. At least one of these labels should be provided when calling \code{add_labels}.
    \itemize{
      \item \code{varlabels}: a named list containing the variables' labels. The names specify the variables to be labelled. If a character vector is provided, it is coerced to list and should be named.

      \item \code{vallabels}: a named list containing the value labels. The names specify the variables to be labelled and the elements are named vectors of value label.

      \item \code{datalabel}: A one-length character vector specifying the data set label.

      The actual argument to \code{\link{list_labels}} can be easily created using \code{extract_labels} function after reading a data set into R (with \code{haven:read_*()}).
    }

  }
  \item{skip_absent}{ A \code{TRUE} or \code{FALSE} (default). Indicate if variable names in \code{dat} but not in \code{list_labels} and vice-versa should be skipped. When \code{FALSE}, any variable name used in \code{list_labels} but not in \code{dat} throws an error. }

  \item{levels}{ Specify whether the factor levels contain the labels or the values of the value label in  \code{list_labels}. Use \code{"labels"} (default) if the factor levels contain the labels and \code{"values"} if they contain the values }

  \item{droplevels}{ Specify if factor levels not in \code{list_labels} should be dropped. It default to \code{"none"}; that is, no level should be dropped; and if some levels exist in \code{dat} but not in \code{list_labels}, an error is thrown. \code{"unused"} can be used to drop any factor level not in \code{list_labels}, which become missing in the variables after convertion. }
}

\value{
    A data frame or data frame extension with the specified factor variables converted into haven_labelled variables; and the data set and variables labels updated.
}
\seealso{
 \code{\link{extract_labels}}
}

\examples{
# Create sample data

dat = tibble::tibble(
id = 1:40,
x = haven::labelled(rep(0:1, 20), labels=c(No=0, Yes=1), label="Are you happy?"),
y = haven::labelled(rep(c(1:3, 99), 10), labels=c(Small=1, Medium=2, Large=3, Unkown=99), label="Land size")
)

initial_labels = extract_labels(dat)

# convert haven_labelled to factors using as_factor function: --> factor levels are initial labels
dat1 = dat
dat1[2:3] = lapply(dat[2:3], haven::as_factor)
dat1
dat1$x                 # levels initial labels

# convert haven_labelled to factors using as.factor function: --> factor levels are initial values
dat2 = dat
dat2[2:3] = lapply(dat[2:3], as.factor)
dat2
dat2$x                 # levels initial values

# convert factors back to haven_labelled
add_labels(dat1, list_labels=initial_labels)
add_labels(dat2, list_labels=initial_labels, levels="values")    #

# update initial_labels to add/modify data set label
initial_labels$datalabel = "Data set description"
dat1 = add_labels(dat1, list_labels=initial_labels)
attr(dat1, "label", TRUE)

}
