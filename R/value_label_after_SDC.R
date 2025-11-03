
############ utility functions

stopf = function(fmt, ..., call.=TRUE, domain=NULL) {
  stop(gettextf(fmt=fmt, ...), call.=call., domain=domain)
}

toString2 = function(x, n=2L) {   #
  if(!length(x)) return("")
  elts = head(x, n=n)
  ans = paste0("[", toString(elts), if(length(x)>n) ", ...", "]")
  ans
}

isNamedList = function(x) {
  is.list(x) && !is.null(names(x)) && anyDuplicated(names(x))==0L && all(nzchar(names(x)))
}


############ function for adding a value label to ONE variable

to_vlabelX = function(x,
                      vallabels,
                      varlabel=NULL,
                      levels=c("labels", "values"),
                      droplevels=c("none", "unused")) {
  # x: x is factor
  # vallabels: named vector of value label
  # levels: are levels of x labels or values? compare as_factor(y) vs as.factor(y) where y is haven_labelled to understand

  if(!is.factor(x)) {
    stopf("'x' should be a factor variable.")
  }

  if(!length(vallabels) || !is.numeric(vallabels)) {
    stopf("'vallabels' should be a non-empty numeric vector.")
  }

  if(!(is.null(varlabel) || is.character(varlabel) && length(varlabel)==1)) {
    stopf("'varlabel' should be NULL or a single-length character vector.")
  }

  vlbl = as.double(unname(vallabels))             # in case it is of type integer
  nvlbl = names(vallabels)

  if(anyDuplicated(vlbl) || is.null(nvlbl) || any(!nzchar(nvlbl)) || anyDuplicated(nvlbl)) {
    stopf("'vallabels' should be a named numeric vector with no duplicated name or value, and no empty name.")
  }

  lvls = levels(x)

  if(!length(lvls)) {
    ans = haven::labelled(double(), vallabels)
    if(!is.null(varlabel)) attr(ans, "label") = varlabel
    return(ans)
  }

  if(match.arg(levels)=="values") {

    lvls = as.numeric(lvls)

    if(!all(lvls %in% vlbl)) {
      if(match.arg(droplevels)=="none") {
        stopf("Some values in 'levels(x)' do not exist in 'vallabels' argument. They can be excluded by explicitely specifying 'droplevels=\"unused\"'.")
      }

      lvls = lvls[lvls %in% vlbl]

      if(!length(lvls)) {
        ans = haven::labelled(double(), vallabels)
        if(!is.null(varlabel)) attr(ans, "label") = varlabel
        return(ans)
      }
    }

    # reoder labels as levels. this case is trivial
    val = lvls

  } else {

    # reoder labels as levels (for fast subsetting).
    if(!anyNA(nvlbl) && !anyNA(lvls) && length(nvlbl)==length(lvls) && all(nvlbl==lvls)) {

      val = vlbl

    } else {

      if(!all(lvls %in% nvlbl)) {

        if(match.arg(droplevels)=="none") {
          stopf("Some values in 'levels(x)' do not exist in 'vallabels' argument. They can be excluded by explicitely specifying 'droplevels=\"unused\"'.")
        }

        idx = lvls %in% nvlbl
        lvls = lvls[idx]
        vlbl = vlbl[idx]

        if(!length(lvls)) {
          ans = haven::labelled(double(), vallabels)
          if(!is.null(varlabel)) attr(ans, "label") = varlabel
          return(ans)
        }
      }

      val = vlbl[match(lvls, nvlbl)] # values corresponding to the levels are reordered as the levels
    }
  }

  xval = val[as.integer(x)]

  ans = haven::labelled(xval, labels=vallabels)
  if(!is.null(varlabel)) attr(ans, "label") = varlabel
  return(ans)
}

############ function for extracting value labels

#' Title
#'
#' @param dat
#' @param cols
#'
#' @return
#' @export
#'
#' @examples
extract_labels = function(dat, cols=names(dat)) {

  if(!is.data.frame(dat)) stopf("'dat' should be a data.frame.")

  if(!missing(cols)) {

    if(!is.atomic(cols) || anyDuplicated(cols)!=0L) {
      stopf("Argument 'cols' should be an atomic a vector specifying the column names or indices.")
    }

    if(!is.character(cols)) {
      cols = names(dat)[cols]
    } else if(!all(idx <- cols %in% names(dat))) {
      stopf("Some variables %s not found in the data.", toString2(cols[!idx]))
    }

    dat = .subset(dat, cols)
  }

  out = list(varlabels=lapply(dat, attr, "label", TRUE),
             vallabels=lapply(dat, attr, "labels", TRUE),
             datalabel=attr(dat, "label", TRUE))

  out
}


############ function for adding labels to factors in a data.frame
#' Title
#'
#' @param dat
#' @param list_labels
#' @param skip_absent
#' @param levels
#' @param droplevels
#'
#' @return
#' @export
#'
#' @examples
add_labels = function(dat, list_labels, skip_absent=FALSE, levels=c("labels", "values"), droplevels=c("none", "unused")) {

  if(!is.data.frame(dat)) stopf("'dat' should be a data.frame.")

  if(is.null(names(list_labels)) || any(!names(list_labels) %in% c("varlabels", "vallabels", "datalabel"))) {
    stopf("'list_labels' should be a named list where the name 'varlabels' is a name list of variable labels (NULL if no label), and/or 'vallabels' is a named list of value labels, and/or 'datalabel' is a character string specifying the data set label. Only these three names are allowed.")
  }

  varlabels = list_labels$varlabels
  vallabels = list_labels$vallabels
  datalabel = list_labels$datalabel
  vrnames = names(varlabels)
  vlnames = names(vallabels)
  dfnames = names(dat)

  if(skip_absent) {
    varlabels = varlabels[intersect(dfnames, vrnames)]
    vallabels = vallabels[intersect(dfnames, vlnames)]
  } else {

    if(all(idx <- !names(varlabels) %in% names(dat))) {
      stopf("Some names %s in 'names(list_labels$varlabels)' not found in 'dat' names.", toString2(vrnames[idx]))
    }

    if(all(idx <- !names(vallabels) %in% names(dat))) {
      stopf("Some names %s in 'names(list_labels$vallabels)' not found in 'dat' names.", toString2(vlnames[idx]))
    }
  }

  if(!(is.null(datalabel) || is.character(datalabel) && length(datalabel)==1L)) {
    stopf("'list_labels$datalabel' should be a one-length character vector or NULL.")
  }

    fctrs = dfnames[vapply(dat, is.factor, NA, USE.NAMES=FALSE)]
    fctrs = fctrs[fctrs %in% vlnames]
    for(vn in fctrs) dat[[vn]] = to_vlabelX(dat[[vn]], vallabels=vallabels[[vn]], levels=match.arg(levels), droplevels=match.arg(droplevels))

  for(vn in vrnames) attr(dat[[vn]], "label") = varlabels[[vn]]

  if(length(datalabel)) attr(dat, "label") = datalabel

  dat
}


# 1- extract_labels
# 2- add_labels

# if (interactive() && "haven" %in% rownames(installed.packages())) {
#
#   perm = haven::read_dta("permanent_crop.dta")
#
#   # extract labels
#   vll = extract_labels(perm)
#
#   # haven_labelled columns
#   cols = which(vapply(perm, haven::is.labelled, NA))
#
#   # levels=labels
#   perm[cols] = lapply(perm[cols], haven::as_factor)
#   lapply(perm[cols], head)
#   perm = add_labels(perm, vll)
#
#   # lavels=values
#   perm[cols] = lapply(perm[cols], as.factor)
#   lapply(perm[cols], head)
#   perm = add_labels(perm, vll, levels="values")
#
#   #
#   # if a variable has no label, should we keep the older label?
#   # is it supposed to be called by script or on all scripts?
#   # what if we provide allow list_labels to be also a data.frame (Excel labels)
#
#   vll = extract_labels(perm, cols=c("crop_id", "region__id2"))
#   add_labels(perm, vll, levels="values")
# }
