setClassUnion("dataframeOrNULL", c("data.frame", "list", "NULL"))
setClassUnion("numericOrNULL", c("numeric", "NULL"))
setClassUnion("characterOrNULL", c("character", "NULL"))
setClassUnion("logicalOrNULL", c("logical", "NULL"))
setClassUnion("matrixOrNULL", c("matrix", "NULL"))
setClassUnion("listOrNULL", c("list", "NULL"))
setClassUnion("factorOrNULL", c("factor", "NULL"))
setClassUnion("sdcmicroOrNULL", c("NULL"))


#' Create an S4 object that store information about the survey
#'
#' @slot wkspName characterOrNULL.
#' @slot organizer characterOrNULL.
#' @slot riskAnalysisDir characterOrNULL.
#' @slot SdcObjDir characterOrNULL.
#'
#' @return
#' @export
#'
#' @examples
methods::setClass(
  Class              = "agriswksp",
  contains = "agrisvy",
  representation     = representation(
    wkspName         = "characterOrNULL",
    subtitle         = "characterOrNULL",
    organizer        = "characterOrNULL",
    riskAnalysisDir  = "characterOrNULL",
    SdcObjDir        = "characterOrNULL"
  ),
  prototype = prototype(
    wkspName         = "Workshop",
    organizer        = "Organizer",
    subtitle         ="",
    riskAnalysisDir  = NULL,
    SdcObjDir        = NULL
  ),
  validity = function(object) {
    if (length(object@wkspName) > 1) {
      stop("enter a character vector of length 1")
    }
  }
)


#' Title
#'
#' @param agrisvy an \code{agrisvy} object
#'
#' @return
#' @export
#'
#' @examples
setMethod("show",signature="agrisvy",function(object){

  cat(is(object)[[1]],"\n",
      "Survey name: ",object@svyName,"\n")

  cat(is(object)[[1]],"\n",
      "Number of datasets: ",
      length(list.files(DataPath(object),
                        pattern = object@type,recursive = TRUE))
      ,"\n")

  showTree(object)

})




#' Create an AGRIS workshop object
#'
#' @param wkspName
#' @param organizer
#' @param language
#' @param workingDir
#' @param path
#' @param type
#'
#' @return
#' @export
#'
#' @examples
createAgriwksp <- function(wkspName        = "[Workshop name]",
                           organizer       = "[Organizer]",
                          language         = "en",
                          subtitle         ="",
                          workingDir       = NULL,
                          path             = NULL,
                          type             = NULL) {
  stopifnot(!is.null(path))
  stopifnot(!is.null(type))
  stopifnot(!is.null(workingDir))
  stopifnot(dir.exists(path) == TRUE)


  obj <- new("agriswksp")

  obj@wkspName  <- wkspName
  obj@organizer <- organizer
  obj@subtitle <- subtitle
  obj@language  <- language
  obj@workingDir<- workingDir
  obj@path      <- path
  obj@type      <- type

  if(obj@language=="en") {
  obj@varClassDir      <- "01. Variable classification"
  obj@preProcScriptDir <- "02. Pre-processing scripts"
  obj@preprocDataDir   <- "03. Pre-processed data"
  obj@riskAnalysisDir  <- "04. SDC risk analysis scripts"
  obj@SdcObjDir        <- "05. SDC object files"
  obj@anoScriptDir    <- "06. Anonymization scripts"
  obj@fileDesDir       <- "07. Files description"
  obj@anoreportDir     <- "08. Anonymization reports"
  obj@anoDataDir       <- "09. Anonymized"
  }

  if(obj@language=="fr") {
    obj@varClassDir      <- "01. Classification des variables"
    obj@preProcScriptDir <- "02. Scripts de pre-traitement"
    obj@preprocDataDir   <- "03. Donnees pre-traitees"
    obj@riskAnalysisDir  <- "04. Fichiers d_analyse du risk"
    obj@SdcObjDir        <- "05. Fichiers object SDC"
    obj@anoScriptDir    <- "06. Scripts anonymization"
    obj@fileDesDir       <- "07. Description des fichiers"
    obj@anoreportDir     <- "08. Rapport anonymization"
    obj@anoDataDir       <- "09. Donnees anonymisees"
  }

  if(obj@language=="es") {
    obj@varClassDir      <- "01. Variable classification"
    obj@preProcScriptDir <- "02. Pre-processing scripts"
    obj@preprocDataDir   <- "03. Pre-processed data"
    obj@riskAnalysisDir  <- "04. SDC risk analysis scripts"
    obj@SdcObjDir        <- "05. SDC object files"
    obj@anoScriptDir    <- "06. Anonymization scripts"
    obj@fileDesDir       <- "07. Files description"
    obj@anoReportDir     <- "08. Anonymization reports"
    obj@anoDataDir       <- "09. Anonymized"
  }
  obj
}



setGeneric("riskAnaDir", function(obj) standardGeneric("riskAnaDir"))

#' give the complete path of the preprocDataDir directory
#'
#' @param agrisvy
#'
#' @return
#' @export
#'
#' @examples
setMethod("riskAnaDir",
          signature = "agriswksp", definition =
            function(obj) {
              file.path(obj@riskAnalysisDir)
            }
)
