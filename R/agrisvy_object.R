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
#' @slot svyName characterOrNULL.
#' @slot author characterOrNULL.
#' @slot language characterOrNULL.
#' @slot path characterOrNULL.
#' @slot varClassDir characterOrNULL.
#' @slot preProcScriptDir characterOrNULL.
#' @slot preprocDataDir characterOrNULL.
#' @slot anoScriptDir characterOrNULL.
#' @slot anoDataDir characterOrNULL.
#' @slot anoreportDir characterOrNULL.
#' @slot fileDesDir characterOrNULL.
#' @slot infoLossReport characterOrNULL.
#' @slot tempfileDir characterOrNULL.
#' @slot aobDir characterOrNULL.
#' @slot rawData dataframeOrNULL.
#' @slot cleanData dataframeOrNULL.
#' @slot proData dataframeOrNULL.
#' @slot anoData dataframeOrNULL.
#' @slot workingDir characterOrNULL
#' @slot type microdata extension
#'
#' @return
#' @export
#'
#' @examples
methods::setClass(
  Class              = "agrisvy",
  representation     = representation(
    svyName          = "characterOrNULL",
    author           = "characterOrNULL",
    language         = "characterOrNULL",
    workingDir       = "characterOrNULL",
    path             = "characterOrNULL",
    type             = "characterOrNULL",
    varClassDir      = "characterOrNULL",
    preProcScriptDir = "characterOrNULL",
    preprocDataDir   = "characterOrNULL",
    anoScriptDir     = "characterOrNULL",
    anoDataDir       = "characterOrNULL",
    anoreportDir     = "characterOrNULL",
    fileDesDir       = "characterOrNULL",
    infoLossReport   = "characterOrNULL",
    tempfileDir      = "characterOrNULL",
    aobDir           = "characterOrNULL",
    rawData          = "dataframeOrNULL",
    cleanData        = "dataframeOrNULL",
    proData          = "dataframeOrNULL",
    anoData          = "dataframeOrNULL"
  ),
  prototype = prototype(
    svyName          = "[Survey name]",
    author           = "[Author]",
    language         = "en",
    workingDir       = NULL,
    path             = NULL,
    type             = NULL,
    varClassDir      = "01_Variable classification",
    preProcScriptDir = "02_Pre-processing scripts",
    preprocDataDir   = "03_Pre-processed data",
    anoScriptDir     = "04_Anonymization report",
    anoDataDir       = "04_Anonymization scripts",
    anoreportDir     = "06_Anonymized data",
    fileDesDir       = "07_Files description",
    infoLossReport   = "08_Information loss report",
    tempfileDir      = "09_Temporary_files",
    aobDir           = "10_Miscellaneous",
    rawData          = NULL,
    cleanData        = NULL,
    proData          = NULL,
    anoData          = NULL
  ),
  validity = function(object) {
    if (length(object@svyName) > 1) {
      stop("message")
    }
  }
)


#' Print agrisvy object
#'
#' @param agrisvy
#'
#' @return
#' @export
#'
#' @examples
setMethod("show",signature="agrisvy",function(obj){

  cat(is(obj)[[1]],"\n",
      "Survey name: ",obj@svyName,"\n")

  cat(is(obj)[[1]],"\n",
      "Number of datasets: ",
      length(list.files(DataPath(obj),
                        pattern = obj@type,recursive = TRUE))
      ,"\n")

  showTree(obj)

})


#' Create an agrisvy object
#'
#' @param svyName name of the survey
#' @param author author (institution)
#' @param language language (display in predefined scripts)
#' @param workingDir working directory (where the folders will be created)
#' @param path path do the microdata of the survey
#' @param type extension of the micro data example ".dta" for stata, ".sav" for SPSS
#' @param varClassDir name of the variable classification folder
#' @param preProcScriptDir name of the pre-processing scripts folder
#' @param preprocDataDir name of the preprocessed data folder
#' @param anoScriptDir name of the anonymization scripts folder
#' @param anoDataDir name of the final anonymized data folder
#' @param anoreportDir name of the anonymization report folder
#' @param fileDesDir name of the file description folder
#' @param infoLossReport name of the information loss report folder
#' @param tempfileDir name of the temporary file folder
#' @param aobDir name of the miscellenous folder
#'
#' @return
#' a agrisvy object
#'  @importFrom methods new
#' @export
#'
#' @examples
createAgrisvy <- function(svyName          = "[Survey name]",
                          author           = "[Author]",
                          language         = "en",
                          workingDir       = NULL,
                          path             = NULL,
                          type             = NULL,
                          varClassDir      = "01_Variable classification",
                          preProcScriptDir = "02_Pre-processing scripts",
                          preprocDataDir   = "03_Pre-processed data",
                          anoScriptDir     = "04_Anonymization report",
                          anoDataDir       = "04_Anonymization scripts",
                          anoreportDir     = "06_Anonymized data",
                          fileDesDir       = "07_Files description",
                          infoLossReport   = "08_Information loss report",
                          tempfileDir      = "09_Temporary_files",
                          aobDir           = "10_Miscellaneous") {
  stopifnot(!is.null(path))
  stopifnot(!is.null(type))
  stopifnot(!is.null(workingDir))
  stopifnot(dir.exists(file.path(workingDir, path)) == TRUE)


  obj <- new("agrisvy")

  obj@svyName          <- svyName
  obj@author           <- author
  obj@language         <- language
  obj@workingDir       <- workingDir
  obj@path             <- path
  obj@type             <- type
  obj@varClassDir      <- varClassDir
  obj@preProcScriptDir <- preProcScriptDir
  obj@preprocDataDir   <- preprocDataDir
  obj@anoScriptDir     <- anoScriptDir
  obj@anoreportDir     <- anoreportDir
  obj@anoDataDir       <- anoDataDir
  obj@fileDesDir       <- fileDesDir
  obj@infoLossReport   <- infoLossReport
  obj@tempfileDir      <- tempfileDir
  obj@aobDir           <- aobDir

  obj
}


setGeneric("varClassDir", function(obj) standardGeneric("varClassDir"))

#' give the complete path of the varClassDir directory
#'
#' @param agrisvy
#'
#' @return
#' character
#' @export
#'
#' @examples
setMethod("varClassDir",
  signature = "agrisvy", definition =
    function(obj) {
      file.path(obj@workingDir, obj@varClassDir)
    }
)


setGeneric("preProcScriptDir", function(obj) standardGeneric("preProcScriptDir"))

#' give the complete path of the preProcScriptDir directory
#'
#' @param agrisvy
#'
#' @return
#' character
#' @export
#'
#' @examples
setMethod("preProcScriptDir",
  signature = "agrisvy", definition =
    function(obj) {
      file.path(obj@workingDir, obj@preProcScriptDir)
    }
)


setGeneric("preprocDataDir", function(obj) standardGeneric("preprocDataDir"))

#' give the complete path of the preprocDataDir directory
#'
#' @param agrisvy
#'
#' @return
#' @export
#'
#' @examples
setMethod("preprocDataDir",
  signature = "agrisvy", definition =
    function(obj) {
      file.path(obj@workingDir, obj@preprocDataDir)
    }
)



setGeneric("anoScriptDir", function(obj) standardGeneric("anoScriptDir"))

#' Give the complete path of the anoScriptDir directory
#'
#' @param agrisvy
#'
#' @return
#' character
#' @export
#'
#' @examples
setMethod("anoScriptDir",
  signature = "agrisvy", definition =
    function(obj) {
      file.path(obj@workingDir, obj@anoScriptDir)
    }
)



setGeneric("anoreportDir", function(obj) standardGeneric("anoreportDir"))

#' Give the complete path of the anoreportDir directory
#'
#' @param agrisvy
#'
#' @return
#' @export
#'
#' @examples
setMethod("anoreportDir",
  signature = "agrisvy", definition =
    function(obj) {
      file.path(obj@workingDir, obj@anoreportDir)
    }
)



setGeneric("anoDataDir", function(obj) standardGeneric("anoDataDir"))

#' Goive the complete path of the anoDataDir directory
#'
#' @param agrisvy
#'
#' @return
#' @export
#'
#' @examples
setMethod("anoDataDir",
  signature = "agrisvy", definition =
    function(obj) {
      file.path(obj@workingDir, obj@anoDataDir)
    }
)


setGeneric("fileDesDir", function(obj) standardGeneric("fileDesDir"))

#' Give the complete path of the fileDesDir directory
#'
#' @param agrisvy
#'
#' @return
#' @export
#'
#' @examples
setMethod("fileDesDir",
  signature = "agrisvy", definition =
    function(obj) {
      file.path(obj@workingDir, obj@fileDesDir)
    }
)



setGeneric("infoLossReport", function(obj) standardGeneric("infoLossReport"))

#' Give the complete path of the infoLossReport directory
#'
#' @param agrisvy
#'
#' @return
#' @export
#'
#' @examples
setMethod("infoLossReport",
  signature = "agrisvy", definition =
    function(obj) {
      file.path(obj@workingDir, obj@infoLossReport)
    }
)



setGeneric("tempfileDir", function(obj) standardGeneric("tempfileDir"))

#' Give the complete path of the tempfileDir directory
#'
#' @param agrisvy
#'
#' @return
#' @export
#'
#' @examples
setMethod("tempfileDir",
  signature = "agrisvy", definition =
    function(obj) {
      file.path(obj@workingDir, obj@tempfileDir)
    }
)



setGeneric("aobDir", function(obj) standardGeneric("aobDir"))

#' Give the complete path of the aobDir directory
#'
#' @param agrisvy
#'
#' @return
#' @export
#'
#' @examples
setMethod("aobDir",
  signature = "agrisvy", definition =
    function(obj) {
      file.path(obj@workingDir, obj@aobDir)
    }
)


setGeneric("DataPath", function(obj) standardGeneric("DataPath"))

#' Give the complete path to the data to be anonymized
#'
#' @param agrisvy
#'
#' @return
#' character
#' @export
#'
#' @examples
setMethod("DataPath",
  signature = "agrisvy", definition =
    function(obj) {
      file.path(obj@workingDir, obj@path)
    }
)
