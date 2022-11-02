setClassUnion("dataframeOrNULL", c("data.frame","list", "NULL"))
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
methods::setClass(Class = "agrisvy",
                  representation = representation(
                    svyName="characterOrNULL",
                    author="characterOrNULL",
                    language="characterOrNULL",
                    workingDir="characterOrNULL",
                    path="characterOrNULL",
                    type="characterOrNULL",
                    varClassDir="characterOrNULL",
                    preProcScriptDir="characterOrNULL",
                    preprocDataDir="characterOrNULL",
                    anoScriptDir="characterOrNULL",
                    anoDataDir="characterOrNULL",
                    anoreportDir="characterOrNULL",
                    fileDesDir="characterOrNULL",
                    infoLossReport="characterOrNULL",
                    tempfileDir="characterOrNULL",
                    aobDir="characterOrNULL",
                    rawData="dataframeOrNULL",
                    cleanData="dataframeOrNULL",
                    proData="dataframeOrNULL",
                    anoData="dataframeOrNULL"
                  ),
                  prototype = prototype(
                    svyName="[Survey name]",
                    author="[Author]",
                    language="en",
                    workingDir=NULL,
                    path=NULL,
                    type=NULL,
                    varClassDir="01_Variable classification",
                    preProcScriptDir="02_Pre-processing scripts",
                    preprocDataDir="03_Pre-processed data",
                    anoScriptDir="04_Anonymization report",
                    anoDataDir="04_Anonymization scripts",
                    anoreportDir="06_Anonymized data",
                    fileDesDir="07_Files description",
                    infoLossReport="08_Information loss report",
                    tempfileDir="09_Temporary_files",
                    aobDir="10_Miscellaneous",
                    rawData=NULL,
                    cleanData=NULL,
                    proData=NULL,
                    anoData=NULL
                  ),
                  validity = function(object) {

                    if(length(object@svyName)>1) {
                      stop("message")
                    }
                  }
)


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
#' @export
#'
#' @examples
createAgrisvy=function(   svyName="[Survey name]",
                          author="[Author]",
                          language="en",
                          workingDir=NULL,
                          path=NULL,
                          type=NULL,
                          varClassDir="01_Variable classification",
                          preProcScriptDir="02_Pre-processing scripts",
                          preprocDataDir="03_Pre-processed data",
                          anoScriptDir="04_Anonymization report",
                          anoDataDir="04_Anonymization scripts",
                          anoreportDir="06_Anonymized data",
                          fileDesDir="07_Files description",
                          infoLossReport="08_Information loss report",
                          tempfileDir="09_Temporary_files",
                          aobDir="10_Miscellaneous"){

  stopifnot(!is.null(path))
  stopifnot(!is.null(type))
  stopifnot(!is.null(workingDir))
  stopifnot(dir.exists(file.path(workingDir,path))==TRUE)


  obj=new("agrisvy")

  obj@svyName           <- svyName
  obj@author            <- author
  obj@language          <- language
  obj@workingDir        <- workingDir
  obj@path              <- path
  obj@type              <- type
  obj@varClassDir       <- varClassDir
  obj@preProcScriptDir  <- preProcScriptDir
  obj@anoScriptDir      <- anoScriptDir
  obj@anoreportDir      <- anoreportDir
  obj@anoDataDir        <- anoDataDir
  obj@fileDesDir        <- fileDesDir
  obj@infoLossReport    <- infoLossReport
  obj@tempfileDir       <- tempfileDir
  obj@aobDir            <- aobDir

  obj
}
