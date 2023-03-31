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
    anoScriptDir     = "04_Anonymization scripts",
    anoDataDir       = "05_Anonymized data",
    anoreportDir     = "06_Anonymization report",
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


#' Create an \code{agrisvy} object
#'
#' An \code{agrisvy} object are used to store useful information that help generate files, folders
#' scripts but most importantly customize pre-populated scripts and anonymization report
#' template. It is used as input by some bulk run function for pre-processing, anonymization
#' archiving, etc.
#'
#' @param svyName Name of the survey (will be used in the code comments and to customize the anonymization report)
#' @param author Author or institution (ex. \code{"FAO"})
#' @param language Language (used to customize the language of folder names
#'  and the anonymization report). The possible values are \code{"en"} for English,
#'  \code{"fr"} for French and \code{"es"} for Spanish. The default value is \code{"en"}.
#' @param workingDir path to the working directory (where the anonymization working folder will be generated).
#' It is recommended for this folder to be empty.
#' @param type Format of the microdata. The possible values are \code{".dta"} for STATA, \code{".sav"} for SPSS
#' @param dataDir path to the folder where the data to be anonymized are located
#'
#' @return an \code{agrisvy} object
#' @importFrom methods new
#' @export
#'
#' @examples
  createAgrisvy <- function(svyName          = "[Survey name]",
                            author           = "[Author]",
                            language         = "en",
                            workingDir       = NULL,
                            dataDir             = NULL,
                            type             = NULL) {
    stopifnot(!is.null(path))
    stopifnot(!is.null(type))
    stopifnot(!is.null(workingDir))
    stopifnot(dir.exists(path) == TRUE)


    obj <- new("agrisvy")

    obj@svyName          <- svyName
    obj@author           <- author
    obj@language         <- language
    obj@workingDir       <- workingDir
    obj@path             <- path
    obj@type             <- type

    if(obj@language=="en") {
      obj@varClassDir      <- "01.Variable classification"
      obj@preProcScriptDir <- "02.Pre-processing scripts"
      obj@preprocDataDir   <- "03.Pre-processed data"
      obj@anoScriptDir     <- "04.Anonymization scripts"
      obj@anoreportDir     <- "06.Anonymization report"
      obj@anoDataDir       <- "05.Anonymized data"
      obj@fileDesDir       <- "07.Files description"
      obj@infoLossReport   <- "08.Information loss report"
      obj@tempfileDir      <- "09.Temporary files"
      obj@aobDir           <- "10.Miscellaneous"
    }

    if(obj@language=="fr") {
      obj@varClassDir      <- "01.Classification des variables"
      obj@preProcScriptDir <- "02.Scripts de pre-traitement"
      obj@preprocDataDir   <- "03.Donnees pre-traitees"
      obj@anoScriptDir     <- "04.Scripts anonymization"
      obj@anoreportDir     <- "06.Raport anonymization"
      obj@anoDataDir       <- "05.Donnees anonymisees"
      obj@fileDesDir       <- "07.Description des fichiers"
      obj@infoLossReport   <- "08.Raport sur la perte d_information"
      obj@tempfileDir      <- "09.Fichiers temporaires"
      obj@aobDir           <- "10.Divers fichiers"
    }

    if(obj@language=="es") {
      obj@varClassDir      <- "01.Clasificacion de variables"
      obj@preProcScriptDir <- "02.Guiones de preprocesamiento"
      obj@preprocDataDir   <- "03.Datos preprocesados"
      obj@anoScriptDir     <- "04.Guiones de anonimizacion"
      obj@anoreportDir     <- "06.Informe de anonimizacion"
      obj@anoDataDir       <- "05.Datos anonimizados"
      obj@fileDesDir       <- "07.Descripcion de archivos"
      obj@infoLossReport   <- "08.Informe de perdida de informacion"
      obj@tempfileDir      <- "09.Archivos temporales"
      obj@aobDir           <- "10.varios archivos"
    }
    obj
  }

#' Give the path to the variable classification folder
#'
#' @param obj an \code{agrisvy} object
#'
#' @return a character
#' @export
#'
#' @examples

  varClassDir <- function(obj){
    varClassDirX(obj)
  }

setGeneric("varClassDirX", function(obj) standardGeneric("varClassDirX"))

setMethod(f="varClassDirX",
  signature = "agrisvy", definition =
    function(obj) {
      file.path(obj@varClassDir)
    }
)


#' Give the path to the folder containing the scripts of pre-processing
#'
#' @param obj an \code{agrisvy} object
#'
#' @return a character
#' @export
#'
#' @examples
  preProcScriptDir <- function(obj){
    preProcScriptDirX(obj)
  }

setGeneric("preProcScriptDirX", function(obj) standardGeneric("preProcScriptDirX"))

setMethod("preProcScriptDirX",
  signature = "agrisvy", definition =
    function(obj) {
      file.path(obj@preProcScriptDir)
    }
)


#' Give the path to folder containing the pre-processed data
#'
#' @param obj an \code{agrisvy} object
#'
#' @return a character
#' @export
#'
#' @examples
preprocDataDir <- function(obj){
  preprocDataDirX(obj)
}


setGeneric("preprocDataDirX", function(obj) standardGeneric("preprocDataDirX"))

setMethod("preprocDataDirX",
  signature = "agrisvy", definition =
    function(obj) {
      file.path(obj@preprocDataDir)
    }
)

#' Give the path to the folder coning the anonymization scripts
#'
#' @param obj an \code{agrisvy} object
#'
#' @return a character
#' @export
#'
#' @examples
anoScriptDir <- function(obj){
  anoScriptDirX(obj)
}

setGeneric("anoScriptDirX", function(obj) standardGeneric("anoScriptDirX"))

setMethod("anoScriptDirX",
  signature = "agrisvy", definition =
    function(obj) {
      file.path(obj@anoScriptDir)
    }
)


#' Give the path to the folder containg the anonymization report
#'
#' @param obj an \code{agrisvy} object
#'
#' @return a character
#' @export
#'
#' @examples

anoreportDir <- function(obj){
  anoreportDirX(obj)
}

setGeneric("anoreportDirX", function(obj) standardGeneric("anoreportDirX"))

setMethod("anoreportDirX",
  signature = "agrisvy", definition =
    function(obj) {
      file.path(obj@anoreportDir)
    }
)


#' Give the path to the folder containing the anonymized data
#'
#' @param obj an \code{agrisvy} object
#'
#' @return a character
#' @export
#'
#' @examples
anoDataDir <- function(obj){
  anoDataDirX(obj)
}
setGeneric("anoDataDirX", function(obj) standardGeneric("anoDataDirX"))

setMethod("anoDataDirX",
  signature = "agrisvy", definition =
    function(obj) {
      file.path(obj@anoDataDir)
    }
)

#' Give the path to the folder containing the files description
#'
#' @param obj an \code{agrisvy} object
#'
#' @return a character
#' @export
#'
#' @examples
fileDesDir <- function(obj) {
  fileDesDirX(obj)
}

setGeneric("fileDesDirX", function(obj) standardGeneric("fileDesDirX"))

setMethod("fileDesDirX",
  signature = "agrisvy", definition =
    function(obj) {
      file.path(obj@fileDesDir)
    }
)

#' Give the path to the folder containing the information loss report
#'
#' @param obj an \code{agrisvy} object
#'
#' @return a character
#' @export
#'
#' @examples
infoLossReport <- function(obj){
  infoLossReportX(obj)
}
setGeneric("infoLossReportX", function(obj) standardGeneric("infoLossReportX"))

setMethod("infoLossReportX",
  signature = "agrisvy", definition =
    function(obj) {
      file.path(obj@infoLossReport)
    }
)


#' Give the path of the folder containing temporary files generated during the anonymization process
#'
#' @param obj an \code{agrisvy} object
#'
#' @return a character
#' @export
#'
#' @examples
tempfileDir <- function(obj){
  tempfileDirX(obj)
}

setGeneric("tempfileDirX", function(obj) standardGeneric("tempfileDirX"))

setMethod("tempfileDirX",
  signature = "agrisvy", definition =
    function(obj) {
      file.path(obj@tempfileDir)
    }
)

#' Give the path of the folder any other business (aob)
#'
#' @param obj an \code{agrisvy} object
#'
#' @return a character
#' @export
#'
#' @examples

aobDir <- function(obj){
  aobDirX(obj)
}

setGeneric("aobDir", function(obj) standardGeneric("aobDir"))

setMethod("aobDir",
  signature = "agrisvy", definition =
    function(obj) {
      file.path(obj@aobDir)
    }
)

#' Give the path of the folder containing the microdata to be anonymized
#'
#' @param obj an \code{agrisvy} object
#'
#' @return a character
#' @export
#'
#' @examples
DataPath <- function(obj){
  DataPathX(obj)
}
setGeneric("DataPathX", function(obj) standardGeneric("DataPathX"))

setMethod("DataPathX",
  signature = "agrisvy", definition =
    function(obj) {
      file.path(obj@path)
    }
)


#' Put the different microdata in a list
#'
#' @param path Path to the folder containing the data
#' @param type type of the data. The possible values are \code{".dta"} for STATA and \code{".sav"} for SPSS
#'
#' @return a list
#' @importFrom dplyr %>% filter
#' @importFrom haven read_dta
#' @importFrom haven read_sav
#' @export
#'
#' @examples


genDataList <- function(path,type){

  # path=anoDataDir(agrisvy)

  d=unlist(strsplit(path[1],"/"))
  d=gsub(" ","_",d[length(d)])

  data_files = list.files(path, pattern = type, recursive = TRUE)

  x <-lapply(strsplit(data_files, "/"), function(z) as.data.frame(t(z)))
  x1 <- rbind.fill(x)

  wb = lapply(x, function(z) {
    z=z[!is.na(z)]
    paste(z[1:length(z) - 1], sep = "", collapse = "_")
  })
  wb=unlist(wb)
  wb[which(wb=="")] <- d

  unique_wb = unique(wb)



  data_summary = data.frame(
    file_name = unlist(lapply(x, function(z) {
      paste(gsub(type, "", z[length(z)]), sep = "", collapse = "_")
    })),
    path = file.path(path, data_files),
    workbook = unlist(wb)
  )

  data_list=lapply(unique_wb, function(x){
    df=data_summary %>% dplyr::filter(workbook==x)
    if(type==".dta") res=lapply(df$path,read_dta)
    if(type==".SAV") res=lapply(df$path,read_sav)
    names(res)=df$file_name
    return(res)
  })
  names(data_list)=unique_wb

return(data_list)
}



#' Archive the anonymized data inside the AGRIS survey object
#'
#' @param agrisvy an \code{agrisvy} object
#'
#' @return
#' @export
#'
#' @examples
ArchiveAnoData=function(agrisvy){

  agrisMsg("ARCHIVING","Anonymized data")

  # https://stackoverflow.com/questions/58332390/r-save-within-a-function-preserve-the-original-inputs-name
  originalName <- deparse(substitute(agrisvy))

  data_list=genDataList(anoDataDir(agrisvy),agrisvy@type)
  agrisvy@anoData=data_list
  assign(originalName, agrisvy,envir = .GlobalEnv)
  #TODO: add a message to mention that the agrisvy has been updated
  saveRDS(agrisvy,as.character(file.path(agrisvy@workingDir,"_R",paste0(originalName,".rds"))))
  source(file.path(agrisvy@workingDir,"_R","_setup.R"))
}



#' Archive the clean data inside an \code{agrisvy} object
#'
#' @param agrisvy an \code{agrisvy} object
#'
#' @return
#' @export
#'
#' @examples
ArchiveCleanData=function(agrisvy){

  agrisMsg("ARCHIVING","Cleaned data")

  originalName <- deparse(substitute(agrisvy))
  data_list=genDataList(DataPath(agrisvy),agrisvy@type)
  agrisvy@cleanData=data_list
  assign(originalName, agrisvy)

  saveRDS(agrisvy,as.character(file.path(agrisvy@workingDir,"_R",paste0(originalName,".rds"))))
  source(file.path(agrisvy@workingDir,"_R","_setup.R"))
}


#' Archive the preprocessed data inside an \code{agrisvy} object
#'
#' @param agrisvy an \code{agrisvy} object
#'
#' @return
#' @export
#'
#' @examples
ArchiveProcData=function(agrisvy){

  agrisMsg("ARCHIVING","pre-processed data")

  originalName <- deparse(substitute(agrisvy))

  data_list=genDataList(preprocDataDir(agrisvy),agrisvy@type)
  agrisvy@proData=data_list
  assign(originalName, agrisvy)

  saveRDS(agrisvy,file.path(agrisvy@workingDir,"_R",paste0(originalName,".rds")))
  source(file.path(agrisvy@workingDir,"_R","_setup.R"))
}
