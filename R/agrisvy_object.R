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
      file.path(obj@varClassDir)
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
      file.path(obj@preProcScriptDir)
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
      file.path(obj@preprocDataDir)
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
      file.path(obj@anoScriptDir)
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
      file.path(obj@anoreportDir)
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
      file.path(obj@anoDataDir)
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
      file.path(obj@fileDesDir)
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
      file.path(obj@infoLossReport)
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
      file.path(obj@tempfileDir)
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
      file.path(obj@aobDir)
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
      file.path(obj@path)
    }
)


#' Put all data files in a list
#'
#' @param path path to data folder
#' @param type extension of the data
#'
#' @return
#' @importFrom dplyr %>% filter
#' @export
#'
#' @examples
genDataList <- function(path,type){

  # path=anoDataDir(agrisvy)

  d=unlist(strsplit(path[1],"/"))
  d=gsub(" ","_",d[length(d)])

  pattern=path[2]

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
    res=lapply(df$path,read_dta)
    names(res)=df$file_name
    return(res)
  })
  names(data_list)=unique_wb

return(data_list)
}



#' Archive the anonymized data inside an agrisvy object
#'
#' @param agrisvy an agrisvy object
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



#' Archive the clean data inside an agrisvy object
#'
#' @param agrisvy
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


#' Archive the preprocessed data inside an agrisvy object
#'
#' @param agrisvy
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
