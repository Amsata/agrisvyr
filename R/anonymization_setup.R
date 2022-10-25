#' Create a folder
#'
#' @param directory directory to create
#' @param overwrite if existing directory should be removed
#'
#' @return logical
#'
#'
#' @examples
create_folder=function(directory,overwrite=FALSE){

  if(isTRUE(dir.exists(directory)) & isFALSE(overwrite)) {
    stop(glue::glue("Directory {directory} already exists!"))
  } else {

  if(isTRUE(dir.exists(directory))){

    unlink(directory,recursive = TRUE)
    dir.create(directory)
  } else {

    dir.create(directory)
  }
  }

}

#' Create anonymization working folders
#'
#' @param path directory to create the folders. the defaut is the current
#' working directory
#' @param overwrite should existing folders be overwitten
#'
#' @return logical
#' @export
#'
#' @examples
create_ano_folders <- function(path=getwd(),overwrite=FALSE) {
  folders=c("01_Variable classification","02_Pre-processing scripts",
            "03_Pre-processed data","04_Anonymization scripts",
            "04_Anonymization report","06_Anonymized data",
            "07_Files description","08_Information loss report",
            "09_Temporary_files","10_Miscellaneous","09_Temporary_files/temp_ano")

  purrr::walk(folders,create_folder,overwrite=overwrite)
}


#' Extract label of all variable of a dataset
#'
#' @param fileName dataset name
#'
#' @return
#'
#' @examples
labels <- function(fileName){
  curData <- read_dta(fileName, encoding = 'latin1')
  curLabels <- data.frame(
    "name"  = names(curData),
    "label" = sapply(curData, function(x) attr(x, "label"))  %>% as.character())
  return(curLabels)
}


#' Create an Excel file for variable classification for a given data folder
#'
#' @param data a dataframe containing information on the file tree
#' @param wb_file name the workbook of a data folder
#'
#' @return
#' @import openxlsx
#' @import haven
#' @examples
create_wb <- function(data,wb_file) {


  hd1 = openxlsx::createStyle(
    fontName = "Arial",
    fontSize = 11,
    fontColour = "black",
    numFmt = "GENERAL",
    border = c("top", "bottom", "left", "right"),
    borderColour = getOption("openxlsx.borderColour", "black"),
    borderStyle = getOption("openxlsx.borderStyle", "thin"),
    bgFill = NULL,
    fgFill = "grey",
    halign = "center",
    valign = "center",
    textDecoration = "bold",
    wrapText = TRUE,
    textRotation = NULL,
    indent = NULL,
    locked = NULL,
    hidden = NULL
  )

  hd2 = openxlsx::createStyle(
    fontName = "Arial",
    fontSize = 11,
    fontColour = "black",
    numFmt = "GENERAL",
    border = c("top", "bottom", "left", "right"),
    borderColour = getOption("openxlsx.borderColour", "black"),
    borderStyle = getOption("openxlsx.borderStyle", "thin"),
    bgFill = NULL,
    fgFill = "yellow",
    halign = "center",
    valign = "center",
    textDecoration = "bold",
    wrapText = TRUE,
    textRotation = NULL,
    indent = NULL,
    locked = NULL,
    hidden = NULL
  )


  df = data %>% dplyr::filter(workbook == wb_file)

  fileNames = df$file_name
  wb <- openxlsx::createWorkbook()

  for (i in 1:length(fileNames)) {
    openxlsx::addWorksheet(wb, fileNames[i])
    curDat <- labels(df$path[i])
    curDat <-
      cbind(curDat, data.frame(
        a = "",
        b = "",
        c = "",
        d = ""
      ))
    #colnames(curDat) <- c("Variable name", "Variable label", "Anonymization", "Comments")
    colnames(curDat) <-
      c("Name",
        "Label",
        "Classification",
        "Comments",
        "Anonymization",
        "Questions")

    #prefill know quasi-identifiers like region, zardi etc.
    predef_Q = c("region", "zardi", "sub_region")
    curDat = curDat %>% mutate(Classification = Classification %>%
                                 labelled::recode_if(Name %in% predef_Q, "Q"))

    openxlsx::writeData(wb,
              sheet = fileNames[i],
              x = curDat,
              headerStyle = hd1)
    classification = data.frame(cbind(
      c(
        "Sampling Weight",
        "Direct identifier",
        "quasi identifier",
        "ID variable",
        "To Delete",
        "Sensitive variable",
        "Linked variable",
        "No SDC needed"
      ),
      c("W", "DI", "Q", "ID", "D", "S", "L", "-")
    ))

    names(classification) = c("Classification category", "symbol")
    openxlsx::writeData(
      wb,
      sheet = fileNames[i],
      classification,
      startCol = 8,
      headerStyle = hd2
    )
    openxlsx::setColWidths(
      wb,
      sheet = fileNames[i],
      cols = c(1, 2, 3, 4, 5, 8, 9),
      widths = c(25, 50, 16, 16, 16, 30, 15)
    )
    openxlsx::addStyle(
      wb,
      sheet = fileNames[i],
      style = createStyle(
        fontSize = 9,
        fontName = "Arial",
        halign = "left",
        valign = "center"
      ),
      cols = 1:9,
      rows = 1:nrow(curDat) + 1,
      gridExpand = TRUE,
      stack = TRUE
    )

    openxlsx::addStyle(
      wb,
      sheet = fileNames[i],
      style = createStyle(
        fontSize = 12,
        fontName = "Arial",
        halign = "Center",
        valign = "center",
        textDecoration = "bold",
        fontColour = "#0000FF"
      ),
      cols = 3,
      rows = 1:nrow(curDat) + 1,
      gridExpand = TRUE,
      stack = TRUE
    )

    openxlsx::addStyle(
      wb,
      sheet = fileNames[i],
      style = createStyle(
        fontSize = 10,
        fontName = "Arial",
        halign = "Center",
        valign = "center",
        fontColour = "#000080"
      ),
      cols = 4,
      rows = 1:nrow(curDat) + 1,
      gridExpand = TRUE,
      stack = TRUE
    )

    openxlsx::addStyle(
      wb,
      sheet = fileNames[i],
      style = createStyle(
        fontSize = 11,
        fontName = "Arial",
        halign = "Center",
        valign = "center",
        textDecoration = "bold",
        fontColour = "#008000"
      ),
      cols = 5,
      rows = 1:nrow(curDat) + 1,
      gridExpand = TRUE,
      stack = TRUE
    )

    # Color text in red if variable is classified ad D
    openxlsx::conditionalFormatting(
      wb,
      sheet = fileNames[i],
      cols = 3,
      rows = 1:nrow(curDat) + 1,
      type = "beginsWith",
      rule = "D",
      style = createStyle(
        fontSize = 12,
        fontName = "Arial",
        halign = "Center",
        valign = "center",
        textDecoration = "bold",
        fontColour = "#FF0000"
      )
    )

    # # Color text in red if variable is classified ad D
    # conditionalFormatting(wb,sheet=fileNames[i],
    #                       cols = 3,
    #                       rows = 1:nrow(curDat)+1,  type = "contains",
    #                       rule = "DI",
    #                       style = createStyle(fontSize = 12,fontName = "Arial", halign = "Center",
    #                                           valign = "center", textDecoration = "bold",
    #                                           fontColour = "#FF0000")
    # )

    # Color text in green if variable is classified ad Q
    openxlsx::conditionalFormatting(
      wb,
      sheet = fileNames[i],
      cols = 3,
      rows = 1:nrow(curDat) + 1,
      type = "contains",
      rule = "Q",
      style = createStyle(
        fontSize = 12,
        fontName = "Arial",
        halign = "Center",
        valign = "center",
        textDecoration = "bold",
        fontColour = "#008000"
      )
    )

    # Color text in green if variable is classified ad L
    openxlsx::conditionalFormatting(
      wb,
      sheet = fileNames[i],
      cols = 3,
      rows = 1:nrow(curDat) + 1,
      type = "contains",
      rule = "L",
      style = createStyle(
        fontSize = 12,
        fontName = "Arial",
        halign = "Center",
        valign = "center",
        textDecoration = "bold",
        fontColour = "#008000"
      )
    )

    # Color text in green if variable is classified ad S
    openxlsx::conditionalFormatting(
      wb,
      sheet = fileNames[i],
      cols = 3,
      rows = 1:nrow(curDat) + 1,
      type = "contains",
      rule = "S",
      style = createStyle(
        fontSize = 12,
        fontName = "Arial",
        halign = "Center",
        valign = "center",
        textDecoration = "bold",
        fontColour = "#008000"
      )
    )


  }

  openxlsx::saveWorkbook(wb,
                         paste0("01_Variable classification/", wb_file, "_VarClas.xlsx"),
                         overwrite = TRUE)
}


#' Create Excel file for variable classification for a given data folder
#'
#' @param path_to_data path to data folder
#' @param data_format format of the data (default is stata)
#'
#' @return
#' @importFrom   purrr walk
#' @importFrom  plyr rbind.fill
#' @export
#'
#' @examples
generate_varclas <- function(path_to_data,data_format="stata") {

  if(data_format=="stata") pattern=".dta"

  data_files = list.files(path_to_data, pattern = ".dta$", recursive = TRUE)
  data_files

  x <-lapply(strsplit(data_files, "/"), function(z)as.data.frame(t(z)))
  x1 <- rbind.fill(x)

  wb = lapply(x, function(z) {
    paste(z[1:length(z) - 1], sep = "", collapse = "_")
  })

  unique_wb = unique(unlist(wb))

  data_summary = data.frame(
    file_name = unlist(lapply(x, function(z) {
      paste(gsub(pattern, "", z[length(z)]), sep = "", collapse = "_")
    })),
    path = file.path(path_to_data, data_files),
    workbook = unlist(wb)
  )

  #create the different unique wb

  purrr::walk(unique_wb, function(x){create_wb(data_summary,x)})

}


#' Copy the structure of a folder
#'
#' @param from directory to copy
#' @param to directory where the folder are copied
#'
#' @return
#' @importFrom R.utils copyDirectory
#' @importFrom purrr walk
#' @export
#'
#' @examples
copy_data_folder=function(from,to){

  if(dir.exists(from)==FALSE | dir.exists(to)==FALSE){
    stop("one of the specified directory does not exits")
  }

  if(length(base::list.files(to,recursive = TRUE))!=0) {
    base::unlink(to,recursive = TRUE)
  }
  R.utils::copyDirectory(from,to)
  purrr::walk(file.path(to,list.files(to,recursive = TRUE)),base::file.remove)
}




#' Create a simple  preprocessing R script for a given dataset
#'
#' @param path_to_data  path to the data to be anonymized
#' @param file name of the data file
#' @param pattern extension of the data file
#'
#' @return
#' @importFrom glue glue
#' @export
#'
#' @examples
create_preproc_r <- function(path_to_data,file,pattern) {

  z <- unlist(strsplit(file, "/"))
  file_attributes = list(
    file_name = paste(gsub(pattern, "", z[length(z)]), sep = "", collapse = "_"),
    path = file.path(path_to_data, file),
    r_script = file.path("02_Pre-processing scripts",gsub(paste0(pattern,"$"),"_proc.R",file)),
    xlsx_var_class=file.path("01_Variable classification",
                                paste0(paste(z[1:length(z) - 1], sep = "", collapse = "_"),
                                       "_VarClas.xlsx")),
    msg=paste("PREPROCESSING: ",paste(unlist(strsplit(file, "/")),collapse = "=>")),
    to_save=file.path("03_Pre-processed data",
                      paste0(paste(gsub(pattern, "", file[length(file)]), sep = "",
                                   collapse = "_"),"_proc.dta"))
  )


  file.create(file_attributes$r_script)

  fileConn<-file(file_attributes$r_script)
  writeLines(c(glue::glue(paste(readLines(system.file("txt_template","preprocessing.txt",package = "agrisvyr")),
                                collapse = "\n")))
             ,
             fileConn)
  close(fileConn)

}


#' Generate preprocessing scripts
#'
#' @param path_to_data path to data files
#' @param data_format data format
#'
#' @return
#' @importFrom purrr walk
#' @export
#'
#' @examples
generate_preproc_r <- function(path_to_data,data_format="stata") {

  stopifnot(dir.exists("02_Pre-processing scripts"))
  if(data_format=="stata") pattern=".dta"

  data_files=list.files(path_to_data,pattern=paste0(pattern,"$"),recursive = TRUE)

  purrr::walk(data_files,function(x){create_preproc_r(path_to_data,x,pattern)})

}



#' Create anonymization script for a data file
#'
#' @param path_to_data path to the data folder
#' @param file data file for which to create anonymization script
#' @param pattern extension of the data file
#'
#' @return
#' @export
#'
#' @examples
create_ano_r <- function(path_to_data,file,pattern) {

  z <- unlist(strsplit(file, "/"))
  file_attributes = list(
    file_name = paste(gsub(pattern, "", z[length(z)]), sep = "", collapse = "_"),
    path = file.path("03_Pre-processed data",
                     paste0(paste(gsub(pattern, "", file[length(file)]), sep = "",
                                  collapse = "_"),"_proc.dta")),
    r_script = file.path("04_Anonymization scripts",gsub(paste0(pattern,"$"),"_ano.R",file)),
    xlsx_var_class=file.path("01_Variable classification",
                                paste0(paste(z[1:length(z) - 1], sep = "", collapse = "_"),
                                       "_VarClas.xlsx")),
    msg=paste("ANONYMIZATION: ",paste(unlist(strsplit(file, "/")),collapse = "=>")),
    to_save=file.path("09_Temporary_files/temp_ano",
                      paste0(paste(gsub(pattern, "", file[length(file)]), sep = "",
                                   collapse = "_"),"_tmp.dta"))
  )

  file.create(file_attributes$r_script)

  fileConn<-file(file_attributes$r_script)
  writeLines(c(glue::glue(paste(readLines(system.file("txt_template","anonymization_script.txt",package = "agrisvyr")),
                                collapse = "\n")))
,
  fileConn)
  close(fileConn)

  file.create("04_Anonymization scripts/_RUN_anonymization.R")
  conec=file("04_Anonymization scripts/_RUN_anonymization.R")

  writeLines(
    c(
      "# Clean the folder and recreate it before saving processed data",
      "list_data=list.files(\"03_Pre-processed data\",recursive = TRUE)",
      "path_list_data=paste0(\"03_Pre-processed data/\",list_data)",
      "purrr::walk(path_list_data,file.remove)","","","",
      "#Run all pre-processing",
      "list_script=list.files(\"02_Pre-processing scripts\",pattern = \"_proc.R$\",recursive = TRUE)",
      "path_script=paste0(\"02_Pre-processing scripts/\",list_script)",
      "purrr::walk(path_script,source)","","","",
      "#run anonymization scripts and save to temporary fies",
      "ano_script=list.files(\"04_Anonymization scripts\",pattern = \"_ano.R$\",recursive = TRUE)",
      "path_ano_script=paste0(\"04_Anonymization scripts/\",ano_script)",
      "purrr::walk(path_ano_script,source)","","","",
      "#Run final anonhmization",
      "source(\"04_Anonymization scripts/final.R\")"

    ),
    conec
  )
  close(conec)


}




#' Generate all anonymization template scripts
#'
#' @param path_to_data path to the data folder
#' @param data_format data format
#'
#' @return
#' @export
#'
#' @examples
generate_ano_r <- function(path_to_data,data_format="stata") {

  stopifnot(dir.exists("04_Anonymization scripts"))
  if(data_format=="stata") pattern=".dta"

  data_files=list.files(path_to_data,pattern=paste0(pattern,"$"),recursive = TRUE)

  purrr::walk(data_files,function(x){create_ano_r(path_to_data,x,pattern)})

}

#' Generate the template of the sdc report
#'
#' @param directory directory of anonymization folders
#' @param svy_name name of the survey
#' @param author institution
#'
#' @return
#' @export
#'
#' @examples
generate_report_template <- function(directory=getwd(),svy_name="urvey",author="Amsata"){

  stopifnot(dir.exists("04_Anonymization report"))

  file <- file.path("04_Anonymization report","sdc_report.rmd")
  file.create(file)

  fileConn<-file(file)
  writeLines(c(glue::glue(paste(readLines(system.file("txt_template","sdc_report.txt",package = "agrisvyr")),
                                collapse = "\n"),.open = "{{",.close = "}}"))
             ,
             fileConn)
  close(fileConn)

  # deparse(substitute(my_object))
  # create logo https://www.youtube.com/watch?v=O34vzdHOaEk
  # https://www.pinterest.com/pin/475340935669859089/
  # https://www.youtube.com/watch?v=r3uKkmU4VQE
}

