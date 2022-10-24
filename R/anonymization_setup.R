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
            "05_Anonymization report","06_Anonymized data",
            "07_Files description","08_Information loss report",
            "09_Temporary_files","10_Miscellaneous")

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
#' @import  purrr
#' @import plyr
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
