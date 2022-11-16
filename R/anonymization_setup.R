#' Create a folder
#'
#' @param directory directory to create
#' @param overwrite if existing directory should be removed
#'
#' @return logical
#'
#'
#' @examples
create_folder <- function(directory, overwrite = FALSE) {
  if (isTRUE(dir.exists(directory)) & isFALSE(overwrite)) {
    stop(glue::glue("Directory {directory} already exists!"))
  } else {
    if (isTRUE(dir.exists(directory))) {
      unlink(directory, recursive = TRUE)
      dir.create(directory)
    } else {
      dir.create(directory)
    }
  }
}

#' Create anonymization working folders
#'
#' @param overwrite should existing folders be overwitten
#' @param agrisvy
#'
#' @return logical
#' @export
#'
#' @examples
create_ano_folders <- function(agrisvy, overwrite = FALSE) {
  # stopifnot(inherits(agrisvy,"agrisvy"))

  folders_path <- c(file.path(agrisvy@workingDir,"_R"),
    varClassDir(agrisvy),
    preProcScriptDir(agrisvy),
    preprocDataDir(agrisvy),
    anoScriptDir(agrisvy),
    anoreportDir(agrisvy),
    anoDataDir(agrisvy),
    fileDesDir(agrisvy),
    infoLossReport(agrisvy),
    tempfileDir(agrisvy),
    aobDir(agrisvy),
    file.path(tempfileDir(agrisvy), "temp_ano")
  )

  purrr::walk(folders_path, create_folder, overwrite = overwrite)
}


#' Extract label of all variable of a dataset
#'
#' @param fileName dataset name
#'
#' @return
#'
#' @examples
labels <- function(fileName) {
  curData <- read_dta(fileName, encoding = "latin1")
  curLabels <- data.frame(
    "name"  = names(curData),
    "label" = sapply(curData, function(x) attr(x, "label")) %>% as.character()
  )
  return(curLabels)
}


#' Create an Excel file for variable classification for a given data folder
#'
#' @param data a dataframe containing information on the file tree
#' @param wb_file name the workbook of a data folder
#'
#' @return
#' @import openxlsx
#' @importFrom dplyr filter %>%
#' @rawNamespace import(cli, except=c(cnum_ansi_colors))
#' @examples
create_wb <- function(agrisvy, data, wb_file) {
  # #stopifnot(inherits(agrisvy,"agrisvy"))

  hd1 <- openxlsx::createStyle(
    fontName       = "Arial",
    fontSize       = 11,
    fontColour     = "black",
    numFmt         = "GENERAL",
    border         = c("top", "bottom", "left", "right"),
    borderColour   = getOption("openxlsx.borderColour", "black"),
    borderStyle    = getOption("openxlsx.borderStyle", "thin"),
    bgFill         = NULL,
    fgFill         = "grey",
    halign         = "center",
    valign         = "center",
    textDecoration = "bold",
    wrapText       = TRUE,
    textRotation   = NULL,
    indent         = NULL,
    locked         = NULL,
    hidden         = NULL
  )

  hd2 <- openxlsx::createStyle(
    fontName       = "Arial",
    fontSize       = 11,
    fontColour     = "black",
    numFmt         = "GENERAL",
    border         = c("top", "bottom", "left", "right"),
    borderColour   = getOption("openxlsx.borderColour", "black"),
    borderStyle    = getOption("openxlsx.borderStyle", "thin"),
    bgFill         = NULL,
    fgFill         = "yellow",
    halign         = "center",
    valign         = "center",
    textDecoration = "bold",
    wrapText       = TRUE,
    textRotation   = NULL,
    indent         = NULL,
    locked         = NULL,
    hidden         = NULL
  )


  df <- data %>% dplyr::filter(workbook %in% wb_file)
  fileNames <- df$file_name
  wb <- openxlsx::createWorkbook()

  # cli_progress_bar("Generating variable classification", total = length(fileNames))

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
    # colnames(curDat) <- c("Variable name", "Variable label", "Anonymization", "Comments")
    colnames(curDat) <-
      c(
        "Name",
        "Label",
        "Classification",
        "Comments",
        "Anonymization",
        "Questions"
      )

    # prefill know quasi-identifiers like region, zardi etc.
    predef_Q <- c("region", "zardi", "sub_region")
    curDat <- curDat %>% mutate(Classification = Classification %>%
      labelled::recode_if(Name %in% predef_Q, "Q"))

    openxlsx::writeData(wb,
      sheet = fileNames[i],
      x = curDat,
      headerStyle = hd1
    )
    classification <- data.frame(cbind(
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

    names(classification) <- c("Classification category", "symbol")
    openxlsx::writeData(
      wb          =wb,
      sheet       = fileNames[i],
      x           =classification,
      startCol    = 7,
      headerStyle = hd2
    )
    openxlsx::setColWidths(
      wb     = wb,
      sheet  = fileNames[i],
      cols   = c(1, 2, 3, 4, 5, 8, 9),
      widths = c(25, 50, 16, 16, 16, 30, 15)
    )
    openxlsx::addStyle(
      wb         = wb,
      sheet      = fileNames[i],
      style      = createStyle(
      fontSize   = 9,
      fontName   = "Arial",
      halign     = "left",
      valign     = "center"
      ),
      cols       = 1:9,
      rows       = 1:nrow(curDat) + 1,
      gridExpand = TRUE,
      stack      = TRUE
    )

    openxlsx::addStyle(
      wb               = wb,
      sheet            = fileNames[i],
      style            = createStyle(
      fontSize         = 12,
      fontName         = "Arial",
      halign           = "Center",
      valign           = "center",
      textDecoration   = "bold",
      fontColour       = "#0000FF"
      ),
      cols       = 3,
      rows       = 1:nrow(curDat) + 1,
      gridExpand = TRUE,
      stack      = TRUE
    )

    openxlsx::addStyle(
      wb           = wb,
      sheet        = fileNames[i],
      style        = createStyle(
        fontSize   = 10,
        fontName   = "Arial",
        halign     = "Center",
        valign     = "center",
        fontColour = "#000080"
      ),
      cols       = 4,
      rows       = 1:nrow(curDat) + 1,
      gridExpand = TRUE,
      stack      = TRUE
    )

    openxlsx::addStyle(
      wb               =wb,
      sheet            = fileNames[i],
      style            = createStyle(
        fontSize       = 11,
        fontName       = "Arial",
        halign         = "Center",
        valign         = "center",
        textDecoration = "bold",
        fontColour     = "#008000"
      ),
      cols       = 5,
      rows       = 1:nrow(curDat) + 1,
      gridExpand = TRUE,
      stack      = TRUE
    )

    # Color text in red if variable is classified ad D
    openxlsx::conditionalFormatting(
      wb             =wb,
      sheet          = fileNames[i],
      cols           = 3,
      rows           = 1:nrow(curDat) + 1,
      type           = "beginsWith",
      rule           = "D",
      style          = createStyle(
      fontSize       = 12,
      fontName       = "Arial",
      halign         = "Center",
      valign         = "center",
      textDecoration = "bold",
      fontColour     = "#FF0000"
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
      wb             = wb,
      sheet          = fileNames[i],
      cols           = 3,
      rows           = 1:nrow(curDat) + 1,
      type           = "contains",
      rule           = "Q",
      style          = createStyle(
      fontSize       = 12,
      fontName       = "Arial",
      halign         = "Center",
      valign         = "center",
      textDecoration = "bold",
      fontColour     = "#008000"
      )
    )

    # Color text in green if variable is classified ad L
    openxlsx::conditionalFormatting(
      wb             = wb,
      sheet          = fileNames[i],
      cols           = 3,
      rows           = 1:nrow(curDat) + 1,
      type           = "contains",
      rule           = "L",
      style          = createStyle(
      fontSize       = 12,
      fontName       = "Arial",
      halign         = "Center",
      valign         = "center",
      textDecoration = "bold",
      fontColour     = "#008000"
      )
    )

    # Color text in green if variable is classified ad S
    openxlsx::conditionalFormatting(
      wb             = wb,
      sheet          = fileNames[i],
      cols           = 3,
      rows           = 1:nrow(curDat) + 1,
      type           = "contains",
      rule           = "S",
      style          = createStyle(
      fontSize       = 12,
      fontName       = "Arial",
      halign         = "Center",
      valign         = "center",
      textDecoration = "bold",
      fontColour     = "#008000"
      )
    )
    # cli_progress_update()

  }

  openxlsx::saveWorkbook(wb,
    file.path(varClassDir(agrisvy),
      glue::glue("{wb_file}_VarClas.xlsx")
    ),
    overwrite = TRUE
  )
}


#' Create Excel file for variable classification for a given data folder
#'
#' @param agrisvy
#'
#' @return
#' @importFrom   purrr walk
#' @importFrom  plyr rbind.fill
#' @export
#'
#' @examples
generate_varclas <- function(agrisvy) {
  # stopifnot(inherits(agrisvy,"agrisvy"))


  data_files   <- list.files(file.path(DataPath(agrisvy)),
                             pattern = glue::glue("{agrisvy@type}$"),
                             recursive = TRUE)

  x            <- lapply(strsplit(data_files, "/"), function(z) as.data.frame(t(z)))
  x1           <- rbind.fill(x)

  wb           <- lapply(x, function(z) {
    paste(z[1:length(z) - 1], sep = "", collapse = "_")
  })

  unique_wb    <- unique(unlist(wb))

  file_name = unlist(lapply(x, function(z) {
    paste(gsub(agrisvy@type, "", z[length(z)]), sep = "", collapse = "_")
  }))

  data_summary <- data.frame(
    file_name = file_name,
    path      = file.path(DataPath(agrisvy), data_files),
    workbook  = unlist(wb)
  )

  # create the different unique wb

  purrr::walk(unique_wb, function(x) {
    create_wb(agrisvy, data_summary, x)
  })
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
copyDirStr <- function(from, to) {
  if (dir.exists(from) == FALSE | dir.exists(to) == FALSE) {
    stop("one of the specified directory does not exits")
  }

  if (length(base::list.files(to, recursive = TRUE)) != 0) {
    base::unlink(to, recursive = TRUE)
  }
  dir_list <- list.dirs(from, full.names = FALSE)
  dir_list <- dir_list[2:length(dir_list)]

  purrr::walk(file.path(to, dir_list), base::dir.create)
}




#' Create a simple  preprocessing R script for a given dataset
#'
#' @param agrisvy
#' @param file name of the data file
#'
#' @return
#' @importFrom glue glue
#' @export
#'
#' @examples
create_preproc_r <- function(agrisvy, file) {
  z <- unlist(strsplit(file, "/"))

  file_attributes <- list(
    file_name      = paste(gsub(agrisvy@type, "", z[length(z)]), sep = "", collapse = "_"),
    path           = file.path(DataPath(agrisvy), file),
    r_script       = file.path(preProcScriptDir(agrisvy), gsub(paste0(agrisvy@type, "$"), "_proc.R", file)),
    xlsx_var_class = file.path(varClassDir(agrisvy),
                               paste0(paste(z[1:length(z) - 1], sep = "",
                                            collapse = "_"),
                                      "_VarClas.xlsx"
                                      )
                               ),
    msg            =paste(unlist(strsplit(file, "/")), collapse = "=>"),
    to_save        = file.path(preprocDataDir(agrisvy),
                               paste0(paste(gsub(agrisvy@type,
                                                 "",
                                                 file[length(file)]),
                                            sep = "", collapse = "_"),
                                      "_proc.dta")
                               )
  )


  file.create(file_attributes$r_script)

  fileConn <- file(file_attributes$r_script)

  writeLines(
    c(glue::glue(paste(readLines(system.file("txt_template",
                                             "preprocessing.txt",
                                             package = "agrisvyr"), warn = FALSE),
      collapse = "\n"
    ))),
    fileConn
  )
  close(fileConn)
}


#' Generate preprocessing scripts
#'
#' @param agrisvy
#'
#' @return
#' @importFrom purrr walk
#' @export
#'
#' @examples
generate_preproc_r <- function(agrisvy) {
  # stopifnot(inherits(agrisvy,"agrisvy"))

  stopifnot(dir.exists(preProcScriptDir(agrisvy)))

  data_files <- list.files(DataPath(agrisvy),
                           pattern = paste0(agrisvy@type, "$"),
                           recursive = TRUE
                           )

  purrr::walk(data_files, function(x) {
    create_preproc_r(agrisvy, x)
  })
}



#' Create anonymization script for a data file
#'
#' @param agrisvy
#' @param file data file for which to create anonymization script
#'
#' @return
#' @export
#'
#' @examples
create_ano_r <- function(agrisvy, file) {

  z <- unlist(strsplit(file, "/"))

  file_attributes <- list(
    file_name      = paste(gsub(agrisvy@type, "", z[length(z)]), sep = "", collapse = "_"),
    path           = file.path(preprocDataDir(agrisvy),
                               paste0(paste(gsub(agrisvy@type, "",
                                                 file[length(file)]),
                                            sep = "",collapse = "_" ),
                                      "_proc.dta")
                               ),
    r_script       = file.path(anoScriptDir(agrisvy),
                               gsub(paste0(agrisvy@type, "$"),
                                    "_ano.R", file)),
    xlsx_var_class = file.path(varClassDir(agrisvy),
                               paste0(paste(z[1:length(z) - 1],
                                            sep = "", collapse = "_"),
                                      "_VarClas.xlsx")
                               ),
    msg            = paste(unlist(strsplit(file, "/")),collapse = "=>"),
    to_save        = file.path(tempfileDir(agrisvy),
                               "temp_ano",
                               paste0(paste(gsub(agrisvy@type, "",
                                                 file[length(file)]),
                                            sep = "", collapse = "_"),
                                      "_tmp.dta"))
  )

  file.create(file_attributes$r_script)

  fileConn <- file(file_attributes$r_script)
  writeLines(
    c(glue::glue(paste(readLines(system.file("txt_template",
                                             "anonymization_script.txt",
                                             package = "agrisvyr"),
                                 warn = FALSE
                                 ),
      collapse = "\n"
    ))),
    fileConn
  )
  close(fileConn)

  file.create(file.path(anoScriptDir(agrisvy), "_RUN_anonymization.R"))
  conec <- file(file.path(anoScriptDir(agrisvy), "_RUN_anonymization.R"))

  writeLines(
    c(
      "# Clean the folder and recreate it before saving processed data",
      "list_data=list.files(\"03_Pre-processed data\",recursive = TRUE)",
      "path_list_data=paste0(\"03_Pre-processed data/\",list_data)",
      "purrr::walk(path_list_data,file.remove)", "", "", "",
      "#Run all pre-processing",
      "list_script=list.files(\"02_Pre-processing scripts\",pattern = \"_proc.R$\",recursive = TRUE)",
      "path_script=paste0(\"02_Pre-processing scripts/\",list_script)",
      "purrr::walk(path_script,source)", "", "", "",
      "#run anonymization scripts and save to temporary fies",
      "ano_script=list.files(\"04_Anonymization scripts\",pattern = \"_ano.R$\",recursive = TRUE)",
      "path_ano_script=paste0(\"04_Anonymization scripts/\",ano_script)",
      "purrr::walk(path_ano_script,source)", "", "", "",
      "#Run final anonhmization",
      "source(\"04_Anonymization scripts/final.R\")"
    ),
    conec
  )
  close(conec)


}




#' Generate all anonymization template scripts
#'
#' @param agrisvy
#'
#' @return
#' @export
#'
#' @examples
generate_ano_r <- function(agrisvy) {
  # stopifnot(inherits(agrisvy,"agrisvy"))

  stopifnot(dir.exists(anoScriptDir(agrisvy)))

  data_files <- list.files(DataPath(agrisvy),
                           pattern = glue::glue("{agrisvy@type}$"),
                           recursive = TRUE
                           )

  purrr::walk(data_files, function(x) {
    create_ano_r(agrisvy, x)
  })
}

#' Generate the template of the sdc report
#'
#' @param agrisvy
#'
#' @return
#' @export
#'
#' @examples
generate_report_template <- function(agrisvy) {
  stopifnot(dir.exists(anoreportDir(agrisvy)))

  file <- file.path(anoreportDir(agrisvy), "sdc_report.rmd")
  file.create(file)

  fileConn <- file(file)
  writeLines(
    c(glue::glue(paste(readLines(system.file("txt_template",
                                             "sdc_report.txt",
                                             package = "agrisvyr"),
                                 warn = FALSE
                                 ),
      collapse = "\n"
    ), .open = "{{", .close = "}}")),
    fileConn
  )
  close(fileConn)

  # deparse(substitute(my_object))
  # create logo https://www.youtube.com/watch?v=O34vzdHOaEk
  # https://www.pinterest.com/pin/475340935669859089/
  # https://www.youtube.com/watch?v=r3uKkmU4VQE
  # https://andrewmaclachlan.github.io/CASA0005repo/explaining-spatial-patterns.html
  # https://github.com/rstudio/cheatsheets/tree/main/powerpoints
  #https://github.com/r-lib/cli
  #https://github.com/r-lib/crayon
}



#' set up the anonymization working directory by creating folders and files
#'
#' @param overwrite overwrite existing folders
#' @param agrisvy
#'
#' @return
#' @importFrom  haven read_dta write_dta
#' @export
#'
#' @examples
setup_anonymization <- function(agrisvy, overwrite) {
  # stopifnot(inherits(agrisvy,"agrisvy"))
  create_ano_folders(agrisvy, overwrite = overwrite)
  generate_varclas(agrisvy)
  copyDirStr(from = DataPath(agrisvy), to = preProcScriptDir(agrisvy))
  copyDirStr(from = DataPath(agrisvy), to = preprocDataDir(agrisvy))
  generate_preproc_r(agrisvy)
  copyDirStr(from = DataPath(agrisvy), to = anoScriptDir(agrisvy))
  copyDirStr(from = DataPath(agrisvy), to = file.path(tempfileDir(agrisvy), "temp_ano"))
  generate_ano_r(agrisvy)
  generate_report_template(agrisvy)

  saveRDS(agrisvy,file.path(agrisvy@workingDir,"_R",paste0(deparse(substitute(agrisvy)),".rds")))

  set_up=file.path(agrisvy@workingDir,"_R","_setup.R")

  fileConn <- file(set_up)

  writeLines(
    c(glue::glue("setwd(\"{agrisvy@workingDir}\")"),"",
      glue::glue("{deparse(substitute(agrisvy))} <-readRDS(\"_R/{deparse(substitute(agrisvy))}.rds\")")),
    fileConn
  )
  close(fileConn)

  #Run first prerocessing
  runPreproc(agrisvy)


  #run ano-----------
  #clear data
  list_data=list.files(file.path(tempfileDir(agrisvy),"temp_ano"),recursive = TRUE)
  path_list_data=file.path(file.path(tempfileDir(agrisvy),"temp_ano"),list_data)

  if (length(path_list_data)!=0) {
    purrr::walk(path_list_data,file.remove)
  }

  #Run all pre-processing
  list_script=list.files(anoScriptDir(agrisvy),pattern = "_ano.R$",recursive = TRUE)
  path_script=file.path(anoScriptDir(agrisvy),list_script)

  if(length(path_script)!=0){
    purrr::walk(path_script,source)
  }

  #----------
  #generate final.R ano file


  temp_ano_files=list.files(file.path(tempfileDir(agrisvy),"temp_ano"),pattern = ".dta$",recursive = TRUE)

  path_ano_temp_files=file.path(tempfileDir(agrisvy),"temp_ano",temp_ano_files)

  # read_files=paste0("data=read_dta(\"",path_ano_temp_files,"\")")
  read_files=glue::glue("data=read_dta(\"{path_ano_temp_files}\")")
  dataset=gsub("_tmp","",temp_ano_files)
  ano_dataset=paste0(gsub(agrisvy@type,
                          "",
                          dataset),
                     "_ano.dta")

  # save_files=paste0("write_dta(data,\"07_Anonymized data/","ano_",dataset,"\")")
  save_files=glue::glue("write_dta(data,\"{anoDataDir(agrisvy)}/{ano_dataset}\")")
  ff=paste(read_files,save_files,sep = ",")

  final=c("#--------------------------------------------------------------------------",
          "#| ANonymization of the 2019 Anual Agricultural Survey of Uganda (AAS)    |",
          "#| By Amsata NIANG, amsata.niang@fao.org                                  |",
          "#| November 2022                                                          |",
          "#--------------------------------------------------------------------------",
          "#|FINALIZATION OF ANONYMIZATION",
          "#|-----------------------------|",
          "","","",
          "#|----------------------------------------|",
          "#| PREPROCESSING                          |",
          "#|----------------------------------------|","",
          "rm(list=ls())",
          "library(agrisvyr)",
          "library(dplyr)",
          "library(tidyr)",
          "library(readxl)",
          "library(haven)",
          "library(questionr)",
          "library(labelled)",
          "","",
          "#|----------------------------------------|","","")
  for (i in seq_along(dataset)) {

    final=c(final,
            "#*****************************************************************",
            paste0("# finalizing the naonymization of ",dataset[i]),
            "#*****************************************************************","",
            glue::glue("finMessage(\"{dataset[i]}\")"),"",
            read_files[i],"","","",save_files[i],"")

  }

  file.create(file.path(anoScriptDir(agrisvy),"final.R"))

  conn=file(file.path(anoScriptDir(agrisvy),"final.R"))

  writeLines(
    final,
    conn
  )

  close(conn)

#-------------------------------------

  copyDirStr(from = DataPath(agrisvy), to = anoDataDir(agrisvy))
  source(file.path(anoScriptDir(agrisvy),"final.R"))


}


#' Run all the pre-processing scripts
#'
#' @param agrisvy an agris survey objects
#'
#' @return
#' logical
#' @importFrom purrr walk
#' @export
#'
#' @examples
#'
runPreproc <- function(agrisvy){
  #clear data
  list_data=list.files(preprocDataDir(agrisvy),recursive = TRUE)
  path_list_data=file.path(preprocDataDir(agrisvy),list_data)

  if (length(path_list_data)!=0) {
    purrr::walk(path_list_data,file.remove)
  }

  #Run all pre-processing
  list_script=list.files(preProcScriptDir(agrisvy),pattern = ".R$",recursive = TRUE)
  path_script=file.path(preProcScriptDir(agrisvy),list_script)

  if(length(path_script)!=0){
    purrr::walk(path_script,source)
  }

}



#' Run all anonymization scripts
#'
#' @param agrisvy
#'
#' @return
#' @export
#'
#' @examples
runAnon <- function(agrisvy){

  #clear data
  list_data=list.files(file.path(tempfileDir(agrisvy),"temp_ano"),recursive = TRUE)
  path_list_data=file.path(file.path(tempfileDir(agrisvy),"temp_ano"),list_data)

  if (length(path_list_data)!=0) {
    purrr::walk(path_list_data,file.remove)
  }

  #Run all pre-processing
  list_script=list.files(anoScriptDir(agrisvy),pattern = "_ano.R$",recursive = TRUE)
  path_script=file.path(anoScriptDir(agrisvy),list_script)

  if(length(path_script)!=0){
    purrr::walk(path_script,source)
  }


  #clear data
  list_data=list.files(anoDataDir(agrisvy),recursive = TRUE)
  path_list_data=file.path(anoDataDir(agrisvy),list_data)

  if (length(path_list_data)!=0) {
    purrr::walk(path_list_data,file.remove)
  }

  source(file.path(anoScriptDir(agrisvy),"final.R"))

}

