
create_folder <- function(directory, overwrite = FALSE) {
  if (isTRUE(dir.exists(directory)) & isFALSE(overwrite)) {
    message(glue::glue("Directory {directory} already exists!"))
  } else {
    if (isTRUE(dir.exists(directory))) {
      unlink(directory, recursive = TRUE)
      dir.create(directory,showWarnings = FALSE)
    } else {
      dir.create(directory,showWarnings = FALSE)
    }
  }
}



#' @return logical
#' @importFrom purrr walk
#' @importFrom usethis create_project

create_ano_folders <- function(agrisvy, overwrite = FALSE) {
  # rstudioapi::getActiveProject()
  agrisMsg("INITIAL SETUP","creating folders")
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
    file.path(tempfileDir(agrisvy), "temp_ano"),
    file.path(tempfileDir(agrisvy), "Labels")
  )

  purrr::walk(folders_path, create_folder, overwrite = overwrite)
}


#' Extract label of all variable of a dataset
#'
#' @param fileName dataset name
#' @param type data type
#'
#' @return dataframe
#' @importFrom haven read_dta
#' @importFrom haven read_sav
#' @export
#' @examples
labels <- function(fileName,type,encoding=encoding) {

  if(type %in% ".dta"){
    curData <- haven::read_dta(fileName,encoding = encoding)
    }

  if(type %in% c(".SAV",".sav")){
    curData <- haven::read_sav(fileName,encoding = encoding)
    }
  curLabels <- data.frame(
    "name"  = names(curData),
    "label" = sapply(curData, function(x) attr(x, "label")) %>% as.character()
  )
  return(curLabels)
}


#' @param agrisvy
#'
#' @param data
#' @param wb_file
#' @param encoding
#'
#' @import openxlsx
#' @importFrom dplyr filter %>%
#' @importFrom cli cli_progress_bar cli_progress_update

create_wb <- function(agrisvy, data, wb_file,encoding) {

  agrisMsg("VARIABLE CLASSIFICATION",paste0("Creating workbook ",wb_file))

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

  cli_progress_bar("Generating variable classification", total = length(fileNames))

  for (i in 1:length(fileNames)) {
    cli_progress_update()
    openxlsx::addWorksheet(wb, fileNames[i])
    curDat <- labels(df$path[i],type=agrisvy@type,encoding = encoding)
    curDat <-
      cbind(curDat, data.frame(
        a = "",
        b = "",
        c = "",
        d = ""
      ))

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
    #Locking cells
    openxlsx::protectWorksheet(wb=wb,sheet = fileNames[i], protect = TRUE,
                     lockFormattingCells = FALSE, lockFormattingColumns = FALSE,
                     lockInsertingColumns = TRUE, lockDeletingColumns = TRUE,
                     lockFormattingRows=FALSE,lockInsertingRows=TRUE,
                     lockDeletingRows=TRUE
                     )
    openxlsx::addStyle(wb, sheet = fileNames[i], style = createStyle(locked = FALSE),rows = 1:nrow(curDat)+1, cols = 3)
    openxlsx::addStyle(wb, sheet = fileNames[i], style = createStyle(locked = FALSE),rows = 1:nrow(curDat)+1, cols = 4)
    openxlsx::addStyle(wb, sheet = fileNames[i], style = createStyle(locked = FALSE),rows = 1:nrow(curDat)+1, cols = 5)
    openxlsx::addStyle(wb, sheet = fileNames[i], style = createStyle(locked = FALSE),rows = 1:nrow(curDat)+1, cols = 6)

    validate=c("\"-,D,DI,ID,Q,L,S,W\"")
    dataValidation(wb, sheet = fileNames[i], col = 3, rows = 1:nrow(curDat)+1,
                   type = 'list', value = validate,allowBlank = TRUE)

    openxlsx::setColWidths(wb, sheet = fileNames[i], cols = 1, widths = "auto")



    }

    openxlsx::saveWorkbook(wb,
      file.path(varClassDir(agrisvy),
      glue::glue("{wb_file}_VarClas.xlsx")
    ),
    overwrite = TRUE
  )
}



#' @param agrisvy
#'
#' @param encoding
#'
#' @importFrom   purrr walk
#' @importFrom  plyr rbind.fill

generate_varclas <- function(agrisvy,encoding="UTF-8") {
  exelFilesInfos=.createExcelInfos(agrisvy)
  # create the different unique wb
  purrr::walk(exelFilesInfos$workbook, function(x) {
    create_wb(agrisvy, exelFilesInfos$files_infos, x,encoding=encoding)
  })
}


#' @importFrom R.utils copyDirectory
#' @importFrom purrr walk

copyDirStr <- function(from, to) {
  if (dir.exists(from) == FALSE | dir.exists(to) == FALSE) {
    stop("one of the specified directory does not exits")
  }

  if (length(base::list.files(to, recursive = TRUE)) != 0) {
    base::unlink(to, recursive = TRUE)
  }
  dir_list <- list.dirs(from, full.names = FALSE)

  if("" %in% dir_list & length(dir_list)>1){
    dir_list <- dir_list[2:length(dir_list)]
    purrr::walk(file.path(to, dir_list), base::dir.create,showWarnings = FALSE)
  }
}



#' @importFrom glue glue

create_preproc_r <- function(agrisvy, file,type,obj_name) {

   opn="{"
  clse="}"
msg=paste(unlist(strsplit(file, "/")), collapse = "=>")
  z <- unlist(strsplit(file, "/"))

if(type=="proc"){
  r_file=file.path(preProcScriptDir(agrisvy), gsub(paste0(agrisvy@type, "$"), "_proc.R", file))
  save_path=file.path(preprocDataDir(agrisvy),paste0(paste(gsub(agrisvy@type,"",file[length(file)]),sep = "", collapse = "_"),paste0("_proc",agrisvy@type)))
  if (agrisvy@language=="en") template=system.file("txt_template","preprocessing_script_en.txt",package = "agrisvyr")
  if (agrisvy@language=="fr") template=system.file("txt_template","preprocessing_script_fr.txt",package = "agrisvyr")
  if (agrisvy@language=="es") template=system.file("txt_template","preprocessing_script_es.txt",package = "agrisvyr")
  if (agrisvy@language=="pt") template=system.file("txt_template","preprocessing_script_pt.txt",package = "agrisvyr")
}

if(type=="ano"){
  r_file=file.path(anoScriptDir(agrisvy),gsub(paste0(agrisvy@type, "$"),"_ano.R", file))
  save_path=file.path(tempfileDir(agrisvy),"temp_ano",paste0(paste(gsub(agrisvy@type, "",file[length(file)]),sep = "", collapse = "_"),paste0("_tmp",agrisvy@type)))
  if (agrisvy@language=="en") template=system.file("txt_template","anonymization_script_en.txt",package = "agrisvyr")
  if (agrisvy@language=="fr") template=system.file("txt_template","anonymization_script_fr.txt",package = "agrisvyr")
  if (agrisvy@language=="es") template=system.file("txt_template","anonymization_script_es.txt",package = "agrisvyr")
  if (agrisvy@language=="pt") template=system.file("txt_template","anonymization_script_pt.txt",package = "agrisvyr")
  file = file.path(preprocDataDir(agrisvy),paste0(paste(gsub(agrisvy@type, "",file[length(file)]),sep = "",collapse = "_" ), paste0("_proc",agrisvy@type)))
 }

  if(type=="wksp_proc"){
    r_file=file.path(preProcScriptDir(agrisvy), gsub(paste0(agrisvy@type, "$"), "_proc.qmd", file))
    save_path=file.path(preprocDataDir(agrisvy),paste0(paste(gsub(agrisvy@type,"",file[length(file)]),sep = "", collapse = "_"),paste0("_proc",agrisvy@type)))
    template=system.file("txt_template","preproc_file_wksp_fr.txt",package = "agrisvyr")
    opn="{{"
    clse="}}"
  }

  if(type=="wksp_ano"){
    r_file=file.path(anoScriptDir(agrisvy),gsub(paste0(agrisvy@type, "$"),"_ano.qmd", file))
    save_path=file.path(anoDataDir(agrisvy),paste0(paste(gsub(agrisvy@type, "",file[length(file)]),sep = "", collapse = "_"),paste0("_ano",agrisvy@type)))
    template=system.file("txt_template","ano_file_wksp_fr.txt",package = "agrisvyr")
    opn="{{"
    clse="}}"
  }

  if(type=="wksp_risk"){
    r_file=file.path(riskAnaDir(agrisvy),gsub(paste0(agrisvy@type, "$"),"_risk.qmd", file))
    save_path=file.path(anoDataDir(agrisvy),paste0(paste(gsub(agrisvy@type, "",file[length(file)]),sep = "", collapse = "_"),"_obj.rds"))
    template=system.file("txt_template","risk_analysis_wksp_fr.txt",package = "agrisvyr")
    opn="{{"
    clse="}}"
  }

  varclass=file.path(varClassDir(agrisvy),
            paste0(gsub(" ","_",paste(z[1:length(z) - 1], sep = "",
                                      collapse = "_")),
                   "_VarClas.xlsx"
            )
  )

  varLabels=file.path(fileDesDir(agrisvy),
                     paste0(gsub(" ","_",paste(z[1:length(z) - 1], sep = "",
                                               collapse = "_")),
                            "_labels.xlsx"
                     )
  )

  data_flder=unlist(strsplit(DataPath(agrisvy),split = "/"))
  data_flder=data_flder[length(data_flder)]
  dirs=list.dirs(DataPath(agrisvy), full.names = FALSE)

  if(!("" %in% dirs & length(dirs)>1)) {
    varclass=file.path(varClassDir(agrisvy),gsub(" ","_",paste0(data_flder,"_VarClas.xlsx")))
    varLabels=file.path(fileDesDir(agrisvy),gsub(" ","_",paste0(data_flder,"_labels.xlsx")))

  }

  file_attributes <- list(
    file_name      = paste(gsub(agrisvy@type, "", z[length(z)]), sep = "", collapse = "_"),
    path           = file.path(file),
    r_script       = r_file,
    xlsx_var_class = varclass,
    xlsx_var_labels = varLabels,
    msg            =msg,
    to_save        = save_path,
    read_function=readDataFunc(agrisvy),
    write_function=writeDataFunc(agrisvy)
  )
  dst_name=gsub(agrisvy@type,"",rev(unlist(strsplit(file_attributes$msg, "=>")))[1])
  subfolder <- ifelse(grepl("=>", file_attributes$msg), sub("=>.*", "", file_attributes$msg), "")

  file.create(r_file,showWarnings = FALSE)
  fileConn <- file(r_file)

  writeLines(
      c(glue::glue(paste(readLines(template, warn = FALSE),
      collapse = "\n"
    ),.open = opn,.close = clse)),
    fileConn
  )
  close(fileConn)
}


#' @importFrom purrr walk

generate_preproc_r <- function(agrisvy,type,obj_name) {
  # stopifnot(inherits(agrisvy,"agrisvy"))

  stopifnot(dir.exists(preProcScriptDir(agrisvy)))

  if(type=="proc") agrisMsg("INITIAL SETUP","generating pre-processing R sripts")
  if(type=="ano") agrisMsg("INITIAL SETUP","generating anonymization R sripts")
  if(type=="wksp_proc") agrisMsg("INITIAL SETUP","generating pre-processing quarto files")
  if(type=="wksp_ano") agrisMsg("INITIAL SETUP","generating anonymization quarto files")
  if(type=="wksp_risk") agrisMsg("INITIAL SETUP","generating risk analysis quarto files")


  data_files <- list.files(DataPath(agrisvy),
                           pattern = paste0(agrisvy@type, "$"),
                           recursive = TRUE
                           )

  purrr::walk(data_files, function(x) {
    create_preproc_r(agrisvy, x,type,obj_name)
  })
}


#' @importFrom cli cli_h2
#' @importFrom  crayon blue

create_ano_r <- function(agrisvy, file,md) {

  z <- unlist(strsplit(file, "/"))

  file_attributes <- list(
    file_name      = paste(gsub(agrisvy@type, "", z[length(z)]), sep = "", collapse = "_"),
    path           = file.path(preprocDataDir(agrisvy),
                               paste0(paste(gsub(agrisvy@type, "",
                                                 file[length(file)]),
                                            sep = "",collapse = "_" ),
                                      paste0("_proc",agrisvy@type))
                               ),
    r_script       = file.path(anoScriptDir(agrisvy),
                               gsub(paste0(agrisvy@type, "$"),
                                    "_ano.R", file)),
    xlsx_var_class = file.path(varClassDir(agrisvy),
                               paste0(paste(z[1:length(z) - 1],
                                            sep = "", collapse = "_"),
                                      "_VarClas.xlsx")
                               ),
    xlsx_var_labels = file.path(fileDesDir(agrisvy),
                               paste0(paste(z[1:length(z) - 1],
                                            sep = "", collapse = "_"),
                                      "_labels.xlsx")
    ),
    msg            = paste(unlist(strsplit(file, "/")),collapse = "=>"),
    to_save        = file.path(tempfileDir(agrisvy),
                               "temp_ano",
                               paste0(paste(gsub(agrisvy@type, "",
                                                 file[length(file)]),
                                            sep = "", collapse = "_"),
                                      paste0("_tmp",agrisvy@type)))
  )

template=system.file("txt_template","anonymization_script.txt",package = "agrisvyr")
r_file=file_attributes$r_script
opn = "{"
clse = "}"

if(md==TRUE) {
  template=system.file("txt_template","ano_file_wksp_fr.txt",package = "agrisvyr")
  r_file=gsub(".R",".qmd",r_file)
  opn = "{{"
  clse = "}}"
}

  file.create(r_file,showWarnings = FALSE)
  fileConn <- file(r_file)
  writeLines(
    c(glue::glue(paste(readLines(template,
                                 warn = FALSE
                                 ),
      collapse = "\n"
    ),.open = opn, .close = clse)),
    fileConn
  )
  close(fileConn)

}


generate_ano_r <- function(agrisvy,md=FALSE) {

    agrisMsg("INITIAL SETUP","generating R sripts for anonymization")

    # stopifnot(inherits(agrisvy,"agrisvy"))

    stopifnot(dir.exists(anoScriptDir(agrisvy)))

    data_files <- list.files(DataPath(agrisvy),
                             pattern = glue::glue("{agrisvy@type}$"),
                             recursive = TRUE
                             )

    purrr::walk(data_files, function(x) {
      create_ano_r(agrisvy, x,md)
    })
}



generate_report_template <- function(agrisvy,type) {
  stopifnot(dir.exists(anoreportDir(agrisvy)))
  stopifnot(type %in% c("svy","wksp"))

  if(type=="svy") {

    if (agrisvy@language=="en") template_sdc=system.file("txt_template","sdc_report_en.txt",package = "agrisvyr")
    if (agrisvy@language=="fr") template_sdc=system.file("txt_template","sdc_report_fr.txt",package = "agrisvyr")
    if (agrisvy@language=="es") template_sdc=system.file("txt_template","sdc_report_es.txt",package = "agrisvyr")
    if (agrisvy@language=="pt") template_sdc=system.file("txt_template","sdc_report_pt.txt",package = "agrisvyr")

    if (agrisvy@language=="en") template_info_loss=system.file("txt_template","infos_loss_report_en.txt",package = "agrisvyr")
    if (agrisvy@language=="fr") template_info_loss=system.file("txt_template","infos_loss_report_fr.txt",package = "agrisvyr")
    if (agrisvy@language=="es") template_info_loss=system.file("txt_template","infos_loss_report_es.txt",package = "agrisvyr")
    if (agrisvy@language=="pt") template_info_loss=system.file("txt_template","infos_loss_report_pt.txt",package = "agrisvyr")
  }

  if(type=="wksp") {
    template_sdc=system.file("txt_template","sdc_report_wksp_fr.txt",package = "agrisvyr")
  }
#SDC
  file_sdc <- file.path(anoreportDir(agrisvy), "sdc_report.rmd")
  file.create(file_sdc,showWarnings = FALSE)
#Information loss
  file_info_loss <- file.path(infoLossReport(agrisvy), "information_loss_report.rmd")
  file.create(file_info_loss,showWarnings = FALSE)

  #SDC
  fileConn_sdc <- file(file_sdc)
  writeLines(
    c(glue::glue(paste(readLines(template_sdc,
                                 warn = FALSE
                                 ),
      collapse = "\n"
    ), .open = "{{", .close = "}}")),
    fileConn_sdc
  )
  close(fileConn_sdc)

  #Information loss
  fileConn_info_loss <- file(file_info_loss)
  writeLines(
    c(glue::glue(paste(readLines(template_info_loss,
                                 warn = FALSE
    ),
    collapse = "\n"
    ), .open = "{{", .close = "}}")),
    fileConn_info_loss
  )
  close(fileConn_info_loss)

  # deparse(substitute(my_object))
  # create logo https://www.youtube.com/watch?v=O34vzdHOaEk
  # https://www.pinterest.com/pin/475340935669859089/
  # https://www.youtube.com/watch?v=r3uKkmU4VQE
  # https://andrewmaclachlan.github.io/CASA0005repo/explaining-spatial-patterns.html
  # https://github.com/rstudio/cheatsheets/tree/main/powerpoints
  #https://github.com/r-lib/cli
  #https://github.com/r-lib/crayon
  # https://github.com/tidyverse/purrr/issues/149
}



#' set up the anonymization working directory by creating folders and files
#'
#'This fucntion set up the anonymization working space by creating the folders,
#'generating the pre-populated R scripts, generating the anonymization report,
#'and running the first round of pre-processing and anonymization to make sure
#'that the process is smooth.
#'
#' @param overwrite a \code{logical}. If \code{TRUE},overwrite the working folders
#' if they already exists.
#' @param agrisvy an \code{agrisvy} object
#' @param open
#' @param encoding
#'
#' @return
#' @importFrom  haven read_dta write_dta
#' @importFrom renv init
#' @importFrom renv install
#' @export
#'
#' @examples
#' \dontrun{
#' agrissvy_obj=createAgrisvy(
#'                 svyName = "AGRIS SURVEY 2023",
#'                 author = "AgriSurvey Team",
#'                 language = "en",
#'                 workingDir = "C/Documents/anonymization",
#'                 dataDir = "C/Documents/AgrisData",
#'                 type = ".dta"
#' )
#'
#'
#' }
setup_anonymization <- function(agrisvy, overwrite=FALSE,open=FALSE,encoding="UTF-8",renv=FALSE) {

  #verifify the limit of path lengh that exist and for files that will be
  check_path_limit(agrisvy)

  obj_name=deparse(substitute(agrisvy))

  setwd(agrisvy@workingDir)
  old_wd=agrisvy@workingDir
  if (isTRUE(dir.exists("SDC")) & isFALSE(overwrite)) {
    message(glue::glue("SDC already exists!"))
  } else {
    unlink("SDC", recursive = TRUE)
  }

  #R.utils::copyDirectory(DataPath(agrisvy), "data")
  options(usethis.allow_nested_project = TRUE)
  create_project(path = "SDC", open = open, rstudio = TRUE)
  agrisvy@workingDir <- file.path(agrisvy@workingDir,"SDC")
  setwd(agrisvy@workingDir)


  unlink("R",recursive = TRUE)
  # stopifnot(inherits(agrisvy,"agrisvy"))
  create_ano_folders(agrisvy, overwrite = overwrite)
  generate_varclas(agrisvy,encoding=encoding)
  generateLabelFiles(agrisvy,encoding=encoding)
  export_labels(agrisvy,encoding=encoding,overwrite=overwrite)
  copyDirStr(from = DataPath(agrisvy), to = preProcScriptDir(agrisvy))
  copyDirStr(from = DataPath(agrisvy), to = preprocDataDir(agrisvy))
  generate_preproc_r(agrisvy,type="proc",obj_name=obj_name)
  copyDirStr(from = DataPath(agrisvy), to = anoScriptDir(agrisvy))
  copyDirStr(from = DataPath(agrisvy), to = anoDataDir(agrisvy))
  copyDirStr(from = DataPath(agrisvy), to = file.path(tempfileDir(agrisvy), "temp_ano"))
  copyDirStr(from = DataPath(agrisvy), to = file.path(tempfileDir(agrisvy), "Labels"))
  generate_preproc_r(agrisvy,type="ano",obj_name=obj_name)
  generate_report_template(agrisvy,"svy")
  #putting excel data description in the anonymization report folder and the info loss report folder
  microdata_infos=.createExcelInfos(agrisvy)
  df_infos=microdata_infos$files_infos
  df_infos=df_infos %>% dplyr::select(workbook,file_name)
  df_infos$Infoirmation=""
  number_wb=length(unique(microdata_infos$workbook))
  if(number_wb==1) df_infos=df_infos %>% dplyr::select(-workbook)

  if (number_wb==1){
    if(agrisvy@language=="en") names(df_infos)=c("Data file","file information")
    if(agrisvy@language=="fr") names(df_infos)=c("Data file","file information")
    if(agrisvy@language=="es") names(df_infos)=c("Data file","file information")
    if(agrisvy@language=="pt") names(df_infos)=c("Data file","file information")
  } else {
      if(agrisvy@language=="en") names(df_infos)=c("Data folder","Data file","file information")
      if(agrisvy@language=="fr") names(df_infos)=c("Data folder","Data file","file information")
      if(agrisvy@language=="es") names(df_infos)=c("Data folder","Data file","file information")
      if(agrisvy@language=="pt") names(df_infos)=c("Data folder","Data file","file information")
    }


  openxlsx::write.xlsx(df_infos,file.path(anoreportDir(agrisvy),"microdata_info.xlsx"))
  openxlsx::write.xlsx(df_infos,file.path(agrisvy@infoLossReport,"microdata_info.xlsx"))

  #Exporting excel file containing D and DI classified variables
  genDandDIVariables(agrisvy)
  saveRDS(agrisvy,file.path("_R",paste0(obj_name,".rds")))

  set_up=file.path("_R","_setup.R")
  file.create(set_up,showWarnings = FALSE)
  fileConn <- file(set_up)

  writeLines(
    c(glue::glue("#setwd(\"{agrisvy@workingDir}\")"),"",
      glue::glue("data_path=\"{agrisvy@dataDir}\""),"",
      glue::glue("{obj_name} <-readRDS(\"_R/{obj_name}.rds\")")),
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


  temp_ano_files=list.files(file.path(tempfileDir(agrisvy),"temp_ano"),pattern = paste0(agrisvy@type,"$"),recursive = TRUE)

  path_ano_temp_files=file.path(tempfileDir(agrisvy),"temp_ano",temp_ano_files)

  # read_files=paste0("data=read_dta(\"",path_ano_temp_files,"\")")
  read_files=glue::glue("data={readDataFunc(agrisvy)}(\"{path_ano_temp_files}\")")
  dataset=gsub("_tmp","",temp_ano_files)
  ano_dataset=paste0(gsub(agrisvy@type,
                          "",
                          dataset),
                     paste0("_ano",agrisvy@type))

  # save_files=paste0("write_dta(data,\"07_Anonymized data/","ano_",dataset,"\")")
  save_files=glue::glue("{writeDataFunc(agrisvy)}(data,\"{anoDataDir(agrisvy)}/{ano_dataset}\")")
  ff=paste(read_files,save_files,sep = ",")

  final=c("#|FINALIZATION OF ANONYMIZATION",
          "#|-----------------------------|",
          "","","",
          "#|----------------------------------------|",
          "#| PREPROCESSING                          |",
          "#|----------------------------------------|","",
          "rm(list = ls())",
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

  file.create(file.path(anoScriptDir(agrisvy),"final.R"),showWarnings = FALSE)

  conn=file(file.path(anoScriptDir(agrisvy),"final.R"))

  writeLines(
    final,
    conn
  )

  close(conn)

#-------------------------------------

  source(file.path(anoScriptDir(agrisvy),"final.R"))
  export_labels(agrisvy,encoding,overwrite)

 #generate files description
  genAllFileDes(agrisvy,id_cols=50,encoding=encoding)
  #archive data
  # ArchiveAnoData(agrisvy)
  # ArchiveCleanData(agrisvy)
  # ArchiveProcData(agrisvy)


  if(renv==TRUE) {
    renv::init(project =agrisvy@workingDir )
  }

setwd(old_wd)
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

  #check if the agrisvy object exist

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
  path_script=file.path(anoScriptDir(agrisvy),list.files(anoScriptDir(agrisvy),pattern = "_ano.R$"))

  if(length(path_script)!=0){
    purrr::walk(path_script,source)
  }

  #clear data
  path_list_data=file.path(anoDataDir(agrisvy),list.files(anoDataDir(agrisvy),recursive = TRUE))

  if (length(path_list_data)!=0) {
    purrr::walk(path_list_data,file.remove)
  }
  source(file.path(anoScriptDir(agrisvy),"final.R"))
}

