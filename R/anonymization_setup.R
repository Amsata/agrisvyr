
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
labels <- function(fileName,type,encoding=encoding,password,rounds,size) {

  if(type %in% ".dta"){
    curData <- haven::read_dta(fileName,encoding = encoding)
    }

  if(type %in% c(".SAV",".sav")){
    curData <- haven::read_sav(fileName,encoding = encoding)
  }

  if(type==".enc") {
    curData=read_enc(path=fileName,password=password,rounds=rounds,size=size)
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

create_wb <- function(agrisvy, data, wb_file,encoding,password) {

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
    curDat <- labels(df$path[i],type=agrisvy@type,encoding = encoding,password=password,rounds=agrisvy@enc_args[["rounds"]],size=agrisvy@enc_args[["size"]])
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
    #Locking cellsF
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
#' @importFrom  plyr rbind.fill

generate_varclas <- function(agrisvy,encoding="UTF-8",password) {
  exelFilesInfos=.createExcelInfos(agrisvy)
  # create the different unique wb
  purrr::walk(exelFilesInfos$workbook, function(x) {
    create_wb(agrisvy, exelFilesInfos$files_infos, x,encoding=encoding,password=password)
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




create_preproc_r <- function(agrisvy, file,type,obj_name) {

   opn="{{"
  clse="}}"
msg=paste(unlist(strsplit(file, "/")), collapse = "=>")
  z <- unlist(strsplit(file, "/"))

if(type=="proc"){
  r_file=file.path(preProcScriptDir(agrisvy), gsub(paste0(agrisvy@type, "$"),paste0("_proc.",agrisvy@script_format), file))
  save_path=file.path(preprocDataDir(agrisvy),paste0(paste(gsub(agrisvy@type,"",file[length(file)]),sep = "", collapse = "_"),paste0("_proc",agrisvy@type)))
  template=system.file(agrisvy@script_format,agrisvy@language,"preprocessing_script.txt",package = "agrisvyr")
}

if(type=="ano"){
  r_file=file.path(anoScriptDir(agrisvy),gsub(paste0(agrisvy@type, "$"),paste0("_ano.",agrisvy@script_format), file))
  save_path=file.path(tempfileDir(agrisvy),"temp_ano",paste0(paste(gsub(agrisvy@type, "",file[length(file)]),sep = "", collapse = "_"),paste0("_tmp",agrisvy@type)))
  template=system.file(agrisvy@script_format,agrisvy@language,"anonymization_script.txt",package = "agrisvyr")
  file = file.path(preprocDataDir(agrisvy),paste0(paste(gsub(agrisvy@type, "",file[length(file)]),sep = "",collapse = "_" ), paste0("_proc",agrisvy@type)))
 }

  varclass=file.path(varClassDir(agrisvy),paste0(gsub(" ","_",paste(z[1:length(z) - 1], sep = "",
                                      collapse = "_")),"_VarClas.xlsx" ))

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
    to_save        = save_path
  )

  #read_function=eval(paste0("readDataFunc({glue(obj_name)},","file.path(data_path, '",file.path(file),"'))")),
  #write_function="writeDataFunc(agrisvy,data,file_attributes$to_save)"
  if(type=="proc") {

    file_attributes$read_function=readDataFunc(agrisvy,paste0("file.path(data_path,'",file.path(file),"')"),enquote=FALSE)

    if(agrisvy@script_format=="R") {
      file_attributes$write_function=writeDataFunc(agrisvy,"data",file_attributes$to_save,obj_name=obj_name)
    } else if(agrisvy@script_format %in% c("rmd","qmd")) {
      file_attributes$write_function=writeDataFunc(agrisvy,"data",paste0("file.path(wd,'",file_attributes$to_save,"')"),obj_name=obj_name,enquote=FALSE)
    }

  }

  if(type=="ano") {
    if(agrisvy@script_format=="R") {
      file_attributes$read_function=readDataFunc(agrisvy,file.path(file),enquote=TRUE)
      file_attributes$write_function=writeDataFunc(agrisvy,"ano_data",file_attributes$to_save,obj_name=obj_name)
    } else if (agrisvy@script_format %in% c("rmd","qmd")) {
      file_attributes$read_function=readDataFunc(agrisvy,paste0("file.path(wd,'",file.path(file),"')"),enquote=FALSE)
      file_attributes$write_function=writeDataFunc(agrisvy,"ano_data",paste0("file.path(wd,'",file_attributes$to_save,"')"),obj_name=obj_name,enquote=FALSE)
    }
  }

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

  data_files <- list.files(DataPath(agrisvy),
                           pattern = paste0(agrisvy@type, "$"),
                           recursive = TRUE
                           )

  purrr::walk(data_files, function(x) {
    create_preproc_r(agrisvy, x,type,obj_name)
  })
}



generate_report_template <- function(agrisvy) {

      stopifnot(dir.exists(anoreportDir(agrisvy)))

    template_sdc=system.file(agrisvy@rpt_format,agrisvy@language,"sdc_report.txt",package = "agrisvyr")
    template_info_loss=system.file(agrisvy@rpt_format,agrisvy@language,"infos_loss_report.txt",package = "agrisvyr")

    #if (agrisvy@language=="en") template_sdc=system.file("txt_template","sdc_report_en.txt",package = "agrisvyr")
    #if (agrisvy@language=="fr") template_sdc=system.file("txt_template","sdc_report_fr.txt",package = "agrisvyr")
    #if (agrisvy@language=="es") template_sdc=system.file("txt_template","sdc_report_es.txt",package = "agrisvyr")
    #if (agrisvy@language=="pt") template_sdc=system.file("txt_template","sdc_report_pt.txt",package = "agrisvyr")

    #if (agrisvy@language=="en") template_info_loss=system.file("txt_template","infos_loss_report_en.txt",package = "agrisvyr")
    #if (agrisvy@language=="fr") template_info_loss=system.file("txt_template","infos_loss_report_fr.txt",package = "agrisvyr")
    #if (agrisvy@language=="es") template_info_loss=system.file("txt_template","infos_loss_report_es.txt",package = "agrisvyr")
    #if (agrisvy@language=="pt") template_info_loss=system.file("txt_template","infos_loss_report_pt.txt",package = "agrisvyr")

  file_sdc <- file.path(anoreportDir(agrisvy), paste0("sdc_report.",agrisvy@rpt_format))
  file.create(file_sdc,showWarnings = FALSE)
#Information loss
  file_info_loss <- file.path(infoLossReport(agrisvy), paste0("information_loss_report.",agrisvy@rpt_format))
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
#' @param renv
#' @param encryption
#' @param rounds
#' @param size
#' @param rpt_format
#' @param script_format
#'
#' @return
#' @importFrom  haven read_dta write_dta
#' @importFrom renv init
#' @importFrom renv install
#' @importFrom getPass getPass
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
setup_anonymization <- function(agrisvy,
                                overwrite=FALSE,
                                open=FALSE,
                                encoding="UTF-8",
                                rpt_format="rmd",
                                script_format="R",
                                renv=FALSE,
                                encryption=FALSE,
                                rounds=100L,
                                size=32L) {

  #verifify the limit of path lengh that exist and for files that will be
  sbt_agrisvy=substitute(agrisvy)
  obj_name=deparse(sbt_agrisvy)

  stopifnot(rpt_format %in% c("rmd","qmd"))
  stopifnot(rpt_format %in% c("R","rmd","qmd"))

  check_path_limit(agrisvy)


  agrisvy@rpt_format=rpt_format
  agrisvy@script_format=script_format

  setwd(agrisvy@workingDir)
  old_wd=agrisvy@workingDir

  if (isTRUE(dir.exists("SDC")) & isFALSE(overwrite)) {
    message(glue::glue("SDC already exists!"))
  } else {
    unlink("SDC", recursive = TRUE)
  }

  #R.utils::copyDirectory(DataPath(agrisvy), "data")

  if(encryption){

    agrisMsg("DATA ENCRYPTION","encrypting the input data...")

    password <- getPass::getPass("Enter encryption password: ",noblank=TRUE)

    if (isTRUE(dir.exists("enc_data")) & isFALSE(overwrite)) {
      message(glue::glue("enc_data folder already exists!"))
    } else {
      unlink("enc_data", recursive = TRUE)
      dir.create("enc_data")
    }

    copyDirStr(from = DataPath(agrisvy), to = "enc_data")

    exelFilesInfos=.createExcelInfos(agrisvy)
    nb_wb=unique(exelFilesInfos$workbook)

    if(length(nb_wb)==1) {
      saving_path=file.path("enc_data",paste0(exelFilesInfos$files_infos$file_name,".enc"))
    } else {
      saving_path=file.path("enc_data",exelFilesInfos$files_infos$workbook,paste0(exelFilesInfos$files_infos$file_name,".enc"))
    }

    reading_path=exelFilesInfos$files_infos$path

    purrr::walk2(reading_path,saving_path,function(x,y){

      if(agrisvy@type==".dta") {dat=haven::read_dta(x)}
      if(agrisvy@type %in% c(".SAV",".sav")) {dat=haven::read_sav(x)}

      write_enc(df=dat,path=y,password = password,rounds = rounds,size=size)

    },.progress = TRUE)

    agrisvy@type=".enc"
    agrisvy@dataDir=file.path(agrisvy@workingDir,"enc_data")
    agrisvy@enc_args=list(rounds=as.integer(rounds),size=as.integer(size))
  }

  options(usethis.allow_nested_project = TRUE)
  create_project(path = "SDC", open = open, rstudio = TRUE)
  agrisvy@workingDir <- file.path(agrisvy@workingDir,"SDC")
  setwd(agrisvy@workingDir)


  unlink("R",recursive = TRUE)
  # stopifnot(inherits(agrisvy,"agrisvy"))
  create_ano_folders(agrisvy, overwrite = overwrite)
  generate_varclas(agrisvy,encoding=encoding,password = password)
  #generateLabelFiles(agrisvy,encoding=encoding)
  copyDirStr(from = DataPath(agrisvy), to = preProcScriptDir(agrisvy))
  copyDirStr(from = DataPath(agrisvy), to = preprocDataDir(agrisvy))
  generate_preproc_r(agrisvy,type="proc",obj_name=obj_name)
  copyDirStr(from = DataPath(agrisvy), to = anoScriptDir(agrisvy))
  copyDirStr(from = DataPath(agrisvy), to = anoDataDir(agrisvy))
  copyDirStr(from = DataPath(agrisvy), to = file.path(tempfileDir(agrisvy), "temp_ano"))
  copyDirStr(from = DataPath(agrisvy), to = file.path(tempfileDir(agrisvy), "Labels"))
  export_labels(agrisvy,encoding=encoding,overwrite=overwrite,password=password)
  generate_preproc_r(agrisvy,type="ano",obj_name=obj_name)
  generate_report_template(agrisvy)


  labexist=dir.exists(file.path(tempfileDir(agrisvy), "Labels"))
  if(labexist==FALSE) stop("folder deleted before here...")

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

  if(encryption) {

    write_enc(agrisvy,path=file.path("_R",paste0(obj_name,".enc")),password=password,rounds=rounds,size=size)
  } else{
    saveRDS(agrisvy,file.path("_R",paste0(obj_name,".rds")))
  }

  if(encryption) {

    set_up=file.path("_R","_setup.R")
    file.create(set_up,showWarnings = FALSE)
    fileConn <- file(set_up)

    writeLines(
      c(glue::glue("setwd(\"{agrisvy@workingDir}\")"),"",
        glue::glue("data_path=\"{agrisvy@dataDir}\""),"",
        glue::glue("{obj_name} <-read_enc(\"{agrisvy@workingDir}/_R/{obj_name}.enc\",password=Sys.getenv('pw'),rounds={rounds}L,size={size}L)")),
      fileConn
    )
    close(fileConn)

  } else {
    set_up=file.path("_R","_setup.R")
    file.create(set_up,showWarnings = FALSE)
    fileConn <- file(set_up)

    writeLines(
      c(glue::glue("#setwd(\"{agrisvy@workingDir}\")"),"",
        glue::glue("data_path=\"{agrisvy@dataDir}\""),"",
        glue::glue("{obj_name} <-readRDS(\"{agrisvy@workingDir}/_R/{obj_name}.rds\")")),
      fileConn
    )
    close(fileConn)
  }

#create an R Profile
  if(encryption){
    rprofile=file.path(".Rprofile")
    file.create(rprofile,showWarnings = FALSE)
    fileConn <- file(rprofile)
    writeLines(
      c(glue::glue("library(getPass)"),
        glue::glue("Sys.setenv(pw = getPass::getPass(\"Enter encryption password: \",noblank=TRUE))")
        ),fileConn)

     close(fileConn)
  }
  #Run first prerocessing
  Sys.setenv(pw = password) #allow to run the first
  runPreproc(agrisvy)
  runAnon(agrisvy,with_final=FALSE)
  #run ano-----------
  #clear data
  # list_data=list.files(file.path(tempfileDir(agrisvy),"temp_ano"),recursive = TRUE)
  # path_list_data=file.path(file.path(tempfileDir(agrisvy),"temp_ano"),list_data)
  #
  # if (length(path_list_data)!=0) {
  #   purrr::walk(path_list_data,file.remove)
  # }
  #
  # #Run all pre-processing
  # list_script=list.files(anoScriptDir(agrisvy),pattern = "_ano.R$",recursive = TRUE)
  # path_script=file.path(anoScriptDir(agrisvy),list_script)
  #
  # if(length(path_script)!=0){
  #   purrr::walk(path_script,source)
  # }

  #----------
  #generate final.R ano file


  temp_ano_files=list.files(file.path(tempfileDir(agrisvy),"temp_ano"),pattern = paste0(agrisvy@type,"$"),recursive = TRUE)

  path_ano_temp_files=file.path(tempfileDir(agrisvy),"temp_ano",temp_ano_files)

  # read_files=paste0("data=read_dta(\"",path_ano_temp_files,"\")")
  read_fn=sapply(path_ano_temp_files,function(x){readDataFunc(agrisvy,x)})
  read_files=glue::glue("data={read_fn}")
  dataset=gsub("_tmp","",temp_ano_files)
  ano_dataset=paste0(gsub(agrisvy@type,
                          "",
                          dataset),
                     paste0("_ano",agrisvy@type))

  # save_files=paste0("write_dta(data,\"07_Anonymized data/","ano_",dataset,"\")")

  save_files=sapply(ano_dataset,function(x){writeDataFunc(agrisvy,'data',file.path(anoDataDir(agrisvy),x),obj_name=obj_name)})
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
          "library(labelled)","",
          "purrr::walk(file.path(\"_R\",list.files(path=\"_R\",pattern = \".R$\")),source)",
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

 #generate files description
  genAllFileDes(agrisvy,id_cols=50,encoding=encoding,password=password)
  #archive data
  # ArchiveAnoData(agrisvy)
  # ArchiveCleanData(agrisvy)
  # ArchiveProcData(agrisvy)


  if(renv==TRUE) {
    renv::init(project =agrisvy@workingDir,load = FALSE,restart = FALSE)
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
  list_script=list.files(preProcScriptDir(agrisvy),pattern = paste0(".",agrisvy@script_format,"$"),recursive = TRUE)
  path_script=file.path(preProcScriptDir(agrisvy),list_script)

  if(length(path_script)!=0){
    purrr::walk(path_script, function(f) {
      ext <- agrisvy@script_format

      if (ext == "R") {
        # R script: source directly
        source(f)

      } else if (ext %in% c("rmd", "qmd")) {
        # Rmd or QMD: extract R chunks and source
        tmp_r <- tempfile(fileext = ".R")
        knitr::purl(f, output = tmp_r, documentation = 0,quiet=TRUE)
        source(tmp_r)
      } else {
        message("Unsupported file type: ", f)
      }
    })
  }

}

#' Run all anonymization scripts
#'
#' @param agrisvy
#' @param with_final
#'
#' @return
#' @export
#'
#' @examples
runAnon <- function(agrisvy,with_final=TRUE){

  #clear data
  list_data=list.files(file.path(tempfileDir(agrisvy),"temp_ano"),recursive = TRUE)
  path_list_data=file.path(file.path(tempfileDir(agrisvy),"temp_ano"),list_data)

  if (length(path_list_data)!=0) {
    purrr::walk(path_list_data,file.remove)
  }

  #Run all pre-processing
  path_script=file.path(anoScriptDir(agrisvy),list.files(anoScriptDir(agrisvy),pattern = paste0("_ano.",agrisvy@script_format,"$"),recursive = TRUE))

  if(length(path_script)!=0){
    purrr::walk(path_script, function(f) {
      ext <- agrisvy@script_format

      if (ext == "R") {
        # R script: source directly
        source(f)

      } else if (ext %in% c("rmd", "qmd")) {
        # Rmd or QMD: extract R chunks and source
        tmp_r <- tempfile(fileext = ".R")
        knitr::purl(f, output = tmp_r, documentation = 0,quiet=TRUE)
        source(tmp_r)
      } else {
        message("Unsupported file type: ", f)
      }
    })
  }

  #clear data
  if(with_final) {
    path_list_data=file.path(anoDataDir(agrisvy),list.files(anoDataDir(agrisvy),recursive = TRUE))

    if (length(path_list_data)!=0) {
      purrr::walk(path_list_data,file.remove)
    }

    if(agrisvy@script_format=="R") source(file.path(anoScriptDir(agrisvy),"final.R"))
  }

}

