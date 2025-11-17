


#' @import openxlsx
#' @importFrom haven read_dta
#' @importFrom haven read_sav
#' @import rlang
#' @importFrom dplyr pull
#' @importFrom questionr freq

genFileDes <- function(file, name, id_cols, wb,type,encoding,password,rounds,size) {
  hd1 <- openxlsx::createStyle(
    fontName = "Times New Roman",
    fontSize = 12,
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

  hd2 <- openxlsx::createStyle(
    fontName = "Times New Roman",
    fontSize = 12,
    fontColour = "black",
    numFmt = "GENERAL",
    border = c("top", "bottom", "left", "right"),
    borderColour = getOption("openxlsx.borderColour", "black"),
    borderStyle = getOption("openxlsx.borderStyle", "medium"),
    bgFill = NULL,
    fgFill = NULL,
    halign = "center",
    valign = NULL,
    textDecoration = "bold",
    wrapText = TRUE,
    textRotation = NULL,
    indent = NULL,
    locked = NULL,
    hidden = NULL
  )
  openxlsx::addWorksheet(wb, name, gridLines = FALSE)
  openxlsx::mergeCells(wb, name, 1:3, 1)
  openxlsx::writeData(wb, name, "Files description", rowNames = FALSE, startRow = 1, borders = "all", headerStyle = hd1)


  counter <- 2
  if(type %in% ".dta")   data <- haven::read_dta(file,encoding=encoding)
  if(type %in% c(".SAV",".sav"))   data <- haven::read_sav(file,encoding=encoding)
  if(type %in% ".enc")   data <- read_enc(file,password,rounds,size)

  label <- sapply(data, function(x) attr(x, "label")) %>% as.character()
  variable_names <- names(data)

  suma <- function(data, var) {
    if (class(data %>% dplyr::pull(rlang::sym(var)))[1] %in% c("factor", "character", "haven_labelled") & length(unique(data %>% dplyr::pull(rlang::sym(var)))) < id_cols) {
      if("haven_labelled" %in% class(data %>% dplyr::pull(rlang::sym(var))) & "character" %in% class(data %>% dplyr::pull(rlang::sym(var))) ) {
        data[[var]]=as.character(data[[var]])
      }
      result <- paste(row.names(as.data.frame(questionr::freq(data[, var]))),
        as.data.frame(questionr::freq(data[, var]))[, "n"],
        sep = ": N=", collapse = "\n"
      )
    } else {
      if (class(data %>% dplyr::pull(rlang::sym(var)))[1] %in% c("numeric", "integer", "double")) {
        result <- paste(names(summary(data[, var])), summary(data[, var]), collapse = "\n")
      } else {
        result <- "-"
      }
    }

    return(result)
  }

  variable_description <- sapply(names(data), function(x) {
    suma(data, x)
  })

  data_description <- data.frame(
    cbind(
      Variable = variable_names,
      `variable label` = label,
      `Variable description` = variable_description
    )
  )

  dst_des <- data.frame(
    rbind(
      c("Number of cases", nrow(data)),
      c("Number of variables", length(names(data))),
      c("Content", "")
    )
  )

  names(dst_des) <- c("Dataset name", name)



  openxlsx::writeData(wb, name, dst_des,
    rowNames = FALSE,
    startRow = counter, startCol = 2, headerStyle = hd1,
    borders = "all"
  )
  openxlsx::writeData(wb, name, data_description, rowNames = FALSE, startRow = counter + 5, borders = "all", headerStyle = hd2)

  openxlsx::addStyle(wb, name, style = createStyle(fgFill = "#E5E7E9", fontSize = 10), cols = c(2, 3), rows = (counter + 1):(counter + 3), gridExpand = TRUE, stack = TRUE)
  openxlsx::addStyle(wb, name, style = createStyle(textDecoration = "bold"), cols = c(2), rows = (counter + 1):(counter + 3), gridExpand = TRUE, stack = TRUE)

  openxlsx::addStyle(wb, name, style = createStyle(fontSize = 10), cols = c(1, 2, 3), rows = (counter + 5):(counter + nrow(data_description) + 5), gridExpand = TRUE, stack = TRUE)
  openxlsx::addStyle(wb, name, style = createStyle(textDecoration = "bold"), cols = c(1), rows = (counter + 5):(counter + nrow(data_description) + 5), gridExpand = TRUE, stack = TRUE)


  openxlsx::setColWidths(wb, name, cols = c(1, 2, 3), widths = c(20, 25, 50))
  openxlsx::addStyle(wb, name,
    style = createStyle(
      wrapText = TRUE, valign = "center", fontName = "Times New Roman"
    ), cols = c(1, 2, 3),
    rows = 1:nrow(data_description) + 7, gridExpand = TRUE, stack = TRUE
  )

  openxlsx::protectWorksheet(wb=wb,sheet = name, protect = TRUE,
                             lockFormattingCells = FALSE, lockFormattingColumns = FALSE,
                             lockInsertingColumns = TRUE, lockDeletingColumns = TRUE,
                             lockFormattingRows=FALSE,lockInsertingRows=TRUE,
                             lockDeletingRows=TRUE
  )

  openxlsx::addStyle(wb, sheet = name, style = createStyle(locked = FALSE),rows = 1:nrow(data_description) + 7, cols = 4)
  openxlsx::addStyle(wb, sheet = name, style = createStyle(locked = FALSE),rows = 5, cols = 3)

}




#' @import openxlsx
#' @importFrom dplyr filter
#' @importFrom cli cli_progress_along
#' @importFrom purrr pwalk

genDataFolderDes=function(agrisvy,data,wb_name,id_cols,encoding,password){

  agrisMsg("FILES DESCRIPTION",paste0("Creating workbook ",wb_name))

  wb =openxlsx::createWorkbook()
  df <- data[as.character(data[,3]) %in% wb_name,]

  purrr::walk(cli::cli_progress_along(df$path),~{
    genFileDes(df$path[.x],df$file_name[.x],id_cols,wb,type=agrisvy@type,encoding=encoding,password = password,rounds=agrisvy@enc_args[["rounds"]],size=agrisvy@enc_args[["size"]])
  })
  openxlsx::saveWorkbook(wb,file.path(fileDesDir(agrisvy),paste0(wb_name,".xlsx")),overwrite = TRUE)

}



#' Generate file description for the data
#'
#' @param agrisvy an agris survey object
#' @param id_cols column to ignore
#'
#' @return
#' @importFrom plyr rbind.fill
#' @importFrom purrr walk
#' @export
#'
#' @examples
genAllFileDes=function(agrisvy,id_cols=NULL,encoding="UTF-8",password){

  data_flder=unlist(strsplit(DataPath(agrisvy), "/"))
  data_flder=data_flder[length(data_flder)]

  data_files   <- list.files(anoDataDir(agrisvy),
                             recursive = TRUE,pattern = agrisvy@type)

  x            <- lapply(strsplit(data_files, "/"), function(z) as.data.frame(t(z)))
  x1           <- rbind.fill(x)

  wbk           <- lapply(x, function(z) {
    res=paste(z[1:length(z) - 1], sep = "", collapse = "_")
    if(res=="") res=data_flder
    return(res)
  })

  unique_wb    <- unique(unlist(wbk))

  file_name = unlist(lapply(x, function(z) {
    paste(gsub(agrisvy@type, "", z[length(z)]), sep = "", collapse = "_")
  }))

  data_summary <- data.frame(
    file_name = file_name,
    path      = file.path(anoDataDir(agrisvy), data_files),
    workbook  = unlist(wbk)
  )

  purrr::walk(unique_wb, function(x) {
    genDataFolderDes(agrisvy,data_summary,x,id_cols,encoding=encoding,password=password)
  })

}
