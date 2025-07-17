
#' Title
#'
#' @param agrisvy
#' @param encoding
#'
#' @return
#' @export
#'
#' @examples
generateLabelFiles=function(agrisvy,encoding=encoding) {

  exelFilesInfos=.createExcelInfos(agrisvy)

  # create the different unique wb

  purrr::walk(exelFilesInfos$workbook, function(x) {
    .createLabelsWB(agrisvy, exelFilesInfos$files_infos, x,encoding=encoding)
  })

}

#' Title
#'
#' @param agrisvy
#' @param data
#' @param wb_file
#' @param encoding
#'
#' @return
#'
#' @examples
.createLabelsWB <- function(agrisvy, data, wb_file,encoding) {

  agrisMsg("VARIABLE LABELS EXTRACTION",paste0("Creating workbook ",wb_file))


  df <- data %>% dplyr::filter(workbook %in% wb_file)
  fileNames <- df$file_name
  wb <- openxlsx::createWorkbook()

  cli_progress_bar("Generating variable classification", total = length(fileNames))

  for (i in 1:length(fileNames)) {
    cli_progress_update()
    openxlsx::addWorksheet(wb, fileNames[i])
    curDat <- labels(df$path[i],type=agrisvy@type,encoding=encoding)
    curDat$newLabels <- ""
    curDat$NbrCharacter <- ""

    names(curDat)=c("variable","label","newLabel","Number of character")

    openxlsx::writeData(wb,
                        sheet = fileNames[i],
                        x = curDat,
    )
    openxlsx::setColWidths(wb, sheet = fileNames[i], cols = 1, widths = "auto")
    openxlsx::setColWidths(wb = wb,sheet  = fileNames[i],cols = c(2, 3),widths = c(80, 80))
    openxlsx::addStyle(wb, fileNames[i],
                       style = createStyle(
                         wrapText = TRUE, valign = "center", fontName = "Times New Roman"
                       ), cols = c(2, 3),
                       rows = 1:nrow(curDat)+1, gridExpand = TRUE, stack = TRUE
    )

    openxlsx::protectWorksheet(wb=wb,sheet = fileNames[i], protect = TRUE,
                               lockFormattingCells = FALSE, lockFormattingColumns = FALSE,
                               lockInsertingColumns = TRUE, lockDeletingColumns = TRUE,
                               lockFormattingRows=FALSE,lockInsertingRows=TRUE,
                               lockDeletingRows=TRUE
    )
    openxlsx::addStyle(wb, sheet = fileNames[i], style = createStyle(locked = FALSE),rows = 1:nrow(curDat)+1, cols = 3)
    openxlsx::addStyle(wb, sheet = fileNames[i], style = createStyle(locked = FALSE),rows = 1:nrow(curDat)+1, cols = 4)

  }

  openxlsx::saveWorkbook(wb,
                         file.path(fileDesDir(agrisvy),
                                   glue::glue("{wb_file}_labels.xlsx")
                         ),
                         overwrite = TRUE
  )
}



#' Assign new variable labels define in the Excel file of label in the file description folder
#'
#' @param df
#' @param df_lab
#'
#' @return
#' @export
#'
#' @examples
assignNewVarLabels=function(df,df_lab) {
  varsIndf=names(df)
  varsInNewLabels=df_lab %>% dplyr::filter(newLabel!="" | !is.na(newLabel)) %>% dplyr::pull(variable)
  commonVars= base::intersect(varsIndf,varsInNewLabels)

  if (length(commonVars)>0) {
    for (i in seq_along(commonVars)) {
      var = commonVars[i]
      v = sym(var)
      label = df_lab[df_lab$variable==commonVars[i],]$newLabel[1]
      df=df %>% labelled::set_variable_labels({{v}}:=label)
    }
  }

  return(df)
}
