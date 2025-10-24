
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

#' Title
#'
#' @param agrisvy
#' @param encoding
#' @param overwrite
#' @importFrom tibble tibble
#' @return
#' @export
#'
#' @examples

export_labels=function(agrisvy,encoding="UTF-8",overwrite=TRUE) {
  df_list=.createExcelInfos(agrisvy)
  df=df_list$files_infos
  fileNames <- df$file_name

  for (i in 1:length(fileNames)) {
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "mapping")

    if(agrisvy@type %in% ".dta"){
      df2 <- haven::read_dta(df$path[i],encoding = encoding)
    }

    if(agrisvy@type %in% c(".SAV",".sav")){
      df2 <- haven::read_sav(df$path[i],encoding = encoding)
    }
    curDat <- data.frame(
      "name"  = names(df2),
      "label" = sapply(df2, function(x) attr(x, "label")) %>% as.character(),
      "new_label"=""
    )

    curDat$value_labels_id <- paste0("value_labels_",1:nrow(curDat))
    curDat$has_value_labels <- sapply(names(df2),function(x) {
      res=ifelse(labelled::is.labelled(df2[[x]]),"Y","N")


    })

    openxlsx::writeData(wb,sheet = "mapping",x = curDat)

    extract_labels2 <- function(var, var_name,df) {
      if (is.labelled(df[[var]])) {
        labs <- val_labels(df[[var]])
        res=tibble(
          value = unname(labs),
          label = names(labs),
          new_label=""
        )

      } else {
        res=tibble(value = numeric(), label = character(),new_label=character())
      }
      openxlsx::addWorksheet(wb, var_name)
      openxlsx::writeData(wb, sheet = var_name,x = res)
    }

    # Apply to all variables
    purrr::walk2(curDat$name, curDat$value_labels_id, extract_labels2,df2)

    for (j in seq_len(nrow(curDat))) {
      sheet_name <- curDat$value_labels_id[j]
      # create hyperlink formula: reference cell A1 of the target sheet
      #link <- paste0("#'", sheet_name, "'!A1")

      # Write hyperlink into the cell in column "C" (3rd column)
      writeFormula(
        wb,
        sheet = "mapping",
        x = makeHyperlinkString(sheet = sheet_name, row = 1, col = 4, text=sheet_name),
        startCol = 4,
        startRow = j + 1 # +1 because of header ro

      )

      writeFormula(
        wb,
        sheet = sheet_name,
        x = makeHyperlinkString(sheet = "mapping",j + 1, row = , col = 4, text="back to mapping"),
        startCol = 4,
        startRow = 1 # +1 because of header ro

      )
    }
    nb_wb=length(unique(df$workbook))
    #openxlsx::openXL(wb)
    if(nb_wb==1) {
      path_to_save=file.path(agrisvyr:::tempfileDir(agrisvy), "Labels",paste0(fileNames[i],".xlsx"))
    }
    if(nb_wb>1) {
      path_to_save=file.path(agrisvyr:::tempfileDir(agrisvy), "Labels",df$workbook[i],paste0(fileNames[i],".xlsx"))
    }
    openxlsx::saveWorkbook(wb,path_to_save,overwrite = overwrite)
  }
}
