
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
#' @importFrom labelled val_labels
#' @importFrom labelled is.labelled
#' @return
#' @export
#'
#' @examples

export_labels=function(agrisvy,encoding="UTF-8",overwrite=TRUE,password) {

  agrisMsg("EXPORTING LABELS","exporting data, variable and value labels for eventual updates")
  df_list=.createExcelInfos(agrisvy)
  df=df_list$files_infos
  fileNames <- df$file_name

  for (i in 1:length(fileNames)) {
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "datalabel")
    openxlsx::addWorksheet(wb, "varlabels")
    openxlsx::addWorksheet(wb, "vallabels")


    if(agrisvy@type %in% ".dta"){
      df2 <- haven::read_dta(df$path[i],encoding = encoding)
    }

    if(agrisvy@type %in% c(".SAV",".sav")){
      df2 <- haven::read_sav(df$path[i],encoding = encoding)
    }


    if(agrisvy@type %in% ".enc"){
      df2 <- read_enc(df$path[i],password=password,rounds=agrisvy@enc_args[["rounds"]],size=agrisvy@enc_args[["size"]])
    }
    datalabel_df=data.frame(
      label=as.character(attr(df2, "label")),
      new_label=""
    )
    openxlsx::writeData(wb,sheet = "datalabel",x = datalabel_df)

    curDat <- data.frame(
      "name"  = names(df2),
      "label" = sapply(df2, function(x) attr(x, "label")) %>% as.character(),
      "new_label"=""
    )

    #curDat$value_labels_id <- paste0("value_labels_",1:nrow(curDat))
    # curDat$has_value_labels <- sapply(names(df2),function(x) {
    #   res=ifelse(labelled::is.labelled(df2[[x]]),"Y","N")
    # })

    openxlsx::writeData(wb,sheet = "varlabels",x = curDat)

    extract_labels2 <- function(var,df) {
      if (labelled::is.labelled(df[[var]])) {
        labs <- labelled::val_labels(df[[var]])
        res=tibble(
          var_name=rep(x=var,times=length(labs)),
          value = unname(labs),
          label = names(labs),
          new_label=""
        )

      } else {
        res=tibble(var_name=var,value = numeric(), label = character(),new_label=character())
      }
      return(res)
    }

    # Apply to all variables
    res_lst=lapply(curDat$name, function(x){extract_labels2(x,df2)})
    res_df=do.call("rbind",res_lst)
    openxlsx::writeData(wb,sheet = "vallabels",x = res_df)

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



#' Extract data label, variable labels, value labels from excel file
#'
#' @param agrisvy
#' @param dataset_name
#' @param subfolder
#' @importFrom readxl read_excel
#' @return
#' @export
#'
#' @examples
  update_labels=function(dat,agrisvy,dataset_name,subfolder=NULL){

    df_list=.createExcelInfos(agrisvy)

    df=df_list[["files_infos"]]

    df <- df[df[["file_name"]] == dataset_name, , drop = FALSE]

    fileNames <- df[["file_name"]]

    # --- Build the file path ---
    label_dir <- file.path(agrisvy@workingDir,tempfileDir(agrisvy), "Labels")

    if (!nzchar(subfolder)) {
      path_to_read <- file.path(label_dir, paste0(dataset_name, ".xlsx"))
    } else {
      path_to_read <- file.path(label_dir, subfolder, paste0(dataset_name, ".xlsx"))
    }

    # --- Read Excel (base R cannot read .xlsx directly) ---
    read_excel_base <- function(path, sheet) {
      if (file.exists(path)) {
        openxlsx ::read.xlsx(xlsxFile=path,sheet = sheet)
      } else {
        stop(paste("Expected CSV for sheet", sheet, "not found:", path))
      }
    }

    # --- Read dataframes ---
    datalabel   <- read_excel_base(path_to_read, "datalabel")
    varlabels   <- read_excel_base(path_to_read, "varlabels")
    vallabels_df <- read_excel_base(path_to_read, "vallabels")

    # --- Update dataset label ---
    if (!is.na(datalabel[["new_label"]][1])) attr(dat, "label") <- datalabel[["new_label"]][1]

    # --- Update variable labels ---
    names_varlabels <- varlabels[["name"]]
    varlabel_updated <- !is.na(varlabels[["new_label"]])

    # Replace labels if new_label is present
    varlabels[["label"]][!is.na(varlabels[["new_label"]])] <- varlabels[["new_label"]][!is.na(varlabels[["new_label"]])]

    # Convert to named list and filter only updated ones
    varlabels_list <- as.list(varlabels[["label"]])
    names(varlabels_list) <- names_varlabels
    varlabels_list <- varlabels_list[varlabel_updated]
    # Apply labels
    for (vn in names(varlabels_list)) {

      #considrer only variables in the dataset
      if (vn %in% names(dat)) attr(dat[[vn]], "label") <- varlabels_list[[vn]]
    }

    # --- Update value labels ---
    vallabels_df[["label"]][!is.na(vallabels_df[["new_label"]])] <- vallabels_df[["new_label"]][!is.na(vallabels_df[["new_label"]])]
    vallabels_df[["update"]] <- !is.na(vallabels_df[["new_label"]])

    #TO DO: add the option only_update
    names_vallabels <- unique(vallabels_df[["var_name"]][vallabels_df[["update"]]])
    #names_vallabels <- unique(vallabels_df[["var_name"]])

    # Create value label lists
    vallabels <- lapply(names_vallabels, function(x) {
      subset_df <- vallabels_df[vallabels_df[["var_name"]] == x, , drop = FALSE]
      vals <- as.numeric(subset_df[["value"]])
      names(vals) <- subset_df[["label"]]
      vals
    })

    names(vallabels) <- names_vallabels

    # Attach value labels as attribute
    for (vn in names_vallabels) {

      if(vn %in% names(dat)) attr(dat[[vn]], "labels") <- vallabels[[vn]]
    }

    return(dat)
  }
