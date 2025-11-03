#' Print a initial info message during preprocessing
#'
#' @param msg message to be printed
#'
#' @return
#' @importFrom  crayon green silver italic
#' @importFrom cli cli_alert_info
#' @export
#'
#' @examples
preprocMessage=function(msg){

  cli::cli_alert_info(paste(crayon::green("PREPROCESSING: "),crayon::silver(crayon::italic(msg))))
}


#' Print initial info message during anonymization
#'
#' @param msg message to be printed
#'
#' @return
#' @importFrom  crayon cyan silver italic
#' @importFrom cli cli_alert_info
#' @export
#'
#' @examples
sdcMessage=function(msg){

  cli::cli_alert_info(paste(crayon::cyan("ANONYMIZATION: "),crayon::silver(crayon::italic(msg))))
}


#' Print initial message in finalization of anonymization
#'
#' @param msg message to be printed
#'
#' @return
#' @importFrom  crayon blue silver italic
#' @importFrom cli cli_alert_info
#' @export
#'
#' @examples
finMessage=function(msg){

  cli::cli_alert_info(paste(crayon::blue("FINALIZATION OF ANONYMIZATION: "),crayon::silver(crayon::italic(msg))))
}


addSpace=function(char,n){
  n1=nchar(char)
  n2=n1+n+2
  paste(char,paste(rep(" ",times=(81-n2)),collapse = ""),"|*")
}


#' generate a message
#'
#' @param txt1 first text
#' @param txt2 second text
#'
#' @return
#' a character
#' @importFrom cli cli_alert_info
#' @importFrom crayon green silver italic
#' @export
#'
#' @examples
agrisMsg=function(txt1,txt2) {
  cli::cli_alert_info(paste(crayon::green(paste0(txt1,": ")),crayon::silver(crayon::italic(txt2))))
}


#' Generate a function to label specific variable
#'
#' @param labels
#' @param levels
#'
#' @return
#' @importFrom dplyr %>% mutate
#' @importFrom labelled to_labelled
#' @importFrom rlang :=
#' @export
#'
#' @examples

label_val_gen=function(labels,levels) {

  function(data,var){
    names(levels)=labels
    data %>% mutate({{var}}:=to_labelled(
      as.factor(as.character({{var}})),
      levels
    ))
  }
}



#' specify haven read function depending on the data type
#'
#' @param agrisvy an agrisvy object
#' @param path
#' @param enquote
#'
#' @return
#' @export
#'
#' @examples
readDataFunc <- function(agrisvy,path,enquote=TRUE){
  if (agrisvy@type==".dta"){
    if(enquote){
      res=paste0("read_dta('",path,"')")
    } else{
      res=paste0("read_dta(",path,")")
    }
  }

  if (agrisvy@type %in% c(".SAV",".sav")){
    if(enquote) {
      res=paste0("read_sav('",path,"')")
    } else{
      res=paste0("read_sav(",path,")")
    }
  }
  if (agrisvy@type %in% c(".enc")){
    if(enquote) {
      res=paste0("read_enc('",path,"', password=Sys.getenv('pw'))")
    } else{
      res=paste0("read_enc(",path,", password=Sys.getenv('pw'))")
    }
  }

  return(res)
}



#' Specify haven write function depending on the data type
#'
#' @param agrisvy an agrisvy object
#' @param dat
#' @param path
#' @param enquote
#' @param obj_name
#'
#' @return
#' @export
#'
#' @examples
writeDataFunc <- function(agrisvy,dat,path,enquote=TRUE,obj_name=NULL){

  if (agrisvy@type==".dta"){
    if(enquote) {
      res=paste0("write_dta(",dat,", '",path,"')")
    } else{
      res=paste0("write_dta(",dat,", ",path,")")
    }
  }
  if (agrisvy@type %in% c(".SAV",".sav")){
    if(enquote){
      res=paste0("write_sav(",dat,", '",path,"')")
    } else{
      res=paste0("write_sav(",dat,", ",path,")")
    }
  }
  if (agrisvy@type==".enc"){
    if(enquote) {
      res=paste0("write_enc(",dat,", '",path,"',password=Sys.getenv('pw'), rounds=",obj_name,"@enc_args[['rounds']],size=",obj_name,"@enc_args[['size']])")
    } else{
      res=paste0("write_enc(",dat,", ",path,",password=Sys.getenv('pw'), rounds=",obj_name,"@enc_args[['rounds']],size=",obj_name,"@enc_args[['size']])")
    }
  }

  return(res)
}





#' Lebeling binary variable
#'
#' @param data dataset
#' @param var binary variable to be labeled
#' @param lang takes \code{"en"} for English or \code{"fr"} for french
#' @param bin takes \code{"01"} or \code{"12"}
#'
#' @importFrom dplyr pull
#' @importFrom dplyr mutate
#' @importFrom labelled is.labelled
#' @importFrom labelled set_value_labels
#'
#'
#' @return
#' @export
#'
#' @examples
label_binary=function(data,var,bin="01",lang="en") {
  if(is.factor(data %>% dplyr::pull({{var}}))==TRUE) {
    data=data %>% dplyr::mutate({{var}}:=as.numeric({{var}}))
  }

  if(data %>% pull({{var}}) %>% is.labelled(.)==FALSE) {

    if (bin=="01"){
      data=data %>% set_value_labels(
        {{var}}:=c("Yes"=1,"No"=0)
      )
    }

    if (bin=="12"){
      data=data %>% set_value_labels(
        {{var}}:=c("Yes"=1,"No"=2)
      )
    }


  }
  return(data)
}


#' Label a set of binary labels
#'
#' @param data dataframe
#' @param vars list of binary labels to be labeled
#' @param bin  takes \code{"01"} or \code{"12"}
#' @param lang takes \code{"en"} or \code{"fr"}
#'
#' @return
#' @export
#'
#' @examples
label_binary_at=function(data,vars,bin="01", lang) {

  for (v in vars) {

    vv=sym(v)
    data=data %>% label_binary({{vv}}, bin=bin, lang=lang)

  }

  return(data)
}


#' Label binary variable based on pattern in variable names
#'
#' @param data dataframe
#' @param pattern common pattern of binary variables
#' @param bin takes \code{"01"} or \code{"12"}
#' @param lang takes \code{"en"} for english or \code{"fr"} for french
#'
#' @return
#' @export
#'
#' @examples
label_binary_pattern=function(data,pattern,bin="01",lang) {

  vars=grep(pattern,names(data),value = TRUE)

  for (v in vars) {
    vv=sym(v)
    data=data %>% label_binary({{vv}},bin=bin, lang=lang)
#
  }

  return(data)

}

#' Label binary variable based on pattern in variable names
#'
#' @param agrisvy dataframe

#' @return
#'
#' @examples

check_path_limit=function(agrisvy) {

  .throw_message=function(x){
    long_path_data=full_data_path[which(nchar(x)>260)]
    #check limit path length for input data
    if(length(long_path_data)!=0) {
      message("The below file path are too long!")
      for(i in seq_along(long_path_data)) {
        cli::cli_alert_danger(paste(crayon::red("PATH:"),crayon::silver(crayon::italic(long_path_data[i]))))
      }
      stop()
    }
  }


  data_flder=unlist(strsplit(agrisvy@dataDir, "/"))

  datafd_n=nchar(agrisvy@dataDir)
  wdfld_n=nchar(agrisvy@workingDir)

  if(datafd_n>260) stop("The path of the data folder is too long")

  if(wdfld_n>260) stop("The path of the working directory folder is too long")

  data_flder=data_flder[length(data_flder)]

  data_files   <- list.files(file.path(agrisvy@dataDir),
                             pattern = glue::glue("{agrisvy@type}$"),
                             recursive = TRUE)

  full_data_path=file.path(file.path(agrisvy@dataDir),data_files)

  #check limit path lenth for preprocessed data
  file_path_proc_df=file.path(file.path(agrisvy@workingDir),agrisvy@preprocDataDir,
                              glue::glue("{gsub(agrisvy@type,\"\",{data_files})}_proc{agrisvy@type}"))

  #Check limit path length for anonymized data
  file_path_ano_df=file.path(file.path(agrisvy@workingDir),agrisvy@anoDataDir,
                             glue::glue("{gsub(agrisvy@type,\"\",{data_files})}_ano{agrisvy@type}"))

  #check limit path length for temporary anonymized data
  file_path_tmp_df=file.path(file.path(agrisvy@workingDir),agrisvy@tempfileDir,"temp_ano",
                             glue::glue("{gsub(agrisvy@type,\"\",{data_files})}_tmp{agrisvy@type}"))

  #check limit path length for preprocesed scripts
  file_path_proc_r=file.path(file.path(agrisvy@workingDir),agrisvy@preProcScriptDir,
                             glue::glue("{gsub(agrisvy@type,\"\",{data_files})}_proc.R"))

  #check limit path length for anonymization scripts
  file_path_ano_r=file.path(file.path(agrisvy@workingDir),agrisvy@anoScriptDir,
                            glue::glue("{gsub(agrisvy@type,\"\",{data_files})}_ano.R"))


  file_path=c(full_data_path,file_path_proc_df,file_path_ano_df,file_path_tmp_df,
              file_path_proc_r,file_path_ano_r)
  .throw_message(file_path)


  # check limit path length for varclass
  df_name=gsub(agrisvy@type,"",data_files)
  df_name_nchar_31=df_name[which(nchar(df_name)>31)]
  if(length(df_name_nchar_31)!=0) {
    message("The below dataset name are too long to be used as Excel sheet name in the variable classification file!")
    for(i in seq_along(df_name_nchar_31)) {
      cli::cli_alert_danger(paste(crayon::red("Dataset name:"),crayon::silver(crayon::italic(df_name_nchar_31[i]))))
    }
    stop("Please shorten the name(s) of the dataset(s) above to less or equal than 26 characters")
  }

# sheet names for file description
  df_name=paste0(gsub(agrisvy@type,"",data_files),"_ano")
  df_name_nchar_31=df_name[which(nchar(df_name)>31)]
  if(length(df_name_nchar_31)!=0) {
    message("The below dataset name are too long to be used as Excel sheet name in the dataset description file!")
    for(i in seq_along(df_name_nchar_31)) {
      cli::cli_alert_danger(paste(crayon::red("Dataset name:"),crayon::silver(crayon::italic(df_name_nchar_31[i]))))
    }
    stop("Please shorten the name(s) of the original dataset(s) above to less or equal than 26 characters")
  }



  x            <- lapply(strsplit(data_files, "/"), function(z) as.data.frame(t(z)))
  x1           <- rbind.fill(x)
  wb           <- lapply(x, function(z) {
    res=paste(z[1:length(z) - 1], sep = "", collapse = "_")
    if(res=="") res=data_flder
    res=gsub(" ","_",res)

    return(res)
  })

  unique_wb    <- unique(unlist(wb))
  file_name = unlist(lapply(x, function(z) {
    paste(gsub(agrisvy@type, "", z[length(z)]), sep = "", collapse = "_")
  }))

  varclass=file.path(agrisvy@workingDir,agrisvy@varClassDir,gsub(" ","_",paste0(data_flder,"_VarClas.xlsx")))
  long_varclass_path=varclass[which(nchar(varclass)>260)]
  if(length(long_varclass_path)!=0) {
    message("The path to the variable classication workbook below will be too long!")
    for(i in seq_along(df_name_nchar_31)) {
      cli::cli_alert_danger(paste(crayon::red("workbook:"),crayon::silver(crayon::italic(long_varclass_path[i]))))
    }
    stop()
  }

  #check limit path length for file description
  filedes=file.path(agrisvy@workingDir,agrisvy@fileDesDir,gsub(" ","_",paste0(data_flder,".xlsx")))
  long_filedes_path=filedes[which(nchar(filedes)>260)]

  if(length(long_varclass_path)!=0) {
    message("The path to the file sdescription workbook below will be too long!")
    for(i in seq_along(long_varclass_path)) {
      cli::cli_alert_danger(paste(crayon::red("workbook:"),crayon::silver(crayon::italic(long_varclass_path[i]))))
    }
    stop()
  }


}


#' Load the agrisvy object in the R session by executing the  script '_R/_setup.R'
#'
#' @return
#' @export
#'
#' @examples
loadAgrisvyObj=function(){
source("_R/_setup.R")
}

#' load all R scripts in the folder '_R'
#'
#' @return
#' @importFrom purrr walk
#' @export
#'
#' @examples
loadRscripts=function(){
  purrr::walk(file.path("_R",list.files(path="_R",pattern = ".R$")),source)

}



#' Information on Excel files and workbook to be created
#'
#' @param agrisvy agrisurvey object
#'  @importFrom  plyr rbind.fill
#' @return
#'
#' @examples
.createExcelInfos=function(agrisvy) {
  data_flder=unlist(strsplit(DataPath(agrisvy), "/"))
  data_flder=data_flder[length(data_flder)]

  data_files   <- list.files(file.path(DataPath(agrisvy)),
                             pattern = glue::glue("{agrisvy@type}$"),
                             recursive = TRUE)

  x            <- lapply(strsplit(data_files, "/"), function(z) as.data.frame(t(z)))
  x1           <- rbind.fill(x)

  wb           <- lapply(x, function(z) {
    res=paste(z[1:length(z) - 1], sep = "", collapse = "_")
    if(res=="") res=data_flder
    res=gsub(" ","_",res)

    return(res)
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

  final_res=list(workbook=unique_wb,files_infos=data_summary)

  return(final_res)
}
