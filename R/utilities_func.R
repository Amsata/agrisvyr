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
#'
#' @return
#' @export
#'
#' @examples
readDataFunc <- function(agrisvy){
  if (agrisvy@type==".dta"){res="read_dta"}
  if (agrisvy@type %in% c(".SAV",".sav")){res="read_sav"}
return(res)
}



#' Specify haven write function depending on the data type
#'
#' @param agrisvy an agrisvy object
#'
#' @return
#' @export
#'
#' @examples
writeDataFunc <- function(agrisvy){
  if (agrisvy@type==".dta"){res="write_dta"}
  if (agrisvy@type %in% c(".SAV",".sav")){res="write_sav"}
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
