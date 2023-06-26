
#' @importFrom randomNames randomNames

gen_names=function(sex,first_last){
  randomNames::randomNames(1,
              gender=sex,
              ethnicity=base::sample(c("African American", "Hispanic", "Asian", "White", "Native American"),1),
              which.names=first_last,
              name.sep=" ",
              sample.with.replacement=TRUE,
              return.complete.data=FALSE)

}


#' @importFrom  dplyr select distinct rename
#' @importFrom rlang := sym
#' @importFrom sf st_drop_geometry

label_geo_name=function(data,label,level,agris_var){
  id_sym=sym(level)
  name_sym=sym(label)

  df=data %>% st_drop_geometry() %>% dplyr::select(id:={{id_sym}},name:={{name_sym}}) %>%
    dplyr::distinct(id,name)

  label_name=label_val_gen(df$name,df$id)

  data=data %>% label_name({{name_sym}}) %>% dplyr::select(-{{id_sym}}) %>%
    dplyr::rename({{agris_var}}:={{name_sym}})

  return(data)
}


#' generate agris survey data
#'
#' @param shpfile
#' @param level
#' @param geo_id
#' @param geo_name
#' @param np
#' @param na_prob
#' @param language
#'
#' @return
#' @export
#'
#' @examples
generate_agrisdata <- function(shpfile, level=3,
                               geo_id=c("ID_1","ID_2","ID_3"),
                               geo_name=c("NAME_1","NAME_2","NAME_3"),
                               np=30,na_prob=0.0005,language="en"){

stopifnot(language %in% c("en","fr"))

  res=NULL
  if(language=="en") {
    res=agrisdata_en(shpfile, level=level,
                 geo_id=geo_id,
                 geo_name=geo_name,
                 np=np,na_prob=na_prob)
  }

  if(language=="fr") {
    res=agrisdata_fr(shpfile, level=level,
                 geo_id=geo_id,
                 geo_name=geo_name,
                 np=np,na_prob=na_prob)
  }

return(res)
}
