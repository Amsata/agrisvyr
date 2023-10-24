
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
  agris_var_sym=sym(agris_var)

  df=data %>% st_drop_geometry() %>% dplyr::select(id:={{id_sym}},name:={{name_sym}}) %>%
    dplyr::distinct(id,name)

  label_name=label_val_gen(df$name,df$id)

  data=data %>% label_name({{name_sym}}) %>% dplyr::select(-{{id_sym}}) %>%
    dplyr::rename({{agris_var_sym}}:={{name_sym}})

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


#' Title
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
generateAgrisData <- function(quest,shpfile, level=3,
                               geo_id=c("ID_1","ID_2","ID_3"),
                               geo_name=c("NAME_1","NAME_2","NAME_3"),
                               np=30,na_prob=0.0005,lang="en"){

  stopifnot(lang %in% c("en","fr","es"))

  shpfile=shpfile %>% dplyr::select(all_of(c(geo_id,geo_name)))


  if (level==1){
    shpfile=shpfile %>% label_geo_name(geo_name[1],geo_id[1],quest$Q13a@QuestVar[[lang]]) %>%
      dplyr::mutate(!!sym(quest$Q13b@QuestVar[[lang]]):=NA,
                    !!(quest$Q13c@QuestVar[[lang]]):=NA)
  }

  if(level==2){
    shpfile=shpfile %>% label_geo_name(geo_name[1],geo_id[1],quest$Q13a@QuestVar[[lang]])
    shpfile=shpfile %>% label_geo_name(geo_name[2],geo_id[2],quest$Q13b@QuestVar[[lang]]) %>%
      dplyr::mutate(!!sym(quest$Q13c@QuestVar[[lang]]):=NA)
  }

  if(level==3){
    shpfile=shpfile %>% label_geo_name(geo_name[1],geo_id[1],quest$Q13a@QuestVar[[lang]])
    shpfile=shpfile %>% label_geo_name(geo_name[2],geo_id[2],quest$Q13b@QuestVar[[lang]])
    shpfile=shpfile %>% label_geo_name(geo_name[3],geo_id[3],quest$Q13c@QuestVar[[lang]])
  }


  points=shpfile %>%st_sample(shpfile, size = np:(np+1), type = "random") %>% sf::st_as_sf()
  points_df=as.data.frame(st_coordinates(points))


  final_points=points %>% sf::st_join(shpfile) %>%
    bind_cols(points_df) %>% dplyr::rename(!!sym(quest$Q19a@QuestVar[[lang]]):=X,
                                           !!sym(quest$Q19b@QuestVar[[lang]]):=Y) %>%
    dplyr::mutate(!!sym(quest$Q17d@QuestVar[[lang]]):=!!sym(quest$Q13b@QuestVar[[lang]]),
                  !!sym(quest$Q17c@QuestVar[[lang]]):=!!sym(quest$Q13a@QuestVar[[lang]]),
                  !!sym(quest$Q17b@QuestVar[[lang]]):=!!sym(quest$Q13c@QuestVar[[lang]]))


  data=final_points


  # CREATING CATEGORICAL VARIABLES
  for (q in seq_along(quest)) {
    cat_vars=quest[[q]]@QuestType=="cat." & !is.null(quest[[q]]@QuestValProbs)
    if(cat_vars) {
      data=data %>% dplyr::mutate(
        !!quest[[q]]@QuestVar[[lang]]:= wakefield::r_sample(n,x=quest[[q]]@QuestValue[[lang]],
                                                            prob=quest[[q]]@QuestValProbs)
      )
    }
  }


  data=data %>% dplyr::mutate(

    #*****************************************************************************
    #* PART 1.2: IDENTIFICATION OF THE HOLDING ***********************************
    #* ***************************************************************************
    holdingID=wakefield::id(n)
    #*****************************************************************************
    #* PART 1.2: AGRICULTURAL ACTIVITY         ***********************************
    #* ***************************************************************************

  )   %>%
    # skip controle

    #*****************************************************************************
    #* PART 1.2: IDENTIFICATION OF THE HOLDING ***********************************
    #* ***************************************************************************
    #If Q10 = 1 or 2 GO TO Q12, otherwise GO TO Q14
    dplyr::mutate(
    )  %>%
    #generate enumeration area
    dplyr::mutate(Region=ifelse(!is.na(!!quest$Q13a@QuestVar[[lang]]),!!quest$Q13a@QuestVar[[lang]],
                                !!quest$Q17c@QuestVar[[lang]])) %>%
    dplyr::group_by(Region) %>% dplyr::mutate(!!quest$Q15@QuestVar[[lang]]:=paste0(stringr::str_sub(Region,7),
                                                         base::sample(100:120,1))) %>% dplyr::ungroup() %>%
    dplyr::select(#-check,
                  -Region) %>%
    # perturbate by including some missing information
    wakefield::r_na(prob = na_prob)


  #LABELLING VARIABLES

  for (q in seq_along(quest)) {
    data=data %>% labelled::set_variable_labels(
      !!quest[[q]]@QuestVar[[lang]]:= quest[[q]]@QuestLab[[lang]]
    )
  }

  #LZBELLING VALUES

  for (q in seq_along(quest)) {

    if (!is.null(quest[[q]]@QuestValue[[lang]])) {

      levels=seq_along(quest[[q]]@QuestValue[[lang]])
      labels=quest[[q]]@QuestValue[[lang]]
      names(levels)=labels

      data=data %>% mutate(!!quest[[q]]@QuestVar[[lang]]:=to_labelled(
        as.factor(as.character(!!quest[[q]]@QuestVar[[lang]])),
        levels
      ))

    }

  }


  return(data)

}
