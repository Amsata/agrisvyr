
#' @importFrom randomNames randomNames

gen_names=function(n,first_last){
  randomNames::randomNames(n,
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
#' @importFrom sn rsn
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
    shpfile=shpfile %>% label_geo_name(geo_name[1],geo_id[1],quest$S1_Q13a@QuestVar[[lang]]) %>%
      dplyr::mutate(!!sym(quest$S1_Q13b@QuestVar[[lang]]):=NA,
                    !!(quest$S1_Q13c@QuestVar[[lang]]):=NA)
  }

  if(level==2){
    shpfile=shpfile %>% label_geo_name(geo_name[1],geo_id[1],quest$S1_Q13a@QuestVar[[lang]])
    shpfile=shpfile %>% label_geo_name(geo_name[2],geo_id[2],quest$S1_Q13b@QuestVar[[lang]]) %>%
      dplyr::mutate(!!sym(quest$S1_Q13c@QuestVar[[lang]]):=NA)
  }

  if(level==3){
    shpfile=shpfile %>% label_geo_name(geo_name[1],geo_id[1],quest$S1_Q13a@QuestVar[[lang]])
    shpfile=shpfile %>% label_geo_name(geo_name[2],geo_id[2],quest$S1_Q13b@QuestVar[[lang]])
    shpfile=shpfile %>% label_geo_name(geo_name[3],geo_id[3],quest$S1_Q13c@QuestVar[[lang]])
  }


  points=shpfile %>%st_sample(shpfile, size = np:(np+1), type = "random") %>% sf::st_as_sf()
  points_df=as.data.frame(st_coordinates(points))


  final_points=points %>% sf::st_join(shpfile) %>%
    bind_cols(points_df) %>% dplyr::rename(!!sym(quest$S1_Q19a@QuestVar[[lang]]):=X,
                                           !!sym(quest$S1_Q19b@QuestVar[[lang]]):=Y) %>%
    dplyr::mutate(!!sym(quest$S1_Q17d@QuestVar[[lang]]):=!!sym(quest$S1_Q13b@QuestVar[[lang]]),
                  !!sym(quest$S1_Q17c@QuestVar[[lang]]):=!!sym(quest$S1_Q13a@QuestVar[[lang]]),
                  !!sym(quest$S1_Q17b@QuestVar[[lang]]):=!!sym(quest$S1_Q13c@QuestVar[[lang]]))


  data=final_points
  n=nrow(final_points)

  #TODO: why loop do not run?

  # CREATING CATEGORICAL VARIABLES
  for (q in seq_along(quest)) {
    var=sym(quest[[q]]@QuestVar[[lang]])
    cat_vars=quest[[q]]@QuestType=="cat." & !is.null(quest[[q]]@QuestValProbs)
    if(cat_vars==TRUE) {
      data=data %>% dplyr::mutate(
        {{var}}:= wakefield::r_sample(n,x=quest[[q]]@QuestValue[[lang]],
                                                            prob=quest[[q]]@QuestValProbs)
      )
    }
  }


  data=data %>% dplyr::mutate(

    #*****************************************************************************
    #* PART 1.2: IDENTIFICATION OF THE HOLDING ***********************************
    #* ***************************************************************************
    holdingID=wakefield::id(n),
    !!quest$S1_Q03a@QuestVar[[lang]]:=gen_names(n,"first"),
    !!quest$S1_Q03b@QuestVar[[lang]]:=gen_names(n,"last"),
    !!quest$S1_Q12a@QuestVar[[lang]]:=gen_names(n,"first"),
    !!quest$S1_Q12b@QuestVar[[lang]]:=gen_names(n,"last"),
    !!quest$S1_Q12d@QuestVar[[lang]]:=generator::r_national_identification_numbers(n),
    !!quest$S1_Q14@QuestVar[[lang]]:=gen_names(n,"both"),
    !!quest$S1_Q16@QuestVar[[lang]]:=generator::r_national_identification_numbers(n),
    !!quest$S1_Q17a@QuestVar[[lang]]:=generator::r_ipv4_addresses(n),
    !!quest$S1_Q17e@QuestVar[[lang]]:=generator::r_phone_numbers(n),
    !!quest$S1_Q20@QuestVar[[lang]]:=generator::r_national_identification_numbers(n),
    !!quest$S1_Q21a@QuestVar[[lang]]:=generator::r_national_identification_numbers(n),
    !!quest$S1_Q21b@QuestVar[[lang]]:=generator::r_national_identification_numbers(n),
    !!quest$S1_Q21c@QuestVar[[lang]]:=generator::r_national_identification_numbers(n),
    !!quest$S1_Q21d@QuestVar[[lang]]:=NA,
    !!quest$S1_Q22@QuestVar[[lang]]:=NA,
    !!quest$S2_Q01e@QuestVar[[lang]]:=round(sn::rsn(n, xi=35, omega=7, alpha=9, tau=2,  dp=NULL)),
    !!quest$S2_Q02e@QuestVar[[lang]]:=round(sn::rsn(n, xi=35, omega=7, alpha=9, tau=2,  dp=NULL)),
    !!quest$S3_Q02@QuestVar[[lang]] :=round(sn::rsn(n, xi=0, omega=3, alpha=20, tau=-1,  dp=NULL)),
    !!quest$S2_Q03@QuestVar[[lang]] :=round(sn::rsn(n,  xi=0, omega=2, alpha=20, tau=-1,  dp=NULL)),
    !!quest$S2_Q06@QuestVar[[lang]] :=round(sn::rsn(n,  xi=0, omega=2, alpha=20, tau=-1,  dp=NULL)),
    !!quest$S2_Q07@QuestVar[[lang]] :=round(sn::rsn(n, xi=0, omega=1, alpha=15, tau=0,  dp=NULL)),
    !!quest$S2_Q08@QuestVar[[lang]] :=round(sn::rsn(n, xi=1.5, omega=0.5, alpha=15, tau=0,  dp=NULL))


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
    dplyr::mutate(Region=ifelse(!is.na(!!quest$S1_Q13a@QuestVar[[lang]]),!!quest$S1_Q13a@QuestVar[[lang]],
                                !!quest$S1_Q17c@QuestVar[[lang]])) %>%
    dplyr::group_by(Region) %>% dplyr::mutate(!!quest$S1_Q15@QuestVar[[lang]]:=paste0(stringr::str_sub(Region,7),
                                                         base::sample(100:120,1))) %>% dplyr::ungroup() %>%
    dplyr::select(#-check,
                  -Region) %>%
    # perturbate by including some missing information
    wakefield::r_na(prob = na_prob)



  data=data %>%   dplyr::mutate(!!quest$S1_Q12a@QuestVar[[lang]]:=.data[[quest$S1_Q12a@QuestVar[[lang]]]] %>% labelled::recode_if(!(.data[[quest$S1_Q10@QuestVar[[lang]]]] ==quest$S1_Q10@QuestValue$en[3]),NA),
                                !!quest$S1_Q12b@QuestVar[[lang]]:=.data[[quest$S1_Q12b@QuestVar[[lang]]]] %>% labelled::recode_if((.data[[quest$S1_Q10@QuestVar[[lang]]]]  ==quest$S1_Q10@QuestValue$en[3]),NA),
                                !!quest$S1_Q12c@QuestVar[[lang]]:=.data[[quest$S1_Q12c@QuestVar[[lang]]]] %>% labelled::recode_if((.data[[quest$S1_Q10@QuestVar[[lang]]]]  ==quest$S1_Q10@QuestValue$en[3]),NA),
                                !!quest$S1_Q12d@QuestVar[[lang]]:=.data[[quest$S1_Q12d@QuestVar[[lang]]]] %>% labelled::recode_if((.data[[quest$S1_Q10@QuestVar[[lang]]]]  ==quest$S1_Q10@QuestValue$en[3]),NA),
                                !!quest$S1_Q13a@QuestVar[[lang]]:=.data[[quest$S1_Q13a@QuestVar[[lang]]]] %>% labelled::recode_if((.data[[quest$S1_Q10@QuestVar[[lang]]]]  ==quest$S1_Q10@QuestValue$en[3]),NA),
                                !!quest$S1_Q13b@QuestVar[[lang]]:=.data[[quest$S1_Q13b@QuestVar[[lang]]]] %>% labelled::recode_if((.data[[quest$S1_Q10@QuestVar[[lang]]]]  ==quest$S1_Q10@QuestValue$en[3]),NA),
                                !!quest$S1_Q13c@QuestVar[[lang]]:=.data[[quest$S1_Q13c@QuestVar[[lang]]]] %>% labelled::recode_if((.data[[quest$S1_Q10@QuestVar[[lang]]]]  ==quest$S1_Q10@QuestValue$en[3]),NA),
                                !!quest$S1_Q14@QuestVar[[lang]] :=.data[[quest$S1_Q14@QuestVar[[lang]]]]  %>% labelled::recode_if(!(.data[[quest$S1_Q10@QuestVar[[lang]]]] ==quest$S1_Q10@QuestValue$en[3]),NA),
                                #If Q17 = 1 → Go to Q18
                                !!quest$S1_Q17@QuestVar[[lang]] :=.data[[quest$S1_Q17@QuestVar[[lang]]]]  %>% labelled::recode_if(!(.data[[quest$S1_Q10@QuestVar[[lang]]]] ==quest$S1_Q10@QuestValue$en[3]),quest$S1_Q17@QuestValue$en[2]),
                                !!quest$S1_Q17a@QuestVar[[lang]]:=.data[[quest$S1_Q17a@QuestVar[[lang]]]] %>% labelled::recode_if((.data[[quest$S1_Q17@QuestVar[[lang]]]]  ==quest$S1_Q17@QuestValue$en[1]),NA),
                                !!quest$S1_Q17b@QuestVar[[lang]]:=.data[[quest$S1_Q17b@QuestVar[[lang]]]] %>% labelled::recode_if((.data[[quest$S1_Q17@QuestVar[[lang]]]]  ==quest$S1_Q17@QuestValue$en[1]),NA),
                                !!quest$S1_Q17c@QuestVar[[lang]]:=.data[[quest$S1_Q17c@QuestVar[[lang]]]] %>% labelled::recode_if((.data[[quest$S1_Q17@QuestVar[[lang]]]]  ==quest$S1_Q17@QuestValue$en[1]),NA),
                                !!quest$S1_Q17d@QuestVar[[lang]]:=.data[[quest$S1_Q17d@QuestVar[[lang]]]] %>% labelled::recode_if((.data[[quest$S1_Q17@QuestVar[[lang]]]]  ==quest$S1_Q17@QuestValue$en[1]),NA),
                                !!quest$S1_Q17e@QuestVar[[lang]]:=.data[[quest$S1_Q17e@QuestVar[[lang]]]] %>% labelled::recode_if((.data[[quest$S1_Q17@QuestVar[[lang]]]]  ==quest$S1_Q17@QuestValue$en[1]),NA),
                                #replace by NA if same region, same district, same village and correct Q17
                                check= ((.data[[quest$S1_Q17c@QuestVar[[lang]]]] == .data[[quest$S1_Q13a@QuestVar[[lang]]]]) &
                                          (.data[[quest$S1_Q17b@QuestVar[[lang]]]] ==.data[[quest$S1_Q13c@QuestVar[[lang]]]]) &
                                          (.data[[quest$S1_Q17d@QuestVar[[lang]]]] ==.data[[quest$S1_Q13b@QuestVar[[lang]]]]) &
                                          (.data[[quest$S1_Q17@QuestVar[[lang]]]] ==quest$S1_Q17@QuestValue$en[2])),
                                !!quest$S1_Q17a@QuestVar[[lang]]:=.data[[quest$S1_Q17a@QuestVar[[lang]]]] %>% labelled::recode_if(check==TRUE,NA),
                                !!quest$S1_Q17b@QuestVar[[lang]]:=.data[[quest$S1_Q17b@QuestVar[[lang]]]] %>% labelled::recode_if(check==TRUE,NA),
                                !!quest$S1_Q17c@QuestVar[[lang]]:=.data[[quest$S1_Q17c@QuestVar[[lang]]]] %>% labelled::recode_if(check==TRUE,NA),
                                !!quest$S1_Q17d@QuestVar[[lang]]:=.data[[quest$S1_Q17d@QuestVar[[lang]]]] %>% labelled::recode_if(check==TRUE,NA),
                                !!quest$S1_Q17e@QuestVar[[lang]]:=.data[[quest$S1_Q17e@QuestVar[[lang]]]] %>% labelled::recode_if(check==TRUE,NA),
                                !!quest$S1_Q17@QuestVar[[lang]] :=.data[[quest$S1_Q17@QuestVar[[lang]]]]  %>% labelled::recode_if(check==TRUE,quest$S1_Q17@QuestValue$en[1])
                  )




  #LZBELLING VALUES

  for (q in seq_along(quest)) {

    if (!is.null(quest[[q]]@QuestValue[[lang]])) {

      var=sym(quest[[q]]@QuestVar[[lang]])

      levels=seq_along(quest[[q]]@QuestValue[[lang]])
      labels=quest[[q]]@QuestValue[[lang]]
      names(levels)=labels

      data=data %>% mutate({{var}}:=to_labelled(
        as.factor(as.character({{var}})),
        levels
      ))

    }}


  #LABELLING VARIABLES

  for (q in seq_along(quest)) {
    var=sym(quest[[q]]@QuestVar[[lang]])
    data=data %>% labelled::set_variable_labels(
      {{var}}:= quest[[q]]@QuestLab[[lang]]
    )
  }

  return(data)

}
