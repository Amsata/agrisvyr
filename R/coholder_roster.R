

coholder_questionnaire <- function(){

  Questionnaire <- list()




  return(Questionnaire)
}

.coholder_rst=function(holdingID,n,quest){


  df=data.frame(holdingID=rep(holdingID,n))

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

df=df %>% dplyr::mutate(
  !!quest$S2_Q04a@QuestVar[[lang]]:=gen_names(!!quest$S2_Q04d@QuestVar[[lang]],"first"),
  !!quest$S2_Q04b@QuestVar[[lang]]:=gen_names(!!quest$S2_Q04d@QuestVar[[lang]],"last"),
  !!quest$S2_Q04c@QuestVar[[lang]]:=generator::r_national_identification_numbers(n),
  !!quest$S2_Q04e@QuestVar[[lang]]:=round(sn::rsn(n, xi=35, omega=7, alpha=9, tau=2,  dp=NULL))
  )


#LABELLING VARIABLES

for (q in seq_along(quest)) {
  var=sym(quest[[q]]@QuestVar[[lang]])
  data=data %>% labelled::set_variable_labels(
    {{var}}:= quest[[q]]@QuestLab[[lang]]
  )
}

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


return(df)
}
