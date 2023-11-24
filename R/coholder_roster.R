

coholder_questionnaire <- function(){

  Questionnaire <- list()
  
  ##SECTION 2: CHARACTERISTICS OF THE HOLDERS AND MANAGERS
  
  #CASE 2: THE HOLDER IS A GROUP OF CIVIL/NATURAL PERSONS
  
  #Q04
  Questionnaire$S2_Q04a <-
    createQuestion(QuestVar   = list(en="S2_Q04a",fr="S2_Q04a",es="S2_P04a"),
                   QuestType  = "text",
                   QuestLab   = list(en="First name of Co-Holder",
                                     fr="Prénom du co-exploitant",
                                     es="Nombre del Cotitular"),
                   QuestValue = NULL,
                   QuestValProbs = NULL
    )
  
  Questionnaire$S2_Q04b <-
    createQuestion(QuestVar   = list(en="S2_Q04b",fr="S2_Q04b",es="S2_P04b"),
                   QuestType  = "text",
                   QuestLab   = list(en="Surname of co-holder",
                                     fr="Nom du co-exploitant",
                                     es="Apellido del Cotitular"),
                   QuestValue = NULL,
                   QuestValProbs = NULL
    )
  
  Questionnaire$S2_Q04c <-
    createQuestion(QuestVar   = list(en="S2_Q04c",fr="S2_Q04c",es="S2_P04c"),
                   QuestType  = "cont.",
                   QuestLab   = list(en="Contact number (preferably cell phone)",
                                     fr="Numéro de téléphone (de préférence mobile)",
                                     es="Número	de	contacto	(preferiblemente	teléfono	cel"),
                   QuestValue = NULL,
                   QuestValProbs = NULL
    )
  
  Questionnaire$S2_Q04d <-
    createQuestion(QuestVar   = list(en="S2_Q04d",fr="S2_Q04d",es="S2_P04d"),
                   QuestType  = "cat.",
                   QuestLab   = list(en="Sex of co-holder",
                                     fr="Sexe du co-exploitant",
                                     es="Sexo del	cotitular"),
                   QuestValue = list(en=c("Male","Female"),fr=c("Masculin","Féminin"),es=c("Masculino","Femenino")),
                   QuestValProbs = c(0.50,0.45)
    )
  
  Questionnaire$S2_Q04e <-
    createQuestion(QuestVar   = list(en="S2_Q04e",fr="S2_Q04e",es="S2_P04e"),
                   QuestType  = "cont.",
                   QuestLab   = list(en="Age in completed years",
                                     fr="Âge en années révolues",
                                     es="Edad	en	años	completos"),
                   QuestValue = NULL,
                   QuestValProbs = NULL
    )
  
  
  Questionnaire$S2_Q04f <-
    createQuestion(QuestVar =list(en="S2_Q04f",fr="S2_Q04f",es="S2_P04f"),
                   QuestType     ="cat.",
                   QuestLab      =list(en="Nationality",fr="Nationalité",es="Nacionalidad"),
                   QuestValue    = list(en=c("Local country","Neighbouring country","Other"),
                                        fr=c("Pays de résidence","Pays voisin","Autre"),
                                        es=c("País	local","País	vecino","Otro")),
                   QuestValProbs = c(0.8,0.2,0.1)
    )
  
  Questionnaire$S2_Q04h <-
    createQuestion(QuestVar =list(en="S2_Q04h",fr="S2_Q04h",es="S2_P04h"),
                   QuestType     ="cat.",
                   QuestLab      =list(en="Highest level of education completed",
                                       fr="Plus haut niveau d'étude atteint",
                                       es="Nivel	de	educación	más	alto	completado"),
                   QuestValue    = list(en=c("None","Less than primary","Primary","Lower secondary","Upper secondary","Tertiary/post-secondary"),
                                        fr=c("Aucun","Moins que primaire","Primaire","Secondaire court","Secondaire long","Postsecondaire"),
                                        es=c("Ninguna","Inferior	a	primaria","Primaria","Secundaria	inferior","Secundaria	superior"," Terciaria/postsecundaria")),
                   QuestValProbs = c(0.2,0.2,0.2,0.2,0.1,0.1)
    )
  
  Questionnaire$S2_Q04i <-
    createQuestion(QuestVar =list(en="S2_Q04i",fr="S2_Q04i",es="S2_P04i"),
                   QuestType     ="cat.",
                   QuestLab      =list(en="Share of working time spent working on the holding",
                                       fr="Temps de travail sur l'exploitation",
                                       es="Porción	de	tiempo	de	trabajo	que	dedicó	a	la	unidad	de	producción"),
                   QuestValue    = list(en=c("Less than half ( < 40 %)","About half (40%-59%)","Most/almost all (60%-99%)","All (100%)"),
                                        fr=c("Moins d'un mi-temps ( < 40 %)","Mi-temps (40%-59%)","Plus qu'un mi-temps (60%-99%","Temps complet (100%)"),
                                        es=c("Menos	de	la	mitad	(<40%)","Alrededor	de	la	mitad	(40%-59%)","La	mayoría/casi	todo	(60%-99%)","Todo	(100%)")),
                   QuestValProbs = c(0.2,0.2,0.2,0.2)
    )
  
  Questionnaire$S2_Q04j <-
    createQuestion(QuestVar   = list(en="S2_Q04j",fr="S2_Q04j",es="S2_P04j"),
                   QuestType  = "cat.",
                   QuestLab   = list(en="Does the co-holder have another gainful activity outside of the holding?",
                                     fr="Le co-exploitant a-t-il une autre activité rémunérée en dehors de l'exploitation?",
                                     es="¿Realiza	el	Cotitular	otra	actividad	lucrativa	fuera	de	la	unidad	de	producción?"),
                   QuestValue = list(en=c("Yes","No"),fr=c("Oui","Non"),es=c("Si","No")),
                   QuestValProbs = c(0.9,0.05)
    )
  
 
  Questionnaire$S2_Q04k <-
    createQuestion(QuestVar   = list(en="S2_Q04k",fr="S2_Q04k",es="S2_P04k"),
                   QuestType  = "cat.",
                   QuestLab   = list(en="Is the co-holder also the Manager?",
                                     fr="Le co-exploitant est-il aussi le chef d'exploitation?",
                                     es="¿Es	el	Cotitular	también	Gerente?"),
                   QuestValue = list(en=c("Yes","No"),fr=c("Oui","Non"),es=c("Si","No")),
                   QuestValProbs = c(0.9,0.05)
    )
  


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
