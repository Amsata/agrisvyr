setClassUnion("numericOrNULL", c("numeric", "NULL"))
setClassUnion("characterOrNULL", c("character", "NULL"))
setClassUnion("logicalOrNULL", c("logical", "NULL"))
setClassUnion("listOrNULL", c("list", "NULL"))
setClassUnion("factorOrNULL", c("factor", "NULL"))
setClassUnion("QuestValClass", c("factor","character","numeric","list", "NULL"))



#' An object to model a survey question from Agris survey questionnaire
#'
#' @slot QuestVar name of the variable
#' @slot QuestType type of the variable (categorical, continous or text)
#' @slot QuestLab text of the question
#' @slot QuestValue if categorical, take the different values of the question
#' @slot QuestValProbs if categorical, take the probability of each values
#'
#' @return
#' @export
#'
#' @examples
methods::setClass(
  Class          = "agrisvyQuestion",
  representation = representation(
    QuestVar      = "listOrNULL",
    QuestType     = "characterOrNULL",
    QuestLab      = "listOrNULL",
    QuestValue    = "QuestValClass",
    QuestValProbs = "numericOrNULL"
  ),
  prototype = prototype(
    QuestVar      = NULL,
    QuestType     = NULL,
    QuestLab      = NULL,
    QuestValue    = NULL,
    QuestValProbs = NULL
  ),
  validity = function(object) {

  }
)


#' Title
#'
#' @param QuestVar name of the variable
#' @param QuestType type of the variable (categorical, continous or text)
#' @param QuestLab text of the question
#' @param QuestValue if categorical, take the different values of the question
#' @param QuestValProbs if categorical, take the probability of each values
#'
#' @return
#' @export
#'
#' @examples
createQuestion <- function( QuestVar      = NULL,
                           QuestType      = NULL,
                           QuestLab       = NULL,
                           QuestValue     = NULL,
                           QuestValProbs  = NULL) {

  obj <- new("agrisvyQuestion")

  obj@QuestVar      = QuestVar
  obj@QuestType     = QuestType
  obj@QuestLab      = QuestLab
  obj@QuestValue    = QuestValue
  obj@QuestValProbs = QuestValProbs

  return(obj)
}



#' Create a questionnaire object (liste of question objects)
#'
#' @return
#' @export
#'
#' @examples
createQuestionnaire <- function(){

  Questionnaire <- list()
  
  #Q01
  Questionnaire$Q01 <-
    createQuestion(QuestVar   = list(en="Q01",fr="Q01",es="P01"),
                   QuestType  = "cat.",
                   QuestLab   = list(en="Did I find a farm at the same address or same name?",
                                     fr="Ai-je trouvé une exploitation de même nom et/ou même adresse?",
                                     es="¿Encontré	una	finca	en	la	misma	dirección	o	con	el	mismo	nombre?"),
                   QuestValue = list(en=c("Yes","No"),fr=c("Oui","Non"),es=c("Si","No")),
                   QuestValProbs = c(0.9,0.05)
                   )

  #Q01f
  Questionnaire$Q01f <-
    createQuestion(QuestVar =list(en="Q01f",fr="Q01f",es="P01f"),
                   QuestType     ="cat.",
                   QuestLab      =list(en="ationality",fr="Nationalité",es="Nacionalidad"),
                   QuestValue    = list(en=c("Local country","Neighbouring country","Other"),
                                        fr=c("Pays de résidence","Pays voisin","Autre"),
                                        es=c("País	local","2 País	vecino","Otro")),
                   QuestValProbs = c(0.8,0.2,0.1)
                   )


  Questionnaire$Q10 <-
    createQuestion(QuestVar = list(en="Q10",fr="Q10",es="P10"),
                   QuestType = "cat.",
                   QuestLab = list(en="What is the legal status of the Holder?",
                                   fr="Quel est le statut légal de l'exploitant?",
                                   es="¿Cuál	es	la	condición	jurídica	del	Titular?"),
                   QuestValue = list(en=c("Civil person/natural person",
                                          "Group of civil persons/natural persons",
                                                             "Legal person"),
                                     fr=c("Personne physique","Groupe de personnes physiques",
                                                             "Personne morale"),
                                     es=c("Persona	civil/persona	natural",
                                          "Grupo	de	personas	civiles/personas	naturales",
                                                             "Persona	jurídica")),
                   QuestValProbs = c(0.9,0.2,0.01)
                   )

  #Q13a
  Questionnaire$Q13a <-
    createQuestion(QuestVar = list(en="Q13a",fr="Q13a",es="P13a"),
                   QuestType = "cat.",
                   QuestLab = list(en="Region",fr="Région",es="Región"),
                   QuestValue = NULL,
                   QuestValProbs = NULL
                   )

  #Q13b
  Questionnaire$Q13b <-
    createQuestion(QuestVar = list(en="Q13b",fr="Q13b",es="P13b"),
                   QuestType = "cat.",
                   QuestLab = list(en="District",fr="District",es="Distrito"),
                   QuestValue = NULL,
                   QuestValProbs = NULL
                   )

  #Q13c
  Questionnaire$Q13c <-
    createQuestion(QuestVar = list(en="Q13c",fr="Q13c",es="P13c"),
                   QuestType = "cat.",
                   QuestLab = list(en="Village or town name",
                                   fr="Nom du village ou ville",
                                   es="Nombre	del	pueblo	o	ciudad"),
                   QuestValue = NULL,
                   QuestValProbs = NULL
                   )

  #Q15
  Questionnaire$Q15 <-
    createQuestion(QuestVar = list(en="Q15",fr="Q15",es="P15"),
                   QuestType = "cont.",
                   QuestLab = list(en="Enumeration area of the holding",
                                   fr="Zone de recensement de l'exploitation",
                                   es="Área	de	enumeración	de	la	unidad	de	prod"),
                   QuestValue = NULL,
                   QuestValProbs = NULL
                   )


  # #Q17a
  # Questionnaire$Q17a <-
  #   createQuestion(QuestVar = list(en="Q17a",fr="Q17a",es="P17a"),
  #                  QuestType = "cat.",
  #                  QuestLab = list(en="a Address (street)",
  #                                  fr="a Adresse (rue)",
  #                                  es="Dirección	(calle)"),
  #                  QuestValue = NULL,
  #                  QuestValProbs = NULL
  #                  )

  #Q17b
  Questionnaire$Q17b <-
    createQuestion(QuestVar = list(en="Q17b",fr="Q17b",es="P17b"),
                   QuestType = "cat.",
                   QuestLab = list(en="Village or town name",
                                   fr="Nom du village ou ville",
                                   es=" Nombre	del	pueblo	o	ciudad"),
                   QuestValue = NULL,
                   QuestValProbs = NULL
                   )

  #Q17c
  Questionnaire$Q17c <-
    createQuestion(QuestVar = list(en="Q17c",fr="Q17c",es="P17c"),
                   QuestType = "cat.",
                   QuestLab = list(en="Region",fr="Région",es="Región"),
                   QuestValue = NULL,
                   QuestValProbs = NULL
                   )

  #Q17d
  Questionnaire$Q17d <-
    createQuestion(QuestVar = list(en="Q17d",fr="Q17d",es="P17d"),
                   QuestType = "cat.",
                   QuestLab = list(en="District",fr="District",es="Distrito"),
                   QuestValue = NULL,
                   QuestValProbs = NULL
                   )
  
  #Q18
  Questionnaire$Q18 <-
    createQuestion(QuestVar = list(en="Q18",fr="Q18",es="P18"),
                   QuestType = "cat.",
                   QuestLab = list(en="What is the main location type of the address reported above?",
                                   fr="A quoi correspond l'adresse ci-dessus ?",
                                   es="¿Cuál	es	el	principal	tipo	de	ubicación	de	la	dirección	reportada	en	líneas	anteriores?"),
                   QuestValue = list(en=c("Household dwelling (for HH sector) and farm, including dwelling and agricultural buildings",
                                          "Main agricultural building",
                                          "Main agricultural parcel"),
                                     fr=c("Corps de ferme incluant logement et bâtiments agricoles (pour les exploitations individuelles)",
                                          "Bâtiment agricole principal",
                                          "Parcelle agricole la plus grande"),
                                     es=c("Vivienda	del	hogar	(para	el	sector	del	hogar)	y	finca,	que	incluye	vivienda	y	edificios	agropecuarios",
                                          "Edificio	agropecuario	principa",
                                          "Parcela	agrícola	principal")),
                   QuestValProbs = c(0.9,0.2,0.01)
    )

  #Q19a
  Questionnaire$Q19a <-
    createQuestion(QuestVar = list(en="Q19a",fr="Q19a",es="P19a"),
                   QuestType = "cont.",
                   QuestLab = list(en="Latitude",fr="Latitude",es="Latitud"),
                   QuestValue = NULL,
                   QuestValProbs = NULL
                   )
  #Q19b
  Questionnaire$Q19b <-
    createQuestion(QuestVar = list(en="Q19b",fr="Q19b",es="P19b"),
                   QuestType = "cont.",
                   QuestLab = list(en="Longitude",fr="Longitude",es="Longitud"),
                   QuestValue = NULL,
                   QuestValProbs = NULL
                   )



return(Questionnaire)

}
