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

  ##### SECTION 1: THE HOLDING

  ## PART 1.1: SURVEY PREPARATION

  #Q01
  Questionnaire$S1_Q01 <-
    createQuestion(QuestVar   = list(en="S1_Q01",fr="S1_Q01",es="S1_P01"),
                   QuestType  = "cat.",
                   QuestLab   = list(en="Did I find a farm at the same address or same name?",
                                     fr="Ai-je trouvé une exploitation de même nom et/ou même adresse?",
                                     es="¿Encontré	una	finca	en	la	misma	dirección	o	con	el	mismo	nombre?"),
                   QuestValue = list(en=c("Yes","No"),fr=c("Oui","Non"),es=c("Si","No")),
                   QuestValProbs = c(0.9,0.05)
                   )

  #Q02
  Questionnaire$S1_Q02 <-
    createQuestion(QuestVar   = list(en="S1_Q02",fr="S1_Q02",es="S1_P02"),
                   QuestType  = "cat.",
                   QuestLab   = list(en="Did I find somebody from the holding who accepted to answer?",
                                     fr="Ai-je trouvé quelqu'un dans l'exploitation qui accepte de répondre ?",
                                     es="¿Encontré	a	alguien	de	la	unidad	de	producción	que	aceptó	responder?"),
                   QuestValue = list(en=c("Yes","No"),fr=c("Oui","Non"),es=c("Si","No")),
                   QuestValProbs = c(0.9,0.05)
    )

  #Q03
  Questionnaire$S1_Q03a <-
    createQuestion(QuestVar   = list(en="S1_Q03a",fr="S1_Q03a",es="S1_P03a"),
                   QuestType  = "text",
                   QuestLab   = list(en="First name of the respondent",
                                     fr="Prénom du répondant",
                                     es="Nombre del	encuestado"),
                   QuestValue = NULL,
                   QuestValProbs = NULL
    )

  Questionnaire$S1_Q03b <-
    createQuestion(QuestVar   = list(en="S1_Q03b",fr="S1_Q03b",es="S1_P03b"),
                   QuestType  = "text",
                   QuestLab   = list(en="Surname of the respondent",
                                     fr="Nom de famille du répondant",
                                     es="Apellido del	encuestado"),
                   QuestValue = NULL,
                   QuestValProbs = NULL
    )

  Questionnaire$S1_Q03c <-
    createQuestion(QuestVar   = list(en="S1_Q03c",fr="S1_Q03c",es="S1_P03c"),
                   QuestType  = "cat.",
                   QuestLab   = list(en="Sex of the respondant",
                                     fr="Sexe du répondant",
                                     es="Sexo del	encuestado"),
                   QuestValue = list(en=c("Male","Female"),fr=c("Masculin","Féminin"),es=c("Masculino","Femenino")),
                   QuestValProbs = c(0.50,0.45)
    )

  Questionnaire$S1_Q03d <-
    createQuestion(QuestVar   = list(en="S1_Q03d",fr="S1_Q03d",es="S1_P03d"),
                   QuestType  = "cat.",
                   QuestLab   = list(en="What is your function on the agricultural holding?",
                                     fr="Quelle est votre fonction dans l'exploitation agricole ?",
                                     es="Sexo del	encuestado"),
                   QuestValue = list(en=c("Holder (legal and/or economically responsible for the holding)",
                                          "Co-holder (legal and/or economically co-responsible for the holding)",
                                          "Manager (responsible for the day-to-day decisions on the farming operations)",
                                          "Employee or Household member working on the holding",
                                          "Household member not working on the holding",
                                          "Other"),
                                     fr=c("Exploitant (légal et/ou responsable économique de l'exploitation)",
                                          "Co-exploitant (légal et/ou responsable économique de l'exploitation)",
                                          "Dirigeant (responsable des décisions au jour le jour sur les travaux agricoles)",
                                          "Employé(e) ou membre du ménage travaillant sur l'exploitation",
                                          "Membre du ménage ne travaillant pas sur l'exploitation",
                                          "Autre"),
                                     es=c("Titular	(jurídico	y/o	responsable	económicamente	de	la	unidad	de	producción)",
                                          "Cotitular	(jurídica	y/o	económicamente	corresponsable	de	la	unidad	de	producción)",
                                          "Gerente	(responsable	de	las	decisiones	cotidianas	sobre	el	funcionamiento	de	la	agricultura)",
                                          "Empleado	o	miembros	del	hogar	que	trabajan	en	la	unidad	de	producción",
                                          "Miembro	del	hogar	que	no	trabaja	en	la	unidad	de	producción",
                                          "Otro")),
                   QuestValProbs = c(0.3,0.3,0.15,0.15,0.05,0.01)
    )

  #Q04
  Questionnaire$S1_Q04 <-
    createQuestion(QuestVar   = list(en="S1_Q04",fr="S1_Q04",es="S1_P04"),
                   QuestType  = "cat.",
                   QuestLab   = list(en="Is the holding currently growing any crops or fruits, or raising animals, or did it do so during the reference period?",
                                     fr="L'exploitation a-t-elle et/ou a-t-elle eu pendant la période de référence une activité de production végétale et/ou animale (végétaux, fruits, animaux élevés) ?",
                                     es="¿Está	la	unidad	de	producción		produciendo	algún	cultivo	o	frutas,	criando	animales	actualmente,	o	lo	hizo	durante	el	periodo	de	referencia?"),
                   QuestValue = list(en=c("Yes","No"),fr=c("Oui","Non"),es=c("Si","No")),
                   QuestValProbs = c(0.9,0.05)
    )

  #Q05
  Questionnaire$S1_Q05 <-
    createQuestion(QuestVar   = list(en="S1_Q05",fr="S1_Q05",es="S1_P05"),
                   QuestType  = "cat.",
                   QuestLab   = list(en="Has the holding ceased its activity without any transfer of its means of production?",
                                     fr="L'exploitation a-t-elle cessé son activité sans aucun transfert de ses moyens de production ?",
                                     es="¿Está	la	unidad	de	producción	produciendo	algún	cultivo	o	frutas,	criando	animales	actualmente,	o	lo	hizo	durante	el	periodo	de	referencia?"),
                   QuestValue = list(en=c("Yes","No"),fr=c("Oui","Non"),es=c("Si","No")),
                   QuestValProbs = c(0.05,0.9)
    )

  #Q06
  Questionnaire$S1_Q05 <-
    createQuestion(QuestVar   = list(en="S1_Q05",fr="S1_Q05",es="S1_P05"),
                   QuestType  = "cat.",
                   QuestLab   = list(en="Will the holding resume its activity?",
                                     fr="L'exploitation va-t-elle reprendre son activité ?",
                                     es="¿Ha	suspendido,	la	unidad	de	producción,	su	actividad	sin	ninguna	transferencia	de	sus	medios	de	producción?"),
                   QuestValue = list(en=c("Yes","No"),fr=c("Oui","Non"),es=c("Si","No")),
                   QuestValProbs = c(0.9,0.05)
    )


  ## PART 1.2: IDENTIFICATION OF THE HOLDING

  #Q10
  Questionnaire$S1_Q10 <-
    createQuestion(QuestVar = list(en="S1_Q10",fr="S1_Q10",es="S1_P10"),
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
                   QuestValProbs = c(0.7,0.2,0.01)
                   )

  #Q12
  Questionnaire$S1_Q12a <-
    createQuestion(QuestVar   = list(en="S1_Q12a",fr="S1_Q12a",es="S1_P12a"),
                   QuestType  = "text",
                   QuestLab   = list(en="First name of the Holder/Co-holders",
                                     fr="Prénom de l'exploitant ou des co-exploitants",
                                     es="Nombre del	Titular/Cotitulares"),
                   QuestValue = NULL,
                   QuestValProbs = NULL
    )

  Questionnaire$S1_Q12b <-
    createQuestion(QuestVar   = list(en="S1_Q12b",fr="S1_Q12b",es="S1_P12b"),
                   QuestType  = "text",
                   QuestLab   = list(en="Surname of the Holder/Co-holders",
                                     fr="Nom de famille de l'exploitant ou des co-exploitants",
                                     es="Apellido del	Titular/Cotitulares"),
                   QuestValue = NULL,
                   QuestValProbs = NULL
    )

  Questionnaire$S1_Q12c <-
    createQuestion(QuestVar   = list(en="S1_Q12c",fr="S1_Q12c",es="S1_P12c"),
                   QuestType  = "cat.",
                   QuestLab   = list(en="Sex of the Holder/Co-holders",
                                     fr="Sexe de l'exploitant ou des co-exploitants",
                                     es="Sexo del	Titular/Cotitulares"),
                   QuestValue = list(en=c("Male","Female"),fr=c("Masculin","Féminin"),es=c("Masculino","Femenino")),
                   QuestValProbs = c(0.50,0.45)
    )
  
  Questionnaire$S1_Q12d <-
    createQuestion(QuestVar   = list(en="S1_Q12d",fr="S1_Q12d",es="S1_P12d"),
                   QuestType  = "cont.",
                   QuestLab   = list(en="PERSONAL ID of the Holder",
                                     fr="NUMERO PERSONNEL de l'exploitant",
                                     es="ID	PERSONAL	del	Titular"),
                   QuestValue = NULL,
                   QuestValProbs = NULL
    )


  #Q13a
  Questionnaire$S1_Q13a <-
    createQuestion(QuestVar = list(en="S1_Q13a",fr="S1_Q13a",es="S1_P13a"),
                   QuestType = "cat.",
                   QuestLab = list(en="Region",fr="Région",es="Región"),
                   QuestValue = NULL,
                   QuestValProbs = NULL
                   )

  #Q13b
  Questionnaire$S1_Q13b <-
    createQuestion(QuestVar = list(en="S1_Q13b",fr="S1_Q13b",es="S1_P13b"),
                   QuestType = "cat.",
                   QuestLab = list(en="District",fr="District",es="Distrito"),
                   QuestValue = NULL,
                   QuestValProbs = NULL
                   )

  #Q13c
  Questionnaire$S1_Q13c <-
    createQuestion(QuestVar = list(en="S1_Q13c",fr="S1_Q13c",es="S1_P13c"),
                   QuestType = "cat.",
                   QuestLab = list(en="Village or town name",
                                   fr="Nom du village ou ville",
                                   es="Nombre	del	pueblo	o	ciudad"),
                   QuestValue = NULL,
                   QuestValProbs = NULL
                   )

  #Q14
  Questionnaire$S1_Q14 <-
    createQuestion(QuestVar = list(en="S1_Q14",fr="S1_Q14",es="S1_P14"),
                   QuestType = "text",
                   QuestLab = list(en="What is the legal name of the holding?",
                                   fr="Quelle est la raison sociale de l'exploitation ?",
                                   es="¿Cuál	es	el	nombre	jurídico	de	la	unidad	de	producción"),
                   QuestValue = NULL,
                   QuestValProbs = NULL
    )
  
  
  #Q15
  Questionnaire$S1_Q15 <-
    createQuestion(QuestVar = list(en="S1_Q15",fr="S1_Q15",es="S1_P15"),
                   QuestType = "cont.",
                   QuestLab = list(en="Enumeration area of the holding",
                                   fr="Zone de recensement de l'exploitation",
                                   es="Área	de	enumeración	de	la	unidad	de	prod"),
                   QuestValue = NULL,
                   QuestValProbs = NULL
                   )

  #Q16
  Questionnaire$S1_Q16 <-
    createQuestion(QuestVar   = list(en="S1_Q16",fr="S1_Q16",es="S1_P16"),
                   QuestType  = "cont.",
                   QuestLab   = list(en="Holding Serial Number",
                                     fr="Numéro de série de l'exploitation",
                                     es="Número	de	Serie	de	la	Explotación"),
                   QuestValue = NULL,
                   QuestValProbs = NULL
    )
  
  #Q17
  Questionnaire$S1_Q17 <-
    createQuestion(QuestVar   = list(en="S1_Q17",fr="S1_Q17",es="S1_P17"),
                   QuestType  = "cat.",
                   QuestLab   = list(en="Address of the holding",
                                     fr="L'adresse de l'exploitation est-elle",
                                     es="Dirección	de	la	unidad	de	producción"),
                   QuestValue = list(en=c("Same as the address of the Holder","Different from the address of the Holde"),
                                     fr=c("La même que celle de l'exploitant","Différente de celle l'exploitant"),
                                     es=c("La	misma	dirección	a	la	del	Titular ","Dirección	diferente	a	la	del	Titular")),
                   QuestValProbs = c(0.9,0.05)
    )


  #Q17a
  Questionnaire$S1_Q17a <-
    createQuestion(QuestVar = list(en="S1_Q17a",fr="S1_Q17a",es="S1_P17a"),
                   QuestType = "cat.",
                   QuestLab = list(en="Address (street)",
                                   fr="Adresse (rue)",
                                   es="Dirección	(calle)"),
                   QuestValue = NULL,
                   QuestValProbs = NULL
                   )

  #Q17b
  Questionnaire$S1_Q17b <-
    createQuestion(QuestVar = list(en="S1_Q17b",fr="S1_Q17b",es="S1_P17b"),
                   QuestType = "cat.",
                   QuestLab = list(en="Village or town name",
                                   fr="Nom du village ou de la ville",
                                   es=" Nombre	del	pueblo	o	ciudad"),
                   QuestValue = NULL,
                   QuestValProbs = NULL
                   )

  #Q17c
  Questionnaire$S1_Q17c <-
    createQuestion(QuestVar = list(en="S1_Q17c",fr="S1_Q17c",es="S1_P17c"),
                   QuestType = "cat.",
                   QuestLab = list(en="Region",fr="Région",es="Región"),
                   QuestValue = NULL,
                   QuestValProbs = NULL
                   )

  #Q17d
  Questionnaire$S1_Q17d <-
    createQuestion(QuestVar = list(en="S1_Q17d",fr="S1_Q17d",es="S1_P17d"),
                   QuestType = "cat.",
                   QuestLab = list(en="District",fr="District",es="Distrito"),
                   QuestValue = NULL,
                   QuestValProbs = NULL
                   )
  
  #Q17e
  Questionnaire$S1_Q17e <-
    createQuestion(QuestVar = list(en="S1_Q17e",fr="S1_Q17e",es="S1_P17e"),
                   QuestType = "cont.",
                   QuestLab = list(en="Main telephone number of the holding",
                                   fr="Numéro de téléphone principal de l'exploitation",
                                   es="Número	de	teléfono	principal	de	la	unidad	de	producción"),
                   QuestValue = NULL,
                   QuestValProbs = NULL
    )

  #Q18
  Questionnaire$S1_Q18 <-
    createQuestion(QuestVar = list(en="S1_Q18",fr="S1_Q18",es="S1_P18"),
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
                   QuestValProbs = c(0.7,0.2,0.01)
    )

  #Q19a
  Questionnaire$S1_Q19a <-
    createQuestion(QuestVar = list(en="S1_Q19a",fr="S1_Q19a",es="S1_P19a"),
                   QuestType = "cont.",
                   QuestLab = list(en="Latitude",fr="Latitude",es="Latitud"),
                   QuestValue = NULL,
                   QuestValProbs = NULL
                   )
  #Q19b
  Questionnaire$S1_Q19b <-
    createQuestion(QuestVar = list(en="S1_Q19b",fr="S1_Q19b",es="S1_P19b"),
                   QuestType = "cont.",
                   QuestLab = list(en="Longitude",fr="Longitude",es="Longitud"),
                   QuestValue = NULL,
                   QuestValProbs = NULL
                   )

  #Q20
  Questionnaire$S1_Q20 <-
    createQuestion(QuestVar = list(en="S1_Q20",fr="S1_Q20",es="S1_P20"),
                   QuestType = "cont.",
                   QuestLab = list(en="What is the official identification number of the holding in the national business register?",
                                   fr="Quel est l'identifiant de l'exploitation dans le répertoire national d'entreprises ?",
                                   es="¿Cuál	es	el	número	de	identificación	oficial	de	la	unidad	de	producción	en	el	registro	de	empresas	nacionales?"),
                   QuestValue = NULL,
                   QuestValProbs = NULL
    )


  #Q21a
  Questionnaire$S1_Q21a <-
    createQuestion(QuestVar = list(en="S1_Q21a",fr="S1_Q21a",es="S1_P21a"),
                   QuestType = "cat.",
                   QuestLab = list(en="What are the other administrative identification numbers of the holding?-Livestock",
                                   fr="Quels sont les autres identifiants administratifs de l'exploitation?-Cheptel",
                                   es="¿Cuales	son	los	demás	números	de	identificación	administrativos	de	la	unidad	de	producción?-Ganaderí"),
                   QuestValue = NULL,
                   QuestValProbs = NULL
    )

  #Q21b
  Questionnaire$S1_Q21b <-
    createQuestion(QuestVar = list(en="S1_Q21b",fr="S1_Q21b",es="S1_P21b"),
                   QuestType = "cat.",
                   QuestLab = list(en="What are the other administrative identification numbers of the holding?-Wine production",
                                   fr="Quels sont les autres identifiants administratifs de l'exploitation?-Production de vin",
                                   es="¿Cuales	son	los	demás	números	de	identificación	administrativos	de	la	unidad	de	producción?-Producción	de	vino"),
                   QuestValue = NULL,
                   QuestValProbs = NULL
    )

  #Q21c
  Questionnaire$S1_Q21c <-
    createQuestion(QuestVar = list(en="S1_Q21c",fr="S1_Q21c",es="S1_P21c"),
                   QuestType = "cat.",
                   QuestLab = list(en="What are the other administrative identification numbers of the holding?-Organic production",
                                   fr="Quels sont les autres identifiants administratifs de l'exploitation?-Production biologique",
                                   es="¿Cuales	son	los	demás	números	de	identificación	administrativos	de	la	unidad	de	producción?-Producción	orgánica"),
                   QuestValue = NULL,
                   QuestValProbs = NULL
    )



  #Q21d
  Questionnaire$S1_Q21d <-
    createQuestion(QuestVar = list(en="S1_Q21d",fr="S1_Q21d",es="S1_P21d"),
                   QuestType = "cat.",
                   QuestLab = list(en="What are the other administrative identification numbers of the holding?-Other",
                                   fr="Quels sont les autres identifiants administratifs de l'exploitation?-Other",
                                   es="¿Cuales	son	los	demás	números	de	identificación	administrativos	de	la	unidad	de	producción?-Otro"),
                   QuestValue = NULL,
                   QuestValProbs = NULL
    )



  #Q22
  Questionnaire$S1_Q22 <-
    createQuestion(QuestVar = list(en="S1_Q22",fr="S1_Q22",es="S1_P22"),
                   QuestType = "cat.",
                   QuestLab = list(en=". What is the identification number of the holding from the last agricultural census? (Can be prefilled)",
                                   fr="Quel est l'identifiant de l'exploitation dans le dernier recensement de l'agriculture? (Peut être pré renseigné)",
                                   es="¿Cuál	es	el	número	de	identificación	de	la	unidad	de	producción	en	el	último	censo	agropecuario?	(Puede	llenarse	de	antemano)"),
                   QuestValue = NULL,
                   QuestValProbs = NULL
    )


  ## PART 1.2: AGRICULTURAL ACTIVITY

  #Q23
  Questionnaire$S1_Q23 <-
    createQuestion(QuestVar = list(en="S1_Q23",fr="S1_Q23",es="S1_P23"),
                   QuestType = "cat.",
                   QuestLab = list(en="Does the holding record its agricultural activity or finances on registers or logbooks?",
                                   fr="L'exploitation enregistre-t-elle son activité agricole ou comptable sur des registres ou carnets journaliers?",
                                   es="¿Registra	la	unidad	de	producción	su	actividad	agropecuaria	o	finanzas	en	registros	o	libros	de	diario?"),
                   QuestValue = list(en=c("No, never","Yes, only occasionally or partially","Yes, systematically"),
                                     fr=c("1 Non, jamais","Oui, occasionnellement ou partiellement","Oui, systématiquement"),
                                     es=c(" No,	nunca"," Si,	solo	ocasional	y	parcialmente","Sí,	sistemáticamente")),
                   QuestValProbs = c(0.65,0.25,0.2)
    )


  #Q24_1
  Questionnaire$S1_Q24_1 <-
    createQuestion(QuestVar = list(en="S1_Q24_1",fr="S1_Q24_1",es="S1_P24_1"),
                   QuestType = "cat.",
                   QuestLab = list(en="What information is systematically registered?-Area cultivated/harvested",
                                   fr=". Quelles informations sont systématiquement enregistrées?- Superficie cultivée/récoltée",
                                   es=". ¿Qué	información	se	registra	sistemáticamente?- Área	cultivada/cosechada"),
                   QuestValue = list(en=c("Yes","No"),fr=c("Oui","Non"),es=c("Si","No")),
                   QuestValProbs = c(0.6,02)
    )



  #Q24_2
  Questionnaire$S1_Q24_2 <-
    createQuestion(QuestVar = list(en="S1_Q24_2",fr="S1_Q24_2",es="S1_P24_2"),
                   QuestType = "cat.",
                   QuestLab = list(en="What information is systematically registered?-Crop production",
                                   fr=". Quelles informations sont systématiquement enregistrées?-Productions végétales",
                                   es=". ¿Qué	información	se	registra	sistemáticamente?- Producción	de	cultivos"),
                   QuestValue = list(en=c("Yes","No"),fr=c("Oui","Non"),es=c("Si","No")),
                   QuestValProbs = c(0.6,02)
    )



  #Q24_3
  Questionnaire$S1_Q24_3 <-
    createQuestion(QuestVar = list(en="S1_Q24_3",fr="S1_Q24_3",es="S1_P24_3"),
                   QuestType = "cat.",
                   QuestLab = list(en="What information is systematically registered?-Livestock production",
                                   fr=". Quelles informations sont systématiquement enregistrées?-Productions animales",
                                   es=". ¿Qué	información	se	registra	sistemáticamente?-Producción	de	ganado"),
                   QuestValue = list(en=c("Yes","No"),fr=c("Oui","Non"),es=c("Si","No")),
                   QuestValProbs = c(0.6,02)
    )



  #Q24_4
  Questionnaire$S1_Q24_4 <-
    createQuestion(QuestVar = list(en="S1_Q24_4",fr="S1_Q24_4",es="S1_P24_4"),
                   QuestType = "cat.",
                   QuestLab = list(en="What information is systematically registered?-Unit prices, amounts sold and total sales by product",
                                   fr=". Quelles informations sont systématiquement enregistrées?-Prix de vente, montants des ventes et total vendu par produit",
                                   es=". ¿Qué	información	se	registra	sistemáticamente?-Precios	unitarios,	montos	vendidos	y	ventas	totales	por	producto"),
                   QuestValue = list(en=c("Yes","No"),fr=c("Oui","Non"),es=c("Si","No")),
                   QuestValProbs = c(0.6,02)
    )



  #Q24_5
  Questionnaire$S1_Q24_5 <-
    createQuestion(QuestVar = list(en="S1_Q24_5",fr="S1_Q24_5",es="S1_P24_5"),
                   QuestType = "cat.",
                   QuestLab = list(en="What information is systematically registered?-Input quantities used (seeds, fertilizers, plant protection products, etc.)",
                                   fr=". Quelles informations sont systématiquement enregistrées?-Quantités d'intrants utilisés (semences, fertilisants, produits de protection des plantes, etc.)",
                                   es=". ¿Qué	información	se	registra	sistemáticamente?-5 Cantidades	de	insumos	utilizados	(semillas,	fertilizantes,	productos	fitosanitarios,	etc.)"),
                   QuestValue = list(en=c("Yes","No"),fr=c("Oui","Non"),es=c("Si","No")),
                   QuestValProbs = c(0.6,02)
    )



  #Q24_6
  Questionnaire$S1_Q24_6 <-
    createQuestion(QuestVar = list(en="S1_Q24_6",fr="S1_Q24_6",es="S1_P24_6"),
                   QuestType = "cat.",
                   QuestLab = list(en="What information is systematically registered?-Detailed quantities and prices of inputs bought",
                                   fr=". Quelles informations sont systématiquement enregistrées?-Quantités détaillées et prix des intrants achetés",
                                   es=". ¿Qué	información	se	registra	sistemáticamente?-Cantidades	detalladas	y	precios	de	insumos	adquiridos"),
                   QuestValue = list(en=c("Yes","No"),fr=c("Oui","Non"),es=c("Si","No")),
                   QuestValProbs = c(0.6,02)
    )



  #Q24_7
  Questionnaire$S1_Q24_7 <-
    createQuestion(QuestVar = list(en="S1_Q24_7",fr="S1_Q24_7",es="S1_P24_7"),
                   QuestType = "cat.",
                   QuestLab = list(en="What information is systematically registered?-7 Workers’ time",
                                   fr=". Quelles informations sont systématiquement enregistrées?-Temps de travail des salariés",
                                   es=". ¿Qué	información	se	registra	sistemáticamente?- Tiempo	de	los	trabajadores"),
                   QuestValue = list(en=c("Yes","No"),fr=c("Oui","Non"),es=c("Si","No")),
                   QuestValProbs = c(0.6,02)
    )



  #Q24_8
  Questionnaire$S1_Q24_8 <-
    createQuestion(QuestVar = list(en="S1_Q24_8",fr="S1_Q24_8",es="S1_P24_8"),
                   QuestType = "cat.",
                   QuestLab = list(en="What information is systematically registered?-Workers’ payment",
                                   fr=". Quelles informations sont systématiquement enregistrées?- Paiement des salariés",
                                   es=". ¿Qué	información	se	registra	sistemáticamente?- Pago	de	los	trabajadores"),
                   QuestValue = list(en=c("Yes","No"),fr=c("Oui","Non"),es=c("Si","No")),
                   QuestValProbs = c(0.6,02)
    )



  #Q24_9
  Questionnaire$S1_Q24_9 <-
    createQuestion(QuestVar = list(en="S1_Q24_9",fr="S1_Q24_9",es="S1_P24_9"),
                   QuestType = "cat.",
                   QuestLab = list(en="What information is systematically registered?-Other",
                                   fr=". Quelles informations sont systématiquement enregistrées?-Autre",
                                   es=". ¿Qué	información	se	registra	sistemáticamente?-Otro"),
                   QuestValue = list(en=c("Yes","No"),fr=c("Oui","Non"),es=c("Si","No")),
                   QuestValProbs = c(0.6,02)
    )



  #Q25_1
  Questionnaire$S1_Q25_1 <-
    createQuestion(QuestVar = list(en="S1_Q25_1",fr="S1_Q25_1",es="S1_P25_1"),
                   QuestType = "cat.",
                   QuestLab = list(en="Land tenure-Owned with written documentation (includes a title deed, a will, a purchase agreement, etc.)",
                                   fr="régime foncier-En propriété avec un acte écrit (incluant un titre de propriété, une intention, un accord d'achat, etc.)",
                                   es="tenencia de la tierra-1 En	propiedad	con	documentación	por	escrito	(incluye	un	título	de	propiedad,	un	testamento,	un	contrato	de	compra,	etc.)"),
                   QuestValue = list(en=c("Yes","No"),fr=c("Oui","Non"),es=c("Si","No")),
                   QuestValProbs = c(0.6,02)
    )


  #Q25_2
  Questionnaire$S1_Q25_2 <-
    createQuestion(QuestVar = list(en="S1_Q25_2",fr="S1_Q25_2",es="S1_P25_2"),
                   QuestType = "cat.",
                   QuestLab = list(en="Land tenure-Owned without written documentation",
                                   fr="régime foncier-En propriété sans acte écrit",
                                   es="tenencia de la tierra-En	propiedad	sin	documentación	por	escrito"),
                   QuestValue = list(en=c("Yes","No"),fr=c("Oui","Non"),es=c("Si","No")),
                   QuestValProbs = c(0.6,02)
    )


  #Q25_3
  Questionnaire$S1_Q25_3 <-
    createQuestion(QuestVar = list(en="S1_Q25_3",fr="S1_Q25_3",es="S1_P25_3"),
                   QuestType = "cat.",
                   QuestLab = list(en="Land tenure-Rented-in, leased or sharecropped with written agreement",
                                   fr="régime foncier-Location ou métayage avec un acte écrit",
                                   es="tenencia de la tierra-En	alquiler,	arrendamiento	financiero	o	aparcería	con	acuerdo	por	escrito"),
                   QuestValue = list(en=c("Yes","No"),fr=c("Oui","Non"),es=c("Si","No")),
                   QuestValProbs = c(0.6,02)
    )


  #Q25_4
  Questionnaire$S1_Q25_4 <-
    createQuestion(QuestVar = list(en="S1_Q25_4",fr="S1_Q25_4",es="S1_P25_4"),
                   QuestType = "cat.",
                   QuestLab = list(en="Land tenure-Rented-in, leased or sharecropped without written agreement",
                                   fr="régime foncier-Location ou métayage sans acte écrit",
                                   es="tenencia de la tierra- En	alquiler,	arrendamiento	financiero	o	aparcería	sin	acuerdo	por	escrito"),
                   QuestValue = list(en=c("Yes","No"),fr=c("Oui","Non"),es=c("Si","No")),
                   QuestValProbs = c(0.6,02)
    )


  #Q25_5
  Questionnaire$S1_Q25_5 <-
    createQuestion(QuestVar = list(en="S1_Q25_5",fr="S1_Q25_5",es="S1_P25_5"),
                   QuestType = "cat.",
                   QuestLab = list(en="Land tenure-State or communal land used with written agreement (certified use rights)",
                                   fr="régime foncier-Superficie communale ou d'État utilisée avec un accord écrit (droits d'usage certifiés)",
                                   es="tenencia de la tierra- Tierra	estatal	o	comunal	utilizada	con	acuerdo	por	escrito	(derechos	de	uso	certificados)"),
                   QuestValue = list(en=c("Yes","No"),fr=c("Oui","Non"),es=c("Si","No")),
                   QuestValProbs = c(0.6,02)
    )


  #Q25_6
  Questionnaire$S1_Q25_6 <-
    createQuestion(QuestVar = list(en="S1_Q25_6",fr="S1_Q25_6",es="S1_P25_6"),
                   QuestType = "cat.",
                   QuestLab = list(en="Land tenure-State or communal land used without written agreement (uncertified use rights)",
                                   fr="régime foncier-Superficie communale ou d'État utilisée sans accord écrit (droits d'usage non certifiés)",
                                   es="tenencia de la tierra-Tierra	estatal	o	comunal	utilizada	sin	acuerdo	por	escrito	(derechos	de	uso	certificados)"),
                   QuestValue = list(en=c("Yes","No"),fr=c("Oui","Non"),es=c("Si","No")),
                   QuestValProbs = c(0.6,02)
    )


  #Q25_7
  Questionnaire$S1_Q25_7 <-
    createQuestion(QuestVar = list(en="S1_Q25_7",fr="S1_Q25_7",es="S1_P25_7"),
                   QuestType = "cat.",
                   QuestLab = list(en="Land tenure-Occupied/squatted without any permission",
                                   fr="régime foncier-Occupé/squatté sans permission",
                                   es="tenencia de la tierra- Ocupada/ocupada	ilegalmente	sin	ningún	permiso"),
                   QuestValue = list(en=c("Yes","No"),fr=c("Oui","Non"),es=c("Si","No")),
                   QuestValProbs = c(0.6,02)
    )


  #Q25_8
  Questionnaire$S1_Q25_8 <-
    createQuestion(QuestVar = list(en="S1_Q25_8",fr="S1_Q25_8",es="S1_P25_8"),
                   QuestType = "cat.",
                   QuestLab = list(en="Land tenure-No agricultural land",
                                   fr="régime foncier- Pas de superficie agricole",
                                   es="tenencia de la tierra-Tierra	no	agrícola"),
                   QuestValue = list(en=c("Yes","No"),fr=c("Oui","Non"),es=c("Si","No")),
                   QuestValProbs = c(0.6,02)
    )


  #Q26
  Questionnaire$S1_Q26 <-
    createQuestion(QuestVar = list(en="S1_Q26",fr="S1_Q26",es="S1_P26"),
                   QuestType = "cat.",
                   QuestLab = list(en="From an economic perspective, what is the holding’s main agricultural focus for the reference period?",
                                   fr="D'un point de vue économique, quelle a été l'activité agricole principale de l'exploitation pendant la période de référence?",
                                   es="Desde	una	perspectiva	económica,	¿cuál	es	el	principal	enfoque	agrícola	de	la	unidad	de	producción	para	el	periodo	de	referencia?"),
                   QuestValue = list(en=c("Mainly crop production","Mainly livestock production","A mix of crop and livestock production"),
                                     fr=c("Principalement production végétale","Principalement production animale","Productions animale et végétale"),
                                     es=c(" Principalmente	producción	de	cultivos","Principalmente	producción	de	ganado","Una	combinación	de	producción	de	cultivos	y	producción")),
                   QuestValProbs = c(0.9,0.2,0.3)
                   )

  #Q27
  Questionnaire$S1_Q27 <-
    createQuestion(QuestVar = list(en="S1_Q27",fr="S1_Q27",es="S1_P27"),
                   QuestType = "cat.",
                   QuestLab = list(en="From an economic perspective, what is the main cropping activity?",
                                   fr="D'un point de vue économique, quelle a été l'activité végétale principale?",
                                   es="Desde	una	perspectiva	económica,	¿cuál	es	la	principal	actividad	en	el	cultivo?"),
                   QuestValue = list(en=c("Production of annual field crops (cereals, oilseeds, protein crops, root crops, tobacco, cotton, etc.)",
                                          "Production of vegetables, mushrooms, flowers, ornamental plants, etc.",
                                          "Production of grapes for wine",
                                          "Production of fruits",
                                          "Production of other perennial crops (cacao, coffee, etc.)",
                                          "Mixed cropping (no real prevalence of a specific crop activity)"),
                                     fr=c("Production de grandes cultures annuelles (céréales, oléagineux, protéagineux, tubercules, tabac, coton, etc.)",
                                          "Production de légumes, champignons, fleurs, plantes ornementales, etc",
                                          "Production de raisin de cuve",
                                          "Production de fruits",
                                          "Production d'autres cultures pérennes (cacao, café, etc.)",
                                          "Cultures mélangées sans réelle dominante ou activité spécifique"),

                                     es=c("Producción	de	cultivos	anuales	(cereales,	oleaginosas,	cultivos	de	proteína,	cultivos	de	tubérculos,	tabaco,	algodón,	etc.)",
                                          "Producción	de	hortalizas,	zetas,	flores,	plantas	ornamentales,	etc.",
                                          "Producción	de	uva	para	vino",
                                          "Producción	de	fruta",
                                          "Producción	de	otros	cultivos	perennes	(cacao,	café,	etc.)",
                                          "Cultivos	mixtos	(sin	predominio	real	de	una	actividad	de	cultivo	específica)")),
                   QuestValProbs = c(0.2,0.2,0.2,0.2,0.1,0.05)
    )

  #Q28
  Questionnaire$S1_Q28 <-
    createQuestion(QuestVar = list(en="S1_Q28",fr="S1_Q28",es="S1_P28"),
                   QuestType = "cat.",
                   QuestLab = list(en="From an economic perspective, what is the main livestock activity?",
                                   fr="D'un point de vue économique, quelle a été l'activité animale principale?",
                                   es="Desde	una	perspectiva	económica,	¿cuál	es	la	principal	actividad	en	ganadería?"),
                   QuestValue = list(en=c("Raising ruminant livestock for meat (cattle, sheep, goats, etc.)",
                                          "Raising non-ruminant livestock for meat (pigs, poultry, etc.)",
                                          "Production of eggs",
                                          "Production of milk",
                                          "Mixed livestock (no real prevalence of a specific livestock activity)"),
                                     fr=c("Elevage de ruminants pour la viande (bovins, ovins, caprins, etc.)",
                                          "Elevage de cheptels autres que ruminants pour la viande (porcs, volailles, etc.)",
                                          "Production d'oeufs",
                                          "Production de lait",
                                          "Plusieurs activités d'élevage sans dominante (pas de prévalence d'un élevage particulier)"),
                                     es=c("Cría	de	ganado	rumiante	para	carne	(ganado	vacuno,	ovejas,	cabras,	etc.)",
                                          "Cría	de	ganado	no	rumiante	para	carne	(cerdos,	aves	de	corral,	etc.)",
                                          "Producción	de	huevos",
                                          "Producción	de	leche",
                                          "Cultivos	mixtos	(sin	predominio	real	de	una	actividad	de	ganadería	específica)")),
                   QuestValProbs = c(0.2,0.2,0.2,0.2,0.05)
    )

  #Q29
  Questionnaire$S1_Q29 <-
    createQuestion(QuestVar = list(en="S1_Q29",fr="S1_Q29",es="S1_P29"),
                   QuestType = "cat.",
                   QuestLab = list(en="What is the main intended destination of your agricultural production?",
                                   fr="Quelle est la principale destination de vos productions agricoles?",
                                   es="¿Cuál	es	el	destino	principal	que	pretende	con	su	producción	agrícola?"),
                   QuestValue = list(en=c("Producing primarily for sale (selling 90% or more)",
                                          "Producing mainly for sale, with some own consumption (selling more than 50% and up to 90%)",
                                          "Producing mainly for own consumption, with some sales (selling more than 10% and up to 50%)",
                                          "Producing primarily for own consumption (selling 10% or less)"),
                                     fr=c("Production majoritairement pour la vente (vente de 90% ou plus)",
                                          "Production principalement pour la vente, avec un peu d'autoconsommation (vente de 50% à 90%)",
                                          "Production principalement pour l'autoconsommation avec un peu de vente (vente de 10% à 50%)",
                                          "Production principalement pour l'autoconsommation (vente de 10% ou moins)"),
                                     es=c("Producir	principalmente	para	venta	(vender	90%	o	más)",
                                          "Producir	principalmente	para	venta,	y	algo	para	consumo	propio	(venta	de	más	del	50%	y	hasta	el	90%)",
                                          "Producir	principalmente	para	consumo	propio	y	algo	de	ventas	(venta	de	más	del	10%	y	hasta	el	50%)",
                                          "Producir	principalmente	para	consumo	propio	(vender	10%	o	menos)")),
                   QuestValProbs = c(0.2,0.2,0.2,0.2)
    )


  ##### SECTION 2: CHARACTERISTICS OF THE HOLDERS AND MANAGERS

  ## CASE 1: THE HOLDER IS A CIVIL/NATURAL PERSON

  #Q01d
  Questionnaire$S2_Q01d <-
    createQuestion(QuestVar   = list(en="S2_Q01d",fr="S2_Q01d",es="S2_P01d"),
                   QuestType  = "cat.",
                   QuestLab   = list(en="Sex of the holder",
                                     fr="Sexe de l'exploitant",
                                     es="Sexo del	Titular"),
                   QuestValue = list(en=c("Male","Female"),fr=c("Masculin","Féminin"),es=c("Masculino","Femenino")),
                   QuestValProbs = c(0.50,0.45)
    )

  #Q01e
  Questionnaire$S2_Q01e <-
    createQuestion(QuestVar = list(en="S2_Q01e",fr="S2_Q01e",es="S2_P01e"),
                   QuestType = "cont.",
                   QuestLab = list(en="Age in completed years of the holder",
                                   fr="Age en années révolues de l'exploitation",
                                   es="Edad del	Titular	en	años	completos"),
                   QuestValue = NULL,
                   QuestValProbs = NULL
    )

  #Q01f
  Questionnaire$S2_Q01f <-
    createQuestion(QuestVar =list(en="S2_Q01f",fr="S2_Q01f",es="S2_P01f"),
                   QuestType     ="cat.",
                   QuestLab      =list(en="Nationality",fr="Nationalité",es="Nacionalidad"),
                   QuestValue    = list(en=c("Local country","Neighbouring country","Other"),
                                        fr=c("Pays de résidence","Pays voisin","Autre"),
                                        es=c("País	local","País	vecino","Otro")),
                   QuestValProbs = c(0.8,0.2,0.1)
    )

  #Q01h
  Questionnaire$S2_Q01h <-
    createQuestion(QuestVar =list(en="S2_Q01h",fr="S2_Q01h",es="S2_P01h"),
                   QuestType     ="cat.",
                   QuestLab      =list(en="Highest level of education completed",
                                       fr="Plus haut niveau d'étude atteint",
                                       es="Nivel	de	educación	más	alto	completado"),
                   QuestValue    = list(en=c("None","Less than primary","Primary","Lower secondary","Upper secondary","Tertiary/post-secondary"),
                                        fr=c("Aucun","Moins que primaire","Primaire","Secondaire court","Secondaire long","Postsecondaire"),
                                        es=c("Ninguna","Inferior	a	primaria","Primaria","Secundaria	inferior","Secundaria	superior"," Terciaria/postsecundaria")),
                   QuestValProbs = c(0.2,0.2,0.2,0.2,0.1,0.1)
    )

  #Q01i
  Questionnaire$S2_Q01i <-
    createQuestion(QuestVar =list(en="S2_Q01i",fr="S2_Q01i",es="S2_P01i"),
                   QuestType     ="cat.",
                   QuestLab      =list(en="Share of working time spent working on the holding",
                                       fr="Temps de travail sur l'exploitation",
                                       es="Porción	de	tiempo	de	trabajo	que	dedicó	a	la	unidad	de	producción"),
                   QuestValue    = list(en=c("Less than half ( < 40 %)","About half (40%-59%)","Most/almost all (60%-99%)","All (100%)"),
                                        fr=c("Moins d'un mi-temps ( < 40 %)","Mi-temps (40%-59%)","Plus qu'un mi-temps (60%-99%","Temps complet (100%)"),
                                        es=c("Menos	de	la	mitad	(<40%)","Alrededor	de	la	mitad	(40%-59%)","La	mayoría/casi	todo	(60%-99%)","Todo	(100%)")),
                   QuestValProbs = c(0.2,0.2,0.2,0.2)
    )

  #Q01j
  Questionnaire$S2_Q01j <-
    createQuestion(QuestVar   = list(en="S2_Q01j",fr="S2_Q01j",es="S2_P01j"),
                   QuestType  = "cat.",
                   QuestLab   = list(en="Does the Holder have another gainful activity outside of the holding?",
                                     fr="L'exploitant a-t-il une autre activité rémunérée en dehors de l'exploitation?",
                                     es="¿Realiza	el	Titular	otra	actividad	lucrativa	fuera	de	la	unidad	de	producción?"),
                   QuestValue = list(en=c("Yes","No"),fr=c("Oui","Non"),es=c("Si","No")),
                   QuestValProbs = c(0.9,0.05)
    )

  #Q01k
  Questionnaire$S2_Q01k <-
    createQuestion(QuestVar   = list(en="S2_Q01k",fr="S2_Q01k",es="S2_P01k"),
                   QuestType  = "cat.",
                   QuestLab   = list(en="Is the Holder also the Manager?",
                                     fr="L'exploitant est-il aussi le chef d'exploitation?",
                                     es="¿Es	el	Titular	también	Gerente?"),
                   QuestValue = list(en=c("Yes","No"),fr=c("Oui","Non"),es=c("Si","No")),
                   QuestValProbs = c(0.9,0.05)
    )

  #Q02d
  Questionnaire$S2_Q02d <-
    createQuestion(QuestVar   = list(en="S2_Q02d",fr="S2_Q02d",es="S2_P02d"),
                   QuestType  = "cat.",
                   QuestLab   = list(en="Sex of the manager",
                                     fr="Sexe du chef d'exploitation",
                                     es="Sexo del	Gerente"),
                   QuestValue = list(en=c("Male","Female"),fr=c("Masculin","Féminin"),es=c("Masculino","Femenino")),
                   QuestValProbs = c(0.50,0.45)
    )

  #Q02e
  Questionnaire$S2_Q02e <-
    createQuestion(QuestVar = list(en="S2_Q02e",fr="S2_Q02e",es="S2_P02e"),
                   QuestType = "cont.",
                   QuestLab = list(en="Age in completed years of the manager",
                                   fr="Age en années révolues du chef d'exploitation",
                                   es="Edad del	Gerente	en	años	completos"),
                   QuestValue = NULL,
                   QuestValProbs = NULL
    )

  #Q02f
  Questionnaire$S2_Q02f <-
    createQuestion(QuestVar =list(en="S2_Q02f",fr="S2_Q02f",es="S2_P02f"),
                   QuestType     ="cat.",
                   QuestLab      =list(en="Link with the Holder",fr="Lien de parenté avec l'exploitant",es="Nacionalidad del	Gerente"),
                   QuestValue    = list(en=c("Wife/husband or consensual union partner","Other member of the household","External"),
                                        fr=c("Epouse/mari ou vivant maritalement","Autre membre du ménage","Aucun lien de parenté"),
                                        es=c("Esposa/esposo	o	socio	por	unión	consensual","Otro	miembro	del	hogar","Externo")),
                   QuestValProbs = c(0.7,0.2,0.1)
    )

  #Q02g
  Questionnaire$S2_Q02g <-
    createQuestion(QuestVar =list(en="S2_Q02g",fr="S2_Q02g",es="S2_P02g"),
                   QuestType     ="cat.",
                   QuestLab      =list(en="Nationality of the manager",fr="Nationalité du chef d'exploitation",es="Nacionalidad del	Gerente"),
                   QuestValue    = list(en=c("Local country","Neighbouring country","Other"),
                                        fr=c("Pays de résidence","Pays voisin","Autre"),
                                        es=c("País	local","País	vecino","Otro")),
                   QuestValProbs = c(0.7,0.2,0.05)
    )

  #Q02i
  Questionnaire$S2_Q02i <-
    createQuestion(QuestVar =list(en="S2_Q02i",fr="S2_Q02i",es="S2_Q02i"),
                   QuestType     ="cat.",
                   QuestLab      =list(en="Highest level of education completed by the manager",
                                       fr="Plus haut niveau d'étude atteint par le chef d'exploitation",
                                       es="Nivel	de	educación	más	alto	completado del jefe de Gerente"),
                   QuestValue    = list(en=c("None","Less than primary","Primary","Lower secondary","Upper secondary","Tertiary/post-secondary"),
                                        fr=c("Aucun","Moins que primaire","Primaire","Secondaire court","Secondaire long","Postsecondaire"),
                                        es=c("Ninguna","Inferior	a	primaria","Primaria","Secundaria	inferior","Secundaria	superior"," Terciaria/postsecundaria")),
                   QuestValProbs = c(0.2,0.2,0.2,0.2,0.1,0.1)
    )

  #Q02j
  Questionnaire$S2_Q02j <-
    createQuestion(QuestVar =list(en="S2_Q02j",fr="S2_Q02j",es="S2_Q02j"),
                   QuestType     ="cat.",
                   QuestLab      =list(en="Share of working time spent working on the holding by the manager",
                                       fr="Temps de travail sur l'exploitation",
                                       es="Porción	de	tiempo	de	trabajo	que	dedicó	a	la	unidad	de	producción"),
                   QuestValue    = list(en=c("Less than half ( < 40 %)","About half (40%-59%)","Most/almost all (60%-99%)","All (100%)"),
                                        fr=c("Moins d'un mi-temps ( < 40 %)","Mi-temps (40%-59%)","Plus qu'un mi-temps (60%-99%","Temps complet (100%)"),
                                        es=c("Menos	de	la	mitad	(<40%)","Alrededor	de	la	mitad	(40%-59%)","La	mayoría/casi	todo	(60%-99%)","Todo	(100%)")),
                   QuestValProbs = c(0.2,0.2,0.2,0.2)
    )

  #Q02k
  Questionnaire$S2_Q02k <-
    createQuestion(QuestVar   = list(en="S2_Q02k",fr="S2_Q02k",es="S2_P02k"),
                   QuestType  = "cat.",
                   QuestLab   = list(en="Does the manager have another gainful activity outside of the holding?",
                                     fr="Le chef d'exploitation a-t-il une autre activité rémunérée en dehors de l'exploitation?",
                                     es="¿Realiza	el	Titular	otra	actividad	lucrativa	fuera	de	la	unidad	de	producción?"),
                   QuestValue = list(en=c("Yes","No"),fr=c("Oui","Non"),es=c("Si","No")),
                   QuestValProbs = c(0.9,0.05)
    )

  ## CASE 2: THE HOLDER IS A GROUP OF CIVIL/NATURAL PERSONS
  ## ROSTER !!!

  #Q03
  Questionnaire$S2_Q03 <-
    createQuestion(QuestVar = list(en="S2_Q03",fr="S2_Q03",es="S2_P03"),
                   QuestType = "cont.",
                   QuestLab = list(en="What is the number of civil/natural persons who are members of the Holder group?",
                                   fr="Quel est le nombre de co-exploitants dans le groupe d'exploitants?",
                                   es="¿Cuál	es	el	número	de	personas	civiles/naturales	que	son	miembros	del	grupo	del	Titular?"),
                   QuestValue = NULL,
                   QuestValProbs = NULL
    )

  #.
  #.
  #.

  ## CASE 3: THE HOLDER IS A LEGAL PERSON
  ## ROSTER !!!
  
  #Q05
  
  Questionnaire$S2_Q05a <-
    createQuestion(QuestVar   = list(en="S2_Q05a",fr="S2_Q05a",es="S2_P05a"),
                   QuestType  = "text",
                   QuestLab   = list(en="First name of Manager",
                                     fr="Prénom du chef d'exploitation",
                                     es="Nombre del Gerente"),
                   QuestValue = NULL,
                   QuestValProbs = NULL
    )
  
  Questionnaire$S2_Q05b <-
    createQuestion(QuestVar   = list(en="S2_Q05b",fr="S2_Q05b",es="S2_P05b"),
                   QuestType  = "text",
                   QuestLab   = list(en="Surname of Manager",
                                     fr="Nom du chef d'exploitation",
                                     es="Apellido del Gerente"),
                   QuestValue = NULL,
                   QuestValProbs = NULL
    )
  
  Questionnaire$S2_Q05c <-
    createQuestion(QuestVar   = list(en="S2_Q05c",fr="S2_Q05c",es="S2_P05c"),
                   QuestType  = "cont.",
                   QuestLab   = list(en="Contact number (preferably cell phone)",
                                     fr="Numéro de téléphone (de préférence mobile)",
                                     es="Número	de	contacto	(preferiblemente	teléfono	cel"),
                   QuestValue = NULL,
                   QuestValProbs = NULL
    )
  
  Questionnaire$S2_Q05d <-
    createQuestion(QuestVar   = list(en="S2_Q05d",fr="S2_Q05d",es="S2_P05d"),
                   QuestType  = "cat.",
                   QuestLab   = list(en="Sex of Manager",
                                     fr="Sexe du chef d'exploitation",
                                     es="Sexo del	Gerente"),
                   QuestValue = list(en=c("Male","Female"),fr=c("Masculin","Féminin"),es=c("Masculino","Femenino")),
                   QuestValProbs = c(0.50,0.45)
    )
  
  Questionnaire$S2_Q05e <-
    createQuestion(QuestVar   = list(en="S2_Q05e",fr="S2_Q05e",es="S2_P05e"),
                   QuestType  = "cont.",
                   QuestLab   = list(en="Age in completed years",
                                     fr="Âge en années révolues",
                                     es="Edad	en	años	completos"),
                   QuestValue = NULL,
                   QuestValProbs = NULL
    )
  
  Questionnaire$S2_Q05f <-
    createQuestion(QuestVar =list(en="S2_Q05f",fr="S2_Q05f",es="S2_P05f"),
                   QuestType     ="cat.",
                   QuestLab      =list(en="Link with one of the Holders",
                                       fr="Lien de parenté avec l'un des co-exploitants",
                                       es="Vínculo	con	uno	de	los	Titulares"),
                   QuestValue    = list(en=c("Wife/husband or consensual union partner","Other member of the household","External"),
                                        fr=c("Epouse/mari ou vivant maritalement","Autre membre du ménage","Aucun lien de parenté"),
                                        es=c("Esposa/esposo	o	socio	por	unión	consensual","Otro	miembro	del	hogar","Externo")),
                   QuestValProbs = c(0.4,0.4,0.05)
    )
  
  Questionnaire$S2_Q05g <-
    createQuestion(QuestVar =list(en="S2_Q05g",fr="S2_Q05g",es="S2_P05g"),
                   QuestType     ="cat.",
                   QuestLab      =list(en="Nationality",fr="Nationalité",es="Nacionalidad"),
                   QuestValue    = list(en=c("Local country","Neighbouring country","Other"),
                                        fr=c("Pays de résidence","Pays voisin","Autre"),
                                        es=c("País	local","País	vecino","Otro")),
                   QuestValProbs = c(0.8,0.2,0.1)
    )
  
  Questionnaire$S2_Q05i <-
    createQuestion(QuestVar =list(en="S2_Q05i",fr="S2_Q05i",es="S2_P05i"),
                   QuestType     ="cat.",
                   QuestLab      =list(en="Highest level of education completed",
                                       fr="Plus haut niveau d'étude atteint",
                                       es="Nivel	de	educación	más	alto	completado"),
                   QuestValue    = list(en=c("None","Less than primary","Primary","Lower secondary","Upper secondary","Tertiary/post-secondary"),
                                        fr=c("Aucun","Moins que primaire","Primaire","Secondaire court","Secondaire long","Postsecondaire"),
                                        es=c("Ninguna","Inferior	a	primaria","Primaria","Secundaria	inferior","Secundaria	superior"," Terciaria/postsecundaria")),
                   QuestValProbs = c(0.2,0.2,0.2,0.2,0.1,0.1)
    )
  
  Questionnaire$S2_Q05j <-
    createQuestion(QuestVar =list(en="S2_Q05j",fr="S2_Q05j",es="S2_P05j"),
                   QuestType     ="cat.",
                   QuestLab      =list(en="Share of working time spent working on the holding",
                                       fr="Temps de travail sur l'exploitation",
                                       es="Porción	de	tiempo	de	trabajo	que	dedicó	a	la	unidad	de	producción"),
                   QuestValue    = list(en=c("Less than half ( < 40 %)","About half (40%-59%)","Most/almost all (60%-99%)","All (100%)"),
                                        fr=c("Moins d'un mi-temps ( < 40 %)","Mi-temps (40%-59%)","Plus qu'un mi-temps (60%-99%","Temps complet (100%)"),
                                        es=c("Menos	de	la	mitad	(<40%)","Alrededor	de	la	mitad	(40%-59%)","La	mayoría/casi	todo	(60%-99%)","Todo	(100%)")),
                   QuestValProbs = c(0.2,0.2,0.2,0.2)
    )
  
  Questionnaire$S2_Q05k <-
    createQuestion(QuestVar   = list(en="S2_Q05k",fr="S2_Q05k",es="S2_P05k"),
                   QuestType  = "cat.",
                   QuestLab   = list(en="Does the Manager have another gainful activity outside of the holding?",
                                     fr="Le chef d'exploitation a-t-il une autre activité rémunérée en dehors de l'exploitation?",
                                     es="¿Realiza	el	Gerente	otra	actividad	lucrativa	fuera	de	la	unidad	de	producción?"),
                   QuestValue = list(en=c("Yes","No"),fr=c("Oui","Non"),es=c("Si","No")),
                   QuestValProbs = c(0.9,0.05)
    )
  

  #Q06
  Questionnaire$S2_Q06 <-
    createQuestion(QuestVar = list(en="S2_Q06",fr="S2_Q06",es="S2_P06"),
                   QuestType = "cont.",
                   QuestLab = list(en="What is the number of civil/natural persons participating in the capital of the company?",
                                   fr="Combien de personnes physiques participent au capital de L'exploitation?",
                                   es=". ¿Cuál	es	el	número	de	personas	civiles/naturales	que	participa	en	el	capital	de	la	firma?"),
                   QuestValue = NULL,
                   QuestValProbs = NULL
    )

  #Q07
  Questionnaire$S2_Q07 <-
    createQuestion(QuestVar = list(en="S2_Q07",fr="S2_Q07",es="S2_P07"),
                   QuestType = "cont.",
                   QuestLab = list(en="What is the number of legal persons participating in the capital of the company?",
                                   fr="Combien de personnes morales participent au capital de l'exploitation?",
                                   es="¿Cuál	es	el	número	de	personas	jurídicas	que	participan	en	el	capital	de	la	firma?"),
                   QuestValue = NULL,
                   QuestValProbs = NULL
    )

  #Q08
  Questionnaire$S2_Q08 <-
    createQuestion(QuestVar = list(en="S2_Q08",fr="S2_Q08",es="S2_P08"),
                   QuestType = "cont.",
                   QuestLab = list(en="How many Managers are associated with the holding?",
                                   fr="Combien de chefs d'exploitation dirigent l'exploitation?",
                                   es="¿Cuántos	Gerentes	están	relacionados	con	la	unidad	de	producción?"),
                   QuestValue = NULL,
                   QuestValProbs = NULL
    )

  #.
  #.
  #.

  ##### SECTION 3: CROP PRODUCTION DURING THE REFERENCE PERIOD

  ## PART 3.1: CROP PRODUCTION AND DESTINATIONS

  #Q00
  Questionnaire$S3_Q00 <-
    createQuestion(QuestVar   = list(en="S3_Q00",fr="S3_Q00",es="S3_P00"),
                   QuestType  = "cat.",
                   QuestLab   = list(en="Did the holding grow crops during the reference period, whatever the production or destination?",
                                     fr="Y a-t-il eu une production végétale sur l'exploitation pendant la période de référence, quelle que soit la production ou sa destination?",
                                     es="¿Produjo	la	unidad	de	producción	cultivos	durante	el	periodo	de	referencia,	independientemente	de	la	producción	o	destino?"),
                   QuestValue = list(en=c("Yes","No"),fr=c("Oui","Non"),es=c("Si","No")),
                   QuestValProbs = c(0.9,0.05)
    )

  #Q00a AS A ROSTER?

  #Q00b
  Questionnaire$S3_Q00b <-
    createQuestion(QuestVar   = list(en="S3_Q00b",fr="S3_Q00b",es="S3_P00b"),
                   QuestType  = "cat.",
                   QuestLab   = list(en="Do you plan to introduce crops in the upcomming period?",
                                     fr="Avez-vous l'intention de commencer des cultures végétales dans un proche avenir?",
                                     es="¿Tiene	usted	planeado	introducir	cultivos	en	el	próximo	periodo?"),
                   QuestValue = list(en=c("Yes","No"),fr=c("Oui","Non"),es=c("Si","No")),
                   QuestValProbs = c(0.9,0.05)
    )

  #Q01
  Questionnaire$S3_Q01 <-
    createQuestion(QuestVar   = list(en="S3_Q01",fr="S3_Q01",es="S3_P01"),
                   QuestType  = "cat.",
                   QuestLab   = list(en="Would you be confident in providing an estimate of the area of your holding?",
                                     fr="Pensez-vous pouvoir fournir une estimation correcte des superficies de votre exploitation?",
                                     es="¿Estaría	usted	seguro	al	proporcionar	una	estimación	del	área	de	su	unidad	de	producción?"),
                   QuestValue = list(en=c("Yes","No"),fr=c("Oui","Non"),es=c("Si","No")),
                   QuestValProbs = c(0.9,0.05)
    )

  #Q02
  Questionnaire$S3_Q02 <-
    createQuestion(QuestVar = list(en="S3_Q02",fr="S3_Q02",es="S3_P02"),
                   QuestType = "cont.",
                   QuestLab = list(en="How many parcels did you use for agricultural production (for crops and livestock) during the reference period?",
                                   fr="Combien de parcelles avez-vous utilisé pour les productions agricoles (végétales et animales) pendant la période de référence?",
                                   es="¿Cuántas	parcelas	utilizó	para	producción	agrícola	(para	cultivos	y	ganadería)	durante	el	periodo	de	referencia?"),
                   QuestValue = NULL,
                   QuestValProbs = NULL
    )


return(Questionnaire)

}
