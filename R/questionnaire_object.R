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
                   QuestLab      =list(en="Nationality",fr="Nationalité",es="Nacionalidad"),
                   QuestValue    = list(en=c("Local country","Neighbouring country","Other"),
                                        fr=c("Pays de résidence","Pays voisin","Autre"),
                                        es=c("País	local","2 País	vecino","Otro")),
                   QuestValProbs = c(0.8,0.2,0.1)
                   )

  #Q02
  Questionnaire$Q02 <-
    createQuestion(QuestVar   = list(en="Q02",fr="Q02",es="P02"),
                   QuestType  = "cat.",
                   QuestLab   = list(en="Did I find somebody from the holding who accepted to answer?",
                                     fr="Ai-je trouvé quelqu'un dans l'exploitation qui accepte de répondre ?",
                                     es="¿Encontré	a	alguien	de	la	unidad	de	producción	que	aceptó	responder?"),
                   QuestValue = list(en=c("Yes","No"),fr=c("Oui","Non"),es=c("Si","No")),
                   QuestValProbs = c(0.9,0.05)
    )
  
  #Q03
  Questionnaire$Q03a <-
    createQuestion(QuestVar   = list(en="Q03a",fr="Q03a",es="P03a"),
                   QuestType  = "text",
                   QuestLab   = list(en="First name of the respondent",
                                     fr="Prénom du répondant",
                                     es="Nombre del	encuestado"),
                   QuestValue = NULL,
                   QuestValProbs = NULL
    )
  
  Questionnaire$Q03b <-
    createQuestion(QuestVar   = list(en="Q03b",fr="Q03b",es="P03b"),
                   QuestType  = "text",
                   QuestLab   = list(en="Surname of the respondent",
                                     fr="Nom de famille du répondant",
                                     es="Apellido del	encuestado"),
                   QuestValue = NULL,
                   QuestValProbs = NULL
    )
  
  Questionnaire$Q03c <-
    createQuestion(QuestVar   = list(en="Q03c",fr="Q03c",es="P03c"),
                   QuestType  = "cat.",
                   QuestLab   = list(en="Sex of the respondant",
                                     fr="Sexe du répondant",
                                     es="Sexo del	encuestado"),
                   QuestValue = list(en=c("Male","Female"),fr=c("Masculin","Féminin"),es=c("Masculino","Femenino")),
                   QuestValProbs = c(0.50,0.45)
    )
  
  Questionnaire$Q03d <-
    createQuestion(QuestVar   = list(en="Q03d",fr="Q03d",es="P03d"),
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
  Questionnaire$Q04 <-
    createQuestion(QuestVar   = list(en="Q04",fr="Q04",es="P04"),
                   QuestType  = "cat.",
                   QuestLab   = list(en="Is the holding currently growing any crops or fruits, or raising animals, or did it do so during the reference period?",
                                     fr="L'exploitation a-t-elle et/ou a-t-elle eu pendant la période de référence une activité de production végétale et/ou animale (végétaux, fruits, animaux élevés) ?",
                                     es="¿Está	la	unidad	de	producción		produciendo	algún	cultivo	o	frutas,	criando	animales	actualmente,	o	lo	hizo	durante	el	periodo	de	referencia?"),
                   QuestValue = list(en=c("Yes","No"),fr=c("Oui","Non"),es=c("Si","No")),
                   QuestValProbs = c(0.9,0.05)
    )
  
  #Q05
  Questionnaire$Q05 <-
    createQuestion(QuestVar   = list(en="Q05",fr="Q05",es="P05"),
                   QuestType  = "cat.",
                   QuestLab   = list(en="Has the holding ceased its activity without any transfer of its means of production?",
                                     fr="L'exploitation a-t-elle cessé son activité sans aucun transfert de ses moyens de production ?",
                                     es="¿Está	la	unidad	de	producción	produciendo	algún	cultivo	o	frutas,	criando	animales	actualmente,	o	lo	hizo	durante	el	periodo	de	referencia?"),
                   QuestValue = list(en=c("Yes","No"),fr=c("Oui","Non"),es=c("Si","No")),
                   QuestValProbs = c(0.05,0.9)
    )
  
  #Q06
  Questionnaire$Q05 <-
    createQuestion(QuestVar   = list(en="Q05",fr="Q05",es="P05"),
                   QuestType  = "cat.",
                   QuestLab   = list(en="Will the holding resume its activity?",
                                     fr="L'exploitation va-t-elle reprendre son activité ?",
                                     es="¿Ha	suspendido,	la	unidad	de	producción,	su	actividad	sin	ninguna	transferencia	de	sus	medios	de	producción?"),
                   QuestValue = list(en=c("Yes","No"),fr=c("Oui","Non"),es=c("Si","No")),
                   QuestValProbs = c(0.9,0.05)
    )
  
  #Q10
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
                   QuestValProbs = c(0.7,0.2,0.01)
                   )

  #Q12
  Questionnaire$Q12a <-
    createQuestion(QuestVar   = list(en="Q12a",fr="Q12a",es="P12a"),
                   QuestType  = "text",
                   QuestLab   = list(en="First name of the Holder/Co-holders",
                                     fr="Prénom de l'exploitant ou des co-exploitants",
                                     es="Nombre del	Titular/Cotitulares"),
                   QuestValue = NULL,
                   QuestValProbs = NULL
    )
  
  Questionnaire$Q12b <-
    createQuestion(QuestVar   = list(en="Q12b",fr="Q12b",es="P12b"),
                   QuestType  = "text",
                   QuestLab   = list(en="Surname of the Holder/Co-holders",
                                     fr="Nom de famille de l'exploitant ou des co-exploitants",
                                     es="Apellido del	Titular/Cotitulares"),
                   QuestValue = NULL,
                   QuestValProbs = NULL
    )
  
  Questionnaire$Q12c <-
    createQuestion(QuestVar   = list(en="Q12c",fr="Q12c",es="P12c"),
                   QuestType  = "cat.",
                   QuestLab   = list(en="Sex of the Holder/Co-holders",
                                     fr="Sexe de l'exploitant ou des co-exploitants",
                                     es="Sexo del	Titular/Cotitulares"),
                   QuestValue = list(en=c("Male","Female"),fr=c("Masculin","Féminin"),es=c("Masculino","Femenino")),
                   QuestValProbs = c(0.50,0.45)
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
  
  #Q17
  Questionnaire$Q17 <-
    createQuestion(QuestVar   = list(en="Q17",fr="Q17",es="P17"),
                   QuestType  = "cat.",
                   QuestLab   = list(en="Address of the holding",
                                     fr="L'adresse de l'exploitation est-elle",
                                     es="Dirección	de	la	unidad	de	producción"),
                   QuestValue = list(en=c("Same as the address of the Holder","Different from the address of the Holde"),
                                     fr=c("La même que celle de l'exploitant","Différente de celle l'exploitant"),
                                     es=c("La	misma	dirección	a	la	del	Titular ","Dirección	diferente	a	la	del	Titular")),
                   QuestValProbs = c(0.9,0.05)
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
                                   fr="Nom du village ou de la ville",
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
                   QuestValProbs = c(0.7,0.2,0.01)
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
  
  #Q20
  Questionnaire$Q20 <-
    createQuestion(QuestVar = list(en="Q20",fr="Q20",es="P20"),
                   QuestType = "cont.",
                   QuestLab = list(en="What is the official identification number of the holding in the national business register?",
                                   fr="Quel est l'identifiant de l'exploitation dans le répertoire national d'entreprises ?",
                                   es="¿Cuál	es	el	número	de	identificación	oficial	de	la	unidad	de	producción	en	el	registro	de	empresas	nacionales?"),
                   QuestValue = NULL,
                   QuestValProbs = NULL
    )
  
  #Q21
  Questionnaire$Q21a <-
    createQuestion(QuestVar = list(en="Q21a",fr="Q21a",es="P21a"),
                   QuestType = "cont.",
                   QuestLab = list(en="What are the other administrative identification numbers of the holding? _ Livestock",
                                   fr="Quels sont les autres identifiants administratifs de l'exploitation? _ Cheptel",
                                   es="¿Cuales	son	los	demás	números	de	identificación	administrativos	de	la	unidad	de	producción? _ Ganadería"),
                   QuestValue = NULL,
                   QuestValProbs = NULL
    )
  
  Questionnaire$Q21b <-
    createQuestion(QuestVar = list(en="Q21b",fr="Q21b",es="P21b"),
                   QuestType = "cont.",
                   QuestLab = list(en="What are the other administrative identification numbers of the holding? _ Wine production",
                                   fr="Quels sont les autres identifiants administratifs de l'exploitation? _ Production de vin",
                                   es="¿Cuales	son	los	demás	números	de	identificación	administrativos	de	la	unidad	de	producción? _ Producción	de	vino"),
                   QuestValue = NULL,
                   QuestValProbs = NULL
    )
  
  Questionnaire$Q21c <-
    createQuestion(QuestVar = list(en="Q21c",fr="Q21c",es="P21c"),
                   QuestType = "cont.",
                   QuestLab = list(en="What are the other administrative identification numbers of the holding? _ Organic production",
                                   fr="Quels sont les autres identifiants administratifs de l'exploitation? _ Production biologique",
                                   es="¿Cuales	son	los	demás	números	de	identificación	administrativos	de	la	unidad	de	producción? _ Producción	orgánica"),
                   QuestValue = NULL,
                   QuestValProbs = NULL
    )
  
  #Q22
  Questionnaire$Q22 <-
    createQuestion(QuestVar = list(en="Q22",fr="Q22",es="P22"),
                   QuestType = "cont.",
                   QuestLab = list(en="What is the identification number of the holding from the last agricultural census?",
                                   fr="Quel est l'identifiant de l'exploitation dans le dernier recensement de l'agriculture ?",
                                   es="¿Cuál	es	el	número	de	identificación	de	la	unidad	de	producción	en	el	último	censo	agropecuario?"),
                   QuestValue = NULL,
                   QuestValProbs = NULL
    )
  
  #Q23
  Questionnaire$Q23 <-
    createQuestion(QuestVar = list(en="Q23",fr="Q23",es="P23"),
                   QuestType = "cat.",
                   QuestLab = list(en="Does the holding record its agricultural activity or finances on registers or logbooks?",
                                   fr="L'exploitation enregistre-t-elle son activité agricole ou comptable sur des registres ou carnets journaliers ?",
                                   es="¿Registra	la	unidad	de	producción	su	actividad	agropecuaria	o	finanzas	en	registros	o	libros	de	diario?"),
                   QuestValue = list(en=c("No, never",
                                          "Yes, only occasionally or partially",
                                          "Yes, systematically"),
                                     fr=c("Non, jamais",
                                          "Oui, occasionnellement ou partiellement",
                                          "Oui, systématiquement"),
                                     es=c("No,	nunca",
                                          "Si,	solo	ocasional	y	parcialmente",
                                          "Sí,	sistemáticamente")),
                   QuestValProbs = c(0.7,0.2,0.01)
    )
  
  #Q24
  Questionnaire$Q24_1 <-
    createQuestion(QuestVar   = list(en="Q24_1",fr="Q24_1",es="P24_1"),
                   QuestType  = "cat.",
                   QuestLab   = list(en="What information is systematically registered? _ Area cultivated/harvested",
                                     fr="Quelles informations sont systématiquement enregistrées ? _ Superficie cultivée/récoltée",
                                     es="¿Qué	información	se	registra	sistemáticamente? _ Área	cultivada/cosechada"),
                   QuestValue = list(en=c("Yes","No"),fr=c("Oui","Non"),es=c("Si","No")),
                   QuestValProbs = c(0.5,0.45)
    )
  
  Questionnaire$Q24_2 <-
    createQuestion(QuestVar   = list(en="Q24_2",fr="Q24_2",es="P24_2"),
                   QuestType  = "cat.",
                   QuestLab   = list(en="What information is systematically registered? _ Crop production",
                                     fr="Quelles informations sont systématiquement enregistrées ? _ Productions végétales",
                                     es="¿Qué	información	se	registra	sistemáticamente? _ Producción	de	cultivos"),
                   QuestValue = list(en=c("Yes","No"),fr=c("Oui","Non"),es=c("Si","No")),
                   QuestValProbs = c(0.5,0.45)
    )
  
  Questionnaire$Q24_3 <-
    createQuestion(QuestVar   = list(en="Q24_3",fr="Q24_3",es="P24_3"),
                   QuestType  = "cat.",
                   QuestLab   = list(en="What information is systematically registered? _ Livestock production",
                                     fr="Quelles informations sont systématiquement enregistrées ? _ Productions animales",
                                     es="¿Qué	información	se	registra	sistemáticamente? _ Producción	de	ganado"),
                   QuestValue = list(en=c("Yes","No"),fr=c("Oui","Non"),es=c("Si","No")),
                   QuestValProbs = c(0.5,0.45)
    )
  
  Questionnaire$Q24_4 <-
    createQuestion(QuestVar   = list(en="Q24_4",fr="Q24_4",es="P24_4"),
                   QuestType  = "cat.",
                   QuestLab   = list(en="What information is systematically registered? _ Unit prices, amounts sold and total sales by product",
                                     fr="Quelles informations sont systématiquement enregistrées ? _ Prix de vente, montants des ventes et total vendu par produit",
                                     es="¿Qué	información	se	registra	sistemáticamente? _ Precios	unitarios,	montos	vendidos	y	ventas	totales	por	producto"),
                   QuestValue = list(en=c("Yes","No"),fr=c("Oui","Non"),es=c("Si","No")),
                   QuestValProbs = c(0.5,0.45)
    )
  
  Questionnaire$Q24_5 <-
    createQuestion(QuestVar   = list(en="Q24_5",fr="Q24_5",es="P24_5"),
                   QuestType  = "cat.",
                   QuestLab   = list(en="What information is systematically registered? _ Input quantities used",
                                     fr="Quelles informations sont systématiquement enregistrées ? _ Quantités d'intrants utilisés",
                                     es="¿Qué	información	se	registra	sistemáticamente? _ Cantidades	de	insumos	utilizados"),
                   QuestValue = list(en=c("Yes","No"),fr=c("Oui","Non"),es=c("Si","No")),
                   QuestValProbs = c(0.5,0.45)
    )
  
  Questionnaire$Q24_6 <-
    createQuestion(QuestVar   = list(en="Q24_6",fr="Q24_6",es="P24_6"),
                   QuestType  = "cat.",
                   QuestLab   = list(en="What information is systematically registered? _ Detailed quantities and prices of inputs bought",
                                     fr="Quelles informations sont systématiquement enregistrées ? _ Quantités détaillées et prix des intrants achetés",
                                     es="¿Qué	información	se	registra	sistemáticamente? _ Cantidades	detalladas	y	precios	de	insumos	adquiridos"),
                   QuestValue = list(en=c("Yes","No"),fr=c("Oui","Non"),es=c("Si","No")),
                   QuestValProbs = c(0.5,0.45)
    )
  
  Questionnaire$Q24_7 <-
    createQuestion(QuestVar   = list(en="Q24_7",fr="Q24_7",es="P24_7"),
                   QuestType  = "cat.",
                   QuestLab   = list(en="What information is systematically registered? _ Workers’ time",
                                     fr="Quelles informations sont systématiquement enregistrées ? _ Temps de travail des salariés",
                                     es="¿Qué	información	se	registra	sistemáticamente? _ Tiempo	de	los	trabajadores"),
                   QuestValue = list(en=c("Yes","No"),fr=c("Oui","Non"),es=c("Si","No")),
                   QuestValProbs = c(0.5,0.45)
    )
  
  Questionnaire$Q24_8 <-
    createQuestion(QuestVar   = list(en="Q24_8",fr="Q24_8",es="P24_8"),
                   QuestType  = "cat.",
                   QuestLab   = list(en="What information is systematically registered? _ Workers’ payment",
                                     fr="Quelles informations sont systématiquement enregistrées ? _ Paiement des salariés",
                                     es="¿Qué	información	se	registra	sistemáticamente? _ Pago	de	los	trabajadores"),
                   QuestValue = list(en=c("Yes","No"),fr=c("Oui","Non"),es=c("Si","No")),
                   QuestValProbs = c(0.5,0.45)
    )
  
  Questionnaire$Q24_9 <-
    createQuestion(QuestVar   = list(en="Q24_9",fr="Q24_9",es="P24_9"),
                   QuestType  = "cat.",
                   QuestLab   = list(en="What information is systematically registered? _ Other (specify)",
                                     fr="Quelles informations sont systématiquement enregistrées ? _ Autre (préciser)",
                                     es="¿Qué	información	se	registra	sistemáticamente? _ Otro	(especifique)"),
                   QuestValue = list(en=c("Yes","No"),fr=c("Oui","Non"),es=c("Si","No")),
                   QuestValProbs = c(0.04,0.95)
    )
  


return(Questionnaire)

}
