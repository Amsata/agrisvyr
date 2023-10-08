#' Generate a sample geo-reference data of agris survey based on the generic questionnaire
#'
#' @param n size of sample point per geographic unit
#' @param na_prob proportion of random missing data introduced in the data
#'
#' @return
#' @importFrom dplyr mutate
#' @importFrom dplyr bind_cols
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr %>%
#' @import sf
#' @importFrom wakefield r_sample
#' @importFrom wakefield id
#' @importFrom wakefield sex
#' @importFrom wakefield r_data_frame
#' @importFrom wakefield r_na
#' @importFrom  labelled recode_if
#' @importFrom  labelled set_variable_labels
#' @importFrom  generator r_national_identification_numbers
#' @importFrom  generator r_ipv4_addresses
#' @importFrom  generator r_phone_numbers
#' @export
#'
#' @examples
agrisdata_fr <- function(shpfile, level=3,
                               geo_id=c("ID_1","ID_2","ID_3"),
                               geo_name=c("NAME_1","NAME_2","NAME_3"),
                               np=30,na_prob=0.0005){

  # df=agrisvyr:::SEN3_shp

  shpfile=shpfile %>% dplyr::select(all_of(c(geo_id,geo_name)))

  if (level==1){
    shpfile=shpfile %>% label_geo_name(geo_name[1],geo_id[1],Q13a) %>%
      dplyr::mutate(Q13b=NA,Q13c=NA)
  }

  if(level==2){

    shpfile=shpfile %>% label_geo_name(geo_name[1],geo_id[1],Q13a)
    shpfile=shpfile %>% label_geo_name(geo_name[2],geo_id[2],Q13b) %>%
      dplyr::mutate(Q13c=NA)
  }

  if(level==3){
    shpfile=shpfile %>% label_geo_name(geo_name[1],geo_id[1],Q13a)
    shpfile=shpfile %>% label_geo_name(geo_name[2],geo_id[2],Q13b)
    shpfile=shpfile %>% label_geo_name(geo_name[3],geo_id[3],Q13c)
  }


  points=shpfile %>%st_sample(shpfile, size = np:(np+1), type = "random") %>% sf::st_as_sf()
  points_df=as.data.frame(st_coordinates(points))

  final_points=points %>% sf::st_join(shpfile) %>%
    bind_cols(points_df) %>% dplyr::rename(Q19a=X,Q19b=Y) %>%
    dplyr::mutate(Q17d=Q13b,Q17c=Q13a,Q17b=Q13c)

  n=nrow(final_points)

  data=final_points %>% dplyr::bind_cols(wakefield::r_data_frame(

    #*****************************************************************************
    #* PART 1.2: IDENTIFICATION OF THE HOLDING ***********************************
    #* ***************************************************************************
    n=n,
    holdingID=wakefield::id(),
    Q10=wakefield::r_sample(x=c("Personne physique","Groupe de personnes physiques","Personne morale"),
                            prob=c(0.9,0.2,0.01)),
    Q11=wakefield::r_sample(x=c("Exploitation familiale","entreprise"),prob = c(0.95,0.1)),
    Q12c=wakefield::r_sample(x=c("Homme","Femme"),prob = c(0.8,0.20)),
    Q17=wakefield::r_sample(x=c("Identique à l'adresse de exploitant","Différente de l'adresse de exploitant"),prob = c(0.1,0.95)),
    Q18=wakefield::r_sample(x=c("Logement des ménages (pour le secteur HH) et exploitation agricole, y compris le logement et les bâtiments agricoles",
                                "Bâtiment agricole principal","Principale parcelle agricole"),prob = c(0.8,0.2,0.3)),

    #*****************************************************************************
    #* PART 1.2: AGRICULTURAL ACTIVITY         ***********************************
    #* ***************************************************************************
    Q23=wakefield::r_sample(x=c("Non, Jamais","Oui, seulement occasionnellement ou partiellement","Oui, systématiquement"),prob = c(0.65,0.25,0.2)),
    Q24_1=wakefield::r_sample(x=c("Oui","Non"),prob = c(0.6,02)),
    Q24_2=wakefield::r_sample(x=c("Oui","Non"),prob = c(0.6,02)),
    Q24_3=wakefield::r_sample(x=c("Oui","Non"),prob = c(0.6,02)),
    Q24_4=wakefield::r_sample(x=c("Oui","Non"),prob = c(0.6,02)),
    Q24_5=wakefield::r_sample(x=c("Oui","Non"),prob = c(0.6,02)),
    Q24_6=wakefield::r_sample(x=c("Oui","Non"),prob = c(0.6,02)),
    Q24_7=wakefield::r_sample(x=c("Oui","Non"),prob = c(0.6,02)),
    Q24_8=wakefield::r_sample(x=c("Oui","Non"),prob = c(0.6,02)),
    Q24_9=wakefield::r_sample(x=c("Oui","Non"),prob = c(0.6,02)),
    #NOTE: some correction to do in Q23 in case no Q24 is Yes
    Q25_1=wakefield::r_sample(x=c("Oui","Non"),prob = c(0.6,02)),
    Q25_2=wakefield::r_sample(x=c("Oui","Non"),prob = c(0.6,02)),
    Q25_3=wakefield::r_sample(x=c("Oui","Non"),prob = c(0.6,02)),
    Q25_4=wakefield::r_sample(x=c("Oui","Non"),prob = c(0.6,02)),
    Q25_5=wakefield::r_sample(x=c("Oui","Non"),prob = c(0.6,02)),
    Q25_6=wakefield::r_sample(x=c("Oui","Non"),prob = c(0.6,02)),
    Q25_7=wakefield::r_sample(x=c("Oui","Non"),prob = c(0.6,02)),
    Q25_8=wakefield::r_sample(x=c("Oui","Non"),prob = c(0.6,02)),
    Q26=wakefield::r_sample(x=c("Principalement la production de cultures","Principalement la production animale",
                                "mla combinaison de la production végétale et animale"),prob = c(0.9,0.2,0.3)),
    Q27=wakefield::r_sample(x=c("Production de grandes cultures annuelles (céréales, oléagineux, protéagineux, plantes à racines, tabac, coton, etc.)",
                                "Production de légumes, de champignons, de fleurs, de plantes ornementales, etc.",
                                "Production de raisins pour le vin",
                                "Production de fruits",
                                "Production d'autres cultures pérennes (cacao, café, etc.)",
                                "Cultures mixtes (pas de prévalence réelle d'une culture spécifique)"),
                            prob = c(0.5,0.2,0.6,0.2,0.4,0.6)),
    Q28=wakefield::r_sample(x=c("Élevage de ruminants pour la viande (bovins, ovins, caprins, etc.)",
                                "Élevage de non-ruminants pour la viande (porcs, volailles, etc.)",
                                "Production d'œufs",
                                "Production de lait",
                                "Élevage mixte (pas de prévalence réelle d'une activité d'élevage spécifique)"),
                            prob = c(0.9,0.4,03,0.2,0.1)),
    Q29=wakefield::r_sample(x=c("Produire principalement pour la vente (vendre 90 % ou plus)",
                                "Production principalement destinée à la vente, avec une part d'autoconsommation (vente de plus de 50 % et jusqu'à 90 %)",
                                "Production principalement destinée à l'autoconsommation, avec quelques ventes (plus de 10 % et jusqu'à 50 %)",
                                "Production essentiellement destinée à l'autoconsommation (vente de 10 % ou moins)"),
                            prob = c(0.5,0.4,0.6,0.2))


  )) %>% dplyr::mutate(
    #*****************************************************************************
    #* PART 1.2: IDENTIFICATION OF THE HOLDING ***********************************
    #* ***************************************************************************
    Q12a=gen_names(Q12c,"first"),
    Q12b=gen_names(Q12c,"last"),
    Q12d=generator::r_national_identification_numbers(n),
    Q14=gen_names(Q12c,"both"),
    Q16=generator::r_national_identification_numbers(n),
    Q17a=generator::r_ipv4_addresses(n),
    Q17e=generator::r_phone_numbers(n),
    Q20=generator::r_national_identification_numbers(n),
    Q21a=generator::r_national_identification_numbers(n),
    Q21b=generator::r_national_identification_numbers(n),
    Q21c=generator::r_national_identification_numbers(n),
    Q21d=NA,
    Q22=NA) %>%
    labelled::set_variable_labels(

      #*****************************************************************************
      #* PART 1.2: IDENTIFICATION OF THE HOLDING ***********************************
      #* ***************************************************************************
      Q10="Quel est le statut légal de l'exploitant?",
      Q11="Quel est le statut légal de l'exploitation?",
      Q12a="Prénom",
      Q12b="Nom de famille",
      Q12c="Sex de l\'exploitant/co-exploitant",
      Q12d="NUMERO PERSONNEL de l'exploitant",
      Q13a="Adresse de l'exploitant: Région",
      Q13b="Adresse de l'exploitant: District",
      Q13c="Adresse de l'exploitant: Nom du village ou ville",
      Q14="Quelle est la raison sociale de l'exploitation?",
      Q16="Numéro de série de l'exploitation",
      Q17="L'adresse de l'exploitation est-elle la même que celle de l'exploitant",
      Q17a="Adresse (rue)",
      Q17b="b Nom du village, ville",
      Q17c="Région",
      Q17d="District",
      Q17e="Numéro de téléphone principal de l'exploitation",
      Q18="A quoi correspond l'adresse ci-dessus?",
      Q19a="Latitude",
      Q19b="Longitude",
      Q20="Quel est l'identifiant de l'exploitation dans le répertoire national d'entreprises?",
      Q21a="Quels sont les autres identifiants administratifs de l'exploitation? - Cheptel",
      Q21b="Quels sont les autres identifiants administratifs de l'exploitation? - Production de vin",
      Q21c="Quels sont les autres identifiants administratifs de l'exploitation? - c Production biologique",
      Q21d="Quels sont les autres identifiants administratifs de l'exploitation?- Autre (préciser)",
      Q22="Quel est l'identifiant de l'exploitation dans le dernier recensement de l'agriculture? (Peut être pré renseigné)",
      #*****************************************************************************
      #* PART 1.2: AGRICULTURAL ACTIVITY         ***********************************
      #* ***************************************************************************
      Q23="L'exploitation enregistre-t-elle son activité agricole ou comptable sur des registres ou carnets journaliers?",
      Q24_1="Informations enregistrées: Superficie cultivée/récoltée",
      Q24_2="Informations enregistrées: 2 Productions végétales",
      Q24_3="Informations enregistrées: Productions animales",
      Q24_4="Informations enregistrées: Prix de vente, montants des ventes et total vendu par produit",
      Q24_5="Informations enregistrées: 5 Quantités d'intrants utilisés (semences, fertilisants, produits de protection des plantes, etc.)",
      Q24_6="Informations enregistrées: 6 Quantités détaillées et prix des intrants achetés",
      Q24_7="Informations enregistrées: Temps de travail des salariés",
      Q24_8="Informations enregistrées: 8 Autre (préciser",
      Q24_9="Informations enregistrées: Workers’ payment",
      Q25_1="types de faire valoir: 1 En propriété avec un acte écrit",
      Q25_2="types de faire valoir: En propriété sans acte écrit",
      Q25_3="types de faire valoir: Location ou métayage avec un acte écrit",
      Q25_4="types de faire valoir: Location ou métayage sans acte écrit",
      Q25_5="types de faire valoir: Superficie communale ou d'État utilisée avec un accord écrit ",
      Q25_6="types de faire valoir: Superficie communale ou d'État utilisée sans accord écrit ",
      Q25_7="types de faire valoir: Occupé/squatté sans permission",
      Q25_8="types de faire valoir:  Pas de superficie agricole",
      Q26="D'un point de vue économique, quelle a été l'activité agricole principale de l'exploitation pendant la période de référence?",
      Q27="D'un point de vue économique, quelle a été l'activité végétale principale?",
      Q28="D'un point de vue économique, quelle a été l'activité animale principale? ",
      Q29="Quelle est la principale destination de vos productions agricoles?"
    ) %>%
    # skip controle

    #*****************************************************************************
    #* PART 1.2: IDENTIFICATION OF THE HOLDING ***********************************
    #* ***************************************************************************
    #If Q10 = 1 or 2 GO TO Q12, otherwise GO TO Q14
    dplyr::mutate(Q12a=Q12a %>% labelled::recode_if(!(Q10=="Personne morale"),NA),
                  Q12b=Q12b %>% labelled::recode_if((Q10=="Personne morale"),NA),
                  Q12c=Q12c %>% labelled::recode_if((Q10=="Personne morale"),NA),
                  Q12d=Q12d %>% labelled::recode_if((Q10=="Personne morale"),NA),
                  Q13a=Q13a %>% labelled::recode_if((Q10=="Personne morale"),NA),
                  Q13b=Q13b %>% labelled::recode_if((Q10=="Personne morale"),NA),
                  Q13c=Q13c %>% labelled::recode_if((Q10=="Personne morale"),NA),
                  Q14=Q14 %>% labelled::recode_if(!(Q10=="Personne morale"),NA),
                  #If Q17 = 1 → Go to Q18
                  Q17=Q17 %>% labelled::recode_if(!(Q10=="Personne morale"),"Différente de l'adresse de exploitant"),
                  Q17a=Q17a %>% labelled::recode_if((Q17=="Same as the address of the Holder"),NA),
                  Q17b=Q17b %>% labelled::recode_if((Q17=="Same as the address of the Holder"),NA),
                  Q17c=Q17c %>% labelled::recode_if((Q17=="Same as the address of the Holder"),NA),
                  Q17d=Q17d %>% labelled::recode_if((Q17=="Same as the address of the Holder"),NA),
                  Q17e=Q17e %>% labelled::recode_if((Q17=="Same as the address of the Holder"),NA),
                  #replace by NA if same region, same district, same village and correct Q17
                  check= (Q17c==Q13a) & (Q17b==Q13c) & (Q17d==Q13b) & Q17=="Différente de l'adresse de exploitant",
                  Q17a=Q17a %>% labelled::recode_if(check==TRUE,NA),
                  Q17b=Q17b %>% labelled::recode_if(check==TRUE,NA),
                  Q17c=Q17c %>% labelled::recode_if(check==TRUE,NA),
                  Q17d=Q17d %>% labelled::recode_if(check==TRUE,NA),
                  Q17e=Q17e %>% labelled::recode_if(check==TRUE,NA),
                  Q17=Q17 %>% labelled::recode_if(check==TRUE,"Identique à l'adresse de exploitant")
    ) %>%
    #generate enumeration area
    dplyr::mutate(Region=ifelse(!is.na(Q13a),Q13a,Q17c)) %>%
    dplyr::group_by(Region) %>% dplyr::mutate(Q15=paste0(stringr::str_sub(Region,7),
                                                         base::sample(100:120,1))) %>% dplyr::ungroup() %>%
    dplyr::select(-check,-Region) %>%
    labelled::set_variable_labels(Q15="Zone de recensement de l'exploitation")
    # perturbate by including some missing information
    wakefield::r_na(prob = na_prob)

  return(data)
}
