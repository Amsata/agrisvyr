#' Generate first name and last name based on sex
#'
#' @param sex sex for which to generate name takes "Male" or Female
#' @param first_last takes "first", "last" or "both"
#'
#' @return
#' @importFrom randomNames randomNames
#'
#' @examples
#' \dontrun{
#' gen_names("Male","both")
#' }

gen_names=function(sex,first_last){
  randomNames::randomNames(1,
              gender=sex,
              ethnicity=base::sample(c("African American", "Hispanic", "Asian", "White", "Native American"),1),
              which.names=first_last,
              name.sep=" ",
              sample.with.replacement=TRUE,
              return.complete.data=FALSE)

}




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
generate_agrisdata <- function(n=60,na_prob=0.0005){

df=agrisvyr:::SEN3_shp

points=st_sample(df, size = n:(n+1), type = "random") %>% sf::st_as_sf()

xy=as.data.frame(sf::st_coordinates(points))

final_points=points %>% sf::st_join(df) %>%
  dplyr::bind_cols(xy) %>%
  sf::st_drop_geometry() %>%
  dplyr::select(GEO_ID=CCN_3,Q13a=NAME_1,Q13b=NAME_2,Q13c=NAME_3,Q19a=X,Q19b=Y) %>%
  dplyr::mutate(Q17_id=sample(df$CCN_3,size = nrow(xy),replace = TRUE)) %>%
  dplyr::left_join(st_drop_geometry(df) %>%
                     dplyr::select(Q17d=NAME_1,Q17c=NAME_2,Q17b=NAME_3,GEO_ID=CCN_3)) %>%
  dplyr::select(-GEO_ID)

n=nrow(final_points)

data=final_points %>% dplyr::bind_cols(wakefield::r_data_frame(

  #*****************************************************************************
  #* PART 1.2: IDENTIFICATION OF THE HOLDING ***********************************
  #* ***************************************************************************
  n=n,
  holdingID=wakefield::id(),
  Q10=wakefield::r_sample(x=c("Civil person/natural person","Group of civil persons/natural persons","Legal person"),
               prob=c(0.9,0.2,0.01)),
  Q11=wakefield::r_sample(x=c("Family holding","entreprises"),prob = c(0.95,0.1)),
  Q12c=wakefield::sex(prob = c(0.8,0.15)),
  Q17=wakefield::r_sample(x=c("Same as the address of the Holder","Different from the address of the Holder"),prob = c(0.1,0.95)),
  Q18=wakefield::r_sample(x=c("Household dwelling (for HH sector) and farm, including dwelling and agricultural buildings",
                   "Main agricultural building","Main agricultural parcel"),prob = c(0.8,0.2,0.3)),

  #*****************************************************************************
  #* PART 1.2: AGRICULTURAL ACTIVITY         ***********************************
  #* ***************************************************************************
  Q23=wakefield::r_sample(x=c("No, never","Yes, only occasionally or partially","Yes, systematically"),prob = c(0.65,0.25,0.2)),
  Q24_1=wakefield::r_sample(x=c("Yes","No"),prob = c(0.6,02)),
  Q24_2=wakefield::r_sample(x=c("Yes","No"),prob = c(0.6,02)),
  Q24_3=wakefield::r_sample(x=c("Yes","No"),prob = c(0.6,02)),
  Q24_4=wakefield::r_sample(x=c("Yes","No"),prob = c(0.6,02)),
  Q24_5=wakefield::r_sample(x=c("Yes","No"),prob = c(0.6,02)),
  Q24_6=wakefield::r_sample(x=c("Yes","No"),prob = c(0.6,02)),
  Q24_7=wakefield::r_sample(x=c("Yes","No"),prob = c(0.6,02)),
  Q24_8=wakefield::r_sample(x=c("Yes","No"),prob = c(0.6,02)),
  Q24_9=wakefield::r_sample(x=c("Yes","No"),prob = c(0.6,02)),
  #NOTE: some correction to do in Q23 in case no Q24 is Yes
  Q25_1=wakefield::r_sample(x=c("Yes","No"),prob = c(0.6,02)),
  Q25_2=wakefield::r_sample(x=c("Yes","No"),prob = c(0.6,02)),
  Q25_3=wakefield::r_sample(x=c("Yes","No"),prob = c(0.6,02)),
  Q25_4=wakefield::r_sample(x=c("Yes","No"),prob = c(0.6,02)),
  Q25_5=wakefield::r_sample(x=c("Yes","No"),prob = c(0.6,02)),
  Q25_6=wakefield::r_sample(x=c("Yes","No"),prob = c(0.6,02)),
  Q25_7=wakefield::r_sample(x=c("Yes","No"),prob = c(0.6,02)),
  Q25_8=wakefield::r_sample(x=c("Yes","No"),prob = c(0.6,02)),
  Q26=wakefield::r_sample(x=c("Mainly crop production","Mainly livestock production",
                   "mix of crop and livestock production"),prob = c(0.9,0.2,0.3)),
  Q27=wakefield::r_sample(x=c("Production of annual field crops (cereals, oilseeds, protein crops, root crops, tobacco, cotton, etc.)",
                   "Production of vegetables, mushrooms, flowers, ornamental plants, etc.",
                   "Production of grapes for wine",
                   "Production of fruits",
                   "Production of other perennial crops (cacao, coffee, etc.)",
                   "Mixed cropping (no real prevalence of a specific crop activity)"),
               prob = c(0.5,0.2,0.6,0.2,0.4,0.6)),
  Q28=wakefield::r_sample(x=c("Raising ruminant livestock for meat (cattle, sheep, goats, etc. )",
                   "Raising non-ruminant livestock for meat (pigs, poultry, etc.)",
                   "Production of eggs",
                   "Production of milk",
                   "Mixed livestock (no real prevalence of a specific livestock activity)"),
               prob = c(0.9,0.4,03,0.2,0.1)),
  Q29=wakefield::r_sample(x=c("Producing primarily for sale (selling 90% or more)",
                   "Producing mainly for sale, with some own consumption (selling more than 50% and up to 90%)",
                   "Producing mainly for own consumption, with some sales (selling more than 10% and up to 50%)",
                   "Producing primarily for own consumption (selling 10% or less)"),
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
    Q10="What is the legal status of the Holder?",
    Q11="What is the legal status of the holding?",
    Q12a="First name",
    Q12b="Surname",
    Q12c="Sex of the Holder/Co-holders",
    Q12d="PERSONAL ID of the Holder",
    Q13a="Region",
    Q13b="District",
    Q13c="Village",
    Q14="What is the legal name of the holding?",
    Q16="Holding Serial Number",
    Q17="Address of the holding",
    Q17a="Address (street)",
    Q17b="Village, town",
    Q17c="Region",
    Q17d="District",
    Q17e="Main telephone number of the holding",
    Q18="What is the main location type of the address reported above?",
    Q19a="Latitude",
    Q19b="Longitude",
    Q20="What is the official identification number of the holding in the national business register?",
    Q21a="What are the other administrative identification numbers of the holding?-Livestock",
    Q21b="What are the other administrative identification numbers of the holding?-Wine production",
    Q21c="What are the other administrative identification numbers of the holding?-Organic production",
    Q21c="What are the other administrative identification numbers of the holding?-Other (specify",
    Q22="What is the identification number of the holding from the last agricultural census? (Can be prefilled)",
    #*****************************************************************************
    #* PART 1.2: AGRICULTURAL ACTIVITY         ***********************************
    #* ***************************************************************************
    Q23="Does the holding record its agricultural activity or finances on registers or logbooks?",
    Q24_1="Information registered: Area cultivated/harvested",
    Q24_2="Information registered: Crop production",
    Q24_3="Information registered: Livestock production",
    Q24_4="Information registered: Unit prices, amounts sold and total sales by product",
    Q24_5="Information registered: Input quantities used (seeds, fertilizers, plant protection products, etc.)",
    Q24_6="Information registered: Detailed quantities and prices of inputs bought",
    Q24_7="Information registered: Workers’ time",
    Q24_8="Information registered: Workers’ payment",
    Q24_9="Information registered: Workers’ payment",
    Q25_1="tenure of the agricultural land used: Owned with written documentation (includes a title deed, a will, a purchase agreement, etc.)",
    Q25_2="tenure of the agricultural land used: Owned without written documentation",
    Q25_3="tenure of the agricultural land used: Rented-in, leased or sharecropped with written agreement",
    Q25_4="tenure of the agricultural land used: Rented-in, leased or sharecropped without written agreement",
    Q25_5="tenure of the agricultural land used: State or communal land used with written agreement (certified use rights)",
    Q25_6="tenure of the agricultural land used: State or communal land used without written agreement (uncertified use rights)",
    Q25_7="tenure of the agricultural land used: Occupied/squatted without any permission",
    Q25_8="tenure of the agricultural land used: No agricultural land",
    Q26="From an economic perspective, what is the holding’s main agricultural focus for the reference period?",
    Q27="From an economic perspective, what is the main cropping activity?",
    Q28="From an economic perspective, what is the main livestock activity?",
    Q29="What is the main intended destination of your agricultural production?"
  ) %>%
  # skip controle

  #*****************************************************************************
  #* PART 1.2: IDENTIFICATION OF THE HOLDING ***********************************
  #* ***************************************************************************
  #If Q10 = 1 or 2 GO TO Q12, otherwise GO TO Q14
  dplyr::mutate(Q12a=Q12a %>% labelled::recode_if(!(Q10=="Legal person"),NA),
         Q12b=Q12b %>% labelled::recode_if(!(Q10=="Legal person"),NA),
         Q12c=Q12c %>% labelled::recode_if(!(Q10=="Legal person"),NA),
         Q12d=Q12d %>% labelled::recode_if(!(Q10=="Legal person"),NA),
         Q13a=Q13a %>% labelled::recode_if(!(Q10=="Legal person"),NA),
         Q13b=Q13b %>% labelled::recode_if(!(Q10=="Legal person"),NA),
         Q13c=Q13c %>% labelled::recode_if(!(Q10=="Legal person"),NA),
         Q14=Q14 %>% labelled::recode_if((Q10=="Legal person"),NA),
         #If Q17 = 1 → Go to Q18
         Q17=Q17 %>% labelled::recode_if(!(Q10=="Legal person"),"Different from the address of the Holder"),
         Q17a=Q17a %>% labelled::recode_if((Q17=="Same as the address of the Holder"),NA),
         Q17b=Q17b %>% labelled::recode_if((Q17=="Same as the address of the Holder"),NA),
         Q17c=Q17c %>% labelled::recode_if((Q17=="Same as the address of the Holder"),NA),
         Q17d=Q17d %>% labelled::recode_if((Q17=="Same as the address of the Holder"),NA),
         Q17e=Q17e %>% labelled::recode_if((Q17=="Same as the address of the Holder"),NA),
         #replace by NA if same region, same district, same village and correct Q17
         check= (Q17c==Q13a) & (Q17b==Q13c) & (Q17d==Q13b) & Q17=="Different from the address of the Holder",
         Q17a=Q17a %>% labelled::recode_if(check==TRUE,NA),
         Q17b=Q17b %>% labelled::recode_if(check==TRUE,NA),
         Q17c=Q17c %>% labelled::recode_if(check==TRUE,NA),
         Q17d=Q17d %>% labelled::recode_if(check==TRUE,NA),
         Q17e=Q17e %>% labelled::recode_if(check==TRUE,NA),
         Q17=Q17 %>% labelled::recode_if(check==TRUE,"Same as the address of the Holder")
  ) %>%
  #generate enumeration area
  dplyr::mutate(Region=ifelse(!is.na(Q13a),Q13a,Q17c)) %>%
  dplyr::group_by(Region) %>% dplyr::mutate(Q15=paste0(stringr::str_sub(Region,7),
                                         base::sample(100:120,1))) %>% dplyr::ungroup() %>%
  dplyr::select(-check,-Region) %>%
  # perturbate by including some missing information
  wakefield::r_na(prob = 0.0005)

}
