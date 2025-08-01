setClassUnion("dataframeOrNULL", c("data.frame", "NULL"))
setClassUnion("numericOrNULL", c("numeric", "NULL"))
setClassUnion("characterOrNULL", c("character", "NULL"))
setClassUnion("logicalOrNULL", c("logical", "NULL"))
setClassUnion("matrixOrNULL", c("matrix", "NULL"))
setClassUnion("listOrNULL", c("list", "NULL"))
setClassUnion("factorOrNULL", c("factor", "NULL"))
setClassUnion("logicalOrNULL", c("logical", "NULL"))
setClassUnion("sdcmicroOrNULL", c("sdcMicroObj","NULL"))
setClassUnion("sdcReportObjOrNULL", c("NULL"))


#' Create an object containing information for sdc report
#'
#' @slot intialObj sdcmicroOrNULL.
#' @slot finalObj sdcmicroOrNULL.
#' @slot unit characterOrNULL.
#' @slot hierarchy characterOrNULL.
#' @slot globalRisk logicalOrNULL.
#' @slot indRisk logicalOrNULL.
#' @slot sudaRisk logicalOrNULL.
#' @slot hierRisk logicalOrNULL.
#' @return
#' @import sdcMicro
#' @importClassesFrom sdcMicro  sdcMicroObj
#' @importFrom  dplyr %>% distinct
#' @export
#'
#' @examples

setClass("sdcReportObj",
         representation = representation(
           intialObj="sdcmicroOrNULL",
           finalObj="sdcmicroOrNULL",
           unit="characterOrNULL",
           hierarchy="characterOrNULL",
           global="logicalOrNULL",
           individual="logicalOrNULL",
           suda="logicalOrNULL",
           hierarchical="logicalOrNULL"
         ),
         prototype = prototype(
           intialObj=NULL,
           finalObj=NULL,
           unit=NULL,
           hierarchy=NULL,
           global=TRUE,
           individual=TRUE,
           suda=FALSE,
           hierarchical=FALSE
         )         )

setIs("sdcReportObj", "sdcReportObjOrNULL")


#' Append the anonymization report by integrating automatically the results
#' from the current anonymization exercise
#'
#'This function help to automatically integrate the results from the current
#'anonymization to the anonymization report generated during the anonymization
#'setup. This results include the disclosure risk analysis (global risk,
#'individual risk, SUDA (if enabled),hierarchical risk (if enabled)) and the
#'comparison of these risk measures before and after anonymization.
#'
#' @param intialObj an \code{sdcMicro} object in which no anonymization is
#' applied yet. One can save the \code{sdcMicro} object just after its creation
#' in a object named for example \code{sdc_obj_initial} and use it later in
#' \code{intialObj=sdc_obj_initial}
#' @param finalObj the \code{sdcMicro} object where all anonymization measures
#' has been applied. One can save the last version of the \code{sdcMicro} object
#' created for the anonymization as \code{sdc_obj_final} and use it later for in
#' \code{finalObj=sdc_obj_final}.
#' @param unit statistical unit of the microdata. This is used to make the analysis
#' in the anonymization report dynamic. If the microdata is a household data, we should
#' considered household as \code{unit}. If the microdata is a household member data, we can specify
#' \code{"member"} as \code{unit} or \code{"individual"} as \code{unit}
#'
#' @param hierarchy If \code{hierarchical=TRUE}, \code{hierarchy} contain the
#' higher hierarchical unit that should be used in the analysis of the
#' hierarchical  risk. In the anonymization of household member microdata,one should
#' specify \code{hierarchy="household"} if the household ID is specify in the argument
#' \code{hhid} of the \code{sdcMicro} object and that \code{hierarchical=TRUE} in the
#' anonymization report object.
#' @param global a \code{logical} specifying if \code{TRUE}, that the global risk
#' analysis and comparison before and after anonymization should be included in the
#' report.
#' @param individual a \code{logical} specifying, if \code{individual=TRUE}, that the
#' individual probabilistic risk and its comparison before and after anonymyzation
#' should be included in the anonymization report.
#' @param suda \code{logical} specifying if \code{suda=TRUE}, that the suda score
#' and its comparison before and after anonymization should be included in the report.
#' One has to make sure that the condition to compute SUDA2 score is satisfied, particularly
#' that the number of categorical quasi-identifiers is higher than 2.
#' @param hierarchical a \code{logical} specifying if (hierarchical=TRUE), that
#' hierarchical risk  and its comparison before and after anonymization should be
#' included in the report. One has to make sure that the hierarchical risk is computed
#' when creating the \code{sdcMicro} object, that is, the argument \code{hhid} is
#' specified.
#' @param childName the object name used to save component of the resulting child report
#' in the folder containing the anonymization report.
#'
#' @return
#' @importFrom methods new
#' @export
#'
#' @examples
saveReprtObj <- function(agrisvy,
                      intialObj=NULL,
                      finalObj    =NULL,
                      unit        =NULL,
                      hierarchy   =NULL,
                      global      =TRUE,
                      individual  =TRUE,
                      suda        =FALSE,
                      hierarchical=FALSE,
                      childName   =NULL,
                      inputdata_path=NULL) {

            obj <- new("sdcReportObj")

if (!is.null(intialObj)) obj@intialObj   <- intialObj
if (!is.null(finalObj)) obj@finalObj     <- finalObj
if (!is.null(unit)) obj@unit             <- unit
if (!is.null(hierarchy)) obj@hierarchy   <- hierarchy
if (!is.null(global)) obj@global         <- global
if (!is.null(individual)) obj@individual <- individual
if (!is.null(hierarchical)) obj@hierarchical <- hierarchical
if (!is.null(suda)) obj@suda             <- suda

#extract file information


anodata_path=gsub(agrisvy@preprocDataDir,agrisvy@anoDataDir,gsub("_proc","_ano",inputdata_path))
read_function=readDataFunc(agrisvy)

df_name=tail(unlist(strsplit(inputdata_path,split = "/")),1)
#Put the full path to avoir error with the workshop object when running quarto

# Controling hierarchical input
if(is.null(obj@intialObj@hhId)) obj@hierarchical <- FALSE


#-------------------------------------------------------------------------------
#---------------------------------SDC report -----------------------------------
#-------------------------------------------------------------------------------
saveRDS(obj,file.path(anoreportDir(agrisvy),glue::glue("child_{childName}.rds")))

file_sdc <- file.path(anoreportDir(agrisvy),glue::glue("child_{childName}.rmd"))
if (file.exists(file_sdc)==FALSE) {
  file.create(file_sdc)
  fileConn_sdc<-file(file_sdc)
  if(agrisvy@language=="en") template_rpt_child=system.file("txt_template","sdc_report_child_en.txt",package = "agrisvyr")
  if(agrisvy@language=="fr") template_rpt_child=system.file("txt_template","sdc_report_child_fr.txt",package = "agrisvyr")
  if(agrisvy@language=="es") template_rpt_child=system.file("txt_template","sdc_report_child_es.txt",package = "agrisvyr")
  if(agrisvy@language=="pt") template_rpt_child=system.file("txt_template","sdc_report_child_pt.txt",package = "agrisvyr")

  #TODO: create the french version
  writeLines(c(glue::glue(paste(readLines(template_rpt_child),collapse = "\n"),.open = "{{",.close = "}}")),fileConn_sdc)
  close(fileConn_sdc)

  rpt_file_sdc=file.path(anoreportDir(agrisvy),"sdc_report.rmd")
  # TODO: update by removing the if, else if it works

  sdc_rpt=readLines(rpt_file_sdc)
  appended=grep(glue::glue("child_{childName}.rmd"),sdc_rpt)
  if (length(appended)==0) {
    new_sdc_rpt=c(sdc_rpt, "",glue::glue("```{r,child='child_{{childName}}.rmd'}",.open = "{{",.close = "}}"),"```")
  }

  fileConn_sdc<-file(rpt_file_sdc)
  writeLines(new_sdc_rpt,fileConn_sdc)
  close(fileConn_sdc)
}

# End sdc report------------------------------------------------------

################################################################################
#------------------------------Info loss report---------------------------------
################################################################################

saveRDS(obj,file.path(infoLossReport(agrisvy),glue::glue("child_{childName}.rds")))

file_infol <- file.path(infoLossReport(agrisvy),glue::glue("child_{childName}.rmd"))
if (file.exists(file_infol)==FALSE) {
  file.create(file_infol)
  fileConn_infol<-file(file_infol)

  if(agrisvy@language=="en") template_rpt_child_infol=system.file("txt_template","info_loss_report_child.txt",package = "agrisvyr")
  if(agrisvy@language=="fr") template_rpt_child_infol=system.file("txt_template","info_loss_report_child_fr.txt",package = "agrisvyr")
  if(agrisvy@language=="es") template_rpt_child_infol=system.file("txt_template","info_loss_report_child_es.txt",package = "agrisvyr")
  if(agrisvy@language=="pt") template_rpt_child_infol=system.file("txt_template","info_loss_report_child_pt.txt",package = "agrisvyr")

  #TODO: create the french version
  writeLines(c(glue::glue(paste(readLines(template_rpt_child_infol), collapse = "\n"),.open = "{{",.close = "}}")),fileConn_infol)
  close(fileConn_infol)

  rpt_file_infol=file.path(infoLossReport(agrisvy),"information_loss_report.rmd")

  # Update information loss report
  info_loss_rpt=readLines(rpt_file_infol)
  appended_infol=grep(glue::glue("child_{childName}.rmd"),info_loss_rpt)

  if (length(appended_infol)==0) {
    new_sdc_rpt=c(info_loss_rpt, "",glue::glue("```{r,child='child_{{childName}}.rmd'}",.open = "{{",.close = "}}"),"```")
  }

  fileConn_infol<-file(rpt_file_infol)
  writeLines(new_infol_rpt,fileConn_infol)
  close(fileConn_infol)
}
}



#' @import sdcMicro
#' @import knitr
#' @importFrom   kableExtra kbl kable_classic_2 kable_styling column_spec
#' @importFrom dplyr  %>%


GlobRiskTab=function(sdc,df=FALSE,time,obj) {
# TODO: ameliorate
  title=paste0(time," global disclosure risk")

  res=rbind(
    data.frame(
      `Risk type`="Global risk",
      Value=paste0(round(100 * sdc@risk$global$risk, 2), "%")
    ),
    data.frame(`Risk type`="Expect. num. re.", Value=paste0(round( sdc@risk$global$risk_ER, 2)))
  )%>%
    #formating the table
    kableExtra::kbl(align='rc',caption=title,booktabs = T) %>%
    kableExtra::kable_classic_2(full_width = F) %>%
    kableExtra::column_spec(1, width = "12em", bold = T, border_right = T) %>%
    kableExtra::column_spec(2, width = "8em") %>%
    # kable_styling(position = "float_right") %>%
    kableExtra::kable_styling(latex_options = "HOLD_position")

  if(df){
    res=rbind(
      data.frame(
        `Risk type`="Global risk",
        Value=paste0(round(100 * sdc@risk$global$risk, 2), "%")
      ),
      data.frame(`Risk type`="Expect. num. re.", Value=paste0(round( sdc@risk$global$risk_ER, 2)))
    )
  }

  return(res)
}

#' Render global risk summary in Rmarkdown ducument
#'
#' @param sdcReportObj
#'
#' @return
#' @import sdcMicro
#' @import knitr
#' @importFrom   kableExtra kbl kable_classic_2 kable_styling
#' @importFrom dplyr  %>%
#' @export
#'
#' @examples
RenderGlobalRisk <- function(obj,time="initial"){
  RenderGlobalRiskX(obj,time="initial")
}
setGeneric("RenderGlobalRiskX", function(obj,time="initial") standardGeneric("RenderGlobalRiskX"))

setMethod("RenderGlobalRiskX",signature = "sdcReportObj",
          definition = function(obj,time="initial"){

      if (time=="initial") sdcObj=obj@intialObj
      if (time=="final") sdcObj=obj@finalObj

      GlobRiskTab(sdcObj,df=FALSE,time,obj)

          })


#' @import sdcMicro
#' @import knitr
#' @importFrom   kableExtra kbl kable_classic_2 kable_styling
#' @importFrom dplyr %>%
#' @export

KanoTab=function(sdcObj,df=FALSE,levels=c(2,3,5),time,obj) {

  title=paste0(time," k-anonymity")

  KanoRow=function(k) {

    data.frame(`Level of k anonymity`=k,
               fk=paste0(sum((sdcObj@risk$individual[, "fk"]) < k),
                         " (", 100 * round(sum((sdcObj@risk$individual[, "fk"]) < k)/nrow(sdcObj@origData), 4), "%)"))
  }

  res=do.call("rbind",lapply(levels,KanoRow)) %>%
    kableExtra::kbl(caption = title) %>%
    kableExtra::kable_classic_2(full_width = F) %>%
    kableExtra::kable_styling(latex_options = "HOLD_position")

  if(df){
    res=as.data.frame(do.call("rbind",lapply(levels,KanoRow)))
  }
  return(res)
}

#' render k-anonymity table in rmarkdown
#'
#' @param sdcReportObj
#'
#' @return
#' a dtaframe
#'
#' @import knitr
#' @importFrom   kableExtra kbl kable_classic_2 kable_styling
#' @importFrom  dplyr %>%
#' @export
#'
#' @examples
renderKanoTab <- function(obj,levels=c(2,3,5),time="initial"){
  renderKanoTabX(obj,levels=c(2,3,5),time="initial")
}

setGeneric("renderKanoTabX",function(obj,levels=c(2,3,5),time="initial") standardGeneric("renderKanoTabX"))

setMethod("renderKanoTabX",signature = "sdcReportObj",
          definition = function(obj,levels=c(2,3,5),time="initial") {

            if (time=="initial") sdcObj=obj@intialObj
            if (time=="final") sdcObj=obj@finalObj

            KanoTab(sdcObj,df=FALSE,levels = levels,time,obj)
          })


#' @import knitr
#' @importFrom   kableExtra kbl kable_classic_2 kable_styling
#' @importFrom dplyr %>%
#' @importFrom stats median quantile
#' @export

RiskIndSUmmary=function(sdc,df=FALSE,time,obj){
  title=paste0("Summary of the", time, "individual risk")


  res=rbind(
    data.frame(Indicator="Mean",Value=paste0(round(mean(sdc@risk$individual[, "risk"]),4)*100,"%")),
    data.frame(Indicator="Min",Value=paste0(round(min(sdc@risk$individual[, "risk"]),4),"%")),
    data.frame(Indicator="1st quartile",Value=paste0(round(quantile(sdc@risk$individual[, "risk"],0.25),4)*100,"%")),
    data.frame(Indicator="Median",Value=paste0(round(median(sdc@risk$individual[, "risk"]),4)*100,"%")),
    data.frame(Indicator="3rd quartile",Value=paste0(round(quantile(sdc@risk$individual[, "risk"],0.75)*100,4),"%")),
    data.frame(Indicator="Max",Value=paste0(round(max(sdc@risk$individual[, "risk"]),4)*100,"%"))

  ) %>% kableExtra::kbl(caption=title,booktabs=TRUE) %>%
    kableExtra::kable_classic_2(full_width = F) %>%
    kableExtra::kable_styling(latex_options = "HOLD_position")

  if(df){

    res=rbind(
      data.frame(Indicator="Mean",Value=paste0(round(mean(sdc@risk$individual[, "risk"]),4)*100,"%")),
      data.frame(Indicator="Min",Value=paste0(round(min(sdc@risk$individual[, "risk"]),4),"%")),
      data.frame(Indicator="1st quartile",Value=paste0(round(quantile(sdc@risk$individual[, "risk"],0.25),4)*100,"%")),
      data.frame(Indicator="Median",Value=paste0(round(median(sdc@risk$individual[, "risk"]),4)*100,"%")),
      data.frame(Indicator="3rd quartile",Value=paste0(round(quantile(sdc@risk$individual[, "risk"],0.75)*100,4),"%")),
      data.frame(Indicator="Max",Value=paste0(round(max(sdc@risk$individual[, "risk"]),4)*100,"%"))

    )
  }

  return(res)
}

#' Render summary of individual risk
#'
#' @param sdcReportObj
#'
#' @return
#' @import knitr
#' @importFrom   kableExtra kbl kable_classic_2 kable_styling
#' @importFrom dplyr %>%
#' @export
#'
#' @examples
renderRiskIndSUmmary <- function(obj,time="initial"){
  renderRiskIndSUmmaryX(obj,time="initial")
}

setGeneric("renderRiskIndSUmmaryX",function(obj,time="initial") standardGeneric("renderRiskIndSUmmaryX"))


setMethod("renderRiskIndSUmmaryX",signature = "sdcReportObj",
          definition = function(obj,time = "initial") {

            if (time == "initial") sdcObj = obj@intialObj
            if (time == "final") sdcObj = obj@finalObj

            RiskIndSUmmary(sdcObj,df = FALSE,time,obj)
          })


#' @importFrom   kableExtra kbl kable_classic_2 kable_styling
#' @importFrom dplyr %>% distinct
#' @import sdcMicro
#' @export

HierRiskSummary=function(sdc,dfl=FALSE,time,obj){

  title=paste0("Summary of the",time, "hierarchical risk")
  df=cbind(data.frame(sdc@origData[,sdc@hhId]),
           data.frame(sdc@risk$individual[,"hier_risk"]))
  names(df)=c("hier_id","risk")
  df=df %>% dplyr::distinct(.data$hier_id,.data$risk)
  res=rbind(
    data.frame(Indicator="Mean",Value=paste0(round(mean(df[, "risk"]),4)*100,"%")),
    data.frame(Indicator="Min",Value=paste0(round(min(df[, "risk"]),4),"%")),
    data.frame(Indicator="1st quartile",Value=paste0(round(quantile(df[, "risk"],0.25),4)*100,"%")),
    data.frame(Indicator="Median",Value=paste0(round(median(df[, "risk"]),4)*100,"%")),
    data.frame(Indicator="3rd quartile",Value=paste0(round(quantile(df[, "risk"],0.75)*100,4),"%")),
    data.frame(Indicator="Max",Value=paste0(round(max(df[, "risk"]),4)*100,"%"))

  ) %>% kableExtra::kbl(caption=title,booktabs=TRUE) %>%
    kableExtra::kable_classic_2(full_width = F) %>%
    kableExtra::kable_styling(latex_options = "HOLD_position")

  if(dfl){

    res=rbind(
      data.frame(Indicator="Mean",Value=paste0(round(mean(df[, "risk"]),4)*100,"%")),
      data.frame(Indicator="Min",Value=paste0(round(min(df[, "risk"]),4),"%")),
      data.frame(Indicator="1st quartile",Value=paste0(round(quantile(df[, "risk"],0.25),4)*100,"%")),
      data.frame(Indicator="Median",Value=paste0(round(median(df[, "risk"]),4)*100,"%")),
      data.frame(Indicator="3rd quartile",Value=paste0(round(quantile(df[, "risk"],0.75)*100,4),"%")),
      data.frame(Indicator="Max",Value=paste0(round(max(df[, "risk"]),4)*100,"%"))

    )
  }
  return(res)
}


#' render hierarchical risk summary
#'
#' @param sdcReportObj
#'
#' @return
#' a dataframe
#'
#' @import knitr
#' @importFrom   kableExtra kbl kable_classic_2 kable_styling
#' @importFrom dplyr %>%
#' @export
#'
#' @examples
renderHierRiskSummary <- function(obj,time="initial") {
  renderHierRiskSummaryX(obj,time="initial")
}

setGeneric("renderHierRiskSummaryX",function(obj,time="initial") standardGeneric("renderHierRiskSummaryX"))


setMethod("renderHierRiskSummaryX",signature = "sdcReportObj",
          definition = function(obj,time="initial"){

            if (time == "initial") sdcObj = obj@intialObj
            if (time == "final") sdcObj = obj@finalObj

            HierRiskSummary(sdcObj,dfl = FALSE,time, obj)
          })


#' Title
#'
#' @param agrisvy
#'
#' @return
#' @export
#'
#' @examples
genDandDIVariables=function(agrisvy,overwrite=TRUE) {
  f=agrisvyr:::.createExcelInfos(agrisvy)
  numberOfWOrkbook=length(unique(f$files_infos$workbook))
  res_list=list()
  for (i in seq_along(1:nrow(f$files_infos))) {
    df=readxl::read_excel(path=file.path(agrisvyr:::varClassDir(agrisvy),paste0(f$files_infos$workbook[i],"_VarClas.xlsx")),sheet = ,f$files_infos$file_name[i])
    Dvars=df %>% dplyr::filter(Classification=="D") %>% dplyr::pull(Name) %>% paste(collapse = ";")
    DIVars=df %>% dplyr::filter(Classification=="DI") %>% dplyr::pull(Name) %>% paste(collapse = ";")

    if (numberOfWOrkbook==1) results=data.frame(file=f$files_infos$file_name[i],Dvars=Dvars,DIVars=DIVars)
    if (numberOfWOrkbook>1) results=data.frame(wb=f$files_infos$workbook[i],file=f$files_infos$file_name[i],DIVars=DIVars,Dvars=Dvars)

    res_list[[i]]=results

  }

  df_final=do.call("rbind",res_list)

  if (numberOfWOrkbook==1){
    if(agrisvy@language=="en") names(df_final)=c("File","Variable not useful","Direct identifiers variables")
    if(agrisvy@language=="fr") names(df_final)=c("File","Variable not useful","Direct identifiers variables")
    if(agrisvy@language=="es") names(df_final)=c("File","Variable not useful","Direct identifiers variables")
    if(agrisvy@language=="pt") names(df_final)=c("File","Variable not useful","Direct identifiers variables")
  } else {
    if (numberOfWOrkbook>1){
      if(agrisvy@language=="en") names(df_final)=c("Data folder","File","Variable not useful","Direct identifiers variables")
      if(agrisvy@language=="fr") names(df_final)=c("Dossier de données","Fichier de données","Identifiant direct","Variables pas utiles à diffuser")
      if(agrisvy@language=="es") names(df_final)=c("Data folder","File","Variable not useful","Direct identifiers variables")
      if(agrisvy@language=="pt") names(df_final)=c("Data folder","File","Variable not useful","Direct identifiers variables")
    }
  }

  openxlsx::write.xlsx(df_final,file.path(anoreportDir(agrisvy),"DandDIvariables.xlsx"),overwrite=overwrite)

}


#' Title
#'
#' @param agrisvy
#' @param overwrite
#'
#' @return
#' @export
#'
#' @examples
genAnoVariables=function(agrisvy,overwrite=TRUE) {
  f=agrisvyr:::.createExcelInfos(agrisvy)
  numberOfWOrkbook=length(unique(f$files_infos$workbook))
  res_list=list()
  for (i in seq_along(1:nrow(f$files_infos))) {
    df=readxl::read_excel(path=file.path(agrisvyr:::varClassDir(agrisvy),paste0(f$files_infos$workbook[i],"_VarClas.xlsx")),sheet = ,f$files_infos$file_name[i])
    Dvar.s=df %>% dplyr::filter(Classification=="D") %>% dplyr::pull(Name) %>% paste(collapse = ";")
    DIVars=df %>% dplyr::filter(Classification=="DI") %>% dplyr::pull(Name) %>% paste(collapse = ";")

    if (numberOfWOrkbook==1) results=data.frame(file=f$files_infos$file_name[i],Dvars=Dvars,DIVars=DIVars)
    if (numberOfWOrkbook>1) results=data.frame(wb=f$files_infos$workbook[i],file=f$files_infos$file_name[i],DIVars=DIVars,Dvars=Dvars)

    res_list[[i]]=results

  }

  df_final=do.call("rbind",res_list)

  if (numberOfWOrkbook==1){
    if(agrisvy@language=="en") names(df_final)=c("File","Variable not useful","Direct identifiers variables")
    if(agrisvy@language=="fr") names(df_final)=c("File","Variable not useful","Direct identifiers variables")
    if(agrisvy@language=="es") names(df_final)=c("File","Variable not useful","Direct identifiers variables")
    if(agrisvy@language=="pt") names(df_final)=c("File","Variable not useful","Direct identifiers variables")
  } else {
    if (numberOfWOrkbook>1){
      if(agrisvy@language=="en") names(df_final)=c("Data folder","File","Variable not useful","Direct identifiers variables")
      if(agrisvy@language=="fr") names(df_final)=c("Dossier de données","Fichier de données","Identifiant direct","Variables pas utiles à diffuser")
      if(agrisvy@language=="es") names(df_final)=c("Data folder","File","Variable not useful","Direct identifiers variables")
      if(agrisvy@language=="pt") names(df_final)=c("Data folder","File","Variable not useful","Direct identifiers variables")
    }
  }

  openxlsx::write.xlsx(df_final,file.path(anoreportDir(agrisvy),"DandDIvariables.xlsx"),overwrite=overwrite)

}
