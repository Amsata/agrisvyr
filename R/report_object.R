

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
#' @import dplyr
#' @export
#'
#' @examples


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


#' Title
#'
#' @param intialObj
#' @param finalObj
#' @param unit
#' @param hierarchy
#' @param global
#' @param individual
#' @param suda
#' @param hierarchical
#' @param childName
#'
#' @return
#' @importFrom methods new
#' @export
#'
#' @examples
saveReprtObj=function(intialObj=NULL,
                                finalObj=NULL,
                                unit=NULL,
                                hierarchy=NULL,
                                global=TRUE,
                                individual=TRUE,
                                suda=FALSE,
                                hierarchical=FALSE,
                                childName=NULL) {

            obj <- new("sdcReportObj")

if (!is.null(intialObj)) obj@intialObj <- intialObj
if (!is.null(finalObj)) obj@finalObj <- finalObj
if (!is.null(unit)) obj@unit <- unit
if (!is.null(hierarchy)) obj@hierarchy <- hierarchy
if (!is.null(global)) obj@global <- global
if (!is.null(individual)) obj@individual <- individual
if (!is.null(individual)) obj@individual <- individual
if (!is.null(suda)) obj@suda <- suda


saveRDS(obj,glue("05_Anonymization report/{childName}.rds"))

file <- file.path("05_Anonymization report",glue::glue("child_{childName}.rmd"))
file.create(file)

fileConn<-file(file)
writeLines(c(glue::glue(paste(readLines(system.file("txt_template","sdc_report_child.txt",package = "agrisvyr")),
                              collapse = "\n"),.open = "{{",.close = "}}"))
           ,
           fileConn)
close(fileConn)

}


#' Title
#'
#' @param sdc an object of type SdcMicroObj
#' @param df if the function return a dataframe or styled datafram for rmarkdown display
#' @param title title of the table in case df=FALSE
#'
#' @return
#' @import sdcMicro
#' @import knitr
#' @import kableExtra
#' @importFrom dplyr  %>%
#' @export
#'
#' @examples

GlobRiskTab=function(sdc,df=FALSE,title="") {
  res=rbind(
    data.frame(
      `Risk type`="Global risk",
      Value=paste0(round(100 * sdc@risk$global$risk, 2), "%")
    ),
    data.frame(`Risk type`="Expect. num. re.", Value=paste0(round( sdc@risk$global$risk_ER, 2)))
  )%>%
    #formating the table
    kbl(align='rc',caption=title,booktabs = T) %>%
    kable_classic_2(full_width = F) %>%
    column_spec(1, width = "12em", bold = T, border_right = T) %>%
    column_spec(2, width = "8em") %>%
    # kable_styling(position = "float_right") %>%
    kable_styling(latex_options = "HOLD_position")

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


setGeneric("RenderGlobalRisk", function(obj,time="initial") standardGeneric("RenderGlobalRisk"))

#' render global risk summary in Rmarkdown
#'
#' @param sdcReportObj
#'
#' @return
#' @import sdcMicro
#' @import knitr
#' @import  kableExtra
#' @importFrom dplyr  %>%
#' @export
#'
#' @examples
setMethod("RenderGlobalRisk",signature = "sdcReportObj",
          definition = function(obj,time="initial"){

      if (time=="initial") sdcObj=obj@intialObj
      if (time=="final") sdcObj=obj@finalObj

      title=glue::glue("{time} global disclosure risk of {obj@unit}")

      GlobRiskTab(sdcObj,df=FALSE,title = title)

          })

#' data frame containing k-anonymity information
#'
#' @param sdcObj an sdcMicro object
#' @param df logical. retunrn a dataframe or a styled dataframe
#' @param levels k-anonymity levels
#' @param title title of the table in case df=FALSE
#'
#' @return
#' @import sdcMicro
#' @import knitr
#' @import kableExtra
#' @importFrom dplyr %>%
#' @export
#'
#' @examples

KanoTab=function(sdcObj,df=FALSE,levels=c(2,3,5),title="") {
  KanoRow=function(k) {

    data.frame(`Level of k anonymity`=k,
               fk=paste0(sum((sdcObj@risk$individual[, "fk"]) < k),
                         " (", 100 * round(sum((sdcObj@risk$individual[, "fk"]) < k)/nrow(sdcObj@origData), 4), "%)"))
  }

  res=do.call("rbind",lapply(levels,KanoRow)) %>% kbl(caption = title) %>%
    kable_classic_2(full_width = F) %>%
    kable_styling(latex_options = "HOLD_position")

  if(df){
    res=as.data.frame(do.call("rbind",lapply(levels,KanoRow)))
  }
  return(res)
}


setGeneric("renderKanoTab",function(obj,levels=c(2,3,5),time="initial") standardGeneric("renderKanoTab"))

#' render k-anonymity table in rmarkdown
#'
#' @param sdcReportObj
#'
#' @return
#' a dtaframe
#'
#' @import knitr
#' @import kableExtra
#' @importFrom  dplyr %>%
#' @export
#'
#' @examples
setMethod("renderKanoTab",signature = "sdcReportObj",
          definition = function(obj,levels=c(2,3,5),time="initial") {

            if (time=="initial") sdcObj=obj@intialObj
            if (time=="final") sdcObj=obj@finalObj

            title=glue::glue("{time} k-anonymity of {obj@unit}")

            KanoTab(sdcObj,df=FALSE,levels = levels,title = title)
          })


#' a  dataframe containing the summary of individual risk
#'
#' @param sdc an sdcMicro Object
#' @param df retunrn a simple dataframe or a styled dataframe
#' @param title title of the styled dataframe
#'
#' @return
#' a dataframe
#' @import knitr
#' @import kableExtra
#' @importFrom dplyr %>%
#' @importFrom stats median quantile
#' @export
#' @examples
RiskIndSUmmary=function(sdc,df=FALSE,title=""){

  res=rbind(
    data.frame(Indicator="Mean",Value=paste0(round(mean(sdc@risk$individual[, "risk"]),4)*100,"%")),
    data.frame(Indicator="Min",Value=paste0(round(min(sdc@risk$individual[, "risk"]),4),"%")),
    data.frame(Indicator="1st quartile",Value=paste0(round(quantile(sdc@risk$individual[, "risk"],0.25),4)*100,"%")),
    data.frame(Indicator="Median",Value=paste0(round(median(sdc@risk$individual[, "risk"]),4)*100,"%")),
    data.frame(Indicator="3rd quartile",Value=paste0(round(quantile(sdc@risk$individual[, "risk"],0.75)*100,4),"%")),
    data.frame(Indicator="Max",Value=paste0(round(max(sdc@risk$individual[, "risk"]),4)*100,"%"))

  ) %>% kbl(caption=title,booktabs=TRUE) %>%
    kable_classic_2(full_width = F) %>%
    kable_styling(latex_options = "HOLD_position")

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


setGeneric("renderRiskIndSUmmary",function(obj,time="initial") standardGeneric("renderRiskIndSUmmary"))

#' render summary of individual risk
#'
#' @param sdcReportObj
#'
#' @return
#' @import knitr
#' @import kableExtra
#' @importFrom dplyr %>%
#' @export
#'
#' @examples
setMethod("renderRiskIndSUmmary",signature = "sdcReportObj",
          definition = function(obj,time = "initial") {

            if (time == "initial") sdcObj = obj@intialObj
            if (time == "final") sdcObj = obj@finalObj

            title=glue::glue(" Summary of the {time} individual risk of {obj@unit}")

            RiskIndSUmmary(sdcObj,df = FALSE,title = title)
          })



#' summary of hierarchical risk
#'
#' @param sdc an sdcMicro Object
#' @param dfl return a simple dataframe or a styled dataframe
#' @param title title of the styled dataframe
#'
#' @return
#' a data frame
#' @import knitr
#' @import  kableExtra
#' @importFrom dplyr %>%
#' @import sdcMicro
#' @export
#'
#' @examples
HierRiskSummary=function(sdc,dfl=FALSE,title=""){

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

  ) %>% kbl(caption=title,booktabs=TRUE) %>%
    kable_classic_2(full_width = F) %>%
    kable_styling(latex_options = "HOLD_position")

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

setGeneric("renderHierRiskSummary",function(obj,time="initial") standardGeneric("renderHierRiskSummary"))

#' render hierarchical risk summary
#'
#' @param sdcReportObj
#'
#' @return
#' a dataframe
#'
#' @import knitr
#' @import kableExtra
#' @importFrom dplyr %>%
#' @export
#'
#' @examples
setMethod("renderHierRiskSummary",signature = "sdcReportObj",
          definition = function(obj,time="initial"){

            if (time == "initial") sdcObj = obj@intialObj
            if (time == "final") sdcObj = obj@finalObj

            title=glue::glue(" Summary of the {time} hierarchical risk of {obj@hierarchy} from {obj@unit}")

            HierRiskSummary(sdcObj,dfl = FALSE,title = title)
          })
