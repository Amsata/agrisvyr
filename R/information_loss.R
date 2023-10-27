
#' Summarise missing value due to anonymization
#'
#' @param obj_i initial SDC object
#' @param obj_f final SDC object
#' @param var categorical quasi-identifier
#'
#' @return
#' @export
#'
#' @examples
display_miss=function(obj_i,obj_f,var){

  # Check if there is a grouping of not
  # If there is a grouping filter out that variable

  ano_df=extractManipData(obj_f)

  levels_before=level=unique(obj_i@origData[[var]])
  levels_after=level=unique(ano_df[[var]])

  res=sum(!(sapply(as.character(levels_after)[!is.na(as.character(levels_after))], function(x){ x %in% as.character(levels_before)})))

  df=data.frame(cbind(v1=as.character(obj_i@origData[[var]]),v2=as.character(ano_df[[var]])))

  df$missing=(!is.na(df$v1) & is.na(df$v2))

  df=df %>% group_by(v1) %>% summarise(n_missing=sum(missing,na.rm = TRUE),
                                       missing_pct=round(sum(missing,na.rm = TRUE)/n()*100,2))

  names(df)[1]=var

  if(res==0) {

    df%>%
      kableExtra::kbl(align='rc',caption=paste0("Information loss: missing value count of value of the variable ", var),booktabs = T) %>%
      kableExtra::kable_classic_2(full_width = F) %>%
      kableExtra::column_spec(1, width = "10em", bold = T, border_right = T) %>%
      kableExtra::column_spec(2, width = "10em") %>%
      kableExtra::column_spec(3, width = "10em") %>%
      kableExtra::kable_styling(latex_options = "HOLD_position")
  }

}



#' Provide a matrix to assess dependancy loss due to anonymiziation
#'
#' @param obj_i initial SDCmicro Object
#' @param obj_f Final sdcMicro object
#' @param alpha dependancy threshold (for p-value)
#' @return
#' @export
#'
#' @examples
cat_relation=function(obj_i,obj_f,alpha){

  ano_df=extractManipData(obj_f)
  ori_df=obj_i@origData

  key_vars_ind=obj_i@keyVars
  names_key_vars=names(obj_i@origData)[key_vars_ind]

  m=matrix(nrow = length(key_vars_ind),ncol = length(key_vars_ind))
  dimnames(m)=list(names_key_vars,names_key_vars)
  for (vi in names_key_vars) {

    for (vj in names_key_vars) {

      res1=as.numeric(chisq.test(ori_df[[vi]],ori_df[[vj]])$p.value)
      res2=as.numeric(chisq.test(ano_df[[vi]],ano_df[[vj]])$p.value)
      res=ifelse(res1<alpha & res2 < alpha, FALSE,ifelse(res1>alpha & res2>alpha,FALSE,TRUE))

      pct_point_change=round((res2*100-res1*100),2)

      i=which(names_key_vars==vi)
      j=which(names_key_vars==vj)

      if(i<j) {
        m[vi,vj]=res
      }else {
        m[vi,vj]=pct_point_change
      }
      if (vi==vj) {
        m[i,j]="____"
      }

    }
  }

  return(as.data.frame(m))

}


#' SUmmarise missing value introduced by anonymization
#'
#' @param obj_i initial sdcMicro object
#' @param obj_f final sdcMicro Object
#'
#' @return
#' @export
#'
#' @examples
miss_values_sum <- function(obj_i,obj_f){

  key_vars_ind=obj_i@keyVars
  names_key_vars=names(obj_i@origData)[key_vars_ind]

  missing_before=sapply(names_key_vars,function(x){sum(is.na(obj_i@origData[,x]))})
  missing_after=sapply(names_key_vars,function(x){sum(is.na(extractManipData(obj_f)[,x]))})
  missing_pct_before=round((missing_before)/nrow(obj_i@origData)*100,2)
  missing_pct_after=round((missing_after)/nrow(obj_i@origData)*100,2)
  missing_increase=round(missing_pct_after-missing_pct_before,2)


  df=data.frame(cbind(variable=names_key_vars,
                      missing_before=missing_pct_before,
                      missing_after=missing_pct_after,
                      missing_increase=missing_increase
  )
  )

  names(df)=c("Categorical Quasi-identifiers","Percentage of missing value (before)","Percentage of missing value(after)","Increase of missing value in pct. points")
  rownames(df)=NULL

  df %>%
    kableExtra::kbl(align='rc',caption="Information loss: missing value count (variable level)",booktabs = T) %>%
    kableExtra::kable_classic_2(full_width = F) %>%
    kableExtra::column_spec(1, width = "12em", bold = T, border_right = T) %>%
    kableExtra::column_spec(2, width = "10em") %>%
    kableExtra::column_spec(3, width = "10em") %>%
    kableExtra::column_spec(4, width = "10em") %>%
    kableExtra::kable_styling(latex_options = "HOLD_position")
}



#' Specify sample design for information loss asessment
#'
#' @param obj_i initial \code{"sdcMicroObj"} object
#' @param obj_f final \code{"sdcMicroObj"} object
#' @param ... argument of the function \code{"svydesing"} from the package \code{"survey"} (exept the \code{"data"} arguement)
#'
#' @return
#' @export
#'
#' @examples
surveyDesign <- function(obj_i,obj_f,...){
  list(design_i=survey::svydesign(...,data = extractManipData(obj_i)),
       design_f=survey::svydesign(...,data=extractManipData(obj_f)),
       obj_i=obj_i,obj_f=obj_f)
}


#' CHeck in the anonymized indicator is within the initial confidence interval
#'
#' @param design survey design specified with the function \code{"agrisvyr::surveyDesign"}
#' @param var_index index of continous variables to consider, il not specify, takes all the continous variables
#' @param indicator parameter to estimate take \code{"mean"}, \code{"total"} or \code{"median"}
#'
#' @return
#' @export
#'
#' @examples
confint_check_num <- function(design,indicator="mean",var_index=NULL){


  confint_check <- function(num_var,design,indicator){


    if(indicator=="mean") {
      svyfunc_i=rlang::parse_expr("svymean(var,design$design_i)")
      svyfunc_f=rlang::parse_expr("svymean(var,design$design_f)")
    }


    if(indicator=="total") {
      svyfunc_i=rlang::parse_expr("svytotal(var,design$design_i)")
      svyfunc_f=rlang::parse_expr("svytotal(var,design$design_f)")
    }

    if(indicator=="median") {
      svyfunc_i=rlang::parse_expr("svyquantile(var,design$design_i,na = TRUE,ci=TRUE, c(.5))")
      svyfunc_f=rlang::parse_expr("svyquantile(var,design$design_f,na = TRUE,ci=TRUE, c(.5))")
    }

    var=as.formula(paste0("~",num_var))
    est_i=as.vector(eval(svyfunc_i))[[1]]
    lb_ci=round(as.vector(confint(eval(svyfunc_i)))[1],2)
    ub_ci=round(as.vector(confint(eval(svyfunc_i)))[2],2)
    est_f=as.vector(eval(svyfunc_f))[[1]]

    df=data.frame(
      variable=num_var,
      raw_value=round(est_i,2),
      conf_int=paste("[",lb_ci,",",ub_ci,"]"),
      ano_value=round(est_f,2),
      pct_change=paste0(round((est_f-est_i)/est_i*100,2),"%"),
      is_ano_in_cf= ifelse(est_f>=lb_ci & est_f<= ub_ci,"YES","NO")
    )

    names(df)=c("quasi.identifier",
                "orig.value",
                "conf.int",
                "ano.value",
                "pct.change",
                "is.ano.in.ci")
    return(df)

  }

  if(is.null(var_index)){
    names_num_vars=names(design$obj_i@manipNumVars)
  } else {
    names_num_vars=names(design$obj_i@manipNumVars)[var_index]

  }


  df=lapply(names_num_vars, function(x){confint_check(x,design,indicator)})

  df=do.call("rbind",df)
  row.names(df)=NULL


  df %>% kableExtra::kbl(align='cc',
                         caption=paste0(indicator," value of numerical quasi-identifier before and after anonymization")) %>%
    kableExtra::kable_paper(full_width = F) %>%
    kableExtra::column_spec(1, width = "10em", bold = T, border_right = T) %>%
    kableExtra::column_spec(6, color = ifelse(df$is.ano.in.ci=="YES", "green", "red"),bold = T) %>%
    kableExtra::kable_styling(latex_options = "HOLD_position")
}



#' Compare one way ANOVA test before and after anonymization
#'
#' @param design survey design specified with the function \code{"agrisvyr::surveyDesign"}
#' @param test type of test. \code{"test=c("wilcoxon", "vanderWaerden", "median","KruskalWallis")"}
#' @param alpha threshold for the rejection of null hypothesis
#'
#' @return
#' @export
#'
#' @examples
anova_test_comp <- function(design,test="wilcoxon",alpha=0.05){

  .anova_test_comp <- function(num_var,cat_var,design,test="wilcoxon",alpha=0.05){

    stopifnot(test %in% c("wilcoxon", "vanderWaerden", "median","KruskalWallis"))

    aov_expr_i=rlang::parse_expr(glue::glue("svyranktest({num_var}~{cat_var}, design = design$design_i, na = TRUE, test=\"{test}\")"))
    aov_expr_f=rlang::parse_expr(glue::glue("svyranktest({num_var}~{cat_var}, design = design$design_f, na = TRUE, test=\"{test}\")"))

    aov_i <- eval(aov_expr_i)
    aov_f <- eval(aov_expr_f)

    res <- data.frame(
      num_var=num_var,
      key_var=cat_var,
      ori.p.value=round(aov_i$p.value,6),
      ano.p.value=round(aov_f$p.value,6),
      pct.pt.change=round(aov_f$p.value-aov_i$p.value,6),
      does.test.change=ifelse((aov_i$p.value<= alpha & aov_f$p.value<= alpha) |
                                (aov_i$p.value>= alpha & aov_f$p.value>= alpha),"NO","YES")
    )
    row.names(res) <- NULL

    return(res)
  }

  num_vars=names(obj_i@manipNumVars)
  key_vars <- names(obj_i@manipKeyVars)


  df <- as.data.frame(
    t(
      mapply(function(x,y) .anova_test_comp(x,y,design,test,alpha), num_vars,key_vars)
    )
  )

  row.names(df) <- NULL

  df %>% kableExtra::kbl(align='cc',
                         caption=paste0("One way weighted ANOVA between continous quasi-identifier and categorical quasi-identifiers")) %>%
    kableExtra::kable_paper(full_width = F) %>%
    kableExtra::column_spec(1, width = "10em", bold = T, border_right = T) %>%
    kableExtra::column_spec(6, color = ifelse(df$does.test.change=="NO", "green", "red"),bold = T) %>%
    kableExtra::kable_styling(latex_options = "HOLD_position")
}
