
#' Summarise missing value due to anonymization
#'
#' @param obj_i initial SDC object
#' @param obj_f final SDC object
#' @param var categorical quasi-identifier
#' @param df
#'
#' @return
#' @export
#'
#' @examples
display_miss=function(obj_i,obj_f,var,df){

  # Check if there is a grouping of not
  # If there is a grouping filter out that variable

  if(df==TRUE){
  raw_df=obj_i
  ano_df=obj_f
  }

  if(df==FALSE){
    raw_df=obj_i@origData
    ano_df=extractManipData(obj_f)
  }

  levels_before=level=unique(raw_df[[var]])
  levels_after=level=unique(ano_df[[var]])

  res=sum(!(sapply(as.character(levels_after)[!is.na(as.character(levels_after))], function(x){ x %in% as.character(levels_before)})))

  df=data.frame(cbind(v1=to_character(raw_df[[var]]),v2=to_character(ano_df[[var]])))

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
#' @param vars variable to include
#' @param alpha dependancy threshold (for p-value)
#'
#' @return
#' @export
#'
#' @examples
cat_relation=function(obj_i,obj_f,alpha,vars=NULL,df){


    if(df==TRUE){
      ano_df=obj_f
      ori_df=obj_i
      names_key_vars=vars
    }

    if(df==FALSE){
      ano_df=extractManipData(obj_f)
      ori_df=obj_i@origData
      key_vars_ind=obj_i@keyVars
      names_key_vars=names(obj_i@origData)[key_vars_ind]
    }


    if(!is.null(vars)){
      names_key_vars=vars
    }

    m=matrix(nrow = length(names_key_vars),ncol = length(names_key_vars))
    dimnames(m)=list(names_key_vars,names_key_vars)
    for (vi in names_key_vars) {

      for (vj in names_key_vars) {

        res1=tryCatch({
          as.numeric(chisq.test(ori_df[[vi]],ori_df[[vj]])$p.value)
        },
        error = function(cond) {
          NA
        })
        res2=tryCatch({
          as.numeric(chisq.test(ano_df[[vi]],ano_df[[vj]])$p.value)    },
          error = function(cond) {
            NA
          })

        # res1=as.numeric(chisq.test(ori_df[[vi]],ori_df[[vj]])$p.value)
        # res2=as.numeric(chisq.test(ano_df[[vi]],ano_df[[vj]])$p.value)
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
#' @param df
#' @param vars
#'
#' @return
#' @export
#'
#' @examples
miss_values_sum <- function(obj_i,obj_f,df,vars){



  if(df==TRUE){
    df_i=obj_i
    df_f=obj_f
    names_key_vars=vars
  }

  if(df==FALSE){
    df_i=obj_i@origData
    df_f=extractManipData(obj_f)
    key_vars_ind=obj_i@keyVars
    names_key_vars=names(obj_i@origData)[key_vars_ind]
  }

  missing_ano=sapply(names_key_vars,function(x){sum(!is.na(df_i[,x]) & is.na(df_f[,x]))})
  non_missing_i=sapply(names_key_vars,function(x){sum(!is.na(df_i[,x]))})

  missing_ano_pct=round(missing_ano/non_missing_i*100,2)


  df=data.frame(cbind(variable=names_key_vars,
                      missing_ano=missing_ano,
                      missing_ano_pct=missing_ano_pct
  )
  )

  names(df)=c("Categorical Quasi-identifiers","# local supression","% local suppression")
  rownames(df)=NULL

  df %>%
    kableExtra::kbl(align='rc',caption="Information loss: missing value count (variable level)",booktabs = T) %>%
    kableExtra::kable_classic_2(full_width = F) %>%
    kableExtra::column_spec(1, width = "12em", bold = T, border_right = T) %>%
    kableExtra::column_spec(2, width = "10em") %>%
    kableExtra::column_spec(3, width = "10em") %>%
    kableExtra::kable_styling(latex_options = "HOLD_position")
}



#' Specify sample design for information loss asessment
#'
#' @param x
#' @param y
#' @param df
#'
#' @return
#' @export
#'
#' @examples
surveyDesign <- function(x,y,df=FALSE,...){

if(df==FALSE){

  res=list(design_i=survey::svydesign(...,data = extractManipData(x)),
       design_f=survey::svydesign(...,data=extractManipData(y)),
       obj_i=obj_i,obj_f=obj_f)
}

if (df==TRUE) {
  res=list(design_i=survey::svydesign(...,data = x),
       design_f=survey::svydesign(...,data=y))
}

  return(res)
}


#' CHeck in the anonymized indicator is within the initial confidence interval
#'
#' @param design survey design specified with the function \code{"agrisvyr::surveyDesign"}
#' @param indicator parameter to estimate take \code{"mean"}, \code{"total"} or \code{"median"}
#' @param numvars
#'
#' @return
#' @export
#'
#' @examples
confint_check_num <- function(design,indicator="mean",numvars=NULL){


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

  # if(is.null(var_index)){
  #   names_num_vars=names(design$obj_i@manipNumVars)
  # } else {
  #   names_num_vars=names(design$obj_i@manipNumVars)[var_index]
  #
  # }


  df=lapply(numvars, function(x){confint_check(x,design,indicator)})

  df=do.call("rbind",df)
  row.names(df)=NULL


  df %>% kableExtra::kbl(align='cc',
                         caption=paste0(indicator," value of numerical quasi-identifier before and after anonymization")) %>%
    kableExtra::kable_paper(full_width = F) %>%
    kableExtra::column_spec(1, width = "10em", bold = T, border_right = T) %>%
    kableExtra::column_spec(6, color = ifelse(df$is.ano.in.ci=="YES", "green", "red"),bold = T) %>%
    kableExtra::kable_styling(latex_options = "HOLD_position")
}



# confint_check_num_by <- function(design,indicator="mean",numvars=NULL,by=NULL){
#
# byvar=as.formula(paste0("~",paste(LETTERS[1:3],collapse = "+")))
# var=as.formula(paste0("~",num_var))
#
#   confint_check <- function(num_var,design,indicator){
#
#
#     if(indicator=="mean") {
#
#       svyfunc_i=rlang::parse_expr("svyby(var,byvar,design$design_i),na=TRUE, svymean)")
#       svyfunc_f=rlang::parse_expr("svyby(var,byvar,design$design_f),na=TRUE, svymean)")
#
#     }
#
#
#     if(indicator=="total") {
#       svyfunc_i=rlang::parse_expr("svyby(var,byvar,design$design_i),na=TRUE, svytotal)")
#       svyfunc_f=rlang::parse_expr("svyby(var,byvar,design$design_f),na=TRUE, svytotal)")
#     }
#
#     est_i=eval(svyfunc_i)[[length(by)+1]]
#     lb_ci=round(confint(eval(svyfunc_i))[1],2)
#     ub_ci=round(confint(eval(svyfunc_i))[2],2)
#     est_f=eval(svyfunc_f)[[length(by)+1]]
#
#
#     for (i in seq_along(by)){
#
#     }
#
#     var1_labels=gsub("\\.(.*)$","",row.names(as.data.frame(eval(svyfunc_i))))
#     var2_labels=gsub("^(.*)\\.","",row.names(as.data.frame(eval(svyfunc_i))))
#     var2_labels=gsub("/\.([^\.]+)\./","",row.names(as.data.frame(eval(svyfunc_i))))
#
#
#     df=data.frame(
#       variable=num_var,
#       raw_value=round(est_i,2),
#       conf_int=paste("[",lb_ci,",",ub_ci,"]"),
#       ano_value=round(est_f,2),
#       pct_change=paste0(round((est_f-est_i)/est_i*100,2),"%"),
#       is_ano_in_cf= ifelse(est_f>=lb_ci & est_f<= ub_ci,"YES","NO")
#     )
#
#     names(df)=c("quasi.identifier",
#                 "orig.value",
#                 "conf.int",
#                 "ano.value",
#                 "pct.change",
#                 "is.ano.in.ci")
#     return(df)
#
#   }
#
#   # if(is.null(var_index)){
#   #   names_num_vars=names(design$obj_i@manipNumVars)
#   # } else {
#   #   names_num_vars=names(design$obj_i@manipNumVars)[var_index]
#   #
#   # }
#
#
#   df=lapply(numvars, function(x){confint_check(x,design,indicator)})
#
#   df=do.call("rbind",df)
#   row.names(df)=NULL
#
#
#   df %>% kableExtra::kbl(align='cc',
#                          caption=paste0(indicator," value of numerical quasi-identifier before and after anonymization")) %>%
#     kableExtra::kable_paper(full_width = F) %>%
#     kableExtra::column_spec(1, width = "10em", bold = T, border_right = T) %>%
#     kableExtra::column_spec(6, color = ifelse(df$is.ano.in.ci=="YES", "green", "red"),bold = T) %>%
#     kableExtra::kable_styling(latex_options = "HOLD_position")
# }


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

  .anova_test_comp <- function(num_var,cat_var,design,test="wilcoxon",alpha=alpha){

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






#' Compare proportion before and after anonymization
#'
#' @param variable
#' @param design
#'
#' @return
#' @export
#'
#' @examples
compare_ind_cat_mean <- function(variable,design){

  var=as.formula(paste0("~","factor(",variable,")"))

  res_i=as.data.frame(svymean(var,design$design_i,na.rm=TRUE))
  res_f=as.data.frame(svymean(var,design$design_f,na.rm=TRUE))
  val_labels=gsub(paste0("factor","\\(",variable,"\\)"),"",row.names(res_i))

  ci=as.data.frame(confint(svymean(var,design$design_i,na.rm=TRUE)))
  ci_f=as.data.frame(confint(svymean(var,design$design_f,na.rm=TRUE)))

  names(ci)=c("il","iu")
  names(ci_f)=c("fl","fu")

  ci_df=ci %>% bind_cols(ci_f) %>%
    dplyr::mutate(cond=(fl>iu) | (il>fu)) %>%
    dplyr::mutate( inter_l=ifelse(cond==TRUE,NULL,ifelse(il>fl,il,fl)),
                   inter_u=ifelse(cond==TRUE,NULL,ifelse(iu<fu,iu,fu))) %>%
    dplyr::mutate(UC=ifelse(cond==TRUE,0,((inter_u-inter_l)/(iu-fl)+(inter_u-inter_l)/(fu-fl))/2*100))

  cv=as.data.frame(cv(svymean(var,design$design_i,na.rm=TRUE)))
  names(cv)="cv"
des=design$design_i
des$prob=rep(1,times=nrow(des$cluster))
sample_freq=as.vector(svytable(var, des))

  df=data.frame(
    cbind(
      sample_n=sample_freq,
      ori_value=round(res_i$mean,3)*100,
      CV_in_pct=round(cv$cv*100,2),
      conf_int=paste("[",round(ci$il,3)*100,",",round(ci$iu,3)*100,"]"),
      ano_value=round(res_f$mean,3)*100,
      pct_pt_change=round((res_f$mean-res_i$mean)*100,2),
      is_ano_in_cf= ifelse(res_f$mean>=ci$il & res_f$mean<= ci$iu,"Ano. in inside conf. Int.",
                           "Ano. val. outside conf. int."),
      CI_coverage=paste0(ci_df$UC,"%")
    )
  ) %>% dplyr::mutate(!!sym(variable):=val_labels,.before="sample_n") %>%
    dplyr::arrange(desc(abs(as.numeric(pct_pt_change))))

  row.names(df)=NULL


  df %>% kableExtra::kbl(align='cc',
                         caption=paste0("COMPARISON OF PROPORTION: ",variable)) %>%
    kableExtra::kable_classic_2(full_width = F) %>%
    kableExtra::column_spec(1, width = "10em", bold = T, border_right = T) %>%
    kableExtra::column_spec(8, color = ifelse(df$is_ano_in_cf=="Ano. in inside conf. Int.", "green", "red"),bold = T) %>%
    kableExtra::kable_styling(latex_options = "HOLD_position")

}





#' Compare total of categorical variable values before and after anonymization
#'
#' @param variable
#' @param design
#'
#' @return
#' @export
#'
#' @examples
compare_ind_cat_tot <- function(variable,design){

  var=as.formula(paste0("~","factor(",variable,")"))

  res_i=as.data.frame(svytotal(var,design$design_i,na.rm=TRUE))
  res_f=as.data.frame(svytotal(var,design$design_f,na.rm=TRUE))
  val_labels=gsub(paste0("factor","\\(",variable,"\\)"),"",row.names(res_i))

  ci=as.data.frame(confint(svytotal(var,design$design_i,na.rm=TRUE)))
  ci_f=as.data.frame(confint(svymean(var,design$design_f,na.rm=TRUE)))

  names(ci)=c("il","iu")
  names(ci_f)=c("fl","fu")

  ci_df=ci %>% bind_cols(ci_f) %>%
    dplyr::mutate(cond=(fl>iu) | (il>fu)) %>%
    dplyr::mutate( inter_l=ifelse(cond==TRUE,NULL,ifelse(il>fl,il,fl)),
                   inter_u=ifelse(cond==TRUE,NULL,ifelse(iu<fu,iu,fu))) %>%
    dplyr::mutate(UC=ifelse(cond==TRUE,0,((inter_u-inter_l)/(iu-fl)+(inter_u-inter_l)/(fu-fl))/2*100))

  cv=as.data.frame(cv(svytotal(var,design$design_i,na.rm=TRUE)))
  names(cv)="cv"

  des=design$design_i
  des$prob=rep(1,times=nrow(des$cluster))
  sample_freq=as.vector(svytable(var, des))

  df=data.frame(
    cbind(
      sample_n=sample_freq,
      ori_value=round(res_i$total),
      CV_in_pct=round(cv$cv*100,2),
      conf_int=paste("[",round(ci$il),",",round(ci$iu),"]"),
      ano_value=round(res_f$total),
      pct_change=round((res_f$total-res_i$total)/res_i$total*100,2),
      is_ano_in_cf= ifelse(res_f$total>=ci$il & res_f$total<= ci$iu,
                           "Ano. in inside conf. Int.",
                           "Ano. val. outside conf. int."),
      CI_coverage=paste0(ci_df$UC,"%")
    )
  ) %>% dplyr::mutate(!!sym(variable):=val_labels,.before="sample_n") %>%
    dplyr::arrange(desc(abs(as.numeric(pct_change))))

  row.names(df)=NULL


  df %>% kableExtra::kbl(align='cc',
                         caption=paste0("COMPARISON OF TOTAL: ",variable)) %>%
    kableExtra::kable_classic_2(full_width = F) %>%
    kableExtra::column_spec(1, width = "10em", bold = T, border_right = T) %>%
    kableExtra::column_spec(8, color = ifelse(df$is_ano_in_cf=="Ano. in inside conf. Int.", "green", "red"),
                            bold = T) %>%
    kableExtra::kable_styling(latex_options = "HOLD_position")

}






#' Compare crosstabulation proportion before and after anonymization
#'
#' @param variable1
#' @param variable2
#' @param design
#'
#' @return
#' @export
#'
#' @examples
compare_ind_cats <- function(variable1,variable2,design){

  var=as.formula(paste0("~","interaction(",variable1,",",variable2,")"))

  res_i=as.data.frame(svymean(var,design$design_i,na.rm = TRUE))
  res_f=as.data.frame(svymean(var,design$design_f,na.rm = TRUE))

  val_labels=gsub(paste0("interaction","\\(",variable1,", ",variable2,"\\)"),"",row.names(res_i))
  var1_labels=gsub("\\.(.*)$","",val_labels)
  var2_labels=gsub("^(.*)\\.","",val_labels)
  ci=as.data.frame(confint(svymean(var,design$design_i,na.rm = TRUE)))
  ci_f=as.data.frame(confint(svymean(var,design$design_f,na.rm=TRUE)))

  names(ci)=c("il","iu")
  names(ci_f)=c("fl","fu")

  ci_df=ci %>% bind_cols(ci_f) %>%
    dplyr::mutate(cond=(fl>iu) | (il>fu)) %>%
    dplyr::mutate( inter_l=ifelse(cond==TRUE,NULL,ifelse(il>fl,il,fl)),
                   inter_u=ifelse(cond==TRUE,NULL,ifelse(iu<fu,iu,fu))) %>%
    dplyr::mutate(UC=ifelse(cond==TRUE,0,((inter_u-inter_l)/(iu-fl)+(inter_u-inter_l)/(fu-fl))/2*100))
  cv=as.data.frame(cv(svymean(var,design$design_i,na.rm=TRUE)))
  names(cv)="cv"

  des=design$design_i
  des$prob=rep(1,times=nrow(des$cluster))
  sample_freq=as.vector(svytable(var, des))


  df=data.frame(
    cbind(
      sample_n=sample_freq,
      ori_value=round(res_i$mean,3)*100,
      CV_in_pct=round(cv$cv*100,2),
      conf_int=paste("[",round(ci$il,3)*100,",",round(ci$iu,3)*100,"]"),
      ano_value=round(res_f$mean,3)*100,
      pct_pt_change=round((res_f$mean-res_i$mean)*100,2),
      is_ano_in_cf= ifelse(res_f$mean>=ci$il & res_f$mean<= ci$iu,"Ano. in inside conf. Int.",
                           "Ano. val. outside conf. int."),
      CI_coverage=paste0(ci_df$UC,"%")

    )
  ) %>% dplyr::mutate(!!sym(variable1):=var1_labels,.before="sample_n")%>%
    dplyr::mutate(!!sym(variable2):=var2_labels,.before="sample_n") %>%
    dplyr::arrange(desc(abs(as.numeric(pct_pt_change))))

  row.names(df)=NULL


  df %>% kableExtra::kbl(align='cc',
                         caption=paste0("COMPARISON OF INTERACTION: ",variable1, " and ",variable2)) %>%
    kableExtra::kable_classic_2(full_width = F) %>%
    kableExtra::column_spec(1, width = "10em", bold = T, border_right = T) %>%
    kableExtra::column_spec(9, color = ifelse(df$is_ano_in_cf=="Ano. in inside conf. Int.", "green", "red"),
                            bold = T) %>%
    kableExtra::kable_styling(latex_options = "HOLD_position")
}




#' Compare crosstabulation total before and after anonymization
#'
#' @param variable1
#' @param variable2
#' @param design
#'
#' @return
#' @export
#'
#' @examples
compare_ind_cats_tot <- function(variable1,variable2,design){

  var=as.formula(paste0("~","interaction(",variable1,",",variable2,")"))

  res_i=as.data.frame(svytotal(var,design$design_i,na.rm = TRUE))
  res_f=as.data.frame(svytotal(var,design$design_f,na.rm = TRUE))

  val_labels=gsub(paste0("interaction","\\(",variable1,", ",variable2,"\\)"),"",row.names(res_i))
  var1_labels=gsub("\\.(.*)$","",val_labels)
  var2_labels=gsub("^(.*)\\.","",val_labels)
  ci=as.data.frame(confint(svytotal(var,design$design_i,na.rm = TRUE)))

  ci_f=as.data.frame(confint(svymean(var,design$design_f,na.rm=TRUE)))

  names(ci)=c("il","iu")
  names(ci_f)=c("fl","fu")

  ci_df=ci %>% bind_cols(ci_f) %>%
    dplyr::mutate(cond=(fl>iu) | (il>fu)) %>%
    dplyr::mutate( inter_l=ifelse(cond==TRUE,NULL,ifelse(il>fl,il,fl)),
                   inter_u=ifelse(cond==TRUE,NULL,ifelse(iu<fu,iu,fu))) %>%
    dplyr::mutate(UC=ifelse(cond==TRUE,0,((inter_u-inter_l)/(iu-fl)+(inter_u-inter_l)/(fu-fl))/2*100))

  cv=as.data.frame(cv(svytotal(var,design$design_i,na.rm=TRUE)))
  names(cv)="cv"

  des=design$design_i
  des$prob=rep(1,times=nrow(des$cluster))
  sample_freq=as.vector(svytable(var, des))


  df=data.frame(
    cbind(
      sample_n=sample_freq,
      ori_value=round(res_i$total),
      CV_in_pct=round(cv$cv*100,2),
      conf_int=paste("[",round(ci$il),",",round(ci$iu),"]"),
      ano_value=round(res_f$total),
      pct_change=round((res_f$total-res_i$total)/res_i$total*100),
      is_ano_in_cf= ifelse(res_f$total>=ci$il & res_f$total<= ci$iu,"Ano. in inside conf. Int.",
                           "Ano. val. outside conf. int."),
      CI_coverage=paste0(ci_df$UC,"%")

    )
  ) %>% dplyr::mutate(!!sym(variable1):=var1_labels,.before="sample_n")%>%
    dplyr::mutate(!!sym(variable2):=var2_labels,.before="sample_n")%>%
    dplyr::arrange(desc(abs(as.numeric(pct_change))))

  row.names(df)=NULL


  df %>% kableExtra::kbl(align='cc',
                         caption=paste0("COMPARISON OF INTERACTION: ",variable1, " and ",variable2)) %>%
    kableExtra::kable_classic_2(full_width = F) %>%
    kableExtra::column_spec(1, width = "10em", bold = T, border_right = T) %>%
    kableExtra::column_spec(9, color = ifelse(df$is_ano_in_cf=="Ano. in inside conf. Int.",
                                              "green", "red"),bold = T) %>%
    kableExtra::kable_styling(latex_options = "HOLD_position")
}
