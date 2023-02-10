#' Create folders containing working files of the workshop
#'
#' @param overwrite if existing folders should be overwritten
#' @param agriswksp an agriswksp object
#'
#' @return
#' @importFrom purrr walk
#' @export
#'
#' @examples
create_wksp_folders <- function(agriswksp, overwrite = FALSE) {

  agrisMsg("INITIAL SETUP","creating folders")
  setwd(agriswksp@workingDir)
  # stopifnot(inherits(agrisvy,"agrisvy"))

  folders_path <- c(file.path(agriswksp@workingDir,"_R"),
                    agriswksp@varClassDir,
                    agriswksp@preProcScriptDir,
                    agriswksp@preprocDataDir,
                    agriswksp@riskAnalysisDir,
                    agriswksp@SdcObjDir,
                    agriswksp@anoScriptDir,
                    agriswksp@fileDesDir,
                    agriswksp@anoreportDir,
                    agriswksp@anoDataDir
  )

  purrr::walk(folders_path, create_folder, overwrite = overwrite)
}

#' Generate needed file sfor wkrshop practice
#'
#' @param agrisvy
#' @param overwrite
#'
#' @return
#' @export
#'
#' @examples
generateLabFiles=function(agrisvy,overwrite){
  obj_name=deparse(substitute(agrisvy))
  create_wksp_folders(agrisvy,overwrite)
  generate_varclas(agrisvy)
  generate_preproc_r(agrisvy,type = "wksp_proc",obj_name=obj_name)
  generate_preproc_r(agrisvy,type = "wksp_risk",obj_name=obj_name)
  generate_preproc_r(agrisvy,type = "wksp_ano",obj_name=obj_name)
  generate_report_template(agrisvy,type="wksp")



  saveRDS(agrisvy,file.path(agrisvy@workingDir,"_R",paste0(deparse(substitute(agrisvy)),".rds")))

  set_up=file.path(agrisvy@workingDir,"_R","_setup.R")

  fileConn <- file(set_up)

  writeLines(
    c(glue::glue("setwd(\"{agrisvy@workingDir}\")"),"",
      glue::glue("cwd=\"{agrisvy@workingDir}\""),"",
      glue::glue("data_path=\"{agrisvy@path}\""),"",
      glue::glue("{deparse(substitute(agrisvy))} <-readRDS(\"_R/{deparse(substitute(agrisvy))}.rds\")")),
    fileConn
  )
  close(fileConn)

  renderPreprocFiles(agrisvy,type="proc",format="docx")
  renderPreprocFiles(agrisvy,type="risk",format="docx")
  renderPreprocFiles(agrisvy,type="ano",format="docx")
  genAllFileDes(agrisvy)

}



#' Render html, pdf or MS word file or the pre-processing files
#'
#' @param agrisvy an agriswkp object
#' @param format format of the output (`html`, `pdf`or `docx`)
#' @param type
#'
#' @return
#' @importFrom purrr walk
#' @export
#'
#' @examples
renderPreprocFiles=function(agrisvy,type,format){

  stopifnot(format %in% c("html","pdf","docx"))
  stopifnot(type %in% c("proc","risk","ano"))

if(type=="proc"){

  qmd_file=file.path(preProcScriptDir(agrisvy),
                     list.files(preProcScriptDir(agrisvy),pattern = ".qmd$"))
}

  if(type=="risk"){

    qmd_file=file.path(agrisvy@riskAnalysisDir,
                       list.files(agrisvy@riskAnalysisDir,pattern = ".qmd$"))
  }

  if(type=="ano"){

    qmd_file=file.path(anoScriptDir(agrisvy),
                       list.files(anoScriptDir(agrisvy),pattern = ".qmd$"))
  }

  purrr::walk(qmd_file,function(x){
    quarto::quarto_render(x,output_format = format)
  })

}
