#' Print a initial info message during preprocessing
#'
#' @param msg message to be printed
#'
#' @return
#' @importFrom  crayon green silver bold
#' @importFrom cli cli_h1
#' @export
#'
#' @examples
preprocMessage=function(msg){

  cli::cli_h1(paste(crayon::green(crayon::bold("PREPROCESSING: ")),crayon::silver(msg)))
}


#' Print initial info message during anonymization
#'
#' @param msg message to be printed
#'
#' @return
#' @importFrom  crayon cyan silver bold
#' @importFrom cli cli_h1
#' @export
#'
#' @examples
sdcMessage=function(msg){

  cli::cli_h1(paste(crayon::cyan(crayon::bold("ANONYMIZATION: ")),crayon::silver(msg)))
}


#' Print initial message in finalization of anonymization
#'
#' @param msg message to be printed
#'
#' @return
#' @importFrom  crayon blue silver bold
#' @importFrom cli cli_h3
#' @export
#'
#' @examples
finMessage=function(msg){

  cli::cli_h3(paste(crayon::blue(crayon::bold("FINALIZATION OF ANONYMIZATION: ")),crayon::silver(msg)))
}


