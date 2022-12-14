#' Replace structural missing value by a given level
#'
#' @param data a dataframe.
#' @param var factor column containing structural missing
#' @param replacement an integer value of the replacement value
#' @param label label of the structural missing value
#'
#' @return a dataframe
#' @importFrom dplyr %>% pull mutate
#' @importFrom rlang := as_string ensym

#' @export
#'
#' @examples
#'
replace_missing = function(data,
                           var,
                           replacement = 999,
                           label = "structural Missing") {

  stopifnot(is.data.frame(data))
  stopifnot(rlang::as_string(ensym(var)) %in% names(data))
  stopifnot(is.integer(replacement))
  stopifnot(is.character(label))

  if ("haven_labelled" %in% (data %>% dplyr::pull({{var}}) %>% class())) {
    data %>% dplyr::mutate({{var}} := tidyr::replace_na({{var}}, replacement)) %>%
      labelled::add_value_labels({{var}} := c("structural Missing" = replacement))
  } else {
    data %>% dplyr::mutate({{var}} := tidyr::replace_na(labelled::to_labelled({{var}}), replacement)) %>%
      labelled::add_value_labels({{var}} := c("structural Missing" = replacement))
  }
}
