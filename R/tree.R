



#' Print the tree of a directory
#'
#' @param agrisvy
#'
#' @return
#' @import plyr
#' @importFrom  data.tree as.Node
#' @export
#'
#' @examples
showTree <- function(agrisvy){

  path <- list.files(DataPath(agrisvy),pattern = agrisvy@type,recursive = TRUE)

  x <- lapply(strsplit(path, "/"), function(z) as.data.frame(t(z)))
  x <- rbind.fill(x)
  x$pathString <- apply(x, 1, function(x) paste(trimws(na.omit(x)), collapse="/"))
  tree=data.tree::as.Node(x)
  print(tree)
}
