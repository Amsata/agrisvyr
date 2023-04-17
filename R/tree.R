
#' @importFrom  plyr rbind.fill
#' @importFrom  data.tree as.Node
#' @importFrom dplyr %>%
#' @importFrom haven read_dta
#' @importFrom cli cli_progress_along

showTree <- function(agrisvy,folders_path=NULL){

  if(is.null(folders_path)){
    path=DataPath(agrisvy)
    folder_name=unlist(strsplit(DataPath(agrisvy), "/"))
    folder_name=folder_name[length(folder_name)]
    path1 <- list.files(path,pattern = agrisvy@type,recursive = TRUE)
    path2=file.path(folder_name,path1)
    path3=file.path(DataPath(agrisvy),path1)


  } else {
    path=file.path(agrisvy@workingDir,folders_path)
    folder_name=unlist(strsplit(agrisvy@workingDir, "/"))
    folder_name=folder_name[length(folder_name)]
    path1 <- list.files(path,pattern = agrisvy@type,recursive = TRUE)
    path2=file.path(folder_name,folders_path,path1)
    path3=file.path(agrisvy@workingDir,folders_path,path1)
  }

  x <- lapply(strsplit(path2, "/"), function(z) as.data.frame(t(z)))
  x <- rbind.fill(x)
  x$pathString <- apply(x, 1, function(x) paste(trimws(na.omit(x)), collapse="/"))

  x$`#Cases` <- as.character(sapply(cli_progress_along(path1,"Computing ncols"), function(i){
    read_dta(path3[i]) %>% nrow()})
  )

  x$`#Variables` <- as.character(sapply(cli_progress_along(path1,"Computing ncols"), function(i){
    read_dta(path3[i]) %>% ncol()})
  )


  if(nrow(x)!=0){
    tree=data.tree::as.Node(x)
    print(tree,"#Cases","#Variables")
  }else{
    print(NULL)
  }
}


#' @importFrom  plyr rbind.fill
#' @importFrom  data.tree as.Node
#' @importFrom dplyr filter

showFolderTree <- function(agrisvy,folders_path){

  folder_name=unlist(strsplit(agrisvy@workingDir, "/"))
  folder_name=folder_name[length(folder_name)]
  path=file.path(agrisvy@workingDir,folders_path)

  data_file=list.files(path,pattern = agrisvy@type,recursive = TRUE)

  path_file <- list.files(path,recursive = TRUE)

  if(length(data_file)==length(path_file) & length(path_file)!=0){
    #print data tree
    showTree(agrisvy,folders_path)
  } else {
    x <- lapply(strsplit(path_file, "/"), function(z) as.data.frame(t(z)))

    if(length(x)!=0){
      x <- rbind.fill(x)
      x$pathString <- apply(x, 1, function(x) paste(trimws(na.omit(x)), collapse="/"))
      tree=data.tree::as.Node(x)
      print(tree)
    }else{
      print(NULL)
    }
  }



}
