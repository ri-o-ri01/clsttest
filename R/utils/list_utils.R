#'
#'Get list from object's names
#'
#'@param ... :string names which exist as objects
#'
#' @importFrom foreach foreach
#' @importFrom foreach %do%
#' @export
#'
getListFromObjNames<-function(...){
  names = c(...)
  list = foreach(i = 1:length(names)) %do% get(names[i])
  names(list) = names
  return(list)
}


#'
#'Get list from objects
#'
#'@importFrom stats setNames
#'
#'@param ... :objects
#'
#'@export
#'
getListFromObjs <- function(...){
  object_name <- as.character(eval(substitute(alist(...))))
  x <- list(...)
  vnames <- names(x)
  novnames <- !nzchar(vnames)
  if (length(novnames) > 0L){
    vnames[novnames] <- object_name[novnames]
  } else {
    vnames <- object_name
  }
  setNames(x, vnames)
}