
#' @title Generate a network object from a fileType.
#'
#' @description Generation of a network object.
#'
#' @param netFileName TODO
#' @param fileType TODO
#' @param subNetFileName TODO
#'
#'
#' @return a \code{data.frame}
#'
#' @examples
#'
#' ## TODO
#'
#' @importFrom hash hash has.key
#' @author Pascal Belleau, Astrid Deschenes
#' @keywords internal


network <- function(netFileName, fileType="sif", subNetFileName=NULL){

    network <- NULL
    if(toupper(fileType) == "SIF"){
        if(file.exists(netFileName) &&
           (subNetFileName = NULL ||
            file.exists(subNetFileName))){
            network <- networkFromSif(netFileName, subNetFileName)
        }
    }
    class(network) <- "network"
    return(network)
}
