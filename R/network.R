
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
#' @export



network <- function(netFileName, fileType="sif", subNetFileName=NULL){

    network <- NULL
    if(toupper(fileType) == "SIF"){
        if(file.exists(netFileName) &&
           (is.null(subNetFileName) ||
            file.exists(subNetFileName))){
            network <- networkFromSif(netFileName, subNetFileName)
        }else{
            strErr <- paste0("Problem with network filename ", netFileName)
            if(!(is.null(subNetFileName))) {
                strErr <- paste0(strErr, " or ", subNetFileName)
            }
            stop(strErr)
        }
    } else {
        strErr <- paste0("File type ", FileType, " is not supported")
    }
    class(network) <- "network"
    return(network)
}
