
#' @title Generate a network object.
#'
#' @description Generation of a network object.
#'
#' @param netFileName TODO
#' @param subNetFileName TODO
#'
#'
#' @return a \code{data.frame} containing all simulated sub-network.
#'
#' @examples
#'
#' ## TODO
#'
#' @importFrom hash hash has.key
#' @author Pascal Belleau, Astrid Deschenes
#' @keywords internal


network <- function(netFileName, subNetFileName){
    netFile <- read.table(netFileName, header = FALSE, stringsAsFactors = FALSE,fill = TRUE, na.strings = "")
    subNet <- read.table(subNetFileName, header = FALSE, stringsAsFactors = FALSE,fill = TRUE, na.strings = "")
    netAll <- hash()
    netLinks <- data.frame(n1 = rep(NA, length(netFile[!(is.na(netFile[,3])),1])),
                           n2 = rep(NA, length(netFile[!(is.na(netFile[,3])),1])))


    l <- 1
    for( i in seq_len(length(netFile[,1]))){
        if(!(is.na(netFile[i,3]))){
            if(!(has.key(key = netFile[i,1], hash = netAll))){
                netAll[[netFile[i,1]]] <- hash()
            }
            netAll[[netFile[i,1]]][[netFile[i,3]]] <- l

            if(!(has.key(key = netFile[i,3], hash = netAll))){
                netAll[[netFile[i,3]]] <- hash()
            }
            netAll[[netFile[i,3]]][[netFile[i,1]]] <- l

            l <- l+1
        }

    }
    nodesAll <- keys(netAll)

    nodesOrder <- order(nodesAll)
    nodesAll <- nodesAll[nodesOrder]

    network <- list(netAll=netAll, nodesAll=nodesAll)
}
