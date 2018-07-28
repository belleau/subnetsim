#' @title Generate a network object.
#'
#' @description Generation of a network list from sif.
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


networkFromSif <- function(netFileName, subNetFileName=NULL){

    netFile <- read.table(netFileName, header = FALSE, stringsAsFactors = FALSE,fill = TRUE, na.strings = "")

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

    nodesAll <- nodesAll[order(nodesAll)]

    nodesSubNet <- NULL

    if(!(is.null(subNetFileName))){
        subNet <- read.table(subNetFileName, header = FALSE, stringsAsFactors = FALSE,fill = TRUE, na.strings = "")
        nodesAllSub <- unique(c(subNet[,1], subNet[!is.na(subNet[,3]),3]))
        nodesSubNet <- intersect(nodesAllSub, nodesAll)

        nodesSubNet <- nodesSubNet[order(nodesSubNet)]
        if(length(nodesSubNet) != length(nodesAllSub)){
            warning("Subnetwork  not include in the network, keep only the intersection")
        }
    }

    network <- list(netAll=netAll, nodesAll=nodesAll, nodesSubNet=nodesSubNet)

    return(network)
}
