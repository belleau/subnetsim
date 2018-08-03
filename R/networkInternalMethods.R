#' @title Generate a network object.
#'
#' @description Generation of a list that contain all the
#' global network information extracted from a sif file. When
#' a subnetwork file is present, the information from
#' the subnetwork is added to the list.
#'
#' @param netFileName a \code{character} string for the name of
#' file in SIF format that contains the information of the
#' global network.
#' @param subNetFileName a \code{character} string for the name of
#' file in SIF format that contains the information of the
#' subnetwork. This parameter is not mandatory. The information for
#' the subnetwork is only extracted when a filename is given.
#'
#'
#' @return a \code{list} containing the following components:
#'            \itemize{
#'            \item{ \code{netAll} a \code{hash} containing all
#'            the information from the global network.}
#'            \item{ \code{nodesAll} a vector of string representing
#'            all the nodes present in the global network.}
#'            \item{ \code{nodesSubNet} a vector of string representing
#'            all the nodes present in the subnetwork to be tested or
#'            \code{NULL} when information not known.}
#'            }
#'
#' @examples
#'
#' networkFile <- system.file("extdata", "demo_network.sif",
#' package="subnetsim")
#'
#' ## Create a network without the subnetwork information
#' demo_network <- networkFromSif(netFileName = networkFile)
#'
#' subnetworkFile <- system.file("extdata", "demo_subnetwork.sif",
#' package="subnetsim")
#'
#' ## Create a network with the subnetwork information
#' demo_network_with_sub <- networkFromSif(netFileName = networkFile,
#'     subNetFileName = subnetworkFile)
#'
#' @importFrom hash hash has.key
#' @importFrom utils read.table
#' @author Pascal Belleau, Astrid Deschenes
#' @keywords internal
networkFromSif <- function(netFileName, subNetFileName = NULL) {

    netFile <- read.table(netFileName, header = FALSE,
                        stringsAsFactors = FALSE, fill = TRUE, na.strings = "")

    netAll <- hash()
    netLinks <- data.frame(n1 = rep(NA,
                                length(netFile[!(is.na(netFile[,3])), 1])),
                           n2 = rep(NA,
                                length(netFile[!(is.na(netFile[,3])),1])))


    l <- 1
    for ( i in seq_len(length(netFile[,1]))) {
        if (!(is.na(netFile[i,3]))){
            if (!(has.key(key = netFile[i,1], hash = netAll))) {
                netAll[[netFile[i,1]]] <- hash()
            }
            netAll[[netFile[i,1]]][[netFile[i,3]]] <- l

            if (!(has.key(key = netFile[i,3], hash = netAll))) {
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
        subNet <- read.table(subNetFileName, header = FALSE,
                        stringsAsFactors = FALSE,fill = TRUE, na.strings = "")
        nodesAllSub <- unique(c(subNet[, 1], subNet[!is.na(subNet[,3]), 3]))
        nodesSubNet <- intersect(nodesAllSub, nodesAll)

        nodesSubNet <- nodesSubNet[order(nodesSubNet)]
        if(length(nodesSubNet) != length(nodesAllSub)) {
            warning("Subnetwork  not include in the network, keep only the intersection")
        }
    }

    network <- list(netAll=netAll, nodesAll=nodesAll, nodesSubNet=nodesSubNet)

    return(network)
}
