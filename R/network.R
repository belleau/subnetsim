
#' @title Creates a network object from a SIF file
#'
#' @description Generates a network object using the contain of
#' a SIF file. When a file is specified for the subnetwork to
#' be tested, the information of the subnetwork is extracted
#' from the SIF file and added to the network object.
#'
#' @param netFileName a \code{character} string for the name of
#' file in SIF format that contains the information of the
#' global network.
#' @param fileType The format of the file. At this moment,
#' only "SIF" format is accepted. Default: \code{"sif"}.
#' @param subNetFileName a \code{character} string for the name of
#' file in SIF format that contains the information of the
#' subnetwork. This parameter is not mandatory. The information for
#' the subnetwork is only extracted when a filename is given.
#' Default: \code{NULL}.
#'
#'
#' @return a list marked as an network \code{class} is returned.
#' This list has the following components:
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
#' ## Access SIF file for global network stored in subnetsim package
#' networkFile <- system.file("extdata", "demo_network.sif",
#' package="subnetsim")
#'
#' ## Create a network without the subnetwork information
#' demo_network <- network(netFileName = networkFile)
#'
#' ## Access SIF file for global network stored in subnetsim package
#' subnetworkFile <- system.file("extdata", "demo_subnetwork.sif",
#' package="subnetsim")
#'
#' ## Create a network with the subnetwork information
#' demo_network_with_sub <- network(netFileName = networkFile,
#'     subNetFileName = subnetworkFile)
#'
#' ## The network is of object of class "network"
#' class(demo_network_with_sub)
#'
#' ## The list of nodes present in the subnetwork
#' demo_network_with_sub$nodesSubNet
#'
#' @author Pascal Belleau, Astrid Deschenes
#' @export
network <- function(netFileName, fileType="sif", subNetFileName=NULL) {

    network <- NULL
    if (toupper(fileType) == "SIF") {
        if(file.exists(netFileName) &&
           (is.null(subNetFileName) ||
            file.exists(subNetFileName))) {
            network <- networkFromSif(netFileName, subNetFileName)
        } else {
            strErr <- paste0("Problem with network filename ", netFileName)
            if(!(is.null(subNetFileName))) {
                strErr <- paste0(strErr, " or ", subNetFileName)
            }
            stop(strErr)
        }
    } else {
        strErr <- paste0("File type ", fileType, " is not supported")
        stop(strErr)
    }

    class(network) <- "network"
    return(network)
}
