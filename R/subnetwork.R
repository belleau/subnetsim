#' @title Subnetwork simulation through randomness sampling
#'
#' @description Simulates a fixed number of subnetwork through randomness sampling.
#' Each subnetwork is containing the same fixed number of nodes. For all simulated
#' subetwork, three important informations are extracted: the number of
#' nodes present when including the first-degree neighbor nodes, the number
#' of links and the number of links when includes all links from the first-degree
#' neighbor nodes.
#'
#' @param network an object of \code{class} \code{network} that represents
#' the global network from which the tested subnetwork is extracted. The
#' subnetwork can be included as an element of the \code{network} object.
#' If the subnetwork is included in the \code{network} object, the
#' \code{nbNodes}, \code{nbLink}, \code{nbNodesOneLink} and
#' \code{nbLinkOneLink} parameters should all be \code{NULL}. If the
#' subnetwork is not included, all four parameters should be set.
#' @param nbIter a single \code{numeric}, interpreted as an \code{integer}
#' indicating the number of iterations to be run. Default: \code{10000}.
#' @param nbNodes a single \code{numeric}, interpreted as an \code{integer}
#' the number of nodes of the tested subnetwork. If the value is \code{NULL},
#' the value is obtained through the subnetwork present in the network object.
#' Default: \code{NULL}.
#' @param nbLink a single \code{numeric}, interpreted as an \code{interger},
#' the number of links of the tested subnetwork. If the value is \code{NULL},
#' the value is obtained through the subnetwork present in the network object.
#' Default: \code{NULL}.
#' @param nbNodesOneLink a single \code{numeric}, interpreted as an \code{interger},
#' the number of nodes of the tested subnetwork including the first-degree neighbor
#' nodes. If the value is \code{NULL},
#' the value is obtained through the subnetwork present in the network object.
#' Default: \code{NULL}.
#' @param nbLinkOneLink a single \code{numeric}, interpreted as an \code{interger},
#' the number of links of the tested subnetwork including all the links of the
#' first-degree neighbor nodes. If the value is \code{NULL},
#' the value is obtained through the subnetwork present in the network object.
#' Default: \code{NULL}.
#' @param seedV a single \code{numeric}, interpreted as an \code{integer}, a
#' seed used when reproducible results are
#' needed. When a value inferior or equal to zero is given, a random integer
#' is used. Default: \code{-1}.
#'
#'
#' @return a list is returned for use by \code{print.subnetwork} and
#' \code{plot.subnetwork}. This list has the following components:
#'            \itemize{
#'                \item{ \code{nbNodesOneLink} a vector of the number of nodes
#'                for each simulated subnetwork extended to contain the
#'                first-degree nodes. }
#'                \item{ \code{nbLinkOneLink} a vector of the number of links
#'                for each simulated subnetwork extended to contain all
#'                the links from thefirst-degree nodes. }
#'                \item{ \code{nbLink} a vector of the number of links
#'                for each simulated subnetwork. }
#'                \item{ \code{nbIter} a single \code{integer} corresponding
#'                to the number of iterations done. }
#'                \item{ \code{nbNodes} a single \code{integer} corresponding
#'                to the number of nodes of the tested subnetwork. }
#'                \item{ \code{nbNodesOneLink} a single \code{integer}
#'                corresponding to the number of nodes of the tested subnetwork
#'                when first-degree neighbor nodes are included. }
#'                \item{ \code{nbLinkOneLink} a single \code{integer}
#'                corresponding to the number of links of the tested subnetwork
#'                including all links from the first-degree neighbor nodes.}
#'                \item{ \code{nbLink} a single \code{integer}
#'                corresponding to the number of links of the tested subnetwork.}
#'                \item{ \code{seed} a single \code{numeric} corresponding
#'                to the seed used for the analysis.}
#'            }
#'
#' @author Pascal Belleau, Astrid Deschenes
#' @keywords subnetwork
#' @export
#' @examples
#' ## TODO
#'
#'
subnetwork <- function(network, nbIter = 10000, nbNodes=NULL,
                       nbLink=NULL, nbNodesOneLink=NULL,
                       nbLinkOneLink=NULL, seedV = -1) {

    ####################################################
    # Parameters validation
    ####################################################
    # nbIter has to be a positive integer
    if(!is.numeric(nbIter) || length(nbIter) != 1L || is.na(nbIter) ||
            nbIter <= 0) {
        stop("'nbIter' must be a positive integer")
    }

    # nbNodes has to be a positive integer
    if((is.null(nbNodes) ||
        is.null(nbLink) || is.null(nbNodesOneLink) ||
        is.null(nbLinkOneLink) ) &&
       !(is.null(network$nodesSubNet))) {
        if(!(is.null(nbNodes) &&
           is.null(nbLink) && is.null(nbNodesOneLink) &&
           is.null(nbLinkOneLink))){
            warning("The parameters 'nbNodes', 'nbLink', 'nbNodesOneLink' and
                    'nbLinkOneLink' are mixed NULL and not NULL we used
                    the value of the subNetwork of the objet network.")
        }
        nbNodes <- length(network$nodesSubNet)
        nbLink <- getSubNet(network, network$nodesSubNet)
        tmp <- getOneLink(network, network$nodesSubNet)
        nbNodesOneLink <- tmp[1]
        nbLinkOneLink <- tmp[2]
    }
    if(!is.numeric(nbNodes) || length(nbNodes) != 1L || is.na(nbNodes) ||
            nbNodes <= 0) {
        stop(paste0("The parameters 'nbNodes', 'nbLink', 'nbNodesOneLink' and ",
                    "'nbLinkOneLink' must be a positive integer or a subnetwork ",
                    "must be present in the network object"))
    }

    # seedV has to be an integer
    if(!is.numeric(seedV) || length(seedV) != 1L || is.na(seedV)) {
        stop("'seedV' must be an integer")
    }

    nbIter <- as.integer(nbIter)
    nbNodes <- as.integer(nbNodes)

    analysis <- simuleSubNet(network$netAll, network$nodesAll,
                                nbIter, nbNodes, seedV)

    # Return a list marked as an regression class containing :
    # 1 - a vector of the number of links from all the simulated subnetworks
    #     extended to contain the first-degree nodes
    # 2 - a vector of the number of links in the simulated subnetworks
    #     including all links from the first-degree nodes
    # 3 - a vector of the number of links in the simulated subnetworks
    # 4 - the number of iterations
    # 5 - the number of nodes in the tested subnetwork
    # 6 - the number of links in the tested subnetwork
    # 7 - the number of nodes including first-degree nodes in the
    #     tested subnetwork
    # 8 - the number of links including all links of the first-degree
    #     nodes in the tested subnetwork
    # 6 - the seed
    out <- list(nbNodesOneLink = analysis$nbNodesOneLink,
                    nbLinkOneLink = analysis$nbLinkOneLink,
                    nbLink= analysis$nbLink,
                    nbIter = nbIter, nbNodes = nbNodes,
                    nbNodesOneLink = nbNodesOneLink,
                    nbLinkOneLink = nbLinkOneLink,
                    nbLink = nbLink, seed = seedV)
    class(out) <- "subnetwork"
    return(out)
}
