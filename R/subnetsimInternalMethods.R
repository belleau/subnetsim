#' @title Fix seed value.
#'
#' @description Fix seed value using random value when specified value is -1.
#
#' @param vSeed a \code{integer}, a seed used when reproducible results are
#' needed. When a value inferior or equal to zero is given, a random integer
#' is used.
#'
#' @return a \code{double}, the seed value.
#'
#' @examples
#'
#' ## Return vSeed value when value is superior to zero
#' subnetsim:::fixSeed(vSeed = 10)
#'
#' ## Return new value when value is inferior to zero
#' subnetsim:::fixSeed(vSeed = -1)
#'
#' @author Pascal Belleau, Astrid Deschenes
#' @keywords internal
fixSeed <- function(vSeed) {
    if (vSeed <= 0) {
        tSeed <- as.numeric(Sys.time())
        vSeed <- 1e8 * (tSeed - floor(tSeed))
    }
    return(vSeed)
}


#' @title Generating simulated subnetworks.
#'
#' @description Generationg multiple simulated sub-networks from a network.
#' All subnetworks have the same number of nodes.
#'
#' @param netAll a \code{hash} representing the global network.
#' @param nodesAll \code{list} of \code{character} string representing
#' all the nodes in the global network.
#' @param nbIter a single \code{integer} indicating the number of iterations
#' to be run.
#' @param nbNodes a \code{integer} indicating the number of nodes that each
#' simulated sub-network will contain.
#' @param seedV a \code{integer}, a seed used when reproducible results are
#' needed. When a value inferior or equal to zero is given, a random integer
#' is used.
#'
#' @return a \code{data.frame} containing the information from all simulated
#' subnetworks. Each row of the \code{data.frame} corresponds to one simulation:
#' \itemize{
#' \item nbLink: Number of links in the simulated subnetworks
#' \item nbNodesOneLink: Number of nodes of the simulated subnetworks
#'  including the first-degree neighbor nodes.
#' \item nbLinkOneLink Number of links of the simulated subnetworks
#'  including all the links from the first-degree neighbor nodes.
#' }
#'
#' @examples
#'
#' # Access demo files stored in the subnetsim package
#' networkFile <- system.file("extdata", "demo_network.sif", package="subnetsim")
#' subnetworkFile <- system.file("extdata", "demo_subnetwork.sif", package="subnetsim")
#'
#' # Load a global network and its tested subnetwork using SIF format files
#' demo_network_with_sub <- network(netFileName = networkFile,
#'     subNetFileName = subnetworkFile)
#'
#' # Run simulation
#' simulatedSubnetwork <- subnetsim:::simuleSubNet(netAll = demo_network_with_sub$netAll,
#'     nodesAll = demo_network_with_sub$nodesAll,
#'     nbIter = 2, nbNodes <- length(demo_network_with_sub$nodesSubNet),
#'     seedV = 3122)
#'
#' # To see a summary of the content of the "subnetwork" object
#' simulatedSubnetwork
#'
#' @importFrom hash hash has.key
#' @author Pascal Belleau, Astrid Deschenes
#' @keywords internal
simuleSubNet <- function(netAll, nodesAll, nbIter, nbNodes, seedV) {

    set.seed(fixSeed(seedV))

    pastSub <- hash()

    distSubNet <- data.frame(nbNodes=rep(NA,nbIter),
                                nbLink=rep(NA,nbIter),
                                nbLinkSub=rep(NA,nbIter))

    for (i in seq_len(nbIter)) {
        nodesSub <- sample(length(nodesAll), nbNodes, replace = FALSE)
        nodesSub <- nodesSub[order(nodesSub)]
        distSubNet[i, 1:2] <- getOneLink(netAll, nodesAll[nodesSub])
        distSubNet[i, 3] <- getSubNet(netAll, nodesAll[nodesSub])
        if(i%%100==1){
            print(paste0(i, " iterations completed"))
        }
    }

    return(data.frame(nbNodesOneLink=distSubNet[, 1],
                      nbLinkOneLink=distSubNet[, 2] , nbLink=distSubNet[, 3]))
}

#' @title Counting the number of nodes and the number of links associated to a
#' subnetwork.
#'
#' @description Counting the number of nodes and the number of links in a
#' specific subnetwork. From a list of nodes forming the subnetwork, the number of
#' nodes and the number of links associated to the subnetwork are calculated.
#' The nodes and links inside the subnetwork are counted as well as all first-degree
#' neighbor nodes and all links associated to the first-degree nodes.
#'
#' @param netAll a \code{hash} representing the global network.
#' @param nodesSel a \code{vector} of \code{caracter} strings representing all the
#' nodes contained in the subnetwork.
#'
#' @return a \code{vector} containing 2 \code{integer}. The first
#' \code{integer} represents the number of nodes and the second \code{integer}
#' represents the number of links associated to the tested subnetwork.
#'
#' @examples
#'
#' ## TODO
#'
#' @importFrom hash hash has.key keys
#' @author Pascal Belleau, Astrid Deschenes
#' @keywords internal
getOneLink <- function(netAll, nodesSel) {

    nodesFirst <- hash()
    linkFirst <- hash()
    cptNodes <- 0
    cptLink <- 0

    for (i in seq_len(length(nodesSel))) {

        listNodLink <- keys(netAll[[ nodesSel[i] ]])
        if(!(has.key( nodesSel[i], nodesFirst))){
            nodesFirst[[nodesSel[i]]] <- 1
            cptNodes <- cptNodes + 1
        }
        for (j in seq_len(length(listNodLink))) {

            if (!(has.key( listNodLink[j], nodesFirst))) {
                nodesFirst[[listNodLink[j]]] <- 1
                cptNodes <- cptNodes + 1
            }

            if(!(has.key(key = as.character(netAll[[nodesSel[i]]][[listNodLink[j]]]),
                            hash = linkFirst))) {
                linkFirst[[as.character(netAll[[nodesSel[i]]][[listNodLink[j]]])]] <- 1
                cptLink <- cptLink + 1
            }
        }
    }

    return(c(cptNodes, cptLink))
}

#' @title Counting the number of links in a network.
#'
#' @description Counting the number of links in a network.
#'
#' @param netAll a \code{hash} representing the global network.
#' @param nodesSel a \code{vector} of \code{caracter} strings
#' representing all the nodes contained in the subnetwork.
#'
#' @return a \code{integer} representing the number of links.
#'
#' @examples
#'
#' ## TODO
#'
#' @importFrom hash hash has.key keys
#' @author Pascal Belleau, Astrid Deschenes
#' @keywords internal
getSubNet <- function(netAll, nodesSel) {

    nodesSub <- hash()

    for (i in seq_len(length(nodesSel))) {
        nodesSub[[nodesSel[i]]] <- 1
    }

    linkFirst <- hash()
    cptNodes <- 0
    cptLink <- 0

    for (i in seq_len(length(nodesSel))) {

        listNodLink <- keys(netAll[[nodesSel[i]]])

        for (j in seq_len(length(listNodLink))) {

            if (has.key(key = listNodLink[j], hash = nodesSub)) {
                if(!(has.key(key = as.character(netAll[[nodesSel[i]]][[listNodLink[j]]]),
                                hash = linkFirst))){
                    linkFirst[[as.character(netAll[[nodesSel[i]]][[listNodLink[j]]]) ]] <- 1
                    cptLink <- cptLink + 1
                }
            }
        }
    }

    return(cptLink)
}
