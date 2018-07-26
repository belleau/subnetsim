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


#' @title Generating simulated sub-networks.
#'
#' @description Generationg multiple simulated sub-networks from a network. The
#' sub-networks all have the same number of nodes.
#'
#' @param netAll TODO
#' @param nodesAll TODO
#' @param nbIter a \code{integer} indicating the number of iterations to be
#' run.
#' @param nbNodes a \code indicating the number of nodes that each simulated
#' sub-network will contain.
#' @param seedV a \code{integer}, a seed used when reproducible results are
#' needed. When a value inferior or equal to zero is given, a random integer
#' is used.
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
simuleSubNet <- function(netAll, nodesAll, nbIter, nbNodes, seedV) {

    set.seed(fixSeed(seedV))

    pastSub <- hash()

    distSubNet <- data.frame(nbNodes=rep(NA,nbIter),
                                nbLink=rep(NA,nbIter),
                                nbLinkSub=rep(NA,nbIter))

    for (i in seq_len(nbIter)) {
        nodesSub <- sample(length(nodesAll), nbNodes, replace = FALSE)
        nodesSub <- nodesSub[order(nodesSub)]
        string_node <- paste(unlist(as.character(nodesSub)), sep="_",
                                collapse = '_')

        if (!(has.key(string_node, pastSub))) {
            pastSub[[string_node]] <- 1
            distSubNet[i, 1:2] <- getOneLink(netAll, nodesAll[nodesSub])
            distSubNet[i, 3] <- getSubNet(netAll, nodesAll[nodesSub])
        }
    }

    return(distSubNet)
}

#' @title Counting the number of nodes and the number of links in a
#' network.
#'
#' @description Counting the number of nodes and the number of links in a
#' network.
#'
#' @param netAll TODO
#' @param nodesSel TODO
#'
#' @return a \code{vector} containing 2 \code{integer}. The first \code{integer}
#' represent the number of nodes and the second \code{integer} represents the
#' number of links.
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
#' @param netAll TODO
#' @param nodesSel TODO
#'
#' @return a \code{integer} representing the number of lilnks.
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
