library(hash)


createNetwork <- function(fileName){
    netFile <- read.table(fileName, header = FALSE, stringsAsFactors = FALSE,fill = TRUE, na.strings = "")

}


simuleSubNet <- function(netAll, nodesAll, nbIter, nbNodes, seedV){
    set.seed(seedV)
    pastSub <- hash()

    distSubNet <- data.frame(nbNodes=rep(NA,nbIter), nbLink=rep(NA,nbIter), nbLinkSub=rep(NA,nbIter))

    for(i in seq_len(nbIter)){
        nodesSub <-sample(length(nodesAll), nbNodes, replace = FALSE)
        nodesSub <- nodesSub[order(nodesSub)]
        string_node <- paste(unlist(as.character(nodesSub)),sep="_", collapse = '_')


        if(!(has.key(string_node, pastSub))){
            pastSub[[string_node]] <- 1
            distSubNet[i, 1:2] <- getOneLink(netAll, nodesAll[nodesSub])
            distSubNet[i, 3] <- getSubNet(netAll, nodesAll[nodesSub])
        }
    }
    return(distSubNet)
}

getOneLink <- function(netAll, nodesSel){

    nodesFirst <- hash()
    linkFirst <- hash()
    cptNodes <- 0
    cptLink <- 0
    for(i in seq_len(length(nodesSel))){
        listNodLink <- keys(netAll[[ nodesSel[i] ]])
        for(j in seq_len(length(listNodLink))){
            if(!(has.key( listNodLink[j], nodesFirst))){
                nodesFirst[[listNodLink[j]]] <- 1
                cptNodes <- cptNodes + 1
            }

            if(!(has.key(key = as.character(netAll[[nodesSel[i]]][[listNodLink[j]]]), hash = linkFirst))){
                linkFirst[[as.character(netAll[[nodesSel[i]]][[listNodLink[j]]]) ]] <- 1
                cptLink <- cptLink + 1
            }
        }
    }
    return(c(cptNodes, cptLink))
}

getSubNet <- function(netAll, nodesSel){

    nodesSub <- hash()

    for(i in seq_len(length(nodesSel))){
        nodesSub[[nodesSel[i]]] <- 1
    }

    linkFirst <- hash()
    cptNodes <- 0
    cptLink <- 0
    for(i in seq_len(length(nodesSel))){
        listNodLink <- keys(netAll[[ nodesSel[i] ]])
        for(j in seq_len(length(listNodLink))){

            if(has.key(key = listNodLink[j], hash = nodesSub)){
                if(!(has.key(key = as.character(netAll[[nodesSel[i]]][[listNodLink[j]]]), hash = linkFirst))){
                    linkFirst[[as.character(netAll[[nodesSel[i]]][[listNodLink[j]]]) ]] <- 1
                    cptLink <- cptLink + 1
                }
            }
        }
    }
    return(cptLink)
}
