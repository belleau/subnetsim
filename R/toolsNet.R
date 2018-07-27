library(hash)


createNetwork <- function(fileName){
    netFile <- read.table(fileName, header = FALSE, stringsAsFactors = FALSE,fill = TRUE, na.strings = "")

    l <- 1
    for( i in seq_len(length(netFile[,1]))){
        if(!(is.na(netFile[i,3]))){
            if(!(has.key(key = IRGEN_all[i,1], hash = netAll))){
                netAll[[netFile[i,1]]] <- hash()
            }
            netAll[[netFile[i,1]]][[netFile[i,3]]] <- l

            if(!(has.key(key = netFile[i,3], hash = netAll))){
                netAll[[netFile[i,3]]] <- hash()
            }
            netAll[[IRGEN_all[i,3]]][[netFile[i,1]]] <- l
            netLink[l,1] <- netFile[i,1]
            netLink[l,2] <- netFile[i,3]
            l <- l+1
        }

    }

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





##############################################
##############################################
##############################################


library(hash)


IRGEN_all <- read.table("data/IRGEN_all.sif", header = FALSE, stringsAsFactors = FALSE,fill = TRUE, na.strings = "")
IRGEN_can <- read.table("data/IRGEN_Diabetes_candidats.sif", header = FALSE, stringsAsFactors = FALSE,fill = TRUE, na.strings = "")
colnames(IRGEN_all) <- c("N1", "L", "N2")
head(IRGEN_all)

netAll <- hash()
netLink <- data.frame(n1 = rep(NA, length(IRGEN_all[!(is.na(IRGEN_all[,3])),1])),
                      n2 = rep(NA, length(IRGEN_all[!(is.na(IRGEN_all[,3])),1])))
l <- 1
for( i in seq_len(length(IRGEN_all[,1]))){
    if(!(is.na(IRGEN_all[i,3]))){
        if(!(has.key(key = IRGEN_all[i,1], hash = netAll))){
            netAll[[IRGEN_all[i,1]]] <- hash()
        }
        netAll[[IRGEN_all[i,1]]][[IRGEN_all[i,3]]] <- l

        if(!(has.key(key = IRGEN_all[i,3], hash = netAll))){
            netAll[[IRGEN_all[i,3]]] <- hash()
        }
        netAll[[IRGEN_all[i,3]]][[IRGEN_all[i,1]]] <- l
        netLink[l,1] <- IRGEN_all[i,1]
        netLink[l,2] <- IRGEN_all[i,3]
        l <- l+1
    }

}

nodesAll <- keys(netAll)

nodesOrder <- order(nodesAll)
nodesAll <- nodesAll[nodesOrder]





res <- simuleSubNet(netAll, nodesAll, 10000, 94,32)

library(ggplot2)
breaks <- quantile(res$nbNodes,seq(0,1,by=0.01))

ggplot(res, aes(x=nbNodes)) +
    geom_histogram(breaks=breaks,aes(y=..density..),colour="black",fill=c("black"))

colnames(res) <- c('nbNodes','nbLinks', 'nbLinksSub')
df <-data.frame(nbNodes=272)

gp1 <- ggplot(res, aes(x=nbNodes)) +
    geom_histogram(colour="black",fill=c("black")) +
    geom_vline(data = df,
               aes(xintercept = nbNodes,
                   color="observed"), linetype="longdash",
               show.legend=TRUE)
ggsave(paste0("data/nodesOneLink.pdf"), gp1, width=7,height=5)

df <-data.frame(nbLinks=1070)
gp2 <- ggplot(res, aes(x=nbLinks)) +
    geom_histogram(colour="black",fill=c("black")) +
    geom_vline(data = df,
               aes(xintercept = nbLinks,
                   color="observed"), linetype="longdash",
               show.legend=TRUE)
ggsave(paste0("data/linkssOneLink.pdf"), gp2, width=7,height=5)
df <-data.frame(nbLinks=330)
gp3 <- ggplot(res, aes(x=nbLinksSub)) +
    geom_histogram(colour="black",fill=c("black")) +
    geom_vline(data = df,
               aes(xintercept = nbLinks,
                   color="observed"), linetype="longdash",
               show.legend=TRUE)
ggsave(paste0("data/nodesSubNet.pdf"), gp3, width=7,height=5)


netAll <- hash()
if(!(is.na(IRGEN_all[i,3]))){
    if(!(has.key(key = IRGEN_all[i,1], hash = netAll))){
        netAll[[IRGEN_all[i,1]]] <- hash()
    }
    netAll[[IRGEN_all[i,1]]][[IRGEN_all[i,3]]] <- l

    if(!(has.key(key = IRGEN_all[i,3], hash = netAll))){
        netAll[[IRGEN_all[i,3]]] <- hash()
    }
    netAll[[IRGEN_all[i,3]]][[IRGEN_all[i,1]]] <- l
    netLink[l,1] <- IRGEN_all[i,1]
    netLink[l,2] <- IRGEN_all[i,3]
    l <- l+1
}

inserLink <- function(x,netAll, netLink){
    if(!(is.na(x[3]))){
        if(!(has.key(key = x[1], hash = netAll))){
            netAll[[x[1]]] <- hash()
        }
        l <- length(netLink[!(is.na(netLink[,1])), 1])
        netAll[[x[1]]][[x[3]]] <- l + 1
        if(!(has.key(key = x[3], hash = netAll))){
            netAll[[x[3]]] <- hash()
        }
        netAll[[x[3]]][[x[1]]] <- l + 1
        netLink[l+1,1] <- x[1]
        netLink[l+1,2] <- x[3]

    }else{
        if(!(has.key(key = x[1], hash = netAll))){
            netAll[[x[1]]] <- hash()
        }
    }
    return(c(x[1], x[3]))
}
netAll <- hash()
netLink <- data.frame(n1 = rep(NA, length(IRGEN_all[!(is.na(netFile[,3])),1])),
                      n2 = rep(NA, length(IRGEN_all[!(is.na(netFile[,3])),1])))

