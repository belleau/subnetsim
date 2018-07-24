library(hash)

h <- hash()
h[["aye"]] <- 13
h[["bla"]] <- 5

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
