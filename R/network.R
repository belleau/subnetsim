


network <- function(fileNetwork, fileSubNet){
    netFile <- read.table(fileNetwork, header = FALSE, stringsAsFactors = FALSE,fill = TRUE, na.strings = "")
    netSub <- read.table(fileSubNet, header = FALSE, stringsAsFactors = FALSE,fill = TRUE, na.strings = "")

    netAll <- hash()
    netLink <- data.frame(n1 = rep(NA, length(IRGEN_all[!(is.na(netFile[,3])),1])),
                          n2 = rep(NA, length(IRGEN_all[!(is.na(netFile[,3])),1])))

    apply
}

