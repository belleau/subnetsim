#' @rdname subnetwork
#' @method plot subnetwork
#' @param x the output object from \code{subnetwork} function to be plotted
#' @param bins the number of bins used to generate the histograms
#' @param \ldots arguments passed to or from other methods
#' @importFrom ggplot2 ggplot geom_histogram geom_vline aes xlab ylab scale_color_manual theme
#' @importFrom gridExtra grid.arrange
#' @export
plot.subnetwork <- function(x, bins=30, ...) {
    # Print graph of the subnetwork object
    nbNodes=272
    df <- data.frame(nbNodes = nbNodes)
    data <- data.frame(nbNodesOneLink=x$nbNodesOneLink,
                       nbLinkOneLink = x$nbLinkOneLink,
                       nbLink = x$nbLink)
    graph01 <- ggplot(data, aes(x=nbNodesOneLink)) +
        geom_histogram(colour = "black",fill = c("black"), bins=bins) +
        xlab("Number of nodes of the networks including first degree neighbors") + ylab("Count") +
        geom_vline(aes(xintercept = nbNodes, color = "observed"),
                   linetype="longdash", size = 1.2, show.legend=TRUE) +
        scale_color_manual(name = "", values = c(observed = "red"))+
        theme(legend.position="top")
    #geom_vline(xintercept = nbNodes, colour="red", linetype = "longdash",
    #           size = 1.5, show.legend=TRUE) +

    graph02 <- ggplot(data, aes(x=nbLinkOneLink)) +
        geom_histogram(colour = "black",fill = c("black"), bins=bins) +
        xlab("Number of links of the networks including first degree neighbors") + ylab("Count") +
        geom_vline(aes(xintercept = nbNodes, color = "observed"),
                   linetype="longdash", size = 1.2, show.legend=FALSE) +
        scale_color_manual(name = "", values = c(observed = "red"))

    graph03 <- ggplot(data, aes(x=nbLink)) +
        geom_histogram(colour = "black",fill = c("black"), bins=bins) +
        xlab("Number of links of the networks") + ylab("Count") +
        geom_vline(aes(xintercept = nbNodes, color = "observed"),
                   linetype="longdash", size = 1.2, show.legend=FALSE) +
        scale_color_manual(name = "", values = c(observed = "red")) +
        theme(legend.position="bottom")

    grid.arrange(graph01, graph02, graph03, nrow=3)
}
