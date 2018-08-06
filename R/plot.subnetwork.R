#' @rdname subnetwork
#' @method plot subnetwork
#' @param bins the number of bins used to generate the histograms
#' @importFrom ggplot2 ggplot geom_histogram geom_vline aes xlab ylab scale_color_manual theme ggplot_build ggplot_gtable
#' @importFrom gridExtra grid.arrange
#' @export
plot.subnetwork <- function(x, bins=30, ...) {
    # Print graph of the subnetwork object
    data <- data.frame(nbNodesOneLink=x$nbNodesOneLink,
                       nbLinkOneLink = x$nbLinkOneLink,
                       nbLink = x$nbLink)

    graphTEMP <- ggplot(data, aes(x=nbNodesOneLink)) +
        geom_histogram(colour = "black",fill = c("black"), bins=bins) +
        xlab("Number of nodes of the networks including first-degree neighbors") + ylab("Count") +
        geom_vline(aes(xintercept = x$nbNodesOneLinkTested, color = "observed"),
                   linetype="longdash", size = 1.2, show.legend=TRUE) +
        scale_color_manual(name = "", values = c(observed = "red")) +
        theme(legend.position="top")

    graph01 <- ggplot(data, aes(x=nbNodesOneLink)) +
        geom_histogram(colour = "black",fill = c("black"), bins=bins) +
        xlab("Number of nodes of the networks including first-degree neighbors") + ylab("Count") +
        geom_vline(aes(xintercept = x$nbNodesOneLinkTested, color = "observed"),
                   linetype="longdash", size = 1.2, show.legend=FALSE) +
        scale_color_manual(name = "", values = c(observed = "red")) +
        theme(legend.position="top")

    graph02 <- ggplot(data, aes(x=nbLinkOneLink)) +
        geom_histogram(colour = "black",fill = c("black"), bins=bins) +
        xlab("Number of links of the networks including first-degree neighbors") + ylab("Count") +
        geom_vline(aes(xintercept = x$nbLinkOneLinkTested, color = "observed"),
                   linetype="longdash", size = 1.2, show.legend=FALSE) +
        scale_color_manual(name = "", values = c(observed = "red"))

    graph03 <- ggplot(data, aes(x=nbLink)) +
        geom_histogram(colour = "black",fill = c("black"), bins=bins) +
        xlab("Number of links of the networks") + ylab("Count") +
        geom_vline(aes(xintercept = x$nbLinkTested, color = "observed"),
                   linetype="longdash", size = 1.2, show.legend=FALSE) +
        scale_color_manual(name = "", values = c(observed = "red")) +
        theme(legend.position="bottom")

    g_legend<-function(a.gplot){
        tmp <- ggplot_gtable(ggplot_build(a.gplot))
        leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
        legend <- tmp$grobs[[leg]]
        return(legend)}

    legend <- g_legend(graphTEMP)

    grid.arrange(graph01, graph02, graph03, legend, nrow=4, heights=c(4, 4, 4, 1))
}
