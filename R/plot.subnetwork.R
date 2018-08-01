#' @rdname subnetwork
#' @method plot subnetwork
#' @param x the output object from \code{subnetwork} function to be plotted
#' @param \ldots arguments passed to or from other methods
#' @importFrom ggplot2 ggplot geom_histogram geom_vline aes xlab ylab scale_color_manual theme
#' @export
plot.subnetwork <- function(x, digits=4,...) {
    # Print graph of the subnetwork object
    nbNodes=272
    df <- data.frame(nbNodes = nbNodes)
    data <- data.frame(nbNodesOneLink=x$nbNodesOneLink)
    ggplot(data, aes(x=nbNodesOneLink)) +
        geom_histogram(colour = "black",fill = c("black")) +
        xlab("Number of links") + ylab("Count") +
        geom_vline(aes(xintercept = nbNodes, color = "observed"),
                   linetype="longdash", size = 1.2, show.legend=TRUE) +
        scale_color_manual(name = "", values = c(observed = "red")) +
        theme(legend.position="bottom")
    #geom_vline(xintercept = nbNodes, colour="red", linetype = "longdash",
    #           size = 1.5, show.legend=TRUE) +
}
