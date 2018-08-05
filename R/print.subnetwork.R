#' @rdname subnetwork
#' @method print subnetwork
#' @param x the output object from \code{subnetwork} function
#' @param digits the number of digits to use when printing numbers
#' @param \ldots arguments passed to or from other methods
#' @export
print.subnetwork <- function(x, digits=4,...) {
    # Print title before printing the content of the subnetwork object
    pvalueNbLink  <- length(which(x$nbLink>x$nbLinkTested)) / x$nbIter
    pvalueNbLinkOneLink <- length(which(x$nbLinkOneLink>x$nbLinkOneLinkTested)) / x$nbIter
    pvalueNbNodeOneLink <- length(which(x$nbNodesOneLink>x$nbNodesOneLinkTested)) / x$nbIter
    cat("Simulation subnetwork summary\n")
    cat("Iterations:\n")
    print(x$nbIter, ...)
    cat("Seed:\n")
    print(x$seed, ...)
    cat("--- Tested subnetwork ---\n")
    cat("Number of nodes:\n")
    print(x$nbNodes)
    cat("Number of nodes including first-degree nodes:\n")
    print(x$nbNodesOneLinkTested)
    cat("Number of links:\n")
    print(x$nbLinkTested)
    cat("Number of links including all links from first-degree nodes:\n")
    print(x$nbLinkOneLinkTested)
    cat("--- Simulated subnetworks ---\n")
    cat("Number of nodes in the simulated subnetworks including first-degree nodes:\n")
    print(summary(x$nbNodesOneLink), ...)
    cat("Number of links in the simulated subnetworks including all links from first-degree nodes:\n")
    print(summary(x$nbLinkOneLink), ...)
    cat("Number of links in the simulated subnetworks:\n")
    print(summary(x$nbLink), ...)
    cat("Proportion of simulate subnetworks with higher number of link:\n")
    print(pvalueNbLink)
    cat("Proportion of simulate subnetworks with higher number of link\n")
    cat("including all links from first-degree nodes:\n")
    print(pvalueNbLinkOneLink)
    cat("Proportion of simulate subnetworks with higher number of nodes\n")
    cat("including first-degree nodes:\n")
    print(pvalueNbNodeOneLink)
    invisible(x)
}
