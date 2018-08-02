#' @rdname subnetwork
#' @method print subnetwork
#' @param x the output object from \code{subnetwork} function to be printed
#' @param digits the number of digits to use when printing numbers
#' @param \ldots arguments passed to or from other methods
#' @export
print.subnetwork <- function(x, digits=4,...) {
    # Print title before printing the content of the subnetwork object
    cat("TODO\n")
    cat("TODO\n\n")
    cat("Iterations:\n")
    print(x$nbIter, ...)
    cat("Seed:\n")
    print(x$seed, ...)
    cat("Number of nodes for simulated subnetworks plus one link:\n")
    print(summary(x$nbNodesOneLink), ...)
    cat("Number of links for simulated subnetworks plus one link:\n")
    print(summary(x$nbLinkOneLink), ...)
    cat("Number of links in the simulated subnetworks:\n")
    print(summary(x$nbLink), ...)
    invisible(x)
}
