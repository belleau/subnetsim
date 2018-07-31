#' @rdname subnetwork
#' @method print subnetwork
#' @param x the output object from \code{subnetwork} function to be printed
#' @param \ldots arguments passed to or from other methods
#' @export
print.subnetwork <- function(x, ...) {
    # Print title before printing the content of the regression object
    cat("TODO\n")
    cat("TODO\n\n")
    cat("Iterations:\n")
    print(x$nbIter, ...)
    cat("Seed:\n")
    print(x$seed, ...)
    invisible(x)
}
