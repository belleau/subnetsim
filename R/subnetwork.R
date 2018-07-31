#' @title TODO
#'
#' @description TODO
#'
#' @param network an object \code{network} TODO
#' @param nbIter a single \code{numeric}, interpreted as an \code{integer}
#' indicating the number of iterations to be run. Default: \code{10000}.
#' @param nbNodes a single \code{numeric}, interpreted as an \code{integer}
#' TODO.
#' @param seedV a single \code{numeric}, interpreted as an \code{integer}, a
#' seed used when reproducible results are
#' needed. When a value inferior or equal to zero is given, a random integer
#' is used. Default: \code{-1}.
#'
#'
#' @return a list is returned for use by \code{print.subnetwork}. This list has
#'            the components:
#'            \itemize{
#'                \item{ TODO }
#'                \item{ TODO }
#'            }
#'
#' @author Pascal Belleau
#' @keywords subnetwork
#' @export
#' @examples
#' ## TODO
#'
#'
subnetwork <- function(network, nbIter = 10000, nbNodes=NULL,
                        seedV = -1) {

    ####################################################
    # Parameters validation
    ####################################################
    # nbIter has to be a positive integer
    if(!is.numeric(nbIter) || length(nbIter) != 1L || is.na(nbIter) ||
            nbIter <= 0) {
        stop("'nbIter' must be a positive integer")
    }

    # nbNodes has to be a positive integer
    if(is.null(nbNodes) &&
       !(is.null(network$nodesSubNet))) {
        nbNodes <- length(network$nodesSubNet)
    }
    if(!is.numeric(nbNodes) || length(nbNodes) != 1L || is.na(nbNodes) ||
            nbNodes <= 0) {
        stop("'nbNodes' must be a positive integer")
    }

    # seedV has to be an integer
    if(!is.numeric(seedV) || length(seedV) != 1L || is.na(seedV)) {
        stop("'seedV' must be an integer")
    }

    nbIter <- as.integer(nbIter)
    nbNodes <- as.integer(nbNodes)

    analysis <- simuleSubNet(network$netAll, network$nodesAll, nbIter, nbNodes)

    # Prepare data frame which contains a summary of each step
    final <- data.frame(Iteration=1:length(values), Step=names(values),
                        row.names = NULL)
    final[criterion]<-values

    # Return a list marked as an regression class containing :
    # 1- a list containing the summary data frame
    # 2- the final regression
    out <- list(iterations=final, lm=regression,
                    nbIter=nbIter, nbNodes=nbNodes)
    class(out) <- "subnetwork"
    return(out)
    return(simuleSubNet(network$netAll, network$nodesAll, nbIter, nbNodes))
}
