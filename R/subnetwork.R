#' @title TODO
#'
#' @description TODO
#'
#' @param network a \code{vector} TODO
#' @param globalNetwork an \code{array} or a \code{data.frame} TODO
#' @param nbIter a \code{integer} indicating the number of iterations to be
#' run. Default: \code{10000}.
#' @param nbNodes a \code{integer} TODO.
#' @param seedV a \code{integer}, a seed used when reproducible results are
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
#' @seealso
#' @export
#' @examples
#' ## TODO
#'
#'
subnetwork <- function(network, globalNetwork, nbIter = 10000, nbNodes,
                        seedV = -1) {

    ####################################################
    # Parameters validation
    ####################################################
    # nbIter has to be a positive integer
    if(is.integer(nbIter) && length(nbIter) == 1L && !is.na(nbIter) &&
            nbIter > 0) {
        stop("'nbIter' must be a positive integer")
    }

    # nbNodes has to be a positive integer
    if(is.integer(nbNodes) && length(nbNodes) == 1L && !is.na(nbNodes) &&
            nbNodes > 0) {
        stop("'nbNodes' must be a positive integer")
    }

    # seedV has to be an integer
    if(is.integer(seedV) && length(seedV) == 1L && !is.na(seedV)) {
        stop("'seedV' must be an integer")
    }

    set.seed(fixSeed(vSeed))
}
