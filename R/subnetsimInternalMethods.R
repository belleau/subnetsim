#' @title Fix seed value.
#'
#' @description Fix seed value using random value when specified value is -1.
#
#' @param vSeed a \code{integer}, a seed used when reproducible results are
#' needed. When a value inferior or equal to zero is given, a random integer
#' is used.
#'
#' @return a \code{double}, the seed value.
#'
#' @examples
#'
#' ## Return vSeed value when value is superior to zero
#' subnetsim:::fixSeed(vSeed = 10)
#'
#' ## Return new value when value is inferior to zero
#' subnetsim:::fixSeed(vSeed = -1)
#'
#' @author Pascal Belleau, Astrid Deschenes
#' @keywords internal

fixSeed <- function(vSeed) {
    if (vSeed <= 0) {
        tSeed <- as.numeric(Sys.time())
        vSeed <- 1e8 * (tSeed - floor(tSeed))
    }
    return(vSeed)
}
