### Unit tests for subnetwork.R functions

library(subnetsim)

load(system.file("extdata", "IRGEN_netAll.RData",
                 package = "subnetsim"))

load(system.file("extdata", "IRGEN_nodesAll.RData",
                 package = "subnetsim"))

load(system.file("extdata", "IRGEN_netLink.RData",
                 package = "subnetsim"))

### Tests subnetwork() parameters

context("subnetwork() parameters")

test_that("subnetwork() must return error when seedV not integer", {
    error_message <- "'seedV' must be an integer"
    expect_error( subnetwork(network = IRGEN_netAll,
                            globalNetwork = IRGEN_netAll, nbIter = 3,
                            nbNodes = 10, seedV = "ALLO"), error_message)
    expect_error( subnetwork(network = IRGEN_netAll,
                            globalNetwork = IRGEN_netAll, nbIter = 3,
                            nbNodes = 10, seedV = c(2, 3)), error_message)
    expect_error( subnetwork(network = IRGEN_netAll,
                             globalNetwork = IRGEN_netAll, nbIter = 3,
                             nbNodes = 10, seedV = NA), error_message)
    expect_error( subnetwork(network = IRGEN_netAll,
                             globalNetwork = IRGEN_netAll, nbIter = 3,
                             nbNodes = 10, seedV = "3.3"), error_message)
})

test_that("subnetwork() must return error when nbNodes not positive integer", {
    error_message <- "'nbNodes' must be a positive integer"
    expect_error( subnetwork(network = IRGEN_netAll,
                             globalNetwork = IRGEN_netAll, nbIter = 3,
                             nbNodes = 0, seedV = 33), error_message)
    expect_error( subnetwork(network = IRGEN_netAll,
                             globalNetwork = IRGEN_netAll, nbIter = 3,
                             nbNodes = -1, seedV = 32), error_message)
    expect_error( subnetwork(network = IRGEN_netAll,
                             globalNetwork = IRGEN_netAll, nbIter = 3,
                             nbNodes = "ALLO", seedV = 333), error_message)
    expect_error( subnetwork(network = IRGEN_netAll,
                             globalNetwork = IRGEN_netAll, nbIter = 3,
                             nbNodes = c(2, 3), seedV = 33), error_message)
})