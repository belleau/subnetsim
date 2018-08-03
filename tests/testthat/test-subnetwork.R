### Unit tests for subnetwork.R functions

library(subnetsim)

load(system.file("extdata", "demo_netAll.RData",
                 package = "subnetsim"))

### Tests subnetwork() parameters

context("subnetwork() parameters")

test_that("subnetwork() must return error when seedV not integer", {
    error_message <- "'seedV' must be an integer"
    expect_error( subnetwork(network = demo_netAll,
                            nbIter = 3,
                            nbNodes = 10, seedV = "ALLO"), error_message)
    expect_error( subnetwork(network = demo_netAll,
                            nbIter = 3,
                            nbNodes = 10, seedV = c(2, 3)), error_message)
    expect_error( subnetwork(network = demo_netAll,
                             nbIter = 3,
                             nbNodes = 10, seedV = NA), error_message)
    expect_error( subnetwork(network = demo_netAll,
                             nbIter = 3,
                             nbNodes = 10, seedV = "3.3"), error_message)
})

test_that("subnetwork() must return error when nbNodes not positive integer", {
    error_message <- paste0("The parameters 'nbNodes', 'nbLink', 'nbNodesOneLink' and ",
                            "'nbLinkOneLink' must be a positive integer or a subnetwork ",
                            "must be present in the network object")
    expect_error( subnetwork(network = demo_netAll,
                             nbIter = 3,
                             nbNodes = 0, seedV = 33), error_message)
    expect_error( subnetwork(network = demo_netAll,
                             nbIter = 3,
                             nbNodes = -1, seedV = 32), error_message)
    expect_error( subnetwork(network = demo_netAll,
                             nbIter = 3,
                             nbNodes = "ALLO", seedV = 333), error_message)
    expect_error( subnetwork(network = demo_netAll,
                             nbIter = 3,
                             nbNodes = c(2, 3), seedV = 33), error_message)
})
