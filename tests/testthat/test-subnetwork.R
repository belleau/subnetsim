### Unit tests for subnetwork.R functions

library(subnetsim)

load(system.file("extdata", "demo_netAll.RData",
                 package = "subnetsim"))
load(system.file("extdata", "demo_netWithoutSub.RData",
                 package = "subnetsim"))


### Tests subnetwork() parameters

context("subnetwork() parameters")

test_that("subnetwork() must return error when seedV not integer", {
    error_message <- "'seedV' must be an integer"
    expect_error( subnetwork(network = demo_netAll,
                            nbIter = 3,
                            nbNodes = 10, nbLink = 1,
                            nbNodesOneLink = 1, nbLinkOneLink=1,
                            seedV = "ALLO"), error_message)
    expect_error( subnetwork(network = demo_netAll,
                            nbIter = 3,
                            nbNodes = 10, nbLink = 1,
                            nbNodesOneLink = 1, nbLinkOneLink=1,
                            seedV = c(2, 3)), error_message)
    expect_error( subnetwork(network = demo_netAll,
                             nbIter = 3,
                             nbNodes = 10, nbLink = 1,
                             nbNodesOneLink = 1, nbLinkOneLink=1,
                             seedV = NA), error_message)
    expect_error( subnetwork(network = demo_netAll,
                             nbIter = 3,
                             nbNodes = 10, nbLink = 1,
                             nbNodesOneLink = 1, nbLinkOneLink=1,
                             seedV = "3.3"), error_message)
})

test_that(paste0("subnetwork() must return error when nbNodes, nbLink, ",
                "nbNodesOneLink, and nbLinkOneLink not positive integer"), {
    error_message <- paste0("The parameters 'nbNodes', 'nbLink', 'nbNodesOneLink' and ",
                            "'nbLinkOneLink' must be a positive integer or a subnetwork ",
                            "must be present in the network object")
    expect_error( subnetwork(network = demo_netAll,
                             nbIter = 3,
                             nbNodes = 0, nbLink = 1,
                             nbNodesOneLink = 1, nbLinkOneLink=1,
                             seedV = 33), error_message)
    expect_error( subnetwork(network = demo_netAll,
                             nbIter = 3,
                             nbNodes = -1, nbLink = 1,
                             nbNodesOneLink = 1, nbLinkOneLink=1,
                             seedV = 32), error_message)
    expect_error( subnetwork(network = demo_netAll,
                             nbIter = 3,
                             nbNodes = "ALLO", nbLink = 1,
                             nbNodesOneLink = 1, nbLinkOneLink=1,
                             seedV = 333), error_message)
    expect_error( subnetwork(network = demo_netAll,
                             nbIter = 3,
                             nbNodes = c(2, 3), nbLink = 1,
                             nbNodesOneLink = 1, nbLinkOneLink=1,
                             seedV = 33), error_message)
    # Test nbLink
    expect_error( subnetwork(network = demo_netAll,
                             nbIter = 3,
                             nbNodes = 1, nbLink = 0,
                             nbNodesOneLink = 1, nbLinkOneLink=1,
                             seedV = 33), error_message)
    expect_error( subnetwork(network = demo_netAll,
                             nbIter = 3,
                             nbNodes = 0, nbLink = -1,
                             nbNodesOneLink = 1, nbLinkOneLink=1,
                             seedV = 32), error_message)
    expect_error( subnetwork(network = demo_netAll,
                             nbIter = 3,
                             nbNodes = 1, nbLink = "ALLO",
                             nbNodesOneLink = 1, nbLinkOneLink=1,
                             seedV = 333), error_message)
    expect_error( subnetwork(network = demo_netAll,
                             nbIter = 3,
                             nbNodes = 1, nbLink = c(2, 3),
                             nbNodesOneLink = 1, nbLinkOneLink=1,
                             seedV = 33), error_message)
    # test nbNodesOneLink
    expect_error( subnetwork(network = demo_netAll,
                             nbIter = 3,
                             nbNodes = 1, nbLink = 1,
                             nbNodesOneLink = 0, nbLinkOneLink=1,
                             seedV = 33), error_message)
    expect_error( subnetwork(network = demo_netAll,
                             nbIter = 3,
                             nbNodes = 1, nbLink = 1,
                             nbNodesOneLink = -1, nbLinkOneLink=1,
                             seedV = 32), error_message)
    expect_error( subnetwork(network = demo_netAll,
                             nbIter = 3,
                             nbNodes = 1, nbLink = 1,
                             nbNodesOneLink = "ALLO", nbLinkOneLink=1,
                             seedV = 333), error_message)
    expect_error( subnetwork(network = demo_netAll,
                             nbIter = 3,
                             nbNodes = 1, nbLink = 1,
                             nbNodesOneLink = c(2, 3), nbLinkOneLink=1,
                             seedV = 33), error_message)
    # Test nbLinkOneLink
    expect_error( subnetwork(network = demo_netAll,
                             nbIter = 3,
                             nbNodes = 1, nbLink = 1,
                             nbNodesOneLink = 1, nbLinkOneLink=0,
                             seedV = 33), error_message)
    expect_error( subnetwork(network = demo_netAll,
                             nbIter = 3,
                             nbNodes = 1, nbLink = 1,
                             nbNodesOneLink = 1, nbLinkOneLink=-1,
                             seedV = 32), error_message)
    expect_error( subnetwork(network = demo_netAll,
                             nbIter = 3,
                             nbNodes = 1, nbLink = 1,
                             nbNodesOneLink = 1, nbLinkOneLink="ALLO",
                             seedV = 333), error_message)
    expect_error( subnetwork(network = demo_netAll,
                             nbIter = 3,
                             nbNodes = 1, nbLink = 1,
                             nbNodesOneLink = 1, nbLinkOneLink=c(2, 3),
                             seedV = 33), error_message)

})

test_that(paste0("subnetwork() must return error when at least one of nbNodes, nbLink, ",
                 "nbNodesOneLink, and nbLinkOneLink are NULL and the object network",
                 " don't have subnetwork"), {
    error_message <- paste0("The parameters 'nbNodes', 'nbLink', 'nbNodesOneLink' and ",
                             "'nbLinkOneLink' must be a positive integer or a subnetwork ",
                             "must be present in the network object")
    expect_error( subnetwork(network = demo_netWithoutSub,
                              nbIter = 3,
                              nbLink = 1,
                              nbNodesOneLink = 1, nbLinkOneLink=1,
                              seedV = 33), error_message)
    expect_error( subnetwork(network = demo_netWithoutSub,
                              nbIter = 3,
                              nbNodes = -1,
                              nbNodesOneLink = 1, nbLinkOneLink=1,
                              seedV = 32), error_message)
    expect_error( subnetwork(network = demo_netWithoutSub,
                              nbIter = 3,
                              nbNodes = "ALLO", nbLink = 1,
                              nbLinkOneLink=1,
                              seedV = 333), error_message)
    expect_error( subnetwork(network = demo_netWithoutSub,
                              nbIter = 3,
                              nbNodes = c(2, 3), nbLink = 1,
                              nbNodesOneLink = 1,
                              seedV = 33), error_message)
})

# test_that(paste0("subnetwork() return predicted results when seed fixed"), {
#     expect_equal(subnetwork(demo_netAll, 4, 33, 40, 55, 70, 145),
#                  list(nbNodesOneLink=c(153, 166, 145, 157),
#                             nbLinkOneLink=c(213, 257, 242, 205),
#                             nbLink=c(13, 13, 16, 8),
#                             nbIter=3,
#                             nbNodes=33,
#                             nbNodesOneLink=70,
#                             nbLinkOneLink=70,
#                             nbLink=40,
#                             seed=145
#                             ))
# })

