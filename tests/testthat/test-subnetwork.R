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

test_that(paste0("subnetwork() return predicted results when seed fixed"), {
    bla <- subnetwork(demo_netAll, nbIter=4, nbNodes=33,
                      nbLink=40, nbNodesOneLink=55,
                      nbLinkOneLink=70, seedV=145)
    expect_equal(bla$nbNodesOneLink,
                 c(153, 166, 145, 157))
    expect_equal(bla$nbLinkOneLink,
                 c(213, 257, 242, 205))
    expect_equal(bla$nbLink,
                 c(13, 13, 16, 8))
    expect_equal(bla$nbIter,
                 4)
    expect_equal(bla$nbNodes,
                 33)
    expect_equal(bla$nbNodesOneLinkTested,
                 55)
    expect_equal(bla$nbLinkOneLinkTested,
                 70)
    expect_equal(bla$nbLinkTested,
                 40)
    expect_equal(bla$seed,
                 145)
    bla <- subnetwork(demo_netAll, nbIter=4, seedV=145)
    expect_equal(bla$nbNodesOneLink,
                 c(243, 257, 256, 250))
    expect_equal(bla$nbLinkOneLink,
                 c(667, 677, 639, 608))
    expect_equal(bla$nbLink,
                 c(138, 145, 136, 114))
    expect_equal(bla$nbIter,
                 4)
    expect_equal(bla$nbNodes,
                 103)
    expect_equal(bla$nbNodesOneLinkTested,
                 275)
    expect_equal(bla$nbLinkOneLinkTested,
                 830)
    expect_equal(bla$nbLinkTested,
                 330)
    expect_equal(bla$seed,
                 145)

})

