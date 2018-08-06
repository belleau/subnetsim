### Unit tests for subnetwork.R functions

library(subnetsim)

load(system.file("extdata", "demo_netAll.RData",
                 package = "subnetsim"))
load(system.file("extdata", "demo_netWithoutSub.RData",
                 package = "subnetsim"))


### Tests subnetwork() parameters

context("print.subnetwork() parameters")

test_that("print.subnetwork() must print predicted results when seed fixed", {
    bla <- subnetwork(demo_netAll, nbIter=4, nbNodes=33,
                      nbLink=40, nbNodesOneLink=55,
                      nbLinkOneLink=70, seedV=14533)
    g <- print(bla)
    error_message <- "print.subnetwork() does not return exected values"
    expect_equivalent(g, bla, tolerance = .002, scale=1, error_message)
})
