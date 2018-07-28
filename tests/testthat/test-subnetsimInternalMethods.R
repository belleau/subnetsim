### Unit tests for subnetsimInternalMethods.R functions

library(subnetsim)

load(system.file("extdata", "demo_netAll.RData",
                    package = "subnetsim"))

load(system.file("extdata", "IRGEN_nodesAll.RData",
                                package = "subnetsim"))

load(system.file("extdata", "IRGEN_netLink.RData",
                                package = "subnetsim"))

### Tests fixSeed() results

context("fixSeed() results")

test_that("fixSeed(positive integer) must return the same integer", {
    expect_equal(fixSeed(3554), 3554)
    expect_equal(fixSeed(4), 4)
    expect_equal(fixSeed(324), 324)
})


### Tests simuleSubNet() results

context("simuleSubNet() results")

test_that("simuleSubNet() return predicted results when seed fixed", {
    expect_equal(simuleSubNet(demo_netAll, IRGEN_nodesAll, 4, 33, 145),
                 data.frame(nbNodes=c(153, 166, 145, 157),
                            nbLink=c(213, 257, 242, 205),
                            nbLinkSub=c(13, 13, 16, 8)))
    expect_equal(simuleSubNet(demo_netAll, IRGEN_nodesAll, as.integer(4),
                                as.integer(12), 145),
                 data.frame(nbNodes=c(80, 44, 62, 68),
                            nbLink=c(98, 44, 56, 78),
                            nbLinkSub=c(4, 0, 0, 3)))
    expect_equal(simuleSubNet(demo_netAll, IRGEN_nodesAll, 3, 12, 333),
                 data.frame(nbNodes=c(95, 76, 85),
                            nbLink=c(102, 79, 89),
                            nbLinkSub=c(1, 1, 1)))
    expect_equal(simuleSubNet(demo_netAll, IRGEN_nodesAll, 5, 22, 1111),
                 data.frame(nbNodes=c(122, 130, 118, 97, 132),
                            nbLink=c(177, 143, 166, 116, 179),
                            nbLinkSub=c(7, 3, 5, 4, 7)))
})

### Tests getOneLink() results

context("getOneLink() results")

test_that("getOneLink() return predicted results", {
    expect_equal(getOneLink(demo_netAll, IRGEN_nodesAll[1:9]), c(61, 61))
    expect_equal(getOneLink(demo_netAll, IRGEN_nodesAll[121:133]), c(80, 94))
    expect_equal(getOneLink(demo_netAll, IRGEN_nodesAll[155:203]), c(187, 313))
    expect_equal(getOneLink(demo_netAll, IRGEN_nodesAll[244:263]), c(88, 127))
})



### Tests getSubNet() results

context("getSubNet() results")

test_that("getSubNet() return predicted results", {
    expect_equal(getSubNet(demo_netAll, IRGEN_nodesAll[1:29]), 42)
    expect_equal(getSubNet(demo_netAll, IRGEN_nodesAll[121:163]), 32)
    expect_equal(getSubNet(demo_netAll, IRGEN_nodesAll[55:213]), 295)
    expect_equal(getSubNet(demo_netAll, IRGEN_nodesAll[204:273]), 70)
})



