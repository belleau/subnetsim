### Unit tests for subnetsimInternalMethods.R functions

library(subnetsim)

load(system.file("extdata", "demo_netAll.RData",
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
#data.frame(nbNodesOneLink=distSubNet[, 1], nbLinkOneLink=distSubNet[, 2] , nbLink=distSubNet[, 3])
test_that("simuleSubNet() return predicted results when seed fixed", {
    expect_equal(simuleSubNet(demo_netAll$netAll, demo_netAll$nodesAll, 4, 33, 145),
                 data.frame(nbNodesOneLink=c(153, 166, 145, 157),
                            nbLinkOneLink=c(213, 257, 242, 205),
                            nbLink=c(13, 13, 16, 8)))
    expect_equal(simuleSubNet(demo_netAll$netAll, demo_netAll$nodesAll, as.integer(4),
                                as.integer(12), 145),
                 data.frame(nbNodesOneLink=c(80, 44, 62, 68),
                            nbLinkOneLink=c(98, 44, 56, 78),
                            nbLink=c(4, 0, 0, 3)))
    expect_equal(simuleSubNet(demo_netAll$netAll, demo_netAll$nodesAll, 3, 12, 333),
                 data.frame(nbNodesOneLink=c(95, 76, 85),
                            nbLinkOneLink=c(102, 79, 89),
                            nbLink=c(1, 1, 1)))
    expect_equal(simuleSubNet(demo_netAll$netAll, demo_netAll$nodesAll, 5, 22, 1111),
                 data.frame(nbNodesOneLink=c(122, 130, 118, 97, 132),
                            nbLinkOneLink=c(177, 143, 166, 116, 179),
                            nbLink=c(7, 3, 5, 4, 7)))
})

### Tests getOneLink() results

context("getOneLink() results")

test_that("getOneLink() return predicted results", {
    expect_equal(getOneLink(demo_netAll$netAll, demo_netAll$nodesAll[1:9]), c(61, 61))
    expect_equal(getOneLink(demo_netAll$netAll, demo_netAll$nodesAll[121:133]), c(80, 94))
    expect_equal(getOneLink(demo_netAll$netAll, demo_netAll$nodesAll[155:203]), c(187, 313))
    expect_equal(getOneLink(demo_netAll$netAll, demo_netAll$nodesAll[244:263]), c(88, 127))
})



### Tests getSubNet() results

context("getSubNet() results")

test_that("getSubNet() return predicted results", {
    expect_equal(getSubNet(demo_netAll$netAll, demo_netAll$nodesAll[1:29]), 42)
    expect_equal(getSubNet(demo_netAll$netAll, demo_netAll$nodesAll[121:163]), 32)
    expect_equal(getSubNet(demo_netAll$netAll, demo_netAll$nodesAll[55:213]), 295)
    expect_equal(getSubNet(demo_netAll$netAll, demo_netAll$nodesAll[204:273]), 70)
})



