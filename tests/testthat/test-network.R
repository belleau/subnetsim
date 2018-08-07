### Unit tests for network.R functions

library(subnetsim)

networkFile <- system.file("extdata", "demo_network.sif",  package="subnetsim")

### Tests network() results


context("network() results")


test_that("network() must return error when fileName is not supported", {
     fileType <- "tif"
     error_message <- paste0("File type ", fileType, " is not supported")

     expect_error(network(netFileName = "test.tif", fileType = fileType,
                          subNetFileName=NULL), error_message)
})

test_that("network() returns predicted results ", {
    netTest <- network(netFileName = networkFile, fileType = "sif",
                       subNetFileName=NULL)

    expect_null(netTest$nodesSubNet)
    expect_equal(length(netTest$nodesAll), 313)
    expect_true(is.hash(netTest$netAll))
})
