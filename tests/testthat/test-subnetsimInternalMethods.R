### Unit tests for subnetsimInternalMethods.R functions

library(subnetsim)

### Tests fixSeed() results

context("fixSeed() results")

y_good <- c(10,20,30,40)
x_good <- data.frame(x1=c(2,3,4,5), x2=c(33,44,55,33))

test_that("fixSeed(positive integer) must return the same integer", {
    expect_equal(fixSeed(3554), 3554)
})
