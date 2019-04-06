context("test-myparallel")

test_that("myparallel correctly produces errors", {
  # invalid data
  expect_error(myparallel(data = list(a = 1:10, b = 10:1)))
  # jitter = F, when jittervaribles has specified
  expect_error(myparallel(data = data.frame(a = rnorm(100), b = rnorm(100)),
                          jitter = F, jittervariables = "a"))
  # invalid columns
  expect_error(myparallel(data = data.frame(a = rnorm(100), b = rnorm(100)),
                          columns = list("a")))
  # jittervaribles not in columns
  expect_error(myparallel(data = data.frame(a = rnorm(100),
                                            b = rnorm(100),
                                            c = rnorm(100)),
                          columns = 1:2,
                          jitter = T, jittervariables = "c"))
})

test_that("myparallel works", {
  # works in default
  expect_is(myparallel(data = data.frame(a = rnorm(100),
                                         b = rnorm(100),
                                         c = rnorm(100))), "ggplot")

  # works with selected columns
  expect_is(myparallel(data = data.frame(a = rnorm(100),
                                         b = rnorm(100),
                                         c = rnorm(100)),
                       columns = 1:2), "ggplot")
  # works with jitter
  expect_is(myparallel(data = data.frame(a = rnorm(100),
                                         b = rnorm(100),
                                         c = rnorm(100)),
                       jitter = TRUE), "ggplot")
  # works with color
  expect_is(myparallel(data = data.frame(a = rnorm(100),
                                         b = rnorm(100),
                                         c = rnorm(100),
                                         d = rep(letters[1:4], 25)),
                       columns = 1:3, groupColumn = "d"), "ggplot")

  # works with color, jitter, position and index
  expect_is(myparallel(data = data.frame(a = rnorm(100),
                                         b = rnorm(100),
                                         c = rnorm(100),
                                         d = rep(letters[1:4], 25)),
                       columns = 1:3, groupColumn = "d",
                       jitter = TRUE, jittervariables = "c"), "ggplot")
})
