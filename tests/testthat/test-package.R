test_that("package namespace loads", {
  expect_true(isNamespaceLoaded("alchemeR") || requireNamespace("alchemeR", quietly = TRUE))
})
