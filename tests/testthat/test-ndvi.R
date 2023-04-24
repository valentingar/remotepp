test_that("multiplication works", {
  test_data <- readRDS(test_path("fixtures", "test_data.rds"))

  NDVI_manual <-
    (test_data[[4]] - test_data[[1]]) / (test_data[[4]] + test_data[[1]])

  NDVI <- ndvi(test_data, 1, 4)

  expect_equal(NDVI, NDVI_manual)

})
