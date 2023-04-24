#' test data for unit testing during package development


set.seed(42)
red_layer <- matrix(rnorm(36, 800, 100), nrow = 6)
blue_layer <- matrix(rnorm(36, 800, 100), nrow = 6)
green_layer <- matrix(rnorm(36, 800, 100), nrow = 6)
nearinfrared_layer <- matrix(rnorm(36, 800, 100), nrow = 6)

red_layer <- raster::raster(red_layer)
blue_layer <- raster::raster(blue_layer)
green_layer <- raster::raster(green_layer)
nearinfrared_layer <- raster::raster(nearinfrared_layer)

test_data <-
  raster::stack(
    list(red_layer,
         blue_layer,
         green_layer,
         nearinfrared_layer))

names(test_data) <- c("red", "green", "blue", "NI")

saveRDS(test_data,"tests/testthat/fixtures/test_data.rds")
