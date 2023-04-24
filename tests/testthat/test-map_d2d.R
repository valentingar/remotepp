test_that("map_d2d works", {
  test_data <- readRDS(testthat::test_path("fixtures", "test_data.rds"))

  current_dir <- paste0(tempdir(), "/testthat_map_d2d/")
  current_dir <- gsub("\\", "/", current_dir, fixed = TRUE)
  unlink(current_dir, recursive = TRUE, force = TRUE)
  dir.create(current_dir)

  terra::writeRaster(terra::rast(test_data),
                     paste0(current_dir,"test_data1.tif"),
                      overwrite = TRUE)
  terra::writeRaster(terra::rast(test_data),
                     paste0(current_dir,"test_data2.tif"),
                      overwrite = TRUE)

  my_files <- list.files(current_dir, full.names = TRUE)

  out_dir <- paste0(current_dir,"out_dir/")
  dir.create(out_dir)

  map_d2d(my_files,
             raster::stack,
             function(x) x,
             function(x, y) raster::writeRaster(x, y, format = "GTiff"),
             out_dir,
             overwrite = TRUE
             )

  expected_files <- paste0(out_dir, basename(my_files))

  expect_equal(list.files(out_dir, full.names = TRUE),
               expected_files)


})
