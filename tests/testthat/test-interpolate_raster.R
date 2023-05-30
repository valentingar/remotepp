test_that("interpolation works", {
  test_data <- readRDS(testthat::test_path("fixtures", "test_data.rds"))

  current_dir <- paste0(tempdir(), "/testthat_interpolate_raster/")
  current_dir <- gsub("\\", "/", current_dir, fixed = TRUE)
  unlink(current_dir, recursive = TRUE, force = TRUE)
  dir.create(current_dir)

  terra::writeRaster(terra::rast(test_data),
                     paste0(current_dir,"test_data1.tif"),
                     overwrite = TRUE)
  terra::writeRaster((terra::rast(test_data)*2),
                     paste0(current_dir,"test_data2.tif"),
                     overwrite = TRUE)

  my_files <- list.files(current_dir, full.names = TRUE, pattern = ".tif")

  out_dir <- paste0(current_dir,"out_dir/")
  dir.create(out_dir)

  interpolate_raster(my_files,
                     c(2,4),
                     c(2,3,4),
                     out_dir = out_dir,
                     overwrite = TRUE
                     )

  stack1 <- raster::stack(paste0(current_dir, "test_data1.tif"), bands = 1)
  stack1_new <- raster::stack(paste0(out_dir, "2.tif"), bands = 1)

  stack2_new <- raster::stack(paste0(out_dir, "3.tif"), bands = 1)

  expect_equal(raster::values(stack1_new), raster::values(stack1_new))
  expect_equal(round(raster::values(stack2_new), 2), round(raster::values(stack1)*1.5, 2))


  })
