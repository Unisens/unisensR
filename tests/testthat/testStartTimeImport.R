context("Unisens StartTime import")

test_that("imported example StartTime is as expected", {
  startTimeRef <- as.POSIXct("2008-07-04 13:27:57 CEST")
  startTimeImp <- readUnisensStartTime('../unisensExample')
  expect_equal(startTimeImp, startTimeRef)
})

test_that("Non exsisting unisens xml-file is causing error", {
  expect_error(readUnisensStartTime('../inst/extdata/unisensExample'),"Folder does not contain Unisens data!")
  expect_error(readUnisensStartTime('../../inst/extdata'),"Folder does not contain Unisens data!")
})
