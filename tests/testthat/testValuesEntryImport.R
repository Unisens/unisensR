context("Unisens ValuesEntry import")

test_that("imported example rr data (csv, int32) is as expected", {
  dimOfRrValuesRef <- c(3,3)
  datetimeOneRef <- as.POSIXct("2008-07-04 13:51:43 CEST")
  sumOfBpValuesRef <- 660
  rr <- readUnisensValuesEntry('../unisensExample','rr.csv')
  expect_equal(dim(rr), dimOfRrValuesRef)
  expect_equal(rr["Time"][1,1], datetimeOneRef)
  expect_equal(sum(rr[c("Systolic","Diastolic")]), sumOfBpValuesRef)
})

test_that("Non exsisting ValuesEntry is causing error", {
  expect_error(readUnisensValuesEntry('../unisensExample','egg.bin'),"No ValuesEntry found with name egg.bin")
  expect_error(readUnisensValuesEntry('../unisensExample','ecg.csv'),"No ValuesEntry found with name ecg.csv")
})

test_that("Non exsisting unisens xml-file is causing error", {
  expect_error(readUnisensValuesEntry('../inst/extdata/unisensExample','ecg.bin'),"Folder does not contain Unisens data!")
  expect_error(readUnisensValuesEntry('../../inst/extdata','acc.bin'),"Folder does not contain Unisens data!")
})
