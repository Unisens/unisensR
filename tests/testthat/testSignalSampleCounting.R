context("Unisens signal sample counting")

test_that("example acc data (bin, int16) is counted correct", {
  accSignalSampleCountRef <- 19200
  accSignalSampleCount <- getUnisensSignalSampleCount('../unisensExample','acc.bin')
  expect_equal(accSignalSampleCount, accSignalSampleCountRef)
})

test_that("example ecg data (bin, int32) is counted correct", {
  ecgSignalSampleCountRef <- 60000
  ecgSignalSampleCount <- getUnisensSignalSampleCount('../unisensExample','ecg.bin')
  expect_equal(ecgSignalSampleCount, ecgSignalSampleCountRef)
})

test_that("example temperature data (csv, double) is counted correct", {
  tempMeanSignalSampleCountRef <- 5
  tempMeanSignalSampleCount <- getUnisensSignalSampleCount('../unisensExample','TempMean.csv')
  expect_equal(tempMeanSignalSampleCount, tempMeanSignalSampleCountRef)
})

test_that("Non exsisting SignalEntry is causing error", {
  expect_error(getUnisensSignalSampleCount('../unisensExample','egg.bin'),"No SignalEntry found with name egg.bin")
  expect_error(getUnisensSignalSampleCount('../unisensExample','ecg.csv'),"No SignalEntry found with name ecg.csv")
})

test_that("Non exsisting unisens xml-file is causing error", {
  expect_error(getUnisensSignalSampleCount('../inst/extdata/unisensExample','ecg.bin'),"Folder does not contain Unisens data!")
  expect_error(getUnisensSignalSampleCount('../../inst/extdata','acc.bin'),"Folder does not contain Unisens data!")
})
