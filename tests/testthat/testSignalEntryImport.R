context("Unisens signalEntry import")

test_that("imported example acc data (bin, int16) is as expected", {
  dimOfAccSignalRef <- c(19200,3)
  sumOfAccSignalRef <- 28238.9614257812
  acc <- readUnisensSignalEntry('../unisensExample','acc.bin')
  expect_equal(dim(acc), dimOfAccSignalRef)
  expect_equal(sum(acc), sumOfAccSignalRef)
})

test_that("imported example ecg data (bin, int32) is as expected", {
  dimOfEcgSignalRef <- c(60000,1)
  sumOfEcgSignalRef <- -1096
  ecg <- readUnisensSignalEntry('../unisensExample','ecg.bin')
  expect_equal(dim(ecg), dimOfEcgSignalRef)
  expect_equal(sum(ecg), sumOfEcgSignalRef)
})

test_that("imported example temperature data (csv, double) is as expected", {
  dimOfTempSignalRef <- c(5,1)
  sumOfTempSignalRef <- 158.605
  tempMean <- readUnisensSignalEntry('../unisensExample','TempMean.csv')
  expect_equal(dim(tempMean), dimOfTempSignalRef)
  expect_equal(sum(tempMean), sumOfTempSignalRef)
})

test_that("Non exsisting SignalEntry is causing error", {
  expect_error(readUnisensSignalEntry('../unisensExample','egg.bin'),"No SignalEntry found with name egg.bin")
  expect_error(readUnisensSignalEntry('../unisensExample','ecg.csv'),"No SignalEntry found with name ecg.csv")
})

test_that("Non exsisting unisens xml-file is causing error", {
  expect_error(readUnisensSignalEntry('../inst/extdata/unisensExample','ecg.bin'),"Folder does not contain Unisens data!")
  expect_error(readUnisensSignalEntry('../../inst/extdata','acc.bin'),"Folder does not contain Unisens data!")
})
