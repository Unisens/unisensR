context("Unisens signalEntry import")

test_that("imported example acc data (bin, int16) is as expected", {
  dimOfAccSignal <- c(19200,3)
  sumOfAccSignal <- 28238.9614257812
  acc <- readUnisensSignalEntry('../../inst/extdata/unisensExample','acc.bin')
  expect_equal(dim(acc), dimOfAccSignal)
  expect_equal(sum(acc), sumOfAccSignal)
})

test_that("imported example ecg data (bin, int32) is as expected", {
  dimOfEcgSignal <- c(60000,1)
  sumOfEcgSignal <- -1096
  ecg <- readUnisensSignalEntry('../../inst/extdata/unisensExample','ecg.bin')
  expect_equal(dim(ecg), dimOfEcgSignal)
  expect_equal(sum(ecg), sumOfEcgSignal)
})

test_that("imported example temperature data (csv, double) is as expected", {
  dimOfTempSignal <- c(5,1)
  sumOfTempSignal <- 158.605
  tempMean <- readUnisensSignalEntry('../../inst/extdata/unisensExample','TempMean.csv')
  expect_equal(dim(tempMean), dimOfTempSignal)
  expect_equal(sum(tempMean), sumOfTempSignal)
})

test_that("Non exsisting SignalEntry is causing error", {
  expect_error(readUnisensSignalEntry('../../inst/extdata/unisensExample','egg.bin'),"No SignalEntry found with name egg.bin")
  expect_error(readUnisensSignalEntry('../../inst/extdata/unisensExample','ecg.csv'),"No SignalEntry found with name ecg.csv")
})

test_that("Non exsisting unisens xml-file is causing error", {
  expect_error(readUnisensSignalEntry('../inst/extdata/unisensExample','ecg.bin'),"Folder does not contain Unisens data!")
  expect_error(readUnisensSignalEntry('../../inst/extdata','acc.bin'),"Folder does not contain Unisens data!")
})
