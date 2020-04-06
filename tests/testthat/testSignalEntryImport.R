context("Unisens signalEntry import")

test_that("imported example acc data (bin, int16) is as expected", {
  dimOfAccSignalRef <- c(19200,3)
  sumOfAccSignalRef <- 28238.9614257812
  accX1stRef <- 0.2294921875
  accY1stRef <- 0.81640625
  accZ1stRef <- 0.54638671875
  sumOfAccLastSampleRef <- 1.6904296875

  acc <- readUnisensSignalEntry('../unisensExample','acc.bin')

  expect_equal(dim(acc), dimOfAccSignalRef)
  expect_equal(sum(acc), sumOfAccSignalRef)
  expect_equal(acc$accX[1], accX1stRef)
  expect_equal(acc$accY[1], accY1stRef)
  expect_equal(acc$accZ[1], accZ1stRef)
  expect_equal(sum(acc[nrow(acc),]), sumOfAccLastSampleRef)
})

test_that("by chunk imported example acc data (bin, int16) is as expected", {
  dimOfAccSignalRef <- c(19200,3)
  sumOfAccSignalRef <- 28238.9614257812
  accX1stRef <- 0.2294921875
  accY1stRef <- 0.81640625
  accZ1stRef <- 0.54638671875
  sumOfAccLastSampleRef <- 1.6904296875

  acc <- readUnisensSignalEntry('../unisensExample','acc.bin', readInChunks = TRUE)

  expect_equal(dim(acc), dimOfAccSignalRef)
  expect_equal(sum(acc), sumOfAccSignalRef)
  expect_equal(acc$accX[1], accX1stRef)
  expect_equal(acc$accY[1], accY1stRef)
  expect_equal(acc$accZ[1], accZ1stRef)
  expect_equal(sum(acc[nrow(acc),]), sumOfAccLastSampleRef)
})

test_that("by chunk imported example acc data (bin, int16) is as expected, chunkSize: 32", {
  dimOfAccSignalRef <- c(19200,3)
  sumOfAccSignalRef <- 28238.9614257812
  accX1stRef <- 0.2294921875
  accY1stRef <- 0.81640625
  accZ1stRef <- 0.54638671875
  sumOfAccLastSampleRef <- 1.6904296875

  acc <- readUnisensSignalEntry('../unisensExample','acc.bin', readInChunks = TRUE, readChunkSize = 32)

  expect_equal(dim(acc), dimOfAccSignalRef)
  expect_equal(sum(acc), sumOfAccSignalRef)
  expect_equal(acc$accX[1], accX1stRef)
  expect_equal(acc$accY[1], accY1stRef)
  expect_equal(acc$accZ[1], accZ1stRef)
  expect_equal(sum(acc[nrow(acc),]), sumOfAccLastSampleRef)
})

test_that("example acc data (bin, int16) import with specified start and end index works as expected", {
  dimOfAccSignalRef <- c(9,3)
  sumOfAccSignalRef <- 14.3037109375
  accX1stRef <- 0.23095703125
  accY1stRef <- 0.818359375
  accZ1stRef <- 0.544921875
  sumOfAccLastSampleRef <- 1.59130859375

  acc <- readUnisensSignalEntry('../unisensExample','acc.bin', startIndex = 8, endIndex = 16)

  expect_equal(dim(acc), dimOfAccSignalRef)
  expect_equal(sum(acc), sumOfAccSignalRef)
  expect_equal(acc$accX[1], accX1stRef)
  expect_equal(acc$accY[1], accY1stRef)
  expect_equal(acc$accZ[1], accZ1stRef)
  expect_equal(sum(acc[nrow(acc),]), sumOfAccLastSampleRef)
})

test_that("example acc data (bin, int16) by chunk import with specified start and end index works as expected", {
  dimOfAccSignalRef <- c(9,3)
  sumOfAccSignalRef <- 14.3037109375
  accX1stRef <- 0.23095703125
  accY1stRef <- 0.818359375
  accZ1stRef <- 0.544921875
  sumOfAccLastSampleRef <- 1.59130859375

  acc <- readUnisensSignalEntry('../unisensExample','acc.bin', startIndex = 8, endIndex = 16, readInChunks = TRUE)

  expect_equal(dim(acc), dimOfAccSignalRef)
  expect_equal(sum(acc), sumOfAccSignalRef)
  expect_equal(acc$accX[1], accX1stRef)
  expect_equal(acc$accY[1], accY1stRef)
  expect_equal(acc$accZ[1], accZ1stRef)
  expect_equal(sum(acc[nrow(acc),]), sumOfAccLastSampleRef)
})

test_that("example acc data (bin, int16) by chunk import with specified start and end index works as expected, chunkSize: 4", {
  dimOfAccSignalRef <- c(9,3)
  sumOfAccSignalRef <- 14.3037109375
  accX1stRef <- 0.23095703125
  accY1stRef <- 0.818359375
  accZ1stRef <- 0.544921875
  sumOfAccLastSampleRef <- 1.59130859375

  acc <- readUnisensSignalEntry('../unisensExample','acc.bin', startIndex = 8, endIndex = 16, readInChunks = TRUE, readChunkSize = 4)

  expect_equal(dim(acc), dimOfAccSignalRef)
  expect_equal(sum(acc), sumOfAccSignalRef)
  expect_equal(acc$accX[1], accX1stRef)
  expect_equal(acc$accY[1], accY1stRef)
  expect_equal(acc$accZ[1], accZ1stRef)
  expect_equal(sum(acc[nrow(acc),]), sumOfAccLastSampleRef)
})

test_that("imported example acc data (csv) is as expected", {
  dimOfAccSignalRef <- c(19200,3)
  sumOfAccSignalRef <- 28238.9614257812
  accX1stRef <- 0.2294921875
  accY1stRef <- 0.81640625
  accZ1stRef <- 0.54638671875
  sumOfAccLastSampleRef <- 1.6904296875

  acc <- readUnisensSignalEntry('../unisensExample','acc.csv')

  expect_equal(dim(acc), dimOfAccSignalRef)
  expect_equal(sum(acc), sumOfAccSignalRef)
  expect_equal(acc$accX[1], accX1stRef)
  expect_equal(acc$accY[1], accY1stRef)
  expect_equal(acc$accZ[1], accZ1stRef)
  expect_equal(sum(acc[nrow(acc),]), sumOfAccLastSampleRef)
})

test_that("by chunk imported example acc data (csv) is as expected", {
  dimOfAccSignalRef <- c(19200,3)
  sumOfAccSignalRef <- 28238.9614257812
  accX1stRef <- 0.2294921875
  accY1stRef <- 0.81640625
  accZ1stRef <- 0.54638671875
  sumOfAccLastSampleRef <- 1.6904296875

  acc <- readUnisensSignalEntry('../unisensExample','acc.csv', readInChunks = TRUE)

  expect_equal(dim(acc), dimOfAccSignalRef)
  expect_equal(sum(acc), sumOfAccSignalRef)
  expect_equal(acc$accX[1], accX1stRef)
  expect_equal(acc$accY[1], accY1stRef)
  expect_equal(acc$accZ[1], accZ1stRef)
  expect_equal(sum(acc[nrow(acc),]), sumOfAccLastSampleRef)
})

test_that("by chunk imported example acc data (csv) is as expected, chunkSize: 32", {
  dimOfAccSignalRef <- c(19200,3)
  sumOfAccSignalRef <- 28238.9614257812
  accX1stRef <- 0.2294921875
  accY1stRef <- 0.81640625
  accZ1stRef <- 0.54638671875
  sumOfAccLastSampleRef <- 1.6904296875

  acc <- readUnisensSignalEntry('../unisensExample','acc.csv', readInChunks = TRUE, readChunkSize = 32)

  expect_equal(dim(acc), dimOfAccSignalRef)
  expect_equal(sum(acc), sumOfAccSignalRef)
  expect_equal(acc$accX[1], accX1stRef)
  expect_equal(acc$accY[1], accY1stRef)
  expect_equal(acc$accZ[1], accZ1stRef)
  expect_equal(sum(acc[nrow(acc),]), sumOfAccLastSampleRef)
})

test_that("example acc data (csv) import with specified start and end index works as expected", {
  dimOfAccSignalRef <- c(9,3)
  sumOfAccSignalRef <- 14.3037109375
  accX1stRef <- 0.23095703125
  accY1stRef <- 0.818359375
  accZ1stRef <- 0.544921875
  sumOfAccLastSampleRef <- 1.59130859375

  acc <- readUnisensSignalEntry('../unisensExample','acc.csv', startIndex = 8, endIndex = 16)

  expect_equal(dim(acc), dimOfAccSignalRef)
  expect_equal(sum(acc), sumOfAccSignalRef)
  expect_equal(acc$accX[1], accX1stRef)
  expect_equal(acc$accY[1], accY1stRef)
  expect_equal(acc$accZ[1], accZ1stRef)
  expect_equal(sum(acc[nrow(acc),]), sumOfAccLastSampleRef)
})

test_that("example acc data (csv) by chunk import with specified start and end index works as expected", {
  dimOfAccSignalRef <- c(9,3)
  sumOfAccSignalRef <- 14.3037109375
  accX1stRef <- 0.23095703125
  accY1stRef <- 0.818359375
  accZ1stRef <- 0.544921875
  sumOfAccLastSampleRef <- 1.59130859375

  acc <- readUnisensSignalEntry('../unisensExample','acc.csv', startIndex = 8, endIndex = 16, readInChunks = TRUE)

  expect_equal(dim(acc), dimOfAccSignalRef)
  expect_equal(sum(acc), sumOfAccSignalRef)
  expect_equal(acc$accX[1], accX1stRef)
  expect_equal(acc$accY[1], accY1stRef)
  expect_equal(acc$accZ[1], accZ1stRef)
  expect_equal(sum(acc[nrow(acc),]), sumOfAccLastSampleRef)
})

test_that("example acc data (csv) by chunk import with specified start and end index works as expected, chunkSize: 3", {
  dimOfAccSignalRef <- c(9,3)
  sumOfAccSignalRef <- 14.3037109375
  accX1stRef <- 0.23095703125
  accY1stRef <- 0.818359375
  accZ1stRef <- 0.544921875
  sumOfAccLastSampleRef <- 1.59130859375

  acc <- readUnisensSignalEntry('../unisensExample','acc.csv', startIndex = 8, endIndex = 16, readInChunks = TRUE, readChunkSize = 3)

  expect_equal(dim(acc), dimOfAccSignalRef)
  expect_equal(sum(acc), sumOfAccSignalRef)
  expect_equal(acc$accX[1], accX1stRef)
  expect_equal(acc$accY[1], accY1stRef)
  expect_equal(acc$accZ[1], accZ1stRef)
  expect_equal(sum(acc[nrow(acc),]), sumOfAccLastSampleRef)
})

test_that("imported example ecg data (bin, int32) is as expected", {
  dimOfEcgSignalRef <- c(60000,1)
  sumOfEcgSignalRef <- -1096
  ecg <- readUnisensSignalEntry('../unisensExample','ecg.bin')
  expect_equal(dim(ecg), dimOfEcgSignalRef)
  expect_equal(sum(ecg), sumOfEcgSignalRef)
})

test_that("by chunk imported example ecg data (bin, int32) is as expected", {
  dimOfEcgSignalRef <- c(60000,1)
  sumOfEcgSignalRef <- -1096
  ecg <- readUnisensSignalEntry('../unisensExample','ecg.bin', readInChunks = TRUE)
  expect_equal(dim(ecg), dimOfEcgSignalRef)
  expect_equal(sum(ecg), sumOfEcgSignalRef)
})

test_that("by chunk imported example ecg data (bin, int32) is as expected, readChunkSize smaller sample size", {
  dimOfEcgSignalRef <- c(60000,1)
  sumOfEcgSignalRef <- -1096
  ecg <- readUnisensSignalEntry('../unisensExample','ecg.bin', readInChunks = TRUE, readChunkSize = 128)
  expect_equal(dim(ecg), dimOfEcgSignalRef)
  expect_equal(sum(ecg), sumOfEcgSignalRef)
})

test_that("by chunk imported example ecg data (bin, int32) is as expected, readChunkSize equal to sample size", {
  dimOfEcgSignalRef <- c(60000,1)
  sumOfEcgSignalRef <- -1096
  ecg <- readUnisensSignalEntry('../unisensExample','ecg.bin', readInChunks = TRUE, readChunkSize = 60000)
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

test_that("by chunk imported example temperature data (csv, double) is as expected", {
  dimOfTempSignalRef <- c(5,1)
  sumOfTempSignalRef <- 158.605
  tempMean <- readUnisensSignalEntry('../unisensExample','TempMean.csv', readInChunks = TRUE)
  expect_equal(dim(tempMean), dimOfTempSignalRef)
  expect_equal(sum(tempMean), sumOfTempSignalRef)
})

test_that("by chunk imported example temperature data (csv, double) is as expected, readChunkSize = 2", {
  dimOfTempSignalRef <- c(5,1)
  sumOfTempSignalRef <- 158.605
  tempMean <- readUnisensSignalEntry('../unisensExample','TempMean.csv', readInChunks = TRUE, readChunkSize = 2)
  expect_equal(dim(tempMean), dimOfTempSignalRef)
  expect_equal(sum(tempMean), sumOfTempSignalRef)
})

test_that("by chunk imported example temperature data (csv, double) is as expected, readChunkSize = 1", {
  dimOfTempSignalRef <- c(5,1)
  sumOfTempSignalRef <- 158.605
  tempMean <- readUnisensSignalEntry('../unisensExample','TempMean.csv', readInChunks = TRUE, readChunkSize = 1)
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

test_that("startIndex less than one is causing error", {
  expect_error(readUnisensSignalEntry('../unisensExample','ecg.bin', startIndex = 0, endIndex = 5),"startIndex out of bounds.")
})

test_that("startIndex greater than signal sample count is causing error", {
  expect_error(readUnisensSignalEntry('../unisensExample','ecg.bin', startIndex = 60001, endIndex = 60002),"startIndex out of bounds.")
})

test_that("endIndex greater than signal sample count is causing error", {
  expect_error(readUnisensSignalEntry('../unisensExample','ecg.bin', startIndex = 60000, endIndex = 60002),"endIndex out of bounds.")
})

test_that("startIndex greater than endIndex is causing error", {
  expect_error(readUnisensSignalEntry('../unisensExample','ecg.bin', startIndex = 6, endIndex = 5),"endIndex has to be greater or equal to startIndex.")
})

test_that("readChunkSize smaller than one is causing error", {
  expect_error(readUnisensSignalEntry('../unisensExample','ecg.bin', readChunkSize = 0),"readChunkSize has to be greater than zero!")
})
