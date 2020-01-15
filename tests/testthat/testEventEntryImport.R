context("Unisens EventEntry import")

test_that("imported example qrs-trigger data (csv) is as expected", {
  dimOfQrsEventsRef <- c(394,3)
  datetimeFirstRef <- as.POSIXct("2008-07-04 13:27:57.714 CEST")
  datetimeLastRef <- as.POSIXct("2008-07-04 13:32:56.370 CEST")
  qrs <- readUnisensEventEntry('../unisensExample','qrs-trigger.csv')
  expect_equal(dim(qrs), dimOfQrsEventsRef)
  expect_equal(qrs["Time"][1,1], datetimeFirstRef)
  expect_equal(qrs["Time"][394,1], datetimeLastRef)
})

test_that("Non exsisting EventEntry is causing error", {
  expect_error(readUnisensEventEntry('../unisensExample','egg.bin'),"No EventEntry found with name egg.bin")
  expect_error(readUnisensEventEntry('../unisensExample','ecg.csv'),"No EventEntry found with name ecg.csv")
})

test_that("Non exsisting unisens xml-file is causing error", {
  expect_error(readUnisensEventEntry('../inst/extdata/unisensExample','ecg.bin'),"Folder does not contain Unisens data!")
  expect_error(readUnisensEventEntry('../../inst/extdata','acc.bin'),"Folder does not contain Unisens data!")
})
