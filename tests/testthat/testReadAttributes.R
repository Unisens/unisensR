context("Unisens Read Attributes")

test_that("measurementId is as expected", {
  mId <- getUnisensMeasurementId('../unisensExample')
  expect_equal(mId, '#20080704001')
})

test_that("custom attributes are as expected", {
  ca <- getUnisensCustomAttributes('../unisensExample')
  expect_equal(ca[['personId']], 'Participant 1')
})
