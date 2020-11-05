context("clean_code")
library(codebreaker)

test_that("cb_clean_code() cleans code", {

  expect_equal(codebreaker:::cb_clean_code("RBGY"), "RBGY")
  expect_equal(codebreaker:::cb_clean_code("BGYM"), "BGYM")

  expect_equal(codebreaker:::cb_clean_code("rbgy"), "RBGY")
  expect_equal(codebreaker:::cb_clean_code("bgym"), "BGYM")
  
  expect_equal(codebreaker:::cb_clean_code("RXXX"), "RXXX")
  expect_equal(codebreaker:::cb_clean_code("rxxx"), "RXXX")

  expect_equal(codebreaker:::cb_clean_code("RBGY "), "RBGY")
  expect_equal(codebreaker:::cb_clean_code(" RBGY"), "RBGY")
  expect_equal(codebreaker:::cb_clean_code("R B G Y"), "RBGY")
  expect_equal(codebreaker:::cb_clean_code("R.B.G.Y"), "RBGY")
  expect_equal(codebreaker:::cb_clean_code(" R B G Y "), "RBGY")
  
  expect_equal(codebreaker:::cb_clean_code(""), "XXXX")

})