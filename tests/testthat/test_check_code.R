context("check_code")
library(codebreaker)

test_that("cb_check_code() checks code", {

  expect_equal(codebreaker:::cb_check_code("RBGY", "RBGY"), list(all=4,color=0))
  expect_equal(codebreaker:::cb_check_code("BGYM", "BGYM"), list(all=4,color=0))

  expect_equal(codebreaker:::cb_check_code("RRBB", "RBGY"), list(all=1,color=1))
  expect_equal(codebreaker:::cb_check_code("RBRB", "RBGY"), list(all=2,color=0))
  expect_equal(codebreaker:::cb_check_code("YGBR", "RBGY"), list(all=0,color=4))

  expect_equal(codebreaker:::cb_check_code("RBGX", "RBGY"), list(all=3,color=0))
  expect_equal(codebreaker:::cb_check_code("XXXX", "RBGY"), list(all=0,color=0))

  expect_equal(codebreaker:::cb_check_code("RXXB", "RXXR"), list(all=1,color=0))
  expect_equal(codebreaker:::cb_check_code("RXXB", "RXBX"), list(all=1,color=1))
  expect_equal(codebreaker:::cb_check_code("RXXB", "RXBB"), list(all=2,color=0))
  
})