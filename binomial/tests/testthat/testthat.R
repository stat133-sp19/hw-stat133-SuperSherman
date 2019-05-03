


library(testthat)
source("../../R/binomial.R")

context("check probability arguments")
test_that("the probabilty is valid", {
  expect_true(check_prob(0.8))
  expect_true(check_prob(0.5))
  expect_error(check_prob(1.2),"invalid prob value")
  expect_error(check_prob(-1),"invalid prob value")
})



context("check trials")
test_that("the trials are valid", {
  expect_true(check_trials(2))
  expect_true(check_trials(3))
  expect_error(check_trials(1.2),'invalid trials value')
  expect_error(check_trials(-1),'invalid trials value')
})



context("check success")
test_that("the number of success are valid", {
  expect_true(check_success(2,3))
  expect_error(check_success(1.2,3),"invalid success value")
  expect_error(check_success(5,3),"invalid success value")
  expect_error(check_success(-1,3),"invalid success value")
})


context("aux_mean")
test_that("the output of aux_mean is correct", {
  expect_equal(aux_mean(3,0.5), 1.5)
  expect_equal(aux_mean(4,0.6), 2.4)
  expect_equal(aux_mean(5,0.5), 2.5)
})


context("aux_variance")
test_that("the output of aux_variance is correct", {
  expect_equal(aux_variance(3,0.5), 0.75)
  expect_equal(aux_variance(4,0.6), 0.96)
  expect_equal(aux_variance(5,0.5), 1.25)
})

context("aux_mode")
test_that("the output of aux_mode is correct", {
  expect_equal(aux_mode(3,0.6), 2)
  expect_equal(aux_mode(4,0.7), 3)
  expect_equal(aux_mode(5,0.6), 3)
})

context("aux_skewness")
test_that("the output of aux_skewness is correct", {
  expect_equal(aux_skewness(3,0.5), (1-2*0.5)/sqrt(3*0.5*(1-0.5)))
  expect_equal(aux_skewness(4,0.6), (1-2*0.6)/sqrt(4*0.6*(1-0.6)))
  expect_equal(aux_skewness(5,0.5), (1-2*0.5)/sqrt(5*0.5*(1-0.5)))
})

context("check for aux_kurtosis")
test_that("the output of aux_kurtosis is correct", {
  expect_equal(aux_kurtosis(3,0.5), (1-6*0.5*(1-0.5))/(3*0.5*(1-0.5)))
  expect_equal(aux_kurtosis(4,0.6), (1-6*0.6*(1-0.6))/(4*0.6*(1-0.6)))
  expect_equal(aux_kurtosis(5,0.5), (1-6*0.5*(1-0.5))/(5*0.5*(1-0.5)))
})


context("check if the following binomial functions work")
test_that("the output of bin_choose is correct", {
  expect_error(bin_choose(5, 8),"success cannot be greater than trials")
  expect_equal(bin_choose(5,2), 10)
  expect_equal(bin_choose(5, 1:5),c(5,10,10,5,1))
})


test_that("the output of bin_probability is correct", {
  expect_error(bin_probability(success = 6, trials = 5, prob = 0.5),"invalid success value")
  expect_error(bin_probability(success = -6, trials = 5, prob = 0.5),"invalid success value")
  expect_error(bin_probability(success = 6, trials = -5, prob = 0.5),"invalid trials value")
  expect_error(bin_probability(success = 6, trials = 5, prob = 1.1),'invalid prob value')
  expect_equal(bin_probability(success = 2, trials = 5, prob = 0.5),0.3125)
  expect_equal(bin_probability(success = 0:2, trials = 5, prob = 0.5),c(0.03125,0.15625,0.31250))
})


test_that("the output of bin_distribution is correct", {
  expect_true(is.data.frame(bin_distribution(trials = 5, prob = 0.5)))
  expect_equal(bin_distribution(trials = 4, prob = 0.6)$success, 0:4)
  expect_equal(bin_distribution(trials = 4, prob = 0.6)$probability, bin_probability(0:4,4,0.6))
  #expect_true(class.bin...,c("bindis","data.frame"))
})


test_that("the output of bin_cumulative is correct", {
  expect_true(is.data.frame(bin_cumulative(trials = 5, prob = 0.5)))
  expect_equal(bin_cumulative(trials = 5, prob = 0.5)$success,0:5)
  expect_equal(bin_cumulative(trials = 5, prob = 0.5)$probability,bin_probability(0:5,5,0.5))
  expect_equal(bin_cumulative(trials = 5, prob = 0.5)$cumulative,cumsum(bin_probability(0:5,5,0.5)))
})
