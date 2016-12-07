library("curvematching")
context("calculate_matches()")

set.seed(234)
data <- datasets::ChickWeight

test_that("returns proper length", {
  expect_equal(length(calculate_matches(data, Time == 0, y_name = "sasa")), 0)
  expect_equal(length(calculate_matches(data, Time == 0 & Chick == 48, y_name = "weight")), 1)
  expect_equal(length(calculate_matches(data, Time == 0 & Diet == 2, y_name = "weight")), 10)
})

test_that("warns if no targets are found", {
  expect_warning(calculate_matches(data, Time == 0 & Chick == 2 & Diet == 2,
                                   y_name = "weight"))
})

test_that("warns about rank-deficient fit", {
  expect_warning(calculate_matches(data, Chick == 48 & Time == 0,
                                   y_name = "weight",
                                   x_name = c("Chick","Diet"),
                                   e_name = c("Time")))
})


context("extract_matches()")
set.seed(234)
z <- calculate_matches(data, Time == 0 & Chick == 48,
                       y_name = c("weight", "Time"), t_name = "Diet")

test_that("returns vector of 10 row indices", {
  expect_equal(extract_matches(z, y_name = "Time", t_name = "Diet", c_name = "3"),
               c(342, 369, 375, 411, 414, 423, 428, 438, 457, 459))
})

z <- calculate_matches(data, Chick == 2, y_name = c("weight", "Time"))
test_that("returns vector of 10 row indices", {
  expect_equal(extract_matches(z, i_name = "23", y_name = "Time"),
               c(128, 167, 243, 244, 245, 299, 407, 420, 472, 494))
})

#
# z <- calculate_matches(ChickWeight, list(Chick = 2), y_name = c("weight", "Time"), t_name = "Diet")
# extract_matches(z)
extract_matches(z, i_name = "23", y_name = "Time")
extract_matches(z, i_name = "23", y_name = "Time", t_name = "Diet", c_name = "3")
#
# z <- calculate_matches(ChickWeight, list(Chick = 2), y_name = c("weight", "Time"), t_name = "Time")
# extract_matches(z)
# extract_matches(z, i_name = "23", y_name = "Time")
# extract_matches(z, i_name = "23", y_name = "Time", t_name = "Time", c_name = "2")
