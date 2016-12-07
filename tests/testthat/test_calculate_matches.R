library("curvematching")
context("calculate_matches()")

set.seed(234)
data <- datasets::ChickWeight

test_that("returns proper length", {
  expect_equal(length(calculate_matches(data, list(Time = 0), y_name = "sasa")), 0)
  expect_equal(length(calculate_matches(data, list(Time = 0, Chick = 48), y_name = "weight")), 1)
  expect_equal(length(calculate_matches(data, list(Time = 0, Diet = 2), y_name = "weight")), 10)
})

test_that("return object of zero length if observation is not found", {
  expect_equal(length(z <- calculate_matches(ChickWeight,
                                             list(Time = 0, Chick = 2, Diet = 2),
                                             y_name = "weight")),
               0)
})

context("extract_matches()")
set.seed(234)
z <- calculate_matches(data, list(Time = 0, Chick = 48),
                       y_name = c("weight","Time"), t_name = "Diet")
test_that("returns vector of 10 row indices", {
  expect_equal(extract_matches(z, y_name = "Time", t_name = "Diet", c_name = "3"),
               c(342, 369, 375, 411, 414, 423, 428, 438, 457, 459))
})

# # # warning: prediction from a rank-deficient fit may be misleading
# z <- calculate_matches(ChickWeight, list(Chick = 48, Time = 0), y_name = "weight", x_name = c("Chick","Diet"), e_name = c("Time"))

#z <- calculate_matches(ChickWeight, list(Chick = 2), y_name = c("weight", "Time"))
#extract_matches(z)
# extract_matches(z, i_name = "23", y_name = "Time")
#
# z <- calculate_matches(ChickWeight, list(Chick = 2), y_name = c("weight", "Time"), t_name = "Diet")
# extract_matches(z)
# extract_matches(z, i_name = "23", y_name = "Time")
# extract_matches(z, i_name = "23", y_name = "Time", t_name = "Diet", c_name = "3")
#
# z <- calculate_matches(ChickWeight, list(Chick = 2), y_name = c("weight", "Time"), t_name = "Time")
# extract_matches(z)
# extract_matches(z, i_name = "23", y_name = "Time")
# extract_matches(z, i_name = "23", y_name = "Time", t_name = "Time", c_name = "2")
