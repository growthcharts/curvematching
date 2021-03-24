context("calculate_matches2")

set.seed(234)
data <- datasets::ChickWeight

test_that("skips over crappy y_name", {
  expect_silent(calculate_matches2(data, data[data$Time == 0, ], y_name = "sasa"))
  })

test_that("returns proper length", {
  expect_equal(length(calculate_matches2(data, data[data$Time == 0 & data$Chick == 48, ], y_name = "weight")), 1)
  expect_equal(length(calculate_matches2(data, data[data$Time == 0 & data$Diet == 2, ], y_name = "weight")), 10)
})

test_that("warns if no targets are found", {
  expect_warning(calculate_matches2(data, data[data$Time == 0 & data$Chick == 2 & data$Diet == 2, ], y_name = "weight"))
})

test_that("warns about rank-deficient fit", {
  expect_warning(calculate_matches2(data, data[data$Chick == 48 & data$Time == 0, ],
                                   y_name = "weight",
                                   x_name = c("Chick","Diet"),
                                   e_name = c("Time")))
})


context("extract_matches()")
set.seed(234)
z0 <- calculate_matches2(donor = 1)

test_that("return integer(0) if there are matches", {
  expect_equal(length(extract_matches(z0)), 0L)
})

# z2 <- calculate_matches2(data, data[data$Time == 0 & data$Chick == 48, ],
#                          y_name = c("weight", "Time"), t_name = "Diet")
# m2 <- extract_matches(z2, y_name = "Time", t_name = "Diet", c_name = "3")
# test_that("returns vector of 10 row indices", {
#   expect_equal(m2,
#                c(342, 369, 375, 411, 414, 423, 428, 438, 457, 459))
# })
# data[m2, ]

# z <- calculate_matches2(data, data[data$Chick == 2, ], y_name = c("weight", "Time"))
# test_that("returns vector of 10 row indices", {
#   expect_equal(extract_matches(z, i_name = "23", y_name = "Time"),
#                c(128, 167, 243, 244, 245, 299, 407, 420, 472, 494))
# })
#
# #
# # z <- calculate_matches2(ChickWeight, list(Chick = 2), y_name = c("weight", "Time"), t_name = "Diet")
# # extract_matches(z)
# extract_matches(z, i_name = "23", y_name = "Time")
# extract_matches(z, i_name = "23", y_name = "Time", t_name = "Diet", c_name = "3")
# #
# # z <- calculate_matches2(ChickWeight, list(Chick = 2), y_name = c("weight", "Time"), t_name = "Time")
# # extract_matches(z)
# # extract_matches(z, i_name = "23", y_name = "Time")
# extract_matches(z, i_name = "23", y_name = "Time", t_name = "Diet", c_name = "2")
