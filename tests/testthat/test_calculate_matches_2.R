context("calculate_matches_2")

set.seed(234)
data <- donorloader::load_data(dnr = "smocc", element = "child")

x_name <-  c("dsc_0.0767", "dsc_0.1533", "dsc_0.25")
z <- calculate_matches(data, condition = id %in% 10001,
                       y_name = "dsc_2",
                       x_name = x_name,
                       k = 10)
outcome <- c(283, 426, 525, 552,  574, 855, 1207, 1407, 1575, 1877)

test_that("10001 returns fixed set", {
  expect_equal(extract_matches(z), outcome)
})

# x <- data[c(which(data$id == 10001), extract_matches(z)), c("id", x_name, "dsc_2")]
# head(x)
# hist(x$dsc_2)
