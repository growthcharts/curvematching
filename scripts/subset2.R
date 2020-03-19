# Hadley Advanced R, ch 13
subset2 <- function(x, condition) {
  condition_call <- substitute(condition)
  r <- eval(condition_call, x)
  x[r, ]
}

sample_df <- data.frame(a = 1:5, b = 5:1, c = c(5, 3, 1, 4, 1))

subset2(sample_df, a >= 4)

# 13.3
y <- 4
x <- 4
condition <- 4
condition_call <- 4

subset2(sample_df, a >= 4)
subset2(sample_df, a == y)
subset2(sample_df, a == x)
subset2(sample_df, a == condition)
subset2(sample_df, a == condition_call)

subset2 <- function(x, condition) {
  condition_call <- substitute(condition)
  r <- eval(condition_call, x, parent.frame())
  x[r, ]
}

subset2(sample_df, a >= 4)
subset2(sample_df, a == y)
subset2(sample_df, a == x)
subset2(sample_df, a == condition)
subset2(sample_df, a == condition_call)
