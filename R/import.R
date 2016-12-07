#' @import dplyr
#' @importFrom stats as.formula lm na.exclude predict runif approx cor fitted
# #' @importFrom lazyeval interp
# #' @importFrom tibble tibble add_column
NULL

globalVariables(c(".", ".seqno", "target", "candidate",
                  "agedays2", "y", "yhat",
                  "baz_broad", "haz_broad", "hcaz_broad",
                  "wash_blndall", "waz_broad"))
