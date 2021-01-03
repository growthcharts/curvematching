#' @importFrom stats approx as.formula cor fitted lm na.exclude predict runif sd
#' @importFrom lazyeval interp
#' @importFrom tibble tibble add_column
#' @importFrom rlang .data quo sym enexpr
#' @importFrom dplyr %>% filter select mutate mutate_if bind_rows do
#'             group_by group_size mutate_ n n_groups
#'             slice ungroup
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
