#' @importFrom dplyr    %>% all_of bind_rows do filter group_by group_size
#'                      mutate mutate_ mutate_if n n_groups select slice ungroup
#' @importFrom lazyeval interp
#' @importFrom rlang    .data quo sym enexpr
#' @importFrom stats    approx as.formula cor fitted lm na.exclude predict
#'                      runif sd
#' @importFrom tibble   tibble add_column
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
