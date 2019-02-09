#' Calculates matches for a child by predictive mean matching
#'
#' Curve matching is a technology that aims to predict
#' individual growth curves. The method finds persons similar to the
#' target person, and learn the possible future course of growth
#' from the realized curves of the matched individuals.
#'
#' The function finds \code{k} matches for an individual in the same data set
#' by means of stratified predictive mean matching.
#' @param data A \code{data.frame} or \code{tbl_df}.
#' @param condition Logical expression defining the set of rows in \code{data}
#'  for which matches will be sought. Missing values are taken as false.
#'  If omitted, all rows will be successively taken as targets.
#'  This can result in intensive computation if \code{nrow(data)} is large.
#' @param y_name A character vector containing the names of the dependent
#' variables in \code{data}.
#' @param x_name A character vector containing the names of predictive
#' variables in \code{data} to will go into the linear part of the model.
#' @param e_name A character vector containing the names of the variables for
#' which the match should be exact.
#' @param t_name A character vector containing the names of the treatment
#' variables in \code{data}. The current function will only fit the model to
#' only the first element of \code{t_name}.
#' @param subset Logical expression defining the set of rows taken
#' from \code{data}. This subset is selected before any other calculations
#' are made, and this can be used to trim down the size of the data in which
#' matches are defined and sought.
#' @param k Requested number of matches. The default is \code{k = 10}.
#' @param allow_matched_targets A logical that indicates whether the non-active
#'  target cases may be found as a match. The default is \code{TRUE}.
#' @param include_target A logical that indicates whether the target case
#' is included in the model. See details. The default is \code{TRUE}.
#' @param replace A logical that indicates whether matches should be found with
#' or without replacement. The default is \code{FALSE}.
#' @param verbose A logical indicating whether diagnostic information should
#' be printed.
#' @param \dots Arguments passed down to \code{match_pmm()}.
#' @return An object of class \code{match_list} which can be post-processed
#' by the \code{extract_matches} function to extract the row numbers in
#' \code{data} of the matched children. The length
#' of the list will be always equal to \code{m} if \code{replace == TRUE},
#' but may be shorter if \code{replace == FALSE} if the donors are exhausted. The
#' length is zero if no matches can be found.
#' @author Stef van Buuren 2017
#' @details
#' By default, if the outcome variabe of the target case is observed, then
#' it used to fit the model, together with the candidate donors.
#' The default behavior can be changed by
#' setting \code{include_target = FALSE}. Note that if \code{x_name}
#' contains one or more factors, then it is possible
#' that the factor level of the target case is unique among all potential
#' donors. In that case, the model can still be fit, but prediction will
#' fail, and hence no matches will be found.
#' @references
#' van Buuren, S. (2014). \emph{Curve matching: A data-driven technique to
#' improve individual prediction of childhood growth}. Annals of Nutrition &
#' Metabolism, 65(3), 227-233.
#' van Buuren, S. (2012). \emph{Flexible imputation of missing data}.
#' Boca Raton, FL: Chapman & Hall/CRC.
#' @examples
#' library("curvematching")
#' data <- datasets::ChickWeight
#' data[543, ]
#'
#' # find matches for observation in row 543 for outcome weight
#' m <- calculate_matches(data, Time == 0 & Chick == 48,
#'   y_name = "weight", x_name = c("Time", "Diet"))
#'
#' # row numbers of matched cases
#' extract_matches(m)
#'
#' # data of matched cases
#' data[extract_matches(m), ]
#'
#' @export
calculate_matches <- function(data,
                              condition,
                              y_name,
                              x_name = character(),
                              e_name = character(),
                              t_name = character(),
                              subset = NULL,
                              k = 10,
                              allow_matched_targets = TRUE,
                              include_target = TRUE,
                              replace = FALSE,
                              verbose = TRUE, ...) {

  equals_all <- function(x) {
    if (is.null(names(x)) | length(x) == 0) return(character(0))
    cv <- vector("character", length(x))
    for (j in 1:length(x)) {
      xj <- x[[j]]
      if (is.factor(xj)) cv[j] <- paste0(names(x)[j], ' == "', as.character(xj), '"')
      else cv[j] <- paste0(names(x)[j], ' == ', xj)
    }
    paste0(cv, collapse = " & ")
  }

  # check input
  if (!is.data.frame(data)) return(no_match())

  # append unique row id
  data <- data %>%
    mutate(.row = 1:n())

  # restrict to subset if specified
  subset_call <- substitute(subset)
  if (!is.null(subset_call)) data <- filter(data, !! subset_call)

  # construct target variable
  condition_call <- substitute(condition)
  data <- mutate(data, .target = !! condition_call) %>%
    group_by(.data$.target) %>%
    mutate(.seqno = 1:n())

  if (!any(data$.target, na.rm = TRUE)) {
    if (verbose) warning(paste("No rows conform to", quote(condition)))
    return(no_match())
  }

  # no matches if all outcomes in the donor part are missing
  # if (all(is.na(data[!data$.target, y_name]))) return(no_match())
  # FIMXE: add test for absent donor outcomes

  # note: assuming TRUE is last group
  ng <- group_size(data)
  nt <- ng[n_groups(data)]
  data <- ungroup(data)

  # validate variable names
  y_name <- y_name[y_name %in% names(data)]
  x_name <- x_name[x_name %in% names(data)]
  e_name <- e_name[e_name %in% names(data)]
  t_name <- t_name[t_name %in% names(data)]

  # exception handlers
  if (k <= 0 || length(y_name) == 0)
    return(no_match())

  # select active variables
  sys <- c(".row", ".target", ".seqno")
  var_names <- unique(c(sys, y_name, x_name, e_name, t_name))
  data <- select(data, !! var_names)

  # loop over tgts
  l1 <- vector("list", nt)
  names(l1) <- as.vector(unlist(select(filter(data, .data$.target), .data$.row)))
  for (i in 1:nt) {
    # define active case
    data <- mutate(data, active = .data$.target & .data$.seqno == i)
    active <- filter(data, active)
    # target_names[i] <- select(active, ".row")

    # re-define candidate set according to allow_matched_targets flag
    if (allow_matched_targets)
      data <- mutate(data, candidate = !active)
    else data <- mutate(data, candidate = !.data$.target)

    # trim candidate set by requiring exact matches on
    # variables listed in `e_name`
    trimmed <- select(active, !! e_name)
    cond <- equals_all(trimmed)
    if (length(cond) > 0) {
      cond <- paste0("candidate == TRUE & ", cond)
      data <- mutate_(data, candidate = cond)
    }

    # loop over outcome names
    ny <- length(y_name)
    l2 <- vector("list", ny)
    names(l2) <- y_name
    for (iy in 1:ny) {
      yvar <- y_name[iy]

      # extract subset of candidates
      xy <- filter(data, .data$candidate | active)
      if (!include_target) xy[xy$active, yvar] <- NA

      # split by treatment variables
      if (length(t_name) > 0) {
        # duplicate one tgt row for each treatment level
        # FIXME: as.name will only split on first variable name
        t_name <- t_name[[1]]
        # t_name_list <- as.list(t_name)
        t_unique <- unique(xy[, t_name])
        mutate_call <- quo(!! sym(t_name))
        augment <- slice(active, rep(1:n(), each = nrow(t_unique)))
        augment[, t_name] <- t_unique
        matched <- augment %>%
          bind_rows(filter(xy, .data$candidate)) %>%
          group_by(!!! mutate_call) %>%
          do(.row = match_pmm(., y_name = yvar, x_name = x_name, k = k, ...)) %>%
          mutate(.by = TRUE)
      } else {
        row <- match_pmm(xy, y_name = yvar, x_name = x_name, k = k, ...)
        matched <- tibble(.by = FALSE, .row = list(row))
      }
      l2[[iy]] <- matched
    }
    l1[[i]] <- l2
  }
  # names(l1) <- target_names
  class(l1) <- "match_list"
  l1
}

match_pmm <- function(data, y_name, x_name, k, exclude_NA = FALSE, ...) {
  if (nrow(data) <= 1) return(no_match())

  if (sum(data$active) > 1) stop("Too many active cases: ", sum(data$active))
  if (sum(data$active) == 0) stop("No active case found.")

  # keep only independent variables taking at least two values
  # note: apparently, next statement cannot be nested in keep <- statement,
  # perhaps to force evaluation of data
  x <- select(data, !! x_name)
  keep <- sapply(lapply(x, unique), length) >= 2
  x_keep <- x_name[keep]
  x_terms <- paste(c("1", x_keep), sep = "", collapse = " + ")
  form <- as.formula(paste(y_name, "~", x_terms))

  # fit model, and estimate prediction
  fit <- lm(form, data = data, na.action = na.exclude, ...)
  if (exclude_NA) yhat <- fitted(fit, ...)
  else yhat <- predict(fit, newdata = data, ...)
  data <- mutate(data, yhat = yhat)
  yhat_active <- yhat[data$active]

  d <- abs(yhat - yhat_active)
  d[data$active] <- NA
  f <- d > 0

  # browser()
  # add little noise to break ties
  a1 <- ifelse(any(f, na.rm = TRUE),
               min(d[f], na.rm = TRUE), 1)
  d <- d + runif(length(d), 0, a1 / 10^10)

  nmatch <- min(k, length(d) - 1)  # large nmatch: take all
  if (nmatch == 1) return(as.integer(data[which.min(d), ".row"]))
  ds <- sort.int(d, partial = nmatch)
  matched <- data[d <= ds[nmatch] & !is.na(d), ".row"]
  matched$.row
}

#' Returns empty match_list object
#'
#' The empty \code{match_list} signals that no matches have been found. The
#' empty list should be returned if an error occurs in
#' \code{calculate_matches()} so that its output remains consistent.
#' @param mode By default a \code{"list"}
#' @return An empty object of class \code{match_list}
#' @export
no_match <- function(mode = "list") {
  z <- vector(mode, length = 0)
  class(z) <- "match_list"
  z
}


