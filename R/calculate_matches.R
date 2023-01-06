#' Calculates matches for a child by blended distance matching
#'
#' Curve matching is a technology that aims to predict individual growth curves.
#' The method finds persons similar to the target person, and learn the possible
#' future course of growth from the realized curves of the matched individuals.
#'
#' The function finds `k` matches for an individual in the same data set by
#' means of stratified predictive mean matching or nearest neighbour matching.
#'
#' @param data A `data.frame` or `tbl_df`.
#' @param condition Logical expression defining the set of rows in `data`
#'   for which matches will be sought. Missing values are taken as false. If
#'   omitted, all rows will be successively taken as targets. This can result in
#'   intensive computation if `nrow(data)` is large.
#' @param y_name A character vector containing the names of the dependent
#'   variables in `data`.
#' @param x_name A character vector containing the names of predictive variables
#'   in `data` to will go into the linear part of the model.
#' @param e_name A character vector containing the names of the variables for
#'   which the match should be exact.
#' @param t_name A character vector containing the names of the treatment
#'   variables in `data`. The current function will only fit the model to
#'   only the first element of `t_name`.
#' @param subset Logical expression defining the set of rows taken from
#'   `data`. This subset is selected before any other calculations are
#'   made, and this can be used to trim down the size of the data in which
#'   matches are defined and sought.
#' @param k Requested number of matches. The default is `k = 10`.
#' @param replace A logical that indicates whether to match with or without
#'   replacement. The default is `FALSE`.
#' @param blend An integer value between 0 and 1 that indicates the blend
#'   between predictive mean matching with replacement (`1`) and euclidian
#'   distance matching (`0`). The default is `1`.
#' @param kappa A numeric value that serves as the sensitivity parameter for the
#'   inverse distance weighting. Used when drawing with replacement. The default
#'   is `3`.
#' @param break_ties A logical indicating whether ties should broken randomly.
#'   The default (`TRUE`) breaks ties randomly.
#' @param allow_matched_targets A logical that indicates whether the non-active
#'   target cases may be found as a match. The default is `TRUE`.
#' @param include_target A logical that indicates whether the target case is
#'   included in the model. See details. The default is `TRUE`.
#' @param verbose A logical indicating whether diagnostic information should be
#'   printed.
#' @param \dots Arguments passed down to `match_pmm()`.
#' @return An object of class `match_list` which can be post-processed by
#'   the `extract_matches` function to extract the row numbers in
#'   `data` of the matched children. The length of the list will be always
#'   equal to `m` if `replace == TRUE`, but may be shorter if
#'   `replace == FALSE` if the donors are exhausted. The length is zero if
#'   no matches can be found.
#' @author Stef van Buuren 2017
#' @details By default, if the outcome variabe of the target case is observed,
#'   then it used to fit the model, together with the candidate donors. The
#'   default behavior can be changed by setting `include_target = FALSE`.
#'   Note that if `x_name` contains one or more factors, then it is
#'   possible that the factor level of the target case is unique among all
#'   potential donors. In that case, the model can still be fit, but prediction
#'   will fail, and hence no matches will be found.
#'
#'   If `break_ties == FALSE`, the function returns the first `nmatch`
#'   matches as they appear in the order of `data`. This method leads to an
#'   overuse of the first part of the data, and hence underestimates
#'   variability. The better option is to break ties randomly (the default).
#'
#' @references van Buuren, S. (2014). *Curve matching: A data-driven
#'   technique to improve individual prediction of childhood growth*. Annals of
#'   Nutrition & Metabolism, 65(3), 227-233. van Buuren, S. (2012).
#'   *Flexible imputation of missing data*. Boca Raton, FL: Chapman &
#'   Hall/CRC.
#' @examples
#' library("curvematching")
#' data <- datasets::ChickWeight
#' data[543, ]
#'
#' # find matches for observation in row 543 for outcome weight
#' m <- calculate_matches(data, Time == 0 & Chick == 48, y_name = "weight", x_name = c("Time", "Diet"))
#'
#' # row numbers of matched cases
#' extract_matches(m)
#'
#' # data of matched cases
#' data[extract_matches(m), ]
#' @export
calculate_matches <- function(data,
                              condition,
                              y_name,
                              x_name = character(),
                              e_name = character(),
                              t_name = character(),
                              subset = NULL,
                              k = 10,
                              replace = FALSE,
                              blend = 1,
                              break_ties = TRUE,
                              allow_matched_targets = TRUE,
                              include_target = TRUE,
                              kappa = 3,
                              verbose = TRUE, ...) {
  equals_all <- function(x) {
    if (is.null(names(x)) | length(x) == 0) {
      return(character(0))
    }
    cv <- vector("character", length(x))
    for (j in 1:length(x)) {
      xj <- x[[j]]
      if (is.factor(xj)) {
        cv[j] <- paste0(names(x)[j], ' == "', as.character(xj), '"')
      } else {
        cv[j] <- paste0(names(x)[j], " == ", xj)
      }
    }
    paste0(cv, collapse = " & ")
  }

  # check input
  if (!is.data.frame(data)) {
    return(no_match())
  }

  # append unique row id
  data <- data %>%
    mutate(.row = 1:n())

  # restrict to subset if specified
  if (!is.null(subset)) data <- filter(data, {{ subset }})

  # construct target variable
  data <- data %>%
    mutate(.target = {{ condition }}) %>%
    group_by(.data$.target) %>%
    mutate(.seqno = 1:n())

  if (!any(data$.target, na.rm = TRUE)) {
    if (verbose) warning("No target rows conform to condition.")
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
  if (k <= 0 || length(y_name) == 0) {
    return(no_match())
  }

  # select active variables
  sys <- c(".row", ".target", ".seqno")
  var_names <- unique(c(sys, y_name, x_name, e_name, t_name))
  data <- select(data, all_of(var_names))

  # loop over tgts
  l1 <- vector("list", nt)
  names(l1) <- as.vector(unlist(select(filter(data, .data$.target), ".row")))
  for (i in 1:nt) {
    # FIXME: Is it really OK to overwrite data? SvB 20200319
    # define active case
    data <- mutate(data, active = .data$.target & .data$.seqno == i)
    active <- filter(data, active)
    # target_names[i] <- select(active, ".row")

    # re-define candidate set according to allow_matched_targets flag
    if (allow_matched_targets) {
      data <- mutate(data, candidate = !.data$active)
    } else {
      data <- mutate(data, candidate = !.data$.target)
    }

    # trim candidate set by requiring exact matches on
    # variables listed in `e_name`
    trimmed <- select(active, !!e_name)
    cond <- equals_all(trimmed)
    if (length(cond) > 0) {
      cond <- paste0("candidate == TRUE & ", cond)
      expr <- parse(text = cond)
      data <- dplyr::mutate(data, candidate = eval(expr))
    }

    # loop over outcome names
    ny <- length(y_name)
    l2 <- vector("list", ny)
    names(l2) <- y_name
    for (iy in 1:ny) {
      yvar <- y_name[iy]

      # extract subset of candidates
      xy <- filter(data, .data$candidate | .data$active)
      if (!include_target) xy[xy$active, yvar] <- NA

      # split by treatment variables
      if (length(t_name) > 0) {
        # duplicate one tgt row for each treatment level
        # FIXME: as.name will only split on first variable name
        t_name <- t_name[[1]]
        # t_name_list <- as.list(t_name)
        t_unique <- unique(xy[, t_name])
        mutate_call <- quo(!!sym(t_name))
        augment <- slice(active, rep(1:n(), each = nrow(t_unique)))
        augment[, t_name] <- t_unique
        matched <- augment %>%
          bind_rows(filter(xy, .data$candidate)) %>%
          group_by(!!mutate_call) %>%
          do(.row = match_bdm(.,
            y_name = yvar, x_name = x_name, k = k, replace = replace,
            blend = blend, break_ties = break_ties, kappa = kappa, ...
          )) %>%
          mutate(.by = TRUE)
      } else {
        row <- match_bdm(xy,
          y_name = yvar, x_name = x_name, k = k, replace = replace,
          blend = blend, break_ties = break_ties, kappa = kappa, ...
        )
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

# Blended distance matching
match_bdm <- function(data, y_name, x_name, k, replace = TRUE, blend = 1,
                      break_ties = TRUE, exclude_NA = FALSE, kappa = 3, ...) {
  if (nrow(data) <= 1) {
    return(no_match())
  }

  if (sum(data$active) > 1) stop("Too many active cases: ", sum(data$active))
  if (sum(data$active) == 0) stop("No active case found.")
  if (blend > 1 | blend < 0) stop("blend needs to be a value between 0 and 1.")

  # keep only independent variables taking at least two values
  # note: apparently, next statement cannot be nested in keep <- statement,
  # perhaps to force evaluation of data
  x <- select(data, !!x_name)
  keep <- sapply(lapply(x, unique), length) >= 2
  x_keep <- x_name[keep]
  x_terms <- paste(c("1", x_keep), sep = "", collapse = " + ")
  form <- as.formula(paste(y_name, "~", x_terms))
  # obtain pmm weights
  if (blend > 0) {
    # fit lm model
    fit <- lm(form, data = data, na.action = na.exclude, ...)
    if (exclude_NA) {
      yhat <- fitted(fit, ...)
    } else {
      yhat <- predict(fit, newdata = data, ...)
    }
    # predicted means
    data <- mutate(data, yhat = yhat)
    yhat_active <- yhat[data$active]
    d <- abs(yhat - yhat_active)
    d[data$active] <- NA

    # add noise
    f <- d > 0
    a1 <- ifelse(any(f, na.rm = TRUE),
      min(d[f], na.rm = TRUE), 1
    )
    if (replace) {
      # with replacement
      if (any(!f, na.rm = TRUE)) d <- d + a1
      d <- d^kappa
      l1 <- (1 / d) / (sum(1 / d, na.rm = TRUE))
      l1[is.na(l1)] <- 0 # exclude NA
    } else {
      # no replacement
      if (break_ties) d <- d + runif(length(d), 0, a1 / 10^10)
      nmatch <- min(k, length(d) - 1L) # large nmatch: take all
      if (nmatch == 1L) {
        return(as.integer(data$.row[which.min(d)]))
      }
      d_cut <- sort.int(d, partial = nmatch)[nmatch][1L]
      l1 <- as.integer(data$.row[d <= d_cut & !is.na(d)][1L:nmatch])
    }
  } else {
    # no pmm
    l1 <- 0
  }

  # obtain distance weights
  if (blend < 1) {
    xdata <- data[, x_keep]
    # transform for euclidian distance calculations
    xdata <- mutate_if(xdata, is.character, as.numeric)
    xdata <- mutate_if(xdata, is.factor, as.numeric)
    xdata <- mutate_if(xdata, is.numeric, function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))
    if (length(x_keep) < 1) xdata <- data.frame(rep(1, nrow(data)))

    # neirest neighbours
    d <- rowSums(sqrt((xdata - c(xdata[data$active, ]))^2))
    d[data$active] <- NA
    # add noise
    f <- d > 0
    a1 <- ifelse(any(f, na.rm = TRUE),
      min(d[f], na.rm = TRUE), 1
    )
    if (replace) {
      # with replacement
      if (any(!f, na.rm = TRUE)) d <- d + a1
      d <- d^kappa
      l2 <- (1 / d) / (sum(1 / d, na.rm = TRUE))
      l2[is.na(l2)] <- 0 # exclude NA
    } else {
      # no replacement
      if (break_ties) d <- d + runif(length(d), 0, a1 / 10^10)
      nmatch <- min(k, length(d) - 1L) # large nmatch: take all
      if (nmatch == 1L) {
        return(as.integer(data$.row[which.min(d)]))
      }
      data$d <- d
      d_cut <- sort.int(d, partial = nmatch)[nmatch][1L]
      l2 <- as.integer(data$.row[d <= d_cut & !is.na(d)][1L:nmatch])
    }
  } else {
    # no nn
    l2 <- 0
  }

  if (replace) {
    l <- (blend * l1) + ((1 - blend) * l2)
    # return matches
    return(sample(1:length(l), size = k, prob = l, replace = TRUE))
  } else {
    # with replacement without blending as of now.
    if (blend < 0.5) {
      return(l2)
    } else {
      return(l1)
    }
  }
}

# Match predictive mean matching (legacy)
match_pmm <- function(data, y_name, x_name, k, break_ties = TRUE,
                      exclude_NA = FALSE, ...) {
  if (nrow(data) <= 1) {
    return(no_match())
  }

  if (sum(data$active) > 1) stop("Too many active cases: ", sum(data$active))
  if (sum(data$active) == 0) stop("No active case found.")

  # keep only independent variables taking at least two values
  # note: apparently, next statement cannot be nested in keep <- statement,
  # perhaps to force evaluation of data
  x <- select(data, !!x_name)
  keep <- sapply(lapply(x, unique), length) >= 2
  x_keep <- x_name[keep]
  x_terms <- paste(c("1", x_keep), sep = "", collapse = " + ")
  form <- as.formula(paste(y_name, "~", x_terms))

  # fit model, and estimate prediction
  fit <- lm(form, data = data, na.action = na.exclude, ...)
  if (exclude_NA) {
    yhat <- fitted(fit, ...)
  } else {
    yhat <- predict(fit, newdata = data, ...)
  }
  data <- mutate(data, yhat = yhat)
  yhat_active <- yhat[data$active]

  d <- abs(yhat - yhat_active)
  d[data$active] <- NA

  # add little noise to break ties
  f <- d > 0
  a1 <- ifelse(any(f, na.rm = TRUE),
    min(d[f], na.rm = TRUE), 1
  )
  if (break_ties) d <- d + runif(length(d), 0, a1 / 10^10)

  nmatch <- min(k, length(d) - 1L) # large nmatch: take all
  if (nmatch == 1L) {
    return(as.integer(data$.row[which.min(d)]))
  }
  d_cut <- sort.int(d, partial = nmatch)[nmatch][1L]
  as.integer(data$.row[d <= d_cut & !is.na(d)][1L:nmatch])
}

#' Returns empty match_list object
#'
#' The empty `match_list` signals that no matches have been found. The
#' empty list should be returned if an error occurs in
#' `calculate_matches()` so that its output remains consistent.
#' @param mode By default a `"list"`
#' @return An empty object of class `match_list`
#' @export
no_match <- function(mode = "list") {
  z <- vector(mode, length = 0)
  class(z) <- "match_list"
  z
}
