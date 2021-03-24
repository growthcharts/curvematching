#' Calculates matches for one or more children by blended distance matching
#'
#' Curve matching is a technology that aims to predict individual growth curves.
#' The method finds persons similar to the target person, and learn the possible
#' future course of growth from the realized curves of the matched individuals.
#'
#' The function finds \code{k} matches for an individual in the same data set by
#' means of stratified predictive mean matching or by nearest neighbour matching.
#'
#' @param data A \code{data.frame} or \code{tbl_df} with donor data. One row is
#' one potential donor.
#' @param newdata A \code{data.frame} or \code{tbl_df} with data of children for
#'  which we seek matches. Every row corresponds to one child.
#' @param y_name A character vector containing the names of the dependent
#'   variables in \code{data}.
#' @param x_name A character vector containing the names of predictive variables
#'   in \code{data} to will go into the linear part of the model.
#' @param e_name A character vector containing the names of the variables for
#'   which the match should be exact.
#' @param t_name A character vector containing the names of the treatment
#'   variables in \code{data}. The current function will only fit the model to
#'   only the first element of \code{t_name}.
#' @param subset Logical expression defining the set of rows taken from
#'   \code{data}. This subset is selected before any other calculations are
#'   made, and this can be used to trim down the size of the data in which
#'   matches are defined and sought.
#' @param k Requested number of matches. The default is \code{k = 10}.
#' @param replace A logical that indicates whether to match with or without
#'   replacement. The default is \code{FALSE}.
#' @param blend An integer value between 0 and 1 that indicates the blend
#'   between predictive mean matching with replacement (\code{1}) and euclidian
#'   distance matching (\code{0}). The default is \code{1}.
#' @param kappa A numeric value that serves as the sensitivity parameter for the
#'   inverse distance weighting. Used when drawing with replacement. The default
#'   is \code{3}.
#' @param break_ties A logical indicating whether ties should broken randomly.
#'   The default (\code{TRUE}) breaks ties randomly.
#' @param verbose A logical indicating whether diagnostic information should be
#'   printed.
#' @param \dots Arguments passed down to \code{match_pmm()}.
#' @return An object of class \code{match_list} which can be post-processed by
#'   the \code{extract_matches} function to extract the row numbers in
#'   \code{data} of the matched children. The length of the list will be always
#'   equal to \code{m} if \code{replace == TRUE}, but may be shorter if
#'   \code{replace == FALSE} if the donors are exhausted. The length is zero if
#'   no matches can be found.
#' @author Stef van Buuren 2021
#' @details
#'   The procedure search for matches in \code{data} for each row
#'   in \code{newdata}.
#'   Note that if \code{x_name} contains one or more factors, then it is
#'   possible that the factor level of the target case is unique among all
#'   potential donors. In that case, the model can still be fit, but prediction
#'   will fail, and hence no matches will be found.
#'
#'   If \code{break_ties} is \code{FALSE}, the function returns the first \code{nmatch}
#'   matches as they appear in the order of \code{data}. This method overuses
#'   the first part of the data if there are ties, and hence may underestimate
#'   variability. The default option is to break ties randomly.
#'
#' @references van Buuren, S. (2014). \emph{Curve matching: A data-driven
#'   technique to improve individual prediction of childhood growth}. Annals of
#'   Nutrition & Metabolism, 65(3), 227-233. van Buuren, S. (2012).
#'   \emph{Flexible imputation of missing data}. Boca Raton, FL: Chapman &
#'   Hall/CRC.
#' @examples
#' data <- datasets::ChickWeight
#' data[1, ]
#'
#' # find matches for observation in row 1
#' m1 <- calculate_matches2(data, data[1, ], subset = !rownames(data) %in% "1",
#'     y_name = "weight", x_name = c("Time", "Diet"))
#'
#' # data of matched cases (may vary because of tie breaking)
#' data[extract_matches(m1), ]
#'
#' # without tie breaking, we pick the earlier rows (not recommended)
#' m2 <- calculate_matches2(data, data[1, ], subset = !rownames(data) %in% "1",
#'      y_name = "weight", x_name = c("Time", "Diet"), break_ties = FALSE)
#' data[extract_matches(m2), ]
#'
#' @export
calculate_matches2 <- function(data,
                               newdata,
                               y_name = character(0L),
                               x_name = character(0L),
                               e_name = character(0L),
                               t_name = character(0L),
                               subset = TRUE,
                               k = 10L,
                               replace = FALSE,
                               blend = 1.0,
                               break_ties = TRUE,
                               kappa = 3,
                               verbose = TRUE, ...) {
  equals_all <- function(x) {
    if (is.null(names(x)) | !length(x)) {
      return(character(0))
    }
    cv <- vector("character", length(x))
    for (j in 1:length(x)) {
      xj <- x[[j]]
      if (!is.numeric(xj)) {
        cv[j] <- paste0(names(x)[j], ' == "', as.character(xj), '"')
      } else {
        cv[j] <- paste0(names(x)[j], " == ", xj)
      }
    }
    paste0(cv, collapse = " & ")
  }

  # validity checks
  if (!is.data.frame(data)) {
    return(no_match())
  }
  if (!nrow(newdata) || !is.data.frame(newdata)) {
    if (verbose) warning("No rows in newdata.")
    return(no_match())
  }
  if (k <= 0 || !length(y_name) || !hasName(data, y_name)) {
    return(no_match())
  }

  # model variables
  vars <- intersect(unique(c(y_name, x_name, e_name, t_name)),
                    names(data))

  # preserve donor row number before subset
  data <- data %>%
    mutate(.row = 1L:n()) %>%
    filter({{subset}}) %>%
    select(all_of(c(".row", !! vars)))

  newdata <- newdata %>%
    mutate(.row = 1L:n()) %>%
    select(all_of(c(".row", !! vars)))

  # loop over target children
  l1 <- vector("list", nrow(newdata))
  names(l1) <- as.vector(unlist(select(newdata, .data$.row)))
  for (i in 1L:nrow(newdata)) {
    # define active case
    active <- slice(newdata, i)

    # trim candidate set by requiring exact matches on
    # variables listed in `e_name`
    trimmed <- select(active, !! e_name)
    cond <- equals_all(trimmed)
    if (length(cond)) {
      expr <- parse(text = cond)
      data <- data %>%
        mutate(candidate = eval(expr))
    } else {
      data <- data %>%
        mutate(candidate = TRUE)
    }

    # loop over outcome names
    ny <- length(y_name)
    l2 <- vector("list", ny)
    names(l2) <- y_name
    for (iy in 1:ny) {
      yvar <- y_name[iy]

      # extract subset of candidates
      xy <- filter(data, .data$candidate)

      # split by treatment variables
      if (length(t_name) > 0) {
        # duplicate one tgt row for each treatment level
        # FIXME: as.name will only split on first variable name
        t_name <- t_name[[1]]
        # t_name_list <- as.list(t_name)
        t_unique <- unique(xy[, t_name])
        mutate_call <- quo(!!sym(t_name))
        augment <- slice(active, rep(1:n(), each = length(t_unique)))
        augment[, t_name] <- t_unique

        # loop over all treatments
        matched <- vector("list", length(t_unique))
        for (c in 1:nrow(augment)) {
          active_trt <- augment[c, ]
          trt <- t_unique[c]
          data_trt <- filter(data, !!mutate_call == !!trt)
          matched[[c]] <- match_bdm2(data = data_trt,
                                     active = active_trt,
                                     y_name = yvar, x_name = x_name, k = k, replace = replace,
                                     blend = blend, break_ties = break_ties, kappa = kappa, ...)
        }
        # store
        matched <- augment %>%
          bind_rows(filter(xy, .data$candidate)) %>%
          group_by(!!mutate_call) %>%
          summarise(.row = !! matched) %>%
          mutate(.by = TRUE)

        # this code isn't yet right, do not use t_name for now (SvB 22/3/2021)
        # matched <- augment %>%
        #   bind_rows(filter(xy, .data$candidate))
        # matched <- matched %>%
        #   group_by(!!mutate_call) %>%
        #   do(.row = match_bdm2(., active = active,
        #     y_name = yvar, x_name = x_name, k = k, replace = replace,
        #     blend = blend, break_ties = break_ties, kappa = kappa, ...
        #   )) %>%
        #   mutate(.by = TRUE)
      } else {
        row <- match_bdm2(xy, active = active,
          y_name = yvar, x_name = x_name, k = k, replace = replace,
          blend = blend, break_ties = break_ties, kappa = kappa, ...
        )
        matched <- tibble(.by = FALSE, .row = list(row))
      }
      l2[[iy]] <- matched
    }
    l1[[i]] <- l2
  }
  # names(l1) <- child_names
  class(l1) <- "match_list"
  l1
}

# Blended distance matching
match_bdm2 <- function(data, active, y_name, x_name, k, replace = TRUE, blend = 1,
                      break_ties = TRUE, exclude_NA = FALSE, kappa = 3, ...) {
  if (nrow(data) <= 0L) return(no_match())
  if (nrow(active) != 1L) stop("Argument active not one row.")
  if (blend > 1 | blend < 0) stop("blend needs to be a value between 0 and 1.")

  # keep only independent variables taking at least two values
  # note: apparently, next statement cannot be nested in keep <- statement,
  # perhaps to force evaluation of data
  x <- select(data, !! x_name)
  keep <- sapply(lapply(x, unique), length) >= 2
  x_keep <- x_name[keep]
  x_terms <- paste(c("1", x_keep), sep = "", collapse = " + ")
  form <- as.formula(paste(y_name, "~", x_terms))

  # obtain pmm weights
  if (blend > 0){
    # fit lm model
    fit <- lm(form, data = data, na.action = na.exclude, ...)
    if (exclude_NA) yhat <- fitted(fit, ...)
    else yhat <- predict(fit, newdata = data, ...)

    # predicted means
    yhat_active <- predict(fit, newdata = active, ...)
    d <- abs(yhat - yhat_active)

    # add noise
    f <- d > 0
    a1 <- ifelse(any(f, na.rm = TRUE),
                 min(d[f], na.rm = TRUE), 1)
    if (replace) {
      # with replacement
      if (any(!f, na.rm = TRUE)) d <- d + a1
      d <- d^kappa
      l1 <- (1/d)/(sum(1/d, na.rm = TRUE))
      l1[is.na(l1)] <- 0 # exclude NA
    } else {
      # no replacement
      if (break_ties) d <- d + runif(length(d), 0, a1 / 10^10)
      nmatch <- min(k, length(d) - 1L)  # large nmatch: take all
      if (nmatch == 1L) return(as.integer(data$.row[which.min(d)]))
      d_cut <- sort.int(d, partial = nmatch)[nmatch][1L]
      l1 <- as.integer(data$.row[d <= d_cut & !is.na(d)][1L:nmatch])
    }
  } else {
    # no pmm
    l1 <- 0
  }

  l2 <- 0

  if (replace) {
    l <- (blend * l1) + ((1 - blend) * l2)
    # return matches
    return(sample(1:length(l), size = k, prob = l, replace = TRUE))
  } else {
    # with replacement without blending as of now.
    if (blend < 0.5) {
      return(l2)
    } else{
      return(l1)
    }
  }
}


