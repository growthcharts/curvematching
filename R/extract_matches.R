#' Extracts the calcated matches for indexing
#'
#' @param matches An object created by \code{calculate_matches()}. Such an
#' object has class \code{"match_list"}.
#' @param i_name The name of the indvidual for which matches are wanted.
#' If unspecified, it takes the first individual.
#' @param y_name The name of the outcome variable. If unspecified, it takes the
#' first outcome.
#' @param t_name A character vector containing the name of the treatment
#' variables.
#' @param c_name The name of the category level within the treatment variable.
#' @return The row numbers in
#' \code{data} corresponding to the matched children. The length
#' of the list will be always equal to \code{m} if \code{replace == TRUE},
#' but may be shorter if \code{replace == FALSE} if the donors are exhausted. The
#' length is zero if no matches can be found.
#' @author Stef van Buuren 2016
#' @references
#' van Buuren, S. (2014). \emph{Curve matching: A data-driven technique to
#' improve individual prediction of childhood growth}. Annals of Nutrition &
#' Metabolism, 65(3), 227-233.
#' van Buuren, S. (2012). \emph{Flexible imputation of missing data}.
#' Boca Raton, FL: Chapman & Hall/CRC.
#' @export
extract_matches <- function(matches,
                            i_name = names(matches)[[1]],
                            y_name = names(matches[[i_name]])[[1]],
                            t_name = character(0),
                            c_name = character(0)) {
  if (!inherits(matches, "match_list")) stop("Argument `matches` not of class `match_list`.")

  if (length(matches) == 0) return(integer(0))

  found <- i_name %in% names(matches)
  if (any(!found)) {
    warning("i_name elements ", i_name[!found], " not found.")
    return(integer(0))
  }

  found <- y_name %in% names(matches[[i_name]])
  if (any(!found)) {
    warning("y_name elements", y_name[!found], " not found.")
    return(integer(0))
  }

  # if there is no .by variable, then simply ignore
  # any treatment parameters and return first row
  tab <- matches[[i_name]][[y_name]]
  has_by <- tab[1, ".by"]
  if (!has_by) return(as.vector(unlist(tab[1, ".row"])))

  # handle treatment selection: t_name and c_name
  if (length(t_name) == 0) {
    tn <- names(tab)
    t_name <- tn[!tn %in% c(".by", ".row")][1]
  }
  if (!t_name %in% names(tab)) {
    warning("Name (t) ", t_name, " not found.")
    return(integer(0))
  }
  if (length(c_name) == 0) c_name <- unlist(tab[1, t_name])
  if (!c_name %in% unlist(tab[, t_name])) {
    warning("Name (c) ", c_name, " not found.")
    return(integer(0))
  }
  as.vector(unlist(tab[unlist(tab[, t_name]) == c_name, ".row"]))
}
