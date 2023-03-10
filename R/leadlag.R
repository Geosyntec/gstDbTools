#' Lead/Lag a logical
#'
#' @param lgl logical vector
#' @param bef integer, number of elements to lead by
#' @param aft integer, number of elements to lag by
#' @return logical, same length as 'lgl'
#' @export
leadlag <- function(lgl, bef = 1, aft = 1) {
  n <- length(lgl)
  bef <- min(n, max(0, bef))
  aft <- min(n, max(0, aft))
  befx <- if (bef > 0) sapply(seq_len(bef), function(b) c(utils::tail(lgl, n = -b), rep(FALSE, b)))
  aftx <- if (aft > 0) sapply(seq_len(aft), function(a) c(rep(FALSE, a), utils::head(lgl, n = -a)))
  rowSums(cbind(befx, lgl, aftx), na.rm = TRUE) > 0
}