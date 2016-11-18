#' Find peaks
#'
#' Find local maxima in a chromatogram
#'
#' This function searches for local maxima along the curve of a chromatogram by
#' dividing it into smaller windows. It works on a single curve at a time,
#' therefore the data must be grouped before calling this function.
#'
#' Window size is defined by \code{window_factor}, which is a multiple of
#' the total run volume. It determines the sensitivity of the search: A smaller
#' window size may result in too many false peaks but it runs faster. A larger
#' window size is more robust but it slows down the calculation. The default
#' value (\code{0.001}) corresponds to 1/1000th of the total run volume and it
#' is a fairly good starting point to search for peaks.
#'
#' @param chr_curve A subset of chromatogram data frame that corresponds to a
#' singe curve
#' @param window_factor A coefficient to be used in determining the peak
#' search window size. Default is \code{0.001}.
#' @param threshold_factor A coefficient to be used in filtering out baseline-
#' level peaks (i.e. noise). Default is \code{0.01}.
#' @param verbose Prints progress during the run. Default is \code{FALSE}.
#'
#' @examples
#' \dontrun{
#' peaks <- find_peaks(chr_data, window_factor = 0.001, threshold_factor = 0.01)
#' }
#'
#' @export

find_peaks <- function(chr_curve,
                       window_factor = 0.001,
                       threshold_factor = 0.01,
                       verbose = FALSE) {
  # Validate parameters
  assert_that(
    is.data.frame(chr_curve),
    is.number(window_factor),
    is.number(threshold_factor),
    is.flag(verbose)
  )
  
  # Calculate window size
  window_volume <- diff(range(chr_curve$Volume)) * window_factor
  window <- floor(length(chr_curve$Volume) * window_volume)
  if (verbose) {
    message(paste0("Window volume: ", window_volume))
  }
  
  # Extract curve
  curve <- chr_curve %>%
    select(A_norm) %>%
    filter(A_norm > max(A_norm) * threshold_factor + min(A_norm)) %>%
    unlist(use.names = F)
  
  # Find peaks
  windowed_space <- embed(curve, window)
  peaks <-
    windowed_space[which(max.col(windowed_space) == (window %/% 2))]
  peak_list <- chr_curve %>%
    filter(A_norm %in% peaks) %>%
    select(Volume) %>%
    unlist(use.names = F) %>%
    round(3) %>%
    unique
  
  return(peak_list)
}
