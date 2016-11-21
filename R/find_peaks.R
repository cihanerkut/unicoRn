#' Find peaks
#'
#' Find local maxima in a chromatogram
#'
#' This function searches for local maxima along the curves of chromatograms by
#' dividing them into smaller windows. It works on a single curve at a time,
#' therefore the data are silently grouped before calling the workhorse function
#' \code{find_peaks_single}. By default, it works on the baseline-subtracted
#' absorbance values (A_norm). Alternatively, raw absorbance values (A) can be
#' used by setting \code{use_norm = FALSE}.
#'
#' Window size is defined by \code{window_factor}, which is a multiple of
#' the total run volume. It determines the sensitivity of the search: A smaller
#' window size may result in too many false peaks but it runs faster. A larger
#' window size is more robust but it slows down the calculation. The default
#' value (\code{0.001}) corresponds to 1/1000th of the total run volume and it
#' is a fairly good starting point to search for peaks.
#'
#' There is a default \code{threshold_factor} of 0.05, which corresponds to
#' 1/20th of the absorbance range of the curve. This is useful to eliminate
#' finding false peaks in the background.
#'
#' One can also change the \code{maximum_peaks} (default is 20) to find the
#' top peaks according to absorbance.
#'
#' @param chr_data Chromatogram data imported by \code{read_unicorn}
#' @param ... Arguments to be passed to \code{find_peaks_single}.
#' @param chr_curve A subset of chromatogram data frame that corresponds to a
#' singe curve
#' @param use_norm Use baseline-subtracted absorbance (default).
#' @param window_factor A coefficient to be used in determining the peak
#' search window size. Default is \code{0.001}.
#' @param threshold_factor A coefficient to be used in filtering out baseline-
#' level peaks (i.e. noise). Default is \code{0.01}.
#' @param maximum_peaks Maximum number of peaks to find.
#' @param verbose Prints progress during the run. Default is \code{FALSE}.
#'
#' @examples
#' \dontrun{
#' peaks <- find_peaks(chr_data,
#'                     window_factor = 0.001,
#'                     threshold_factor = 0.01)
#' }
#'
#' @export

find_peaks <- function(chr_data, ...) {
  peaks_df <- chr_data %>%
    group_by(Wavelength, Sample) %>%
    do(peaks = find_peaks_single(., ...)) %>%
    rowwise %>%
    .$peaks %>%
    bind_rows
  
  return(peaks_df)
}

#' @describeIn find_peaks Find peaks in a single curve. This is the workhorse
#' function and should not be called without proper filtering.

find_peaks_single <- function(chr_curve,
                              use_norm = TRUE,
                              window_factor = 0.001,
                              threshold_factor = 0.05,
                              maximum_peaks = 20,
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
  if (use_norm) {
    chr_curve %<>% mutate(A_temp = A_norm)
  } else {
    chr_curve %<>% mutate(A_temp = A)
  }
  
  curve <- chr_curve %>%
    select(A_temp) %>%
    filter(A_temp > max(A_temp) * threshold_factor + min(A_temp)) %>%
    unlist(use.names = F)
  
  # Find peaks
  windowed_space <- embed(curve, window)
  peaks <-
    windowed_space[which(max.col(windowed_space) == (window %/% 2))]
  
  peak_list <- chr_curve %>%
    filter(A_temp %in% peaks) %>%
    arrange(desc(A_temp)) %>%
    top_n(maximum_peaks, A_temp) %>%
    mutate(Volume = round(Volume, 2)) %>%
    distinct(Volume, .keep_all = TRUE) %>%
    distinct(A_temp, .keep_all = TRUE) %>%
    select(-A, -A_norm)
  
  if (use_norm) {
    peak_list %<>% rename(A_norm = A_temp)
  } else {
    peak_list %<>% rename(A = A_temp)
  }
  
  return(peak_list)
}
