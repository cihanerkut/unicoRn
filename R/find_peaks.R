#' Find peaks
#'
#' Find local maxima in a chromatogram
#'
#' This function searches for local maxima along the curves of chromatograms. 
#' Because the search can yield even slightest spikes as peaks, the hits are 
#' sorted in a descending order based on the UV absorbance the the top 
#' \code{maximum_peaks} (default 1) are reported.
#' 
#' The algorithm is modified from the \code{findPeaks} function in 
#' \href{https://CRAN.R-project.org/package=quantmod}{quantmod} package.
#'
#' @param chr_data Chromatogram data imported by \code{read_unicorn} or 
#' \code{read_res}
#' @param maximum_peaks Maximum number of peaks to find.
#'
#' @examples
#' \dontrun{
#' peaks <- find_peaks(chr_data, 5)
#' }
#' 
#' @importFrom dplyr group_by rowwise bind_rows top_n slice do
#' @export

find_peaks <- function(chr_data, maximum_peaks = 1) {
  peaks_df <- chr_data %>%
    group_by(Wavelength, Sample) %>%
    do(peaks = fp(., maximum_peaks)) %>%
    rowwise %>%
    .$peaks %>%
    bind_rows
  
  return(peaks_df)
}

fp <- function(d, n) {
  p <- which(diff(sign(diff(d$A))) < 0) + 1
  p.df <- d %>%
    slice(p) %>%
    top_n(n, A)
}