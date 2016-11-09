#' Import chromatograms
#'
#' Read exported UNICORN chromatogram data
#'
#' If \code{sample_names} is a character vector, a new column named as
#' \code{Sample} will be appended to the data frame \emph{in the same order} as
#' curve names. However, this is not recommended as the order of the curves may
#' change or some curves may be excluded at some point during data import. In
#' the latter case, you will get an error message. The safest way is to define a
#' data frame with the column names \code{Curve} and \code{Sample}, matching the
#' curve name to the sample name, respectively. When you use it to define sample
#' names, only those curves that are found in the data will be processed, i.e.
#' missing curves will be ignored.
#'
#' When \code{hardware_autozero = TRUE}, chromatograms are assumed to be
#' baselined already during acquisition and this baseline level will is trusted.
#' Otherwise (default), the baseline will be subtracted from each chromatogram
#' using the measurement defined by \code{reference_measurement}. This is a
#' linear operation, which will not change the shape of the curve but it will
#' only shift its baseline towards 0.
#'
#' Please note that \code{single_channel} refers to the UV cell rather than how
#' the data are exported. Even if you export only one channel out of many from a
#' chromatogram file, the naming will include the channel information and it
#' will interfere with data import as well as grouping. Therefore, use
#' \code{single_channel = TRUE} only when you are sure that the UV detector can
#' operate with one channel at a time.
#'
#' @param file_name The file to be imported. It can be either an Excel file or
#' an ASC (currently not implemented).
#' @param sample_names A vector or a data frame containing actual sample names
#' corresponding to curve names
#' @param combined \code{TRUE} (default) when several spectra are combined
#' by the UNICORN software prior to export.
#' @param single_channel \code{TRUE} if the UV cell can only work in single
#' channel mode. Default is \code{FALSE}.
#' @param hardware_autozero \code{TRUE} when the UV cell was autozeroed during
#' acquisition. Default is \code{FALSE}.
#' @param reference_measurement The number of measurement to be used in
#' baseline subtraction. Ignored when \code{hardware_autozero = TRUE}.
#' @param verbose Display additional progress information. Default is
#' \code{FALSE}.
#'
#' @return A tibble data frame that contains volume, (baselined) absorbance,
#' wavelength/channel, curve and sample name information for the experiment.
#'
#' @examples
#' \dontrun{
#' samples <- data.frame(
#'     Curve = c("Manual run 10:1", "Manual run 10:5", "Manual run 10:3"),
#'     Sample = c("Negative control", "Positive control", "Test"))
#' chr_data <- read_unicorn_excel('Chromatogram.xls', samples)
#' }
#'
#' @importFrom magrittr '%>%' '%<>%' set_colnames
#' @importFrom tools file_ext
#' @import assertthat
#' @import dplyr
#' @import tidyr
#' @importFrom readxl read_excel
#' @export
#'

# TODO: Tests for different combinations, work on combined

read_unicorn <- function(file_name,
                         sample_names = NULL,
                         combined = TRUE,
                         single_channel = FALSE,
                         hardware_autozero = FALSE,
                         reference_measurement = 1,
                         verbose = FALSE) {

  # Custom assertion for normalized retention volume
  is_retention_normalized <- function(x) {
    !anyDuplicated(colnames(x))
  }
  on_failure(is_retention_normalized) <- function(call, env) {
    'Curves must be normalized for retention volume!'
  }

  # Custom assertion for file type
  is_valid_filetype <- function(x) {
    assert_that(is.readable(file_name))
    has_extension(x, 'xls') | has_extension(x, 'asc')
  }
  on_failure(is_valid_filetype) <- function(call, env) {
    'File extension must be either xls or asc'
  }

  # Validate file type
  assert_that(is_valid_filetype(file_name))

  # Validate and organize sample_names
  if (!is.null(sample_names)) {
    if (combined) {
      if (is.data.frame(sample_names)) {
        assert_that(has_name(sample_names, 'Curve'),
                    has_name(sample_names, 'Sample'))
        sample_names %<>%
          mutate_at(vars(Curve), as.character)
      } else {
        assert_that(is.character(sample_names))
      }
    } else {
      assert_that(is.string(sample_names))
    }
  }

  # Validate other parameters
  assert_that(is.flag(hardware_autozero),
              is.flag(single_channel),
              is.flag(combined),
              is.numeric(reference_measurement))

  file_type <- file_ext(file_name)

  if (file_type == 'asc') {
    stop('Parsing asc files not yet implemented!')
  }

  # Import raw data
  if (verbose) {
    message(paste0('Constructing data from ', file_name))
  }

  if (single_channel) {
    chr_data <-
      read_excel(file_name, skip = 2) %>%
      set_colnames(c('Volume', 'A')) %>%
      filter(!is.na(A)) %>%
      mutate_all(as.numeric) %>%
      mutate(Sample = sample_names)
  } else {
    chr_data <- read_excel(file_name, skip = 1) %>%
      slice(-1)
    assert_that(is_retention_normalized(chr_data))
    colnames(chr_data)[1] <- 'Volume'
    chr_data %<>%
      mutate_all(as.numeric) %>%
      gather(key = 'ID', value = 'A', -Volume) %>%
      filter(!is.na(A)) %>%
      separate(ID, c('Curve', 'Channel', 'Wavelength'), sep = '_')
    if (!is.null(sample_names)) {
      if (is.data.frame(sample_names)) {
        chr_data %<>%
          left_join(y = sample_names, by = 'Curve')
      } else {
        chr_data %<>%
          mutate(Sample = factor(Curve, labels = unique(sample_names)))
      }
    }
  }

  # Software autozero
  if (verbose) {
    message('Shifting baseline towards 0')
  }

  if (!hardware_autozero & !single_channel) {
    chr_data %<>%
      group_by(Curve, Wavelength) %>%
      mutate(A_norm = A - A[reference_measurement])
  }

  chr_data %<>%
    ungroup %>%
    mutate_if(function(x) !is.numeric(x), factor)

  if (verbose) {
    print(summary(chr_data, digits = 3))
  }

  return(chr_data)
}
