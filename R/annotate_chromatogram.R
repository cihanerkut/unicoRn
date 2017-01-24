#' Annotate chromatograms
#' 
#' Merge additional information about the sample conditions into a chromatogram.
#' 
#' UNICORN sometimes codes sample information into file names using a funny 
#' naming scheme. In such cases, adding sample information during import via 
#' \code{read_unicorn} is not straightforward. An easier approach is to parse 
#' the data without sample names, list sample IDs, build a data frame 
#' accordingly, and finally merge it with the chromatogram.
#' 
#' The information is supplied in a data frame called \code{sample_info}. This
#' data frame must have at least two columns: \code{ID} and \code{Sample} (case 
#' sensitive). Merging is done based on the \code{ID} column. \code{Sample} and 
#' all other columns are added accordingly. Missing \code{ID}s in 
#' \code{sample_info} get an \code{NA} as a sample name but this is reported as
#' as warning.
#' 
#' @note This function may overwrite some of the annotation from chromatogram
#' data if \code{sample_names} was already used during import via 
#' \code{read_unicorn}. All other columns are be added accordingly.
#' 
#' @param chr_data Chromatogram data imported via \code{read_unicorn}
#' @param sample_info Sample information data frame.
#' @param Sample Obligatory \code{Sample} column for the sample information data
#' frame.
#' @param ... Additional columns for the sample information data frame.
#' 
#' @examples
#' \dontrun{
#' chr_data <- read_unicorn("Chromatogram.xls")
#' 
#' list_IDs(chr_data)
#' #                ID
#' # 1 Manual run 10:1
#' # 2 Manual run 10:5
#' # 3 Manual run 10:3
#' 
#' sample_info <- create_annotation(
#'     chr_data,
#'     Sample = c("Negative control", "Positive control", "Test"),
#'     Run = c(1, 2, 3))
#'   
#' annotate_chromatogram(chr_data, sample_info)
#' }
#' 
#' @family data annotation functions
#' 
#' @seealso \code{\link{read_unicorn}}
#' 
#' @importFrom magrittr "%<>%" "%>%" set_colnames
#' @importFrom dplyr left_join select
#' @import assertthat
#' @export 

# TODO: Write a function to import sample_info from a CSV file.

annotate_chromatogram <- function(chr_data, sample_info) {
  assert_that(is.data.frame(sample_info),
              has_name(sample_info, "ID"),
              has_name(sample_info, "Sample"))
  missing_IDs <- setdiff(chr_data$ID, sample_info$ID)
  if (length(missing_IDs) > 0) {
    warning(paste0('Following IDs are missing in sample_info: ', missing_IDs))
  }
  chr_data %<>%
    left_join(y = sample_info, by = "ID")
}

#' @rdname annotate_chromatogram
#' @export
list_IDs <- function(chr_data) {
  IDs <- chr_data %>%
    select(ID) %>%
    unique %>%
    unlist(use.names = FALSE) %>%
    data.frame(ID = .)
  return(IDs)
}

#' @rdname annotate_chromatogram
#' @export
create_annotation <- function(chr_data, Sample = NULL, ...) {
  ID <- list_IDs(chr_data)
  n = nrow(ID)
  n_groups <- sapply(list(...), length)
  
  if (is.null(Sample)) {
    Sample <- list_IDs(chr_data) %>%
      set_colnames('Sample')
  } else {
    assert_that(is.vector(Sample), are_equal(length(Sample), n))
  }
  assert_that(all(n_groups == n))
  
  annotation_df <- data.frame(ID, Sample, ...)
  return(annotation_df)
}