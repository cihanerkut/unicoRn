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
#' data if \code{sample_names} was already used during import via \code{read_unicorn}.
#' all other columns are be added accordingly.
#' 
#' @param chr_data Chromatogram data imported via \code{read_unicorn}
#' @param sample_info Sample information data frame.

#' @family data annotation functions
#' @seealso \code{\link{read_unicorn}}
#' @importFrom magrittr "%<>%" "%>%"
#' @import assertthat
#' @import dplyr
#' @export 

# TODO: Write a function to import sample_info from a CSV file.
# TODO: Write an addin to create the CSV file interactively.

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
  IDs <- chr_data$ID %>%
    unique %>%
    unlist(use.names = FALSE) %>%
    data.frame(IDs = .)
  print(IDs)
}