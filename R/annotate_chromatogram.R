#' Annotate chromatograms
#' 
#' Merge additional information about the sample conditions into a chromatogram
#' 
#' UNICORN sometimes codes sample information in file names using a very 
#' non-intuitive naming scheme. In such cases, adding sample information during
#' import via \code{read_unicorn} is not straightforward. An easier approach is
#' then to parse the data without sample names, list sample IDs, build a data 
#' frame accordingly, and merge it with the chromatogram.
#' 
#' The information is supplied in a data frame called \code{sample_info}. This
#' data frame must have at least two columns: \code{ID} and \code{Sample} (case 
#' sensitive). Merging is done based on the \code{ID} column. \code{Sample} and 
#' all other columns are be added accordingly.
#' @
#' @note This function may overwrite some of the annotation from chromatogram
#' data if \code{sample_names} was used during import via \code{read_unicorn}.
#' 
#' @param chr_data Chromatogram data imported via \code{read_unicorn}
#' @param sample_info Sample information data frame.
#'
#' @importFrom magrittr "%<>%"
#' @import assertthat
#' @import dplyr
#' @export 

# TODO: Check if any IDs are missing in sample_info
# TODO: Write a function to list IDs.
# TODO: Write a function to import sample_info from a CSV file.
# TODO: Write an addin to create the CSV file interactively.

annotate_chromatogram <- function(chr_data, sample_info) {
  assert_that(is.data.frame(sample_info),
              has_name(sample_info, "ID"),
              has_name(sample_info, "Sample"))
  chr_data %<>%
    left_join(y = sample_info, by = "ID")
}