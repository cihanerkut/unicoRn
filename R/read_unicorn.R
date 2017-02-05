#' Import chromatograms
#'
#' Read UNICORN chromatogram data
#'
#' If \code{sample_names} is a character vector, a new column named as
#' \code{Sample} will be appended to the data frame \emph{in the same order} as
#' curve names. However, this is not recommended as the order of the curves may
#' change or some curves may be excluded at some point during data import. In
#' the latter case, you will get an error message. The safest way is to define a
#' data frame with the column names \code{ID} and \code{Sample}, matching the
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
#' @section Parsing RES files:
#' UNICORN has a native, binary file format to store all data acquired during 
#' a chromatography run. These files have the extension ".res" and they are 
#' stored locally on the computer connected to the HPLC system. The function 
#' \code{read_res} is implemented to extract UV cell sensor data directly from 
#' a RES file. However, the file format is closed-source. It has been partially 
#' reverse engineered and a Python script was written to extract data from a 
#' RES file. The \code{read_res} function here is based on those specifications. 
#' For more information, please see 
#' \href{https://github.com/pyahmed/PyCORN/blob/master/pycorn/docs/RES_files_layout.txt}{this}.
#'
#' @param file_name The file to be imported. \code{read_unicorn} can import 
#' either an Excel file or an ASC (currently not implemented). \code{read_res} 
#' can import a binary RES file (not fully implemented).
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
#' @param smooth Use cubic spline smoothing on sensor data imported from a RES 
#' file.
#' @param verbose Display additional progress information. Default is
#' \code{FALSE}.
#'
#' @return A tibble that contains volume, (baselined) absorbance, 
#' wavelength/channel, curve and sample name information for the experiment.
#'
#' @examples
#' \dontrun{
#' samples <- data.frame(
#'     ID = c("Manual run 10:1", "Manual run 10:5", "Manual run 10:3"),
#'     Sample = c("Negative control", "Positive control", "Test"))
#' chr_data <- read_unicorn("Chromatogram.xls", samples)
#' }
#'
#' @seealso \code{\link{annotate_chromatogram}} for annotating chromatograms
#' without sample information, \code{\link{list_IDs}} to see parsed curve 
#' names, \code{\link{create_annotation}} to create a data frame with sample
#' information.
#' 
#' @importFrom magrittr "%>%" "%<>%" set_colnames
#' @importFrom tools file_ext
#' @importFrom dplyr filter mutate group_by mutate_at mutate_all left_join 
#' combine as.tbl vars ungroup
#' @importFrom tidyr gather separate
#' @importFrom readxl read_excel
#' @import assertthat

# TODO: Tests for different combinations, work on combined
# TODO: Import .RES from scouting directory

#' @export
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
    "Curves must be normalized for retention volume!"
  }
  
  # Custom assertion for file type
  is_valid_filetype <- function(x) {
    assert_that(is.readable(file_name))
    has_extension(x, "xls") | has_extension(x, "asc")
  }
  on_failure(is_valid_filetype) <- function(call, env) {
    "File extension must be either xls or asc"
  }
  
  # Validate file type
  assert_that(is_valid_filetype(file_name))
  
  # Validate and organize sample_names
  if (!is.null(sample_names)) {
    if (combined) {
      if (is.data.frame(sample_names)) {
        assert_that(has_name(sample_names, "ID"),
                    has_name(sample_names, "Sample"))
        sample_names %<>%
          mutate_at(vars(ID), as.character)
      } else {
        assert_that(is.character(sample_names))
      }
    } else {
      assert_that(is.string(sample_names))
    }
  }
  
  # Validate other parameters
  assert_that(
    is.flag(hardware_autozero),
    is.flag(single_channel),
    is.flag(combined),
    is.numeric(reference_measurement)
  )
  
  file_type <- file_ext(file_name)
  
  (file_type == "asc") &&
    stop("Parsing asc files is not yet implemented!")
  
  # Import raw data
  if (verbose) {
    message(paste0("Constructing data from ", file_name))
  }
  
  if (single_channel) {
    chr_data <-
      read_excel(file_name, skip = 2) %>%
      set_colnames(c("Volume", "A")) %>%
      filter(!is.na(A)) %>%
      mutate_all(as.numeric)
    if (!is.null(sample_names)) {
      chr_data %<>%
        mutate(Sample = sample_names)
    }
  } else {
    chr_data <- read_excel(file_name, skip = 1) %>%
      slice(-1)
    assert_that(is_retention_normalized(chr_data))
    colnames(chr_data)[1] <- "Volume"
    chr_data %<>%
      mutate_all(as.numeric) %>%
      gather(key = "ID", value = "A", -Volume) %>%
      filter(!is.na(A)) %>%
      separate(ID, c("ID", "Channel", "Wavelength"), sep = "_")
    chr_data$ID %<>% factor
    if (!is.null(sample_names)) {
      if (is.data.frame(sample_names)) {
        sample_names$ID %<>% factor
        chr_data %<>%
          left_join(y = sample_names, by = "ID")
      } else {
        chr_data %<>%
          mutate(Sample = factor(ID, labels = unique(sample_names)))
      }
    }
  }
  
  # Software autozero
  if (verbose) {
    message("Shifting baseline towards 0")
  }
  
  if (!hardware_autozero) {
    if (single_channel) {
      chr_data %<>%
        mutate(A_norm = A - A[reference_measurement])
    } else {
      chr_data %<>%
        group_by(ID, Wavelength) %>%
        mutate(A_norm = A - A[reference_measurement])
    }
  }
  
  chr_data %<>%
    ungroup
  
  if (verbose) {
    print(summary(chr_data, digits = 3))
  }
  
  return(chr_data)
}

#' @rdname read_unicorn
#' @export
read_res <- function(file_name, 
                     smooth = TRUE,  
                     verbose = FALSE) {
  
  # Verify the RES file
  if (verbose) {
    message('Parsing .RES file...')
  }
  id1 <- as.raw(c(0x11, 0x47, 0x11, 0x47, 0x18, 0x00, 0x00, 0x00, 0xB0, 0x02, 
                  0x00, 0x00, 0x20, 0x6C, 0x03, 0x00))
  id2 <- charToRaw('UNICORN 3.10')
  id3 = as.raw(c(0x00, 0x00, 0x01, 0x00, 0x02, 0x00, 0x01, 0x13))
  
  file_size_external <- file.size(file_name)
  raw_data <- readBin(file_name, raw(), n = file_size_external)
  file_size_internal <- btoi(raw_data, 17)
  stopifnot(grepRaw(id1, raw_data) > 0, 
            grepRaw(id2, raw_data, offset = 25) > 0, 
            file_size_internal == file_size_external)
  
  # Extract data addresses
  if (verbose) {
    message('Reading header table...')
  }
  header_end <- grepRaw(id3, raw_data) + 342
  header_table <- data.frame()
  for (i in seq(687, header_end + 342, by = 344)) {
    data_group <- raw_data[i:(i + 320)]
    
    group_id <- paste0(data_group[1:8], collapse = '')
    group_label <- data_group[9:304]
    group_label <- rawToChar(group_label[group_label != 0])
    group_data_size <- btoi(data_group, 305)
    # group_next_offset <- raw_to_int(data_group, 309)
    group_data_address <- btoi(data_group, 313)
    group_data_offset <- btoi(data_group, 317)
    
    group_data_start <- group_data_address + group_data_offset
    group_data_end <- group_data_address + group_data_size
    
    header_line <- data.frame(group_id, 
                              group_label, 
                              group_data_address, 
                              group_data_size, 
                              group_data_start, 
                              group_data_end)
    header_table <- rbind(header_table, header_line)
  }
  colnames(header_table) <- c('ID', 
                              'Label', 
                              'Data address', 
                              'Data size', 
                              'Start', 
                              'End')
  
  # Extract sensor data (currently only UV)
  if (verbose) {
    message('Extracting sensor data...')
  }
  UV_header <- header_table %>% 
    slice(grep('UV', .$Label)) %>% 
    mutate(Start = Start + 1) %>%
    separate(Label, c('Curve', 'Channel', 'Wavelength'), '_', remove = FALSE)
  
  UV_data <- list()
  for (i in 1:nrow(UV_header)) {
    h <- UV_header[i,]

    UV_df <- raw_data[h$Start:h$End] %>%
      rawToBits %>%
      matrix(ncol = 32, byrow = TRUE) %>%
      apply(1, function(x, i) packBits(x[i], 'int')) %>%
      matrix(ncol = 2, byrow = TRUE) %>%
      as.data.frame %>%
      set_colnames(c('Volume', 'A')) %>%
      mutate(Volume = Volume / 100,
             A = A / 1000)
    
    if (smooth) {
      message(paste0("Smoothing curve ", h$Channel, "..."))
      UV_df <- spline(UV_df$Volume, 
                      UV_df$A, 
                      n = length(UV_df$A)) %>%
        data.frame %>%
        set_colnames(c('Volume', 'A'))
    }
    
    UV_data <- combine(UV_data, UV_df)
  }
  
  if (verbose) {
    message('Reshaping sensor data...')
  }
  UV_data %<>% 
    unique %>%
    data.frame %>%
    set_colnames(c('Volume', paste(UV_header$Channel, 
                                   UV_header$Wavelength, 
                                   sep = '_'))) %>%
    gather(key = 'ID', value = 'A', -Volume) %>%
    separate(ID, c('Channel', 'Wavelength'), '_') %>%
    mutate(Sample = unique(UV_header$Curve),
           Channel = factor(Channel),
           Wavelength = factor(Wavelength)) %>%
    group_by(Wavelength) %>%
    mutate(A_norm = A - min(A)) %>%
    as.tbl
  
  return(UV_data)
}
  
btoi <- function(raw_data, i) {
  raw_data[i:(i + 3)] %>%
    rawToBits %>%
    packBits('int')
}