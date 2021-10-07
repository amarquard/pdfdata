#' Read data table from a pdf
#'
#' @param file_path path to pdf
#' @param word_start word that marks the start of the table (first word in the table of interest)
#' @param word_end word that marks the end of the table (first word after the table ends)
#' @param names_from table columns that are used to construct names for values
#' (eg. "CD4+", "Bead events", etc...)
#' @param values_from table columns that contain actual values (eg. "Percent", "Value/AbsCnt").
#' Currently all values are assumed to be numeric.
#'
#' @return a tibble with extracted data values, and columns 'patient', 'sample' and 'file'
#' @export
#'
#' @examples
#' # Get a list of all PDF files in a folder (in this case our testdata folder)
#' folder <- system.file("testdata", "pdfs", package = "pdfdata")
#' files <- list.files(folder, pattern = "\\.pdf", full.names = TRUE)
#'
#' pdf_tables <- purrr::map(files,
#' extract_pdf_table,
#' word_start = "Parameter",
#' word_end = "QC Messages",
#' names_from = "Parameter",
#' values_from = c("Percent", "Value/AbsCnt")) %>%
#'   # Combine the rows from each file
#'   dplyr::bind_rows()
#'
#' pdf_tables
extract_pdf_table <- function(file_path, word_start, word_end, names_from, values_from) {

  file_name <- basename(file_path)

  # Read raw data from pdf
  raw_lines <- get_lines_from_pdf(file_path)

  # Sample is the first line of the pdf
  sample_name <- raw_lines[1]

  # Extract table values from the raw lines
  raw_table <- get_table_from_lines(raw_lines, word_start = word_start, word_end = word_end)

  # Check colnames vs. params
  if (any(!names_from %in% colnames(raw_table))) {
    usethis::ui_stop("names_from must be a valid column name of the extracted table")
  }
  if (any(!values_from %in% colnames(raw_table))) {
    usethis::ui_stop("values_from must be valid column names of the extracted table")
  }

  raw_table %>%

    # Convert values to numeric
    dplyr::mutate(dplyr::across(dplyr::all_of(values_from),  as.numeric)) %>%

    # Reshape to have one value per column
    tidyr::pivot_wider(names_from = dplyr::all_of(names_from),
                       values_from = dplyr::all_of(values_from)) %>%

    # Add info about sample and filename
    dplyr::bind_cols(sample = sample_name, file = file_name) %>%

    # Reorder columns
    dplyr::select(file, sample, dplyr::everything())
}



get_table_from_lines <- function(pdf_lines, word_start, word_end) {

  # Decide where to start and end
  start_pos <- grep(word_start, pdf_lines)
  end_pos <- grep(word_end, pdf_lines)
  inds <- start_pos:(end_pos - 1)

  table_lines <- pdf_lines[inds] %>%
    # Remove empty lines
    purrr::discard(~ .x == "")

  # Split first row on whitespace, to get colnames
  col_names <- stringr::str_split(table_lines[1], " {2,}")[[1]]

  # Get column positions from fixed-width data
  col_positions <- readr::fwf_empty(table_lines[-1], col_names = col_names)

  # Create table from fixed-width data
  readr::read_fwf(table_lines[-1], col_positions)
}

get_lines_from_pdf <- function(file) {
  # Read all content as one long character string
  raw_text <- pdftools::pdf_text(file)

  # Split it by newlines
  raw_lines <- raw_text %>% stringr::str_split("\n")

  raw_lines[[1]]
}
