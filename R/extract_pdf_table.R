#' Read data table from a pdf
#'
#' @param file_path path to pdf
#'
#' @return a tibble with extracted data values, and columns 'patient', 'sample' and 'file'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get a list of all PDF files in a folder (in this case our testdata folder)
#' folder <- system.file("testdata", "pdfs", package = "pdfdata")
#' files <- list.files(folder, pattern = "\\.pdf", full.names = TRUE)
#'
#' pdf_tables <- purrr::map(files, extract_pdf_table) %>%
#'   # Combine the rows from each file
#'   dplyr::bind_rows()
#'
#' pdf_tables
#' }
extract_pdf_table <- function(file_path) {

  file_name <- basename(file_path)

  # Read raw data from pdf
  raw_lines <- get_lines_from_pdf(file_path)

  # Sample is the first line of the pdf
  sample_name <- raw_lines[[1]][1]

  # Extract table values from the raw lines
  get_table_from_lines(raw_lines) %>%
    # Reshape to have one value per column
    tidyr::pivot_wider(names_from = Parameter, values_from = c(Percent, `Value/AbsCnt`)) %>%
    # Add info about sample and filename
    dplyr::bind_cols(sample = sample_name, file = file_name) %>%
    # Reorder columns
    dplyr::select(file, sample, dplyr::everything())
}

utils::globalVariables(c("Parameter", "Percent", "Value/AbsCnt"))

# # Save as excel, name the file by today's date
# openxlsx::write.xlsx(pdf_tables, file = paste0("excel/facs_tables_", Sys.Date(), ".xlsx"))



get_table_from_lines <- function(pdf_lines) {

  # Decide where to start and end
  start_pos <- grep("Parameter", pdf_lines[[1]])
  end_pos <- grep("QC Messages", pdf_lines[[1]])

  relevant_fields <- pdf_lines[[1]][start_pos:(end_pos - 1)] %>%
    purrr::discard(~ .x == "") %>%
    stringr::str_split(" {2,}")
  # TODO: shouls be fixed width splitting instead

  relevant_3_fields <- purrr::map(relevant_fields, ~{
    if (length(.x)==2) {
      c(.x[1], "", .x[2]) # add the empty missing field
    } else {
      .x
    }
  })

  headers <- relevant_3_fields[[1]]

  purrr::map(relevant_3_fields[-1], ~purrr::set_names(x = .x, nm = headers)) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(Percent = as.numeric(Percent),
                  `Value/AbsCnt` = as.numeric(`Value/AbsCnt`))

}

get_lines_from_pdf <- function(file) {
  # Read all content as one long character string
  raw_text <- pdftools::pdf_text(file)

  # Split it by newlines
  raw_text %>% stringr::str_split("\n")
}
