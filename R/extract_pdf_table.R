#' Read data table from a pdf
#'
#' @param file_path path to pdf
#' @param patient_pattern pattern to use to extract patient name from file name
#' @param patient_sep separator between patient name and sample name in the file name
#'
#' @return a tibble with extracted data values, and columns 'patient', 'sample' and 'file'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get a list of all files in the 'pdfs' folder
#' files <- list.files("pdfs", pattern = "\\.pdf$", full.names = TRUE)
#'
#' pdf_tables <- purrr::map(files, extract_pdf_table) %>%
#'   # Combine the rows from each file
#'   dplyr::bind_rows() %>%
#'   # Get patient, sample and filename as the first columns
#'   dplyr::select(patient, sample, file, dplyr::everything())
#'
#' pdf_tables
#' }
extract_pdf_table <- function(file_path, patient_pattern, patient_sep = "_") {

  file_name <- basename(file_path)

  # Extract the patient name, which is XXXXXX. followed by a number of digits
  patient <- stringr::str_extract(file_name, patient_pattern)

  # Extract the sample name, which is everything except the patient name and the .pdf suffix
  sample <- stringr::str_remove(file_name, paste0(patient, patient_sep)) %>% stringr::str_remove("\\.pdf")

  # Now apply our facs_to_table function, and reshape to have one value per column
  facs_to_table(file_path) %>%
    tidyr::pivot_wider(names_from = Parameter, values_from = c(Percent, `Value/AbsCnt`)) %>%
    dplyr::bind_cols(patient = patient, sample = sample, file = file_name)
}

utils::globalVariables(c("Parameter", "Percent", "Value/AbsCnt"))

# # Save as excel, name the file by today's date
# openxlsx::write.xlsx(pdf_tables, file = paste0("excel/facs_tables_", Sys.Date(), ".xlsx"))



#' Function that takes one filepath, reads the text, and converts to a table
#'
#' @param file path to pdf file
#'
#' @return tibble with data values from table
#' @export
#'
#' @examples
#' \dontrun{
#' facs_to_table(file_path)
#' }
facs_to_table <- function(file) {

  raw_lines <- get_lines_from_pdf(file)

  sample_name <- raw_lines[[1]][1]

  # Decide where to start and end
  start_pos <- grep("Parameter", raw_lines[[1]])
  end_pos <- grep("QC Messages", raw_lines[[1]])

  relevant_fields <- raw_lines[[1]][start_pos:(end_pos - 1)] %>%
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

  final_table <- purrr::map(relevant_3_fields[-1], ~purrr::set_names(x = .x, nm = headers)) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(Percent = as.numeric(Percent),
                  `Value/AbsCnt` = as.numeric(`Value/AbsCnt`))

  return(final_table)
}

get_lines_from_pdf <- function(file) {
  # Read all content as one long character string
  raw_text <- pdftools::pdf_text(file)

  # Split it by newlines
  raw_text %>% stringr::str_split("\n")
}
