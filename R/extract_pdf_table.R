# Read a table with FACS results from a pdf
# Andrea M. Marquard, a.m.marquard@gmail.com
# Copyright Oct 1, 2021

# install.packages("tesseract")
# install.packages("pdftools")
# install.packages("tidyverse")

# for (x in c("tesseract", "pdftools", "tidyverse")) {
#   if (!require(x, character.only = TRUE)) {
#     install.packages(x, dependencies = TRUE)
#   }
# }


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

  # Read all content as one long character string
  raw_text <- pdftools::pdf_text(file)

  # Split it by newlines
  raw_lines <- raw_text %>% stringr::str_split("\n")

  # Decide where to start and end
  start_pos <- grep("Parameter", raw_lines[[1]])
  end_pos <- grep("QC Messages", raw_lines[[1]])

  relevant_lines <- raw_lines[[1]][start_pos:(end_pos - 1)]
  relevant_fields <- relevant_lines %>% stringr::str_split(" {2,}")

  relevant_3_fields <- purrr::map(relevant_fields, ~{
    if (length(.x)==2) {
      c(.x[1], "", .x[2]) # add the empty missing field
    } else {
      .x
    }
  })

  headers <- relevant_3_fields[[1]]

  final_table <- purrr::map(relevant_3_fields[-1], ~purrr::set_names(x = .x, nm = headers)) %>%
    dplyr::bind_rows %>%
    dplyr::mutate(Percent = as.numeric(Percent),
           `Value/AbsCnt` = as.numeric(`Value/AbsCnt`))

  return(final_table)
}


#' Read data table from a pdf
#'
#' @param file_path path to pdf
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
extract_pdf_table <- function(file_path) {

  file_name <- basename(file_path)

  # Extract the patient name, which is XXXXXX. followed by a number of digits
  patient <- stringr::str_extract(file_name, "XXXXXX\\.\\d+")

  # Extract the sample name, which is everything excepth the patient name and the .pdf suffix
  sample <- stringr::str_remove(file_name, "XXXXXX\\.\\d+_") %>% stringr::str_remove("\\.pdf")

  # Now apply our facs_to_table function, and reshape to have one value per column
  facs_to_table(file_path) %>%
    tidyr::pivot_wider(names_from = Parameter, values_from = c(Percent, `Value/AbsCnt`)) %>%
    dplyr::bind_cols(patient = patient, sample = sample, file = file_name)
}

utils::globalVariables(c("Parameter", "Percent", "Value/AbsCnt"))

# # Save as excel, name the file by today's date
# openxlsx::write.xlsx(pdf_tables, file = paste0("excel/facs_tables_", Sys.Date(), ".xlsx"))
