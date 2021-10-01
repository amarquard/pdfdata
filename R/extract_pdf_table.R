# Read a table with FACS results from a pdf
# Andrea M. Marquard, a.m.marquard@gmail.com
# Copyright Oct 1, 2021

# install.packages("tesseract")
# install.packages("pdftools")
# install.packages("tidyverse")

for (x in c("tesseract", "pdftools", "tidyverse")) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
  }
}

library(pdftools)
library(tidyverse)

# Function that takes one filepath, reads the text, and converts to a table
facs_to_table <- function(file) {

  # Read all content as one long character string
  raw_text <- pdftools::pdf_text(file)

  # Split it by newlines
  raw_lines <- raw_text %>% str_split("\n")

  # Decide where to start and end
  start_pos <- grep("Parameter", raw_lines[[1]])
  end_pos <- grep("QC Messages", raw_lines[[1]])

  relevant_lines <- raw_lines[[1]][start_pos:(end_pos - 1)]
  relevant_fields <- relevant_lines %>% str_split(" {2,}")

  relevant_3_fields <- map(relevant_fields, ~{
    if (length(.x)==2) {
      c(.x[1], "", .x[2]) # add the empty missing field
    } else {
      .x
    }
  })

  headers <- relevant_3_fields[[1]]

  final_table <- map(relevant_3_fields[-1], ~set_names(x = .x, nm = headers)) %>%
    bind_rows %>%
    mutate(Percent = as.numeric(Percent),
           `Value/AbsCnt` = as.numeric(`Value/AbsCnt`))

  return(final_table)
}

# Get a list of all files in the 'pdfs' folder
files <- list.files("pdfs", pattern = "\\.pdf$", full.names = TRUE)
names(files) <- basename(files)

# Loop over each filename, and read the table
pdf_tables <- imap(files, ~{
  # Extract the patient name, which is XXXXXX. followed by a number of digits
  patient <- str_extract(.y, "XXXXXX\\.\\d+")
  # Extract the sample name, which is everything excepth the patient name and the .pdf suffix
  sample <- str_remove(.y, "XXXXXX\\.\\d+_") %>% str_remove("\\.pdf")
  # Now apply our facs_to_table function, and reshape to have one value per column
  facs_to_table(.x) %>%
    pivot_wider(names_from = Parameter, values_from = c(Percent, `Value/AbsCnt`)) %>%
    bind_cols(patient = patient, sample = sample, file = .y)
}) %>%
  # Combine the rows from each file
  bind_rows %>%
  # Get patient, sample and filename as the first columns
  select(patient, sample, file, everything())

pdf_tables

# Save as excel, name the file by today's date
openxlsx::write.xlsx(pdf_tables, file = paste0("excel/facs_tables_", Sys.Date(), ".xlsx"))
