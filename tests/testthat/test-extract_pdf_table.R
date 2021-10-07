data_file <- system.file("testdata", "pdfs", "TEST.01_bsl.pdf", package = "pdfdata")

test_that("Table is converted into one row", {
  data_row <- extract_pdf_table(data_file,
                                word_start = "Parameter",
                                word_end = "QC Messages",
                                names_from = "Parameter",
                                values_from = c("Percent", "Value/AbsCnt"))
  expect_equal(dim(data_row), c(1, 22))
  expect_equal(data_row$sample[1], "LU2006.01_bsl")
  expect_equal(data_row$file[1], "TEST.01_bsl.pdf")
})

test_that("Invalid colnames give error", {
  expect_error(extract_pdf_table(data_file,
                                 word_start = "Parameter",
                                 word_end = "QC Messages",
                                 names_from = "BAD",
                                 values_from = c("Percent", "Value/AbsCnt")),
               "names_from must be a valid column name of the extracted table")

  expect_error(extract_pdf_table(data_file,
                                 word_start = "Parameter",
                                 word_end = "QC Messages",
                                 names_from = "Parameter",
                                 values_from = c("Percent", "BAD")),
               "values_from must be valid column names of the extracted table")

})
