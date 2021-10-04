data_file <- system.file("testdata", "pdfs", "TEST.01_bsl.pdf", package = "pdfdata")

test_that("Table is extracted correctly from pdf", {

  data_tab <- get_lines_from_pdf(data_file) %>%
    get_table_from_lines(word_start = "Parameter", word_end = "QC Messages")

  expect_equal(dim(data_tab), c(10, 3))

  expected_data_tab <- structure(list(Parameter = c("Lymph Events", "Bead Events", "CD3+", "CD3+CD8+", "CD3+CD4+", "CD3+CD4+CD8+", "CD16+CD56+", "CD19+", "CD45+", "4/8 Ratio"),
                                      Percent = c("", "", "44.71", "19.56", "24.71", "0.92", "51.50", "3.51", "", ""),
                                      `Value/AbsCnt` = c("2505", "1231", "893.45", "390.89", "493.79", "18.35", "1029.07", "70.20",  "1998.30", "1.26")),
                                 row.names = c(NA, -10L),
                                 class = c("tbl_df", "tbl", "data.frame"))

  expect_equal(data_tab, expected_data_tab)
})

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
                                names_from = "Bad",
                                values_from = c("Percent", "Value/AbsCnt")),
               "names_from must be a valid column name of the extracted table")

  expect_error(extract_pdf_table(data_file,
                                 word_start = "Parameter",
                                 word_end = "QC Messages",
                                 names_from = "Parameter",
                                 values_from = c("Percent", "Value / AbsCnt")),
               "values_from must be valid column names of the extracted table")

})
