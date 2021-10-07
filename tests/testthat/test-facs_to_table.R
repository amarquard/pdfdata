data_file <- system.file("testdata", "pdfs", "TEST.01_bsl.pdf", package = "pdfdata")

test_that("Table is extracted correctly from pdf", {

  # Check number of lines
  data_lines <- get_lines_from_pdf(data_file)

  expect_length(data_lines, 45)

  # Check dims
  data_tab <- data_lines %>%
    get_table_from_lines(word_start = "Parameter", word_end = "QC Messages")

  expect_equal(dim(data_tab), c(10, 3))

  # Check data values
  expected_data_tab <- structure(
    list(
      Parameter = c("Lymph Events", "Bead Events", "CD3+", "CD3+CD8+", "CD3+CD4+", "CD3+CD4+CD8+", "CD16+CD56+", "CD19+", "CD45+", "4/8 Ratio"),
      Percent = c(NA, NA, 44.71, 19.56, 24.71, 0.92, 51.50, 3.51, NA, NA),
      `Value/AbsCnt` = c(2505, 1231, 893.45, 390.89, 493.79, 18.35, 1029.07, 70.20,  1998.30, 1.26)
    ),
    row.names = c(NA, -10L),
    class = c("spec_tbl_df", "tbl_df", "tbl", "data.frame")
  )

  attr(data_tab, "spec") <- NULL

  expect_equal(data_tab, expected_data_tab)
})
