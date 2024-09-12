test_that("grab cached GEDI works", {
  find_check <- function(gedi_prod) {
    pc_find <- find_gedi(pcreek,
      gedi_product = gedi_prod,
      date_start = "2023-01-01",
      date_end = "2023-01-31"
    )

    grabbed <- grab_gedi(pc_find)

    testthat::expect_s3_class(grabbed, "arrow_dplyr_query")
  }

  purrr::walk(
    c("1B", "2A", "2B", "4A"), find_check
  )
})
