test_that("find cached GEDI works", {
  find_check <- function(gedi_prod) {
    pc_find <- find_gedi(pcreek,
      gedi_product = gedi_prod,
      date_start = "2023-01-01",
      date_end = "2023-01-31"
    )

    testthat::expect_s3_class(pc_find, "chewie.find")
    testthat::expect_equal(nrow(pc_find), 1)
  }

  purrr::walk(
    c("1B", "2A", "2B", "4A"), find_check
  )
})
