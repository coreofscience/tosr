test_that("create ToS correctly", {
  data("my_tosr_load")

  expect_equal(
    nrow(tosSAP(my_tosr_load$graph,
                my_tosr_load$df,
                my_tosr_load$nodes)),
    141
  )
})
