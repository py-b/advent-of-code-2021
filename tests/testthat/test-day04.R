test_that("solve04", {

  expect_equal(
    solve04a(example_data_04()),
    4512
  )

  expect_equal(
    solve04b(example_data_04()),
    1924
  )

})
