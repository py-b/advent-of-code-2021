test_that("solve08", {

  expect_equal(
    solve08a(example_data_08()),
    26
  )

  expect_equal(
    solve08b(example_data_08()),
    61229
  )

})
