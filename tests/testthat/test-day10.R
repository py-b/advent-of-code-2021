test_that("solve10", {

  expect_equal(
    solve10a(example_data_10()),
    26397
  )

  expect_equal(
    solve10b(example_data_10()),
    288957
  )

})
