context("download")

## TODO: Add more tests

test_that("download works", {
  x <- baixa_img_audio()
  expect_is(x, 'list')
  expect_length(x, 2L)
})
