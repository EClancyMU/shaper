library(shaper)


test_that("area.rectangle calculates area correctly", {
  my_rectangle <- rectangle(height = 2, width = 4)
  area_rectangle <- area(my_rectangle)
  expect_equal(area_rectangle, 2 * 4)

  expect_equal(2 * 2, 4)
})
