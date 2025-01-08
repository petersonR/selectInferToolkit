data(raw_data)
y= raw_data$hdl1
x <-model.matrix(hdl1 ~., model.frame(~ ., raw_data, na.action=na.pass))[,-1]


test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
