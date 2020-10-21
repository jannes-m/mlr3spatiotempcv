context("ResamplingRepeatedSptCVcluto")

test_that("folds can be printed", {
  skip_on_os("mac")
  task = tsk("cookfarm")
  rsp = rsmp("repeated_sptcv_cluto", folds = 3, repeats = 5, time_var = "Date")
  rsp$instantiate(task)

  expect_equal(rsp$folds(4:8), c(4, 5, 1, 2, 3))
})

test_that("reps and folds can be printed", {
  skip_on_os("mac")
  task = tsk("cookfarm")
  rsp = rsmp("repeated_sptcv_cluto", folds = 3, repeats = 5, time_var = "Date")
  rsp$instantiate(task)

  expect_equal(rsp$repeats(4:8), c(2, 2, 2, 3, 3))
  expect_equal(rsp$folds(10), 5)
})

test_that("resampling iterations equals folds * repeats", {
  skip_on_os("mac")
  task = tsk("cookfarm")
  rsp = rsmp("repeated_sptcv_cluto", folds = 3, repeats = 5, time_var = "Date")
  rsp$instantiate(task)

  expect_equal(rsp$iters, 15)
})

test_that("resampling iterations equals folds * repeats", {
  skip_on_os("mac")
  task = tsk("cookfarm")
  rsp = rsmp("repeated_sptcv_cluto", folds = 3, repeats = 5, time_var = "Date")
  rsp$instantiate(task)

  expect_equal(rsp$iters, 15)
})