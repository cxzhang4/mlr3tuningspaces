test_that("tuning spaces can be applied", {
  data = as.data.table(mlr_tuning_spaces)
  mapply(test_tuning_space, data$key, data$learner)
})


test_that("rtdl tuning spaces work", {
  skip_if_not_installed("mlr3torch")

  ts = lts("regr.ft_transformer.rtdl")
  lrn_regr_ft = ts$get_learner(validate = 0.2, measures_valid = msr("regr.mae"))

  lrn_regr_ft$param_set$set_values(batch_size = 10, epochs = 10)

  lrn_regr_ft$param_set$values

  instance = ti(
    task = tsk("boston_housing"),
    learner = lrn_regr_ft,
    resampling = rsmp("cv", folds = 3),
    measures = msr("regr.rmse"),
    terminator = trm("none")
  )

  tuner = tnr("random_search")

  tuner$optimize(instance)
})
