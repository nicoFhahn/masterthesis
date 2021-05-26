# this is the script for computing the predictive models for norway
library(mlr)
library(tibble)
source("R/preprocess_norge.R")
# remove the geometry column
newest_numbers$geometry <- NULL
# get nicer feature names for plotting later
feature_names <- tibble(
  ugly = colnames(newest_numbers[, 2:14]),
  noice = c(
    "Median age", "Total unemployment", "Unemployed immigrants",
    "Total immigrants", "Marketplace", "Place of worship", "Nursing home",
    "Office", "Platform", "Higher education", "Urban density",
    "Sex", "Vaccinations"
  )
)
set.seed(7918)
# draw the test sample
test <- sample(
  seq_len(nrow(newest_numbers)),
  size = floor(0.2 * nrow(newest_numbers))
)
# create the training and test data
newest_numbers_train <- newest_numbers[-test, ]
newest_numbers_test <- newest_numbers[test, ]
# create the task
task <- makeRegrTask(
  id = "demographic",
  data = newest_numbers_train[, 1:14],
  target = "value"
)
# define the learners
learners <- list(
  makeLearner("regr.fnn"),
  makeLearner("regr.nnet"),
  makeLearner("regr.randomForestSRC"),
  makeLearner("regr.rpart"),
  makeLearner("regr.xgboost")
)
# define the param sets used for tuning
hyper_pars <- list(
  makeParamSet(
    makeIntegerParam(
      "k",
      1,
      12
    )
  ),
  makeParamSet(
    makeIntegerParam(
      "size",
      1,
      12
    ),
    makeIntegerParam(
      "maxit",
      25,
      400
    )
  ),
  makeParamSet(
    makeIntegerParam(
      "ntree",
      250,
      4000
    ),
    makeIntegerParam(
      "nodesize",
      2,
      20
    )
  ),
  makeParamSet(
    makeIntegerParam(
      "minsplit",
      5,
      80
    ),
    makeIntegerParam(
      "minbucket",
      1,
      10
    ),
    makeIntegerParam(
      "maxcompete",
      1,
      16
    ),
    makeIntegerParam(
      "maxsurrogate",
      2,
      20
    ),
    makeIntegerParam(
      "maxdepth",
      1,
      30
    ),
    makeIntegerParam(
      "xval",
      3,
      40
    )
  ),
  makeParamSet(
    makeNumericParam(
      "eta",
      0,
      1
    ),
    makeNumericParam(
      "gamma",
      0,
      10
    ),
    makeIntegerParam(
      "max_depth",
      2,
      24
    )
  )
)
# use 3-fold cv for resampling
rdesc <- makeResampleDesc("CV", iters = 3)
# use irace as tune control
ctrl <- makeTuneControlIrace(maxExperiments = 300)
# create a list for the tunes
tunes <- list()
# use parallel computing for faster tuning
parallelMap::parallelStartSocket(7)
# do the tuning
for (x in 1:5) {
  tunes <- c(
    tunes, list(
      tuneParams(
        learners[[x]],
        task,
        rdesc,
        par.set = hyper_pars[[x]],
        control = ctrl
      )
    )
  )
}
# stop parallel computing
parallelMap::parallelStop()
# set the parameter values for the learners
learners <- lapply(
  1:5,
  function(x, ...) {
    setHyperPars(learners[[x]], par.vals = tunes[[x]]$x)
  }
)
# train the models
models <- lapply(
  learners,
  train,
  task
)
# predict using the test data
predictions_test <- lapply(
  models,
  predict,
  newdata = newest_numbers_test[, 1:14]
)
# predict using the train data
predictions_train <- lapply(
  models,
  predict,
  task
)
# calculate the mae for train
mae_train <- lapply(
  predictions_train,
  function(x) {
    mean(abs(x$data$truth - x$data$response))
  }
)
# calculate the mae for test
mae_test <- lapply(
  predictions_test,
  function(x) {
    mean(abs(x$data$truth - x$data$response))
  }
)
# get the learner names
learner_names <- lapply(
  learners,
  function(x) x$id
)
# create tibble with all the results saved
results_tibble <- tibble(
  learner = unlist(learner_names),
  mae_train = unlist(mae_train),
  mae_test = unlist(mae_test)
)
# and save everything in a nice list
model_list <- list(
  results_tibble = results_tibble,
  models = models,
  newest_numbers_test = newest_numbers_test,
  newest_numbers_train = newest_numbers_train,
  feature_names = feature_names
)
save(model_list, file = "models/predictive_norway.Rda")
