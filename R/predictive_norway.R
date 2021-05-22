library(mlr)
library(tibble)
library(iml)
library(patchwork)
source("R/preprocess_norge.R")
newest_numbers$geometry <- NULL
feature_names <- tibble(
  ugly = colnames(newest_numbers[, 2:15]),
  noice = c(
    "Median age", "Total unemployment", "Unemployed immigrants",
    "Total immigrants", "Marketplace", "Place of worship", "Nursing home",
    "Aerodrome", "Office", "Platform", "Higher education", "Urban density",
    "Sex", "Vaccinations"
  )
)
set.seed(7918)
test <- sample(
  seq_len(nrow(newest_numbers)),
  size = floor(0.2 * nrow(newest_numbers))
)
newest_numbers_train <- newest_numbers[-test, ]
newest_numbers_test <- newest_numbers[test, ]
task <- makeRegrTask(
  id = "demographic",
  data = newest_numbers_train[, 1:15],
  target = "value"
)
learners <- list(
  makeLearner("regr.fnn"),
  makeLearner("regr.nnet"),
  makeLearner("regr.randomForestSRC"),
  makeLearner("regr.rpart"),
  makeLearner("regr.xgboost")
)
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
rdesc <- makeResampleDesc("CV", iters = 3)
ctrl <- makeTuneControlIrace(maxExperiments = 300)
# 385k
# remove node harvest, 31
tunes <- list()
parallelMap::parallelStartSocket(7)
for (x in 1:5) {
  print(x)
  if(class(hyper_pars[[x]]) == "ParamSet") {
    tunes <- c(
      tunes, list(try(
        tuneParams(
          learners[[x]],
          task,
          rdesc,
          par.set = hyper_pars[[x]],
          control = ctrl
        ), silent = TRUE
      )
    ))
  } else {
    tunes <- c(
      tunes,
      NA
    )
  }
}
parallelMap::parallelStop()
learners <- lapply(
  1:5,
  function(x, ...) {
    if (class(tunes[[x]]) %in% c("logical", "try-error")) {
      learners[[x]]
    } else {
      setHyperPars(learners[[x]], par.vals = tunes[[x]]$x)
    }
  }
)

models <- lapply(
  learners,
  function(x, ...) {
    try(train(x, task), silent = TRUE)
  }
)

predictions_test <- lapply(
  models,
  function(x, ...) {
    try(predict(x, newdata = newest_numbers_test[, 1:15]), silent = TRUE)
  }
)

all_positive <- lapply(
  predictions_test,
  function(x) {
    all(x$data$response > 0)
  }
)

pos_predictions_test <- predictions_test[unlist(all_positive)]

predictions_train <- lapply(
  models,
  function(x, ...) {
    try(predict(x, task), silent = TRUE)
  }
)

pos_predictions_train <- predictions_train[unlist(all_positive)]

pos_learners <- learners[unlist(all_positive)]

mae_train <- lapply(
  pos_predictions_train,
  function(x) {
    mean(abs(x$data$truth - x$data$response))
  }
)
mae_test <- lapply(
  pos_predictions_test,
  function(x) {
    mean(abs(x$data$truth - x$data$response))
  }
)

learner_names <- lapply(
  pos_learners,
  function(x) x$id
)

results_tibble <- tibble(
  learner = unlist(learner_names),
  mae_train = unlist(mae_train),
  mae_test = unlist(mae_test)
)
results_tibble[order(results_tibble$mae_test), ]
model_list <- list(
  results_tibble = results_tibble,
  models = models,
  newest_numbers_test = newest_numbers_test,
  newest_numbers_train = newest_numbers_train,
  feature_names = feature_names
)
save(model_list, file = "models/predictive_norway.Rda")