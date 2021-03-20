library(ggregplot)
library(INLA)
library(INLAutils)
source("R/preprocess_germany.R")
parallelMap::parallelStartSocket(7)
newest_numbers$geometry <- NULL
set.seed(420)
stack_all <- inla.stack(
  data = list(value = newest_numbers$value),
  A = list(1),
  effects = list(
    data.frame(
      Intercept = 1,
      newest_numbers[, c(2:15, 20:35, 43:46)]
    )
  )
)
stack_demo <- inla.stack(
  data = list(value = newest_numbers$value),
  A = list(1),
  effects = list(
    data.frame(
      Intercept = 1,
      newest_numbers[, c(2:15, 43:45)]
    )
  )
)
stack_infra <- inla.stack(
  data = list(value = newest_numbers$value),
  A = list(1),
  effects = list(
    data.frame(
      Intercept = 1,
      newest_numbers[, c(20:35, 43, 44, 46)]
    )
  )
)
result_all_backwards <- INLAstep(
  fam1 = "nbinomial",
  newest_numbers,
  in_stack = stack_all,
  invariant = "Intercept",
  direction = "backwards",
  include = c(2:15, 20:35, 43:46),
  y = "value",
  y2 = "value",
  powerl = 1,
  inter = 1,
  thresh = 2,
  num.threads = 7
)
result_all_forwards <- INLAstep(
  fam1 = "nbinomial",
  newest_numbers,
  in_stack = stack_all,
  invariant = "Intercept",
  direction = "forwards",
  include = c(2:15, 20:35, 43:46),
  y = "value",
  y2 = "value",
  powerl = 1,
  inter = 1,
  thresh = 2,
  num.threads = 7
)
result_demo_backwards <- INLAstep(
  fam1 = "nbinomial",
  newest_numbers,
  in_stack = stack_demo,
  invariant = "Intercept",
  direction = "backwards",
  include = c(2:15, 43:45),
  y = "value",
  y2 = "value",
  powerl = 1,
  inter = 1,
  thresh = 2,
  num.threads = 7
)
result_demo_forwards <- INLAstep(
  fam1 = "nbinomial",
  newest_numbers,
  in_stack = stack_demo,
  invariant = "Intercept",
  direction = "forwards",
  include = c(2:15, 43:45),
  y = "value",
  y2 = "value",
  powerl = 1,
  inter = 1,
  thresh = 2,
  num.threads = 7
)
result_infra_backwards <- INLAstep(
  fam1 = "nbinomial",
  newest_numbers,
  in_stack = stack_infra,
  invariant = "Intercept",
  direction = "backwards",
  include = c(20:35, 43, 44, 46),
  y = "value",
  y2 = "value",
  powerl = 1,
  inter = 1,
  thresh = 2,
  num.threads = 7
)
result_infra_forwards <- INLAstep(
  fam1 = "nbinomial",
  newest_numbers,
  in_stack = stack_infra,
  invariant = "Intercept",
  direction = "forwards",
  include = c(20:35, 43, 44, 46),
  y = "value",
  y2 = "value",
  powerl = 1,
  inter = 1,
  thresh = 2,
  num.threads = 7
)