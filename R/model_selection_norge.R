library(ggregplot)
library(INLA)
library(INLAutils)
source("R/preprocess_norge.R")
parallelMap::parallelStartSocket(7)
newest_numbers$geometry <- NULL
stack_all <- inla.stack(
  data = list(value = newest_numbers$value),
  A = list(1),
  effects = list(
    data.frame(
      Intercept = 1,
      newest_numbers[, c(6:29, 31, 38:40)]
    )
  )
)
stack_demo <- inla.stack(
  data = list(value = newest_numbers$value),
  A = list(1),
  effects = list(
    data.frame(
      Intercept = 1,
      newest_numbers[, c(6:13, 38:40)]
    )
  )
)
stack_infra <- inla.stack(
  data = list(value = newest_numbers$value),
  A = list(1),
  effects = list(
    data.frame(
      Intercept = 1,
      newest_numbers[, c(14:29, 31, 38, 39)]
    )
  )
)
result_all_backwards <- INLAstep(
  fam1 = "nbinomial",
  newest_numbers,
  in_stack = stack_all,
  invariant = "Intercept",
  direction = "backwards",
  include = c(6:29, 31, 38:40),
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
  include = c(6:29, 31, 38:40),
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
  include = c(6:13, 38:40),
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
  include = c(6:13, 38:40),
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
  include = c(14:29, 31, 38, 39),
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
  direction = "forwads",
  include = c(14:29, 31, 38, 39),
  y = "value",
  y2 = "value",
  powerl = 1,
  inter = 1,
  thresh = 2,
  num.threads = 7
)
result_all_backwards2 <- INLAstep(
  fam1 = "nbinomial",
  newest_numbers,
  in_stack = stack_all,
  invariant = "Intercept",
  direction = "backwards",
  include = c(6:29, 31, 38:40),
  y = "value",
  y2 = "value",
  powerl = 1,
  inter = 2,
  thresh = 2,
  num.threads = 7
)
result_all_forwards2 <- INLAstep(
  fam1 = "nbinomial",
  newest_numbers,
  in_stack = stack_all,
  invariant = "Intercept",
  direction = "forwards",
  include = c(6:29, 31, 38:40),
  y = "value",
  y2 = "value",
  powerl = 1,
  inter = 2,
  thresh = 2,
  num.threads = 7
)
result_demo_backwards2 <- INLAstep(
  fam1 = "nbinomial",
  newest_numbers,
  in_stack = stack_demo,
  invariant = "Intercept",
  direction = "backwards",
  include = c(6:13, 38:40),
  y = "value",
  y2 = "value",
  powerl = 1,
  inter = 2,
  thresh = 2,
  num.threads = 7
)
result_demo_forwards2 <- INLAstep(
  fam1 = "nbinomial",
  newest_numbers,
  in_stack = stack_demo,
  invariant = "Intercept",
  direction = "forwards",
  include = c(6:13, 38:40),
  y = "value",
  y2 = "value",
  powerl = 1,
  inter = 2,
  thresh = 2,
  num.threads = 7
)
result_infra_backwards2 <- INLAstep(
  fam1 = "nbinomial",
  newest_numbers,
  in_stack = stack_infra,
  invariant = "Intercept",
  direction = "backwards",
  include = c(14:29, 31, 38, 39),
  y = "value",
  y2 = "value",
  powerl = 1,
  inter = 2,
  thresh = 2,
  num.threads = 7
)
result_infra_forwards2 <- INLAstep(
  fam1 = "nbinomial",
  newest_numbers,
  in_stack = stack_infra,
  invariant = "Intercept",
  direction = "forwads",
  include = c(14:29, 31, 38, 39),
  y = "value",
  y2 = "value",
  powerl = 1,
  inter = 2,
  thresh = 2,
  num.threads = 7
)