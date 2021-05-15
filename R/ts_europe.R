library(INLA)
library(SpatialEpi)
library(eurostat)
library(ISOcodes)
library(spdep)
library(stringr)
library(dplyr)
library(MASS)
source("R/preprocess_timeseries.R")
ts_europe$handwashing_facilities <- NULL
ts_europe$new_tests <- NULL
ts_europe$extreme_poverty <- NULL
ts_europe <- ts_europe[complete.cases(ts_europe), ]
iso_code <- ISO_3166_1[, c("Alpha_2", "Name")]
colnames(iso_code) <- c("CNTR_CODE", "Country")
ts_europe <- merge(
  ts_europe,
  iso_code,
  by = "Country"
)
europe_shapes <- get_eurostat_geospatial(nuts_level = "0")
missing <- !unique(ts_europe$CNTR_CODE) %in% europe_shapes$CNTR_CODE
unique(ts_europe$Country)[missing]
unique(ts_europe$CNTR_CODE)[missing]
ts_europe$CNTR_CODE[ts_europe$CNTR_CODE == "GR"] <- "EL"
ts_europe$CNTR_CODE[ts_europe$CNTR_CODE == "GB"] <- "UK"
ts_europe <- ts_europe[ts_europe$CNTR_CODE %in% europe_shapes$CNTR_CODE, ]
population <- read_csv("wrangled_data/demo_pjan_1_Data.csv")
population <- population[population$TIME == 2020, ]
population$GEO[!population$GEO %in% unique(ts_europe$Country)]
unique(ts_europe$Country)[!unique(ts_europe$Country) %in% population$GEO]
population$GEO[10] <- "Germany"
population <- population[population$GEO %in% unique(ts_europe$Country), c("GEO", "Value")]
colnames(population)[1] <- "Country"
ts_europe <- merge(
  ts_europe,
  population,
  by = "Country"
)
ts_europe <- merge(
  ts_europe,
  europe_shapes[, 2],
  by = "CNTR_CODE"
)
ts_europe <- st_as_sf(ts_europe)
colnames(ts_europe)[42] <- "population"
ts_europe$population <- as.numeric(str_replace_all(ts_europe$population, ",", ""))
ts_europe_split <- split(ts_europe, ts_europe$Date)
ts_europe_split_e <- pbapply::pblapply(
  ts_europe_split,
  function(x) {
    expected <- expected(
      x$population,
      x$new_cases,
      1
    )
    x$expected <- expected
    x
  }
)
ts_europe <- bind_rows(ts_europe_split_e)
ts_europe <- ts_europe[order(ts_europe$Country, ts_europe$Date), ]
ts_europe <- ts_europe[ts_europe$new_cases >= 0, ]
date_tibble <- tibble(
  Date = unique(ts_europe$Date),
  id_date_1 = seq_len(length(unique(ts_europe$Date))),
  id_date_2 = seq_len(length(unique(ts_europe$Date)))
)
area_tibble <- tibble(
  CNTR_CODE = unique(ts_europe$CNTR_CODE),
  id_country_1 = seq_len(length(unique(ts_europe$CNTR_CODE))),
  id_country_2 = seq_len(length(unique(ts_europe$CNTR_CODE)))
)
ts_europe <- merge(
  ts_europe,
  date_tibble,
  by = "Date"
)
ts_europe <- merge(
  ts_europe,
  area_tibble,
  by = "CNTR_CODE"
)
ts_europe <- ts_europe[order(ts_europe$Country, ts_europe$Date), ]
ts_europe$id_date_area <- seq_len(nrow(ts_europe))
rm(list = setdiff(ls(), "ts_europe"))
ts_europe <- ts_europe[ts_europe$expected > 0, ]
set.seed(2354324)
backup <- ts_europe
test <- ts_europe[ts_europe$id_date_1 >= 450, ]
test_value <- test$new_cases
ts_europe[ts_europe$id_date_1 >= 450, ]$new_cases <- NA
link <- rep(NA, nrow(ts_europe))
link[which(is.na(ts_europe$new_cases))] <- 1
prior_1 <- list(
    prec = list(
      prior = "pc.prec",
      param = c(1, 0.01)
    )
  )
nb <- poly2nb(ts_europe[!duplicated(ts_europe$CNTR_CODE), ])
nb2INLA("maps/map_3.adj", nb)
g <- inla.read.graph(filename = "maps/map_3.adj")
formula_1 <- as.formula(
  paste(
    "new_cases ~",
    paste(colnames(ts_europe)[7:41], collapse = " + "),
    "+ f(id_country_1, model = 'bym2', graph = g, scale.model = TRUE, hyper = prior_1)",
    "+ f(id_country_2, model = 'iid')",
    "+ Date"
  )
)
res_1 <- inla(
  formula_1,
  family = "nbinomial",
  data = ts_europe,
  Ntrials = ts_europe$population,
  E = expected,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  verbose = TRUE
)
formula_2 <- as.formula(
  paste(
    "new_cases ~",
    paste(colnames(ts_europe)[7:41], collapse = " + "),
    "+ f(id_country_1, model = 'bym2', graph = g, scale.model = TRUE, hyper = prior_1)",
    "+ f(id_date_1, model = 'rw2')",
    "+ f(id_date_2, model = 'iid')"
  )
)
lcs <- inla.make.lincombs(id_date_1 = diag(length(unique(ts_europe$Date))), id_date_2 = diag(length(unique(ts_europe$Date))))
res_2 <- inla(
  formula_2,
  family = "nbinomial",
  data = ts_europe,
  Ntrials = ts_europe$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  E = expected,
  lincomb = lcs,
  verbose = TRUE
)
formula_3 <- as.formula(
  paste(
    "new_cases ~",
    paste(colnames(ts_europe)[7:41], collapse = " + "),
    "+ f(id_country_1, model = 'bym2', graph = g, scale.model = TRUE, hyper = prior_1)",
    "+ f(id_date_1, model = 'rw2')",
    "+ f(id_date_2, model = 'iid')",
    "+ f(id_date_area, model = 'iid')"
  )
)
res_3 <- inla(
  formula_3,
  family = "nbinomial",
  data = ts_europe,
  Ntrials = ts_europe$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  E = expected,
  verbose = TRUE
)
predicted_1 <- c()
predicted_2 <- c()
predicted_3 <- c()
# make predictions
for (i in seq_len(nrow(ts_europe))) {
  predicted_1[i] <- inla.emarginal(
    function(x) x * ts_europe$population[i],
    res_1$marginals.fitted.values[[i]]
  )
  predicted_2[i] <- inla.emarginal(
    function(x) x * ts_europe$population[i],
    res_2$marginals.fitted.values[[i]]
  )
  predicted_3[i] <- inla.emarginal(
    function(x) x * ts_europe$population[i],
    res_3$marginals.fitted.values[[i]]
  )
}
b <- ts_europe[, c(6:41)]
b$geometry <- NULL
sign <- TRUE
while (sign) {
  mod <- glm.nb(
    new_cases ~ .,
    data = b
  )
  if (!any(VIF(mod) > 5)) {
    sign <- FALSE
  } else {
    b[, names(VIF(mod)[, 1])[VIF(mod)[, 1] == max(VIF(mod)[, 1]) ]] <- NULL
  }
}
ts_europe[, c(6:41)] <- NULL
ts_europe <- st_as_sf(cbind(ts_europe, b))
formula_4 <- as.formula(
  paste(
    "new_cases ~",
    paste(colnames(ts_europe)[14:35], collapse = " + "),
    "+ f(id_country_1, model = 'bym2', graph = g, scale.model = TRUE, hyper = prior_1)",
    "+ f(id_country_2, model = 'iid')",
    "+ Date"
  )
)
res_4 <- inla(
  formula_4,
  family = "nbinomial",
  data = ts_europe,
  Ntrials = ts_europe$population,
  E = expected,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  verbose = TRUE
)
formula_5 <- as.formula(
  paste(
    "new_cases ~",
    paste(colnames(ts_europe)[14:35], collapse = " + "),
    "+ f(id_country_1, model = 'bym2', graph = g, scale.model = TRUE, hyper = prior_1)",
    "+ f(id_date_1, model = 'rw2')",
    "+ f(id_date_2, model = 'iid')"
  )
)
lcs <- inla.make.lincombs(id_date_1 = diag(length(unique(ts_europe$Date))), id_date_2 = diag(length(unique(ts_europe$Date))))
res_5 <- inla(
  formula_5,
  family = "nbinomial",
  data = ts_europe,
  Ntrials = ts_europe$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  E = expected,
  lincomb = lcs,
  verbose = TRUE
)
formula_6 <- as.formula(
  paste(
    "new_cases ~",
    paste(colnames(ts_europe)[14:35], collapse = " + "),
    "+ f(id_country_1, model = 'bym2', graph = g, scale.model = TRUE, hyper = prior_1)",
    "+ f(id_date_1, model = 'rw2')",
    "+ f(id_date_2, model = 'iid')",
    "+ f(id_date_area, model = 'iid')"
  )
)
res_6 <- inla(
  formula_6,
  family = "nbinomial",
  data = ts_europe,
  Ntrials = ts_europe$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  E = expected,
  verbose = TRUE
)
predicted_4 <- c()
predicted_5 <- c()
predicted_6 <- c()
# make predictions
for (i in seq_len(nrow(ts_europe))) {
  predicted_4[i] <- inla.emarginal(
    function(x) x * ts_europe$population[i],
    res_4$marginals.fitted.values[[i]]
  )
  predicted_5[i] <- inla.emarginal(
    function(x) x * ts_europe$population[i],
    res_5$marginals.fitted.values[[i]]
  )
  predicted_6[i] <- inla.emarginal(
    function(x) x * ts_europe$population[i],
    res_6$marginals.fitted.values[[i]]
  )
}