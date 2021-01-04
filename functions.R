############## DOWNLOAD KEY DATA  ##############
download_key_data <- function(cities, key, value = NULL, df = TRUE,
                              cityname = NULL) {
  # add some initial values
  data_osm <- NULL
  # if a bbox is given to the function, turn it into a list so that
  # future_map works
  if (class(cities) == "bbox") {
    cities <- list(cities)
  }
  # make request of all cities for which the data is still missing
  # and add it to previous data
  if (length(cities) > 3) {
    func <- future_map
  } else {
    func <- lapply
  }
  start_time <- Sys.time()
  current_download <- func(
    seq_len(length(cities)), function(y, key, ...) {
      x <- cities[y]
      # define the query
      if (is.null(value)) {
        query <- try(osmdata::opq(x) %>%
          osmdata::add_osm_feature(key = key), silent = TRUE)
        while (class(query)[1] == "try-error") {
          query <- try(osmdata::opq(x) %>%
            osmdata::add_osm_feature(key = key), silent = TRUE)
        }
      } else {
        query <- try(osmdata::opq(x) %>%
          osmdata::add_osm_feature(key = key, value = value), silent = TRUE)
        while (class(query) == "try-error") {
          query <- try(osmdata::opq(x) %>%
            osmdata::add_osm_feature(key = key, value = value),
          silent = TRUE
          )
          # if (format(Sys.time() - start_time, units = "secs") > "120 secs") {
          #   return(NULL)
          # }
        }
      }
      httr::set_config(httr::config(http_version = 2))
      cat(".")
      # give the server some time so you (hopefully) don't get blocked
      Sys.sleep(3)
      # see if data can be downloaded
      # try because otherwise it will throw an error
      points <- try(osmdata::osmdata_sf(query), silent = TRUE)
      if (class(points)[1] == "try-error") {
        if (str_detect(points, "Timeout")) {
          while (str_detect(points, "Timeout")) {
            points <- try(osmdata::osmdata_sf(query), silent = TRUE)
            # if (format(Sys.time() - start_time, units = "secs") > "120 secs") {
            #   return(NULL)
            # }
          }
        } else {
          # throw a warning if no data is available
          warning(paste("Unable to download", key, "data for", x))
          return(NULL)
        }
      }
      if (is.null(points$osm_points) && is.null(points$osm_polygons) &&
        is.null(points$osm_multipolygons)) {
        return(NULL)
      }
      result1 <- points$osm_points
      result2 <- points$osm_polygons
      result3 <- points$osm_multipolygons
      cat(".")
      if (is.null(result1)) {
        if (is.null(result2)) {
          result3 <- st_centroid(result3)
          result3$longitude <- st_coordinates(result3)[, 1]
          result3$latitude <- st_coordinates(result3)[, 2]
        } else if (is.null(result3)) {
          result2 <- st_centroid(result2)
          result2$longitude <- st_coordinates(result2)[, 1]
          result2$latitude <- st_coordinates(result2)[, 2]
        } else {
          result2 <- st_centroid(result2)
          result2$longitude <- st_coordinates(result2)[, 1]
          result2$latitude <- st_coordinates(result2)[, 2]
          result3 <- st_centroid(result3)
          result3$longitude <- st_coordinates(result3)[, 1]
          result3$latitude <- st_coordinates(result3)[, 2]
        }
      } else {
        if (!is.null(result2)) {
          inter <- unlist(st_intersects(result2, result1))
          if (length(inter) > 0) {
            result1 <- result1[-inter, ]
          }
          result2 <- st_centroid(result2)
          result2$longitude <- st_coordinates(result2)[, 1]
          result2$latitude <- st_coordinates(result2)[, 2]
        }
        if (!is.null(result3)) {
          inter <- unlist(st_intersects(result3, result1))
          if (length(inter) > 0) {
            result1 <- result1[-inter, ]
          }
          result3 <- st_centroid(result3)
          result3$longitude <- st_coordinates(result3)[, 1]
          result3$latitude <- st_coordinates(result3)[, 2]
        }
        result1$longitude <- st_coordinates(result1)[, 1]
        result1$latitude <- st_coordinates(result1)[, 2]
      }
      cat(".")
      all_results <- list(result1, result2, result3)
      all_results <- list.remove(
        all_results,
        unlist(lapply(all_results, function(x, ...) {
          is.null(x) || nrow(x) == 0
        }))
      )
      all_cols <- unique(unlist(lapply(all_results, colnames)))
      all_results <- lapply(all_results, function(x, ...) {
        missing_cols <- all_cols[!all_cols %in% colnames(x)]
        if (length(missing_cols) > 0) {
          x[, missing_cols] <- NA
        }
        x
      }) # bind the entire list while ensuring that
      # the order of the cols is correct
      results <- do.call(
        rbind,
        lapply(
          all_results,
          function(x)
            x[match(all_cols, names(x))]
        )
      )
      # also save the city name as a variable
      if (nrow(results) > 0) {
        # if no bbox is given to the function add the city to the
        # data.frame
        if (class(x[[1]]) != "bbox") {
          results$city <- x
          # return the data.frame
          results
        } else {
          if (!is.null(cityname)) {
            results$city <- cityname[y]
          }
          # return the data.frame
          results
        }
      } else {
        # return NULL
        NULL
      }
    },
    key
  )
  # check if any data is missing
  any_missing <- suppressWarnings(any(lapply(current_download, is.null)))
  if (any_missing) {
    # remove null values from list
    current_download[unlist(lapply(current_download, is.null))] <- NULL
  }
  cat(".")
  data_osm_2 <- as.list(c(data_osm, current_download))
  data_osm <- data_osm_2
  # return as list or data.frame
  if (!df) {
    return(data_osm)
  } else {
    # get all the colnames
    all_cols <- unique(unlist(lapply(data_osm, colnames)))
    # if columns are missing for some of the data, add them as NA
    data_osm <- lapply(
      data_osm, function(data2, all_cols, ...) {
        if (!is.null(nrow(data2))) {
          col2 <- colnames(data2)
          missing_cols <- all_cols[!all_cols %in% col2]
          if (length(missing_cols) > 0) {
            data2[, missing_cols] <- NA
          }
          return(data2)
        }
      },
      all_cols
    )
    # bind the entire list while ensuring that the order of the cols is
    # correct
    cat(".")
    data_osm_df <- do.call(
      rbind,
      lapply(
        data_osm,
        function(x) x[match(all_cols, names(x))]
      )
    )
    cat(".")
    if (!is.null(data_osm_df$name)) {
      Encoding(data_osm_df$name) <- "UTF-8"
      data_osm_df$name <- tolower(data_osm_df$name)
      data_osm_df$name <- sapply(data_osm_df$name, simple_cap)
      if ("NANA" %in% data_osm_df$name) {
        data_osm_df[data_osm_df$name == "NANA", ]$name <- ""
      }
      weird_stuff <- unlist(lapply(data_osm_df$name, enc2native))
      anything_weird <- weird_stuff[str_detect(weird_stuff, "\\<U")]
      if (length(anything_weird) > 0) {
        data_osm_df$name <- str_remove(weird_stuff, "\\<U.*\\>")
      }
      if (any(data_osm_df$name == "")) {
        data_osm_df[data_osm_df$name == "", ]$name <- "No name available"
      }
    }
    # if is null return a default data.frame
    if (is.null(data_osm_df)) {
      data_osm_df <- data.frame(
        "longitude" = 11.577779,
        "latitude" = 48.137168,
        name = "NODATA"
      )
    }
    data_osm_df[!duplicated(data_osm_df$osm_id), ]
  }
}



simple_cap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
    sep = "", collapse = " "
  )
}

geo_code2 <- function(x) {
  coded <- geocodeHERE_simple(
    x,
    App_id = "g3OqrYH2RijQ8La2hDrN",
    App_code = "aZ1EHzB8zjCUG_63k3xcEg"
  )
  as.numeric(rev(unlist(coded)))
}
