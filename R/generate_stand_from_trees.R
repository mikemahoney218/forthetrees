#' Generate stem maps from tables of individual trees
#'
#' This function simulates stem maps for stands, basing density and attributes
#' on tree measurements provided to `data`.
#'
#' @param data The data.frame
#' @param stand_area sf object: The polygon to simulate trees within. Sampling
#' is perfomed using [sf::st_sample()].
#' @param ... Passed to [sf::st_sample()].
#' @param measured_area Either numeric (the total amount of area represented by
#' `data`) of sf object (representing the measurement boundary). Used to
#' calculate
#' @param cols Names of columns to simulate for each new tree
#'
#' @examples
#' stand_area <- sf::st_bbox(c(xmin = 0, ymin = 0, xmax = 140, ymax = 140))
#' stand_area <- sf::st_as_sfc(stand_area)
#'
#' measured_area <- 19600
#'
#' generate_stand_from_trees(trees[c("SPECIES", "DBH85")], stand_area, measured_area = measured_area)
#'
#' @return An sf object, representing new tree positions in the stem map, with simulated variables.
#'
#' @export
generate_stand_from_trees <- function(data, stand_area, ..., measured_area, cols = dplyr::everything()) {

  if (inherits(measured_area, "sf") || inherits(measured_area, "sfc")) {
    measured_area <- sf::st_area(measured_area)
  }

  if (inherits(measured_area, "SpatRaster")) {
    rlang::check_installed("terra")
    measured_area <- terra::expanse(measured_area)
  }

  if (missing(measured_area) && (inherits(data, "sf") || inherits(data, "sfc"))) {
    rlang::warn(
      "Assuming that points within `data` represent the total measured area.",
      i = "Set `measured_area` to silence this warning."
    )
    measured_area <- sf::st_area(data)
  }
  if (!is.numeric(measured_area)) {
    rlang::abort(
      c(
        "Can't calculate target density.",
        i = "Consider setting `measured_area` to the total amount of area `data` was measured in.",
        i = "Or provide the boundaries of `data` as an sf object to `measured_area`."
      )
    )
  }

  area_of_stand <- sf::st_area(stand_area)
  if (inherits(area_of_stand, "units") && !inherits(measured_area, "units")) {
    rlang::warn(
      c(
        "Assuming `measured_area` is in the same units as `stand_area`.",
        i = "Set units explicitly via units::set_units() to silence this warning."
      )
    )
  }

  if (inherits(area_of_stand, "units")) {
    units(measured_area) <- units(area_of_stand)
  } else if (inherits(measured_area, "units")) {
    units(area_of_stand) <- units(measured_area)
  }

  target_density <- nrow(data) / measured_area
  multiple <- 1
  if (as.vector(target_density) < 1) {
    multiple <- round(1 %/% target_density, -1) * 10
    target_density <- target_density * multiple
  }

  target_density <- rpois(1, target_density) / multiple

  tree_positions <- sf::st_sample(
    stand_area,
    as.vector(area_of_stand * target_density),
    warn_if_not_integer = FALSE,
    exact = FALSE,
    ...
  )
  tree_positions <- sf::st_as_sf(tree_positions)
  sf::st_geometry(tree_positions) <- "geometry"

  tree_attributes <- tidyr::nest(
    dplyr::slice_sample(
      tidyr::uncount(
        dplyr::mutate(
          dplyr::group_data(data),
          x = purrr::map_dbl(.rows, length)
        ),
        x
      ),
      n = nrow(tree_positions),
      replace = TRUE
    ),
    dat = -.rows
  )

  simulate_attributes <- dplyr::select(
    as.data.frame(data),
    all_of(cols),
    -dplyr::group_vars(data)
  )

  tree_attributes <- purrr::map2_dfr(
    tree_attributes$dat,
    tree_attributes$.rows,
    \(df, idx) {
      cbind(
        df,
        purrr::map_dfc(
          simulate_attributes[idx, , drop = FALSE],
          \(x) simulate_from_data(x, nrow(df))
        )
      )
    }
  )

  cbind(tree_attributes, tree_positions)
}

simulate_from_data <- function(x, n) {
  if (is.numeric(x)) {
    simulate_from_continuous_ecdf(x, n)
  } else {
    simulate_from_categorical(x, n)
  }
}

simulate_from_continuous_ecdf <- function(x, n) {
  e <- ecdf(x)
  vapply(runif(n), \(i) quantile(e, i), numeric(1))
}

simulate_from_categorical <- function(x, n) {
  vapply(runif(n), \(i) x[max(round(i * length(x)), 1)], vector(class(x), 1))
}
