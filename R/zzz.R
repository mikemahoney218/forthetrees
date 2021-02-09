#' Standard way to evaluate arguments for class-creating functions
#'
#' This approach enables users to specify arguments as quoted strings
#' referencing column names (passes the first try, passes the second try), as
#' unquoted column names evaluated via rlang's NSE (fails the first try, passes
#' the second try), or as pure values (passes the first try, fails the second
#' try).
#'
#' If the user provides an unquoted string that does not correspond to a column,
#' this function returns NULL.
#'
#' @param data The data frame to try retrieving values from.
#' @param arg The argument to evaluate.
#'
#' @keywords internal
eval_arg <- function(data, arg) {
  arg <- tryCatch(arg, error = function(e) rlang::ensym(arg))
  tryCatch(data[[arg]], error = function(e) arg)
}

#' Standard way to replace & replicate values for class-creation functions
#'
#' @param vec The vector to replace and replicate values throughout
#' @param length_out The target length of the vector
#' @param replace_val The value to replace missing values with
#'
#' @return A vector of length `length_out` with missing values replaced by
#' `replace_val`
#'
#' @keywords internal
calc_val <- function(vec, length_out, replace_val = 0) {
  if (length(vec) == 1 &&
      vec == deparse(substitute(vec))) {
    vec <- NA
  }

  if (all_missing(vec)) vec <- replace_val
  if (any_missing(vec)) vec[which(is_missing(vec))] <- replace_val

  if (length(vec) == 1) vec <- rep(vec, length_out)
  vec
}

#' Check to see if any values in a vector are missing (NULL, NA, INF, or NaN)
#'
#' @param obj The vector of values to check
#'
#' @keywords internal
any_missing <- function(obj) {
  any(is_missing(obj))
}

#' Check to see if all values in a vector are missing (NULL, NA, INF, or NaN)
#'
#' @param obj The vector of values to check
#'
#' @keywords internal
all_missing <- function(obj) {
  all(is_missing(obj))
}

#' Check values in a vector for missingness (NULL, NA, INF, or NaN)
#'
#' @param obj The vector of values to check
#'
#' @keywords internal
is_missing <- function(obj) {
  is.null(obj) |
    is.na(obj) |
    is.infinite(obj) |
    is.nan(obj) |
    length(obj) == 0
}

#' Build the location commands for classes inheriting from `mvdf_obj`
#'
#' @param object An object inheriting from `mvdf_obj`
#'
#' @family utilities
#'
#' @keywords internal
move_location <- function(mvdf_df) {
  outstr <- apply(
    mvdf_df,
    1,
    function(x) {
      treestr <- "bpy.data.objects[-1].location = mathutils.Vector(({xloc}, {yloc}, {zloc}))" # nolint

      treestr
    }
  )

  mapply(
    function(string, x, y, z) {
      glue::glue(
        string,
        xloc = x,
        yloc = y,
        zloc = z
      )
    },
    string = outstr,
    x = mvdf_df$x,
    y = mvdf_df$y,
    z = mvdf_df$z
  )

}

#' Create an options string from a ... object
#'
#' @param dts The results of list(...) to create an options string from
#'
#' @keywords internal
create_options <- function(dts) {
  paste0(
    do.call(
      paste,
      c(
        mapply(
          function(nm, ob) paste0(nm, "=", ob),
          names(dts),
          dts,
          SIMPLIFY = FALSE
        ),
        sep = ","
      )
    ),
    ", "
  )
}

#' Convert R logical values to Python equivalents
#'
#' @param dots The results of running `list(...)` in an exporter function.
#'
#' @family utilities
#'
#' @keywords internal
pythonize_booleans <- function(dots) {
  # friendly conversion from R logicals to Python
  dots[which(is.logical(dots) && dots == FALSE)] <- "False"
  dots[which(is.logical(dots) && dots == TRUE)] <- "True"
  dots
}

.onLoad <- function(libname, pkgname) {
  if (!dir.exists(tools::R_user_dir(package = "forthetrees"))) {
    dir.create(tools::R_user_dir(package = "forthetrees"), recursive = TRUE)
  }
  if (file.exists(paste0(tools::R_user_dir("forthetrees"), "/treedb.db"))) {
    file.remove(paste0(tools::R_user_dir("forthetrees"), "/treedb.db"))
  }
  db_path <- system.file("extdata/treedb",
                         package = "forthetrees",
                         mustWork = TRUE)
  schema <- readLines(paste0(db_path, "/schema.sql"))
  schema <- schema[schema != ""]
  loadsql <- readLines(paste0(db_path, "/load.sql"))
  loadsql <- loadsql[loadsql != ""]
  loadsql <- gsub("inst/extdata/treedb", db_path, loadsql)
  con <- DBI::dbConnect(
    duckdb::duckdb(),
    dbdir = paste0(tools::R_user_dir("forthetrees"), "/treedb.db"))
  invisible(lapply(schema, function(x) DBI::dbExecute(con, x)))
  invisible(lapply(loadsql, function(x) DBI::dbExecute(con, x)))
  DBI::dbDisconnect(con, shutdown = TRUE)
  invisible(NULL)
}

#' Find approximately reasonable default values for tree size parameters
#'
#' @param dbh Diameter at breast height, in meters.
#' @param height Bole height, in meters.
#' @param crown_radius Crown radius, in meters.
#' @param region Currently ignored. Exists to allow future expansion of
#' parameters, to let functions more realistically scale trees for a given
#' region.
#'
#' @return A named list with default values for dbh, height, and crown_radius.
#'
#' @references
#'  B. R. Lockhart, R. C. Weih, and K. M. Smith. 2005 Crown Radius and Diameter
#'  at Breast Height Relationships for Six Bottomland Hardwood Species. Journal
#'  of the Arkansas Academy of Science. 59.
#'  Available at:http://scholarworks.uark.edu/jaas/vol59/iss1/16
#'
#'  L. Zhang. 1997. Cross-validation of Non-linear Growth Functions for
#'  Modelling Tree Height–Diameter Relationships. Annals of Botany. 79(3):
#'  251-257. https://doi.org/10.1006/anbo.1996.0334.
#'
#'  R. C. Yang, A. Kozak, and J. H. G. Smith. 1978. The potential of
#'  Weibull-type functions as flexible growth curves. Canadian Journal of Forest
#'  Research. 8(4): 424-431. https://doi.org/10.1139/x78-062
solve_tree <- function(dbh = NA,
                       height = NA,
                       crown_radius = NA,
                       region = "nw") {

  if (length(dbh) != length(height) ||
      length(height) != length(crown_radius)) {
    stop("dbh, height, and crown_radius arguments must be of the same length.")
  }

  # If all vectors are the same length and dbh is NULL, they're all NULL
  if (is.null(dbh)) dbh <- height <- crown_radius <- NA

  # Weibull equation parameters
  # TODO: Get pleasant values for other regions
  # These values are the mean values form Zhang 1999, which implemented Weibull
  # for a variety of species in the Northwestern United States.
  # The intent is to include the ability to use values for different regions;
  # the PNW is likely going to create larger trees than might be normal in other
  # regions.
  a <- 45.261
  b <- -0.01509
  c <- 1.12103

  # Crown radius parameters from Lockhart 2005. I don't like this formula, but
  # haven't had a ton of luck finding non-linear equations describing crown
  # radius as a function of height or DBH alone -- which is admittedly a weird
  # requirement.
  # TODO: Get pleasant values for other regions
  # Have: diameter, want: crown_radius
  crown_a <- 0.8329683
  crown_b <- 0.06963167
  # Have: crown_radius, want: diameter
  # These parameters cover a _huge_ range and I simply took the mean;
  # this is definitely the least reliable imputation in this function.
  dbh_a <- 0.4621517
  dbh_b <- 10.51051



  tree_params <- mapply(
    function(dbh, height, crown_radius) {
      if (all_missing(c(dbh, height, crown_radius))) dbh <- 0.38 # m
      if (is_missing(dbh)) {
        if (is_missing(height)) {
          # get DBH from crown_radius
          dbh <- dbh_a + dbh_b * crown_radius
        } else {
          # This is Weibull manipulated to return DBH
          dbh <- ((log(1 -((height - 1.37) / a))) / b)^(1 / c)
        }
      } else {
        dbh <- dbh * 100 # cm
      }

      if (is_missing(height)) {
        # This is Weibull
        height <- 1.37 + (a * (1 - exp(b * dbh^c)))
      }

      if (is_missing(crown_radius)) {
        # get crown_radius from height, DBH
        crown_radius <- crown_a + crown_b * dbh
      }

      dbh <- dbh / 100 # back to m

      list(
        dbh = dbh,
        height = height,
        crown_radius = crown_radius
      )
    },
    dbh = dbh,
    height = height,
    crown_radius = crown_radius
  )

  return(
    list(
      dbh = setNames(unlist(tree_params["dbh", ]), NULL),
      height = setNames(unlist(tree_params["height", ]), NULL),
      crown_radius = setNames(unlist(tree_params["crown_radius", ]), NULL)
    )
  )

}
