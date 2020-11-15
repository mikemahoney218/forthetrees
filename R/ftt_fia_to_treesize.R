#' @export
ftt_fia_to_treesize <- function(input_fia,
                                col_names = c(
                                  tree_id = "CN",
                                  dbh = "DIA",
                                  height = "ACTUALHT",
                                  latitude = "LAT",
                                  longitude = "LON",
                                  subplot = "SUBP",
                                  distance = "DIST",
                                  azimuth = "AZIMUTH"
                                ),
                                elevation = TRUE,
                                crs = 4269) {
  default_names <- c(
    tree_id = "CN",
    dbh = "DIA",
    height = "ACTUALHT",
    latitude = "LAT",
    longitude = "LON",
    subplot = "SUBP",
    distance = "DIST",
    azimuth = "AZIMUTH"
  )

  col_names <- c(
    col_names,
    default_names[setdiff(names(default_names), names(col_names))]
  )


  mvdf <- data.frame(input_fia[col_names["tree_id"]])
  names(mvdf) <- "idx"

  plot_loc <- mapply(function(lat, lon, subp) {
    distance <- switch(subp, 0, 120, 120, 120)
    azimuth <- switch(subp, 0, 360, 120, 240)

    terrainr::export_coord_pair(
      terrainr::point_from_distance(c(
        lat = lat,
        lng = lon
      ),
      distance = distance,
      azimuth = azimuth,
      distance_unit = "feet",
      azimuth_unit = "degrees"
      )
    )
  },
  input_fia[[col_names["latitude"]]],
  input_fia[[col_names["longitude"]]],
  input_fia[[col_names["subplot"]]],
  SIMPLIFY = FALSE
  )

  input_fia[col_names["latitude"]] <- vapply(plot_loc, `[`, numeric(1), "lat")
  input_fia[col_names["longitude"]] <- vapply(plot_loc, `[`, numeric(1), "lng")

  tree_loc <- mapply(function(lat, lon, dist, azi) {
    terrainr::export_coord_pair(
      terrainr::point_from_distance(c(
        lat = lat,
        lng = lon
      ),
      distance = dist,
      azimuth = azi,
      distance_unit = "feet",
      azimuth_unit = "degrees"
      )
    )
  },
  input_fia[[col_names["latitude"]]],
  input_fia[[col_names["longitude"]]],
  input_fia[[col_names["distance"]]],
  input_fia[[col_names["azimuth"]]],
  SIMPLIFY = FALSE
  )

  input_fia[col_names["latitude"]] <- vapply(tree_loc, `[`, numeric(1), "lat")
  input_fia[col_names["longitude"]] <- vapply(tree_loc, `[`, numeric(1), "lng")

  input_fia$geom <- sp::SpatialPoints(data.frame(
    x = input_fia[[col_names["longitude"]]],
    y = input_fia[[col_names["latitude"]]]
  ))

  crs <- as.character(crs)
  target_crs <- switch(crs,
  "4269" = "+init=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs",
  "4326" = "+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs",
  NULL)

  if (elevation) {
    sp::proj4string(input_fia$geom) <- sp::CRS(target_crs)

    out_tiles <- terrainr::get_tiles(
      terrainr::add_bbox_buffer(
        terrainr::get_bbox(
          lat = input_fia[[col_names["latitude"]]],
          lng = input_fia[[col_names["longitude"]]]
        ),
        10
      )
    )

    if (length(out_tiles[["3DEPElevation"]]) > 1) {
      out_tiles$merged_elevation <- terrainr::merge_rasters(
        out_tiles[["3DEPElevation"]]
        )
    }

    input_raster <- raster::raster(out_tiles[[1]])

    if (is.null(target_crs)) {
      sp::proj4string(input_fia$geom) <- sp::CRS(
        raster::crs(input_raster)@projargs[[1]]
        )
    }

    input_fia$geom <- sp::spTransform(input_fia$geom,
                                      raster::crs(input_raster)@projargs[[1]])

    mvdf$z <- raster::extract(input_raster, input_fia$geom)

    origin <- terrainr::get_bbox_centroid(
      terrainr::get_bbox(raster::raster(out_tiles[[1]]))
    )

  } else {

    origin <- terrainr::get_bbox_centroid(
      terrainr::get_bbox(input_fia$geom)
    )

  }

  mvdf$x <- vapply(
    input_fia[[col_names["longitude"]]],
    function(x) {
      direction <- 1
      if (x < origin@lng) direction <- -1
      return(terrainr::calc_haversine_distance(
        origin,
        c(origin@lat,
          lng = x
        )
      ) * direction)
    },
    numeric(1)
  )

  mvdf$y <- vapply(
    input_fia[[col_names["latitude"]]],
    function(x) {
      direction <- 1
      if (x < origin@lat) direction <- -1
      return(terrainr::calc_haversine_distance(
        origin,
        c(origin@lng,
          lat = x
        )
      ) * direction)
    },
    numeric(1)
  )

  mvdf$dbh <- terrainr::convert_distance(input_fia[[col_names["dbh"]]], "inches")
  mvdf$height <- terrainr::convert_distance(input_fia[[col_names["height"]]], "feet")

  methods::new("ftt_treesize",
      idx = mvdf$idx,
      x = mvdf$x,
      y = mvdf$y,
      z = mvdf$z,
      dbh = mvdf$dbh,
      height = mvdf$height,
      crown = NA,
      crown_radius = NA_real_,
      metadata = input_fia,
      appendix = out_tiles)

}
