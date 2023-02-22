test_that("generate_stand_from_trees works", {
  skip_if_not_installed("withr")
  stand_area <- sf::st_bbox(c(xmin = 0, ymin = 0, xmax = 140, ymax = 140))
  stand_area <- sf::st_as_sfc(stand_area)

  measured_area <- 19600

  withr::with_seed(
    1107,
    expect_snapshot(
      generate_stand_from_trees(
        tree_positions[c("SPECIES", "DBH85")],
        stand_area,
        measured_area = measured_area
      )
    )
  )
})
