test_that("lookup methods are equivalent", {
  expect_equal(
    setNames(ftt_fia_sapling_lookup(11), NULL),
    setNames(ftt_plants_sapling_lookup("abba"), NULL)
  )
  expect_equal(setNames(ftt_species_sapling_lookup("abies balsamea"), NULL),
               setNames(ftt_genus_sapling_lookup("abies"), NULL))

  expect_equal(setNames(ftt_common_name_sapling_lookup("balsam fir"), NULL),
               setNames(ftt_fia_sapling_lookup(11), NULL))

  expect_equal(setNames(ftt_species_sapling_lookup("abies balsamea"), NULL),
               setNames(ftt_fia_sapling_lookup(11), NULL))

})

test_that("multiple lookup works", {
  expect_equal(length(ftt_fia_sapling_lookup(9:11)), 3)

  expect_equal(
    unique(ftt_fia_sapling_lookup(c(11, 11))),
    setNames(ftt_fia_sapling_lookup(11), NULL)
  )

})
