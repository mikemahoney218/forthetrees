test_that("lookup methods are equivalent", {
  expect_equal(
    ftt_fia_sapling_lookup(11),
    ftt_plants_sapling_lookup("abba")
  )
  expect_equal(ftt_species_sapling_lookup("abies balsamea"),
               ftt_genus_sapling_lookup("abies"))

  expect_equal(ftt_common_name_sapling_lookup("balsam fir"),
               ftt_fia_sapling_lookup(11))

  expect_equal(ftt_species_sapling_lookup("abies balsamea"),
               ftt_fia_sapling_lookup(11))

})
