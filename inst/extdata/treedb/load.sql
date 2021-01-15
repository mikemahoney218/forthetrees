COPY species_lookup FROM 'inst/extdata/treedb/species_lookup.csv' (FORMAT 'csv', quote '"', delimiter ',', header 0);
COPY genus_lookup FROM 'inst/extdata/treedb/genus_lookup.csv' (FORMAT 'csv', quote '"', delimiter ',', header 0);
COPY common_name_lookup FROM 'inst/extdata/treedb/common_name_lookup.csv' (FORMAT 'csv', quote '"', delimiter ',', header 0);
COPY plants_code_lookup FROM 'inst/extdata/treedb/plants_code_lookup.csv' (FORMAT 'csv', quote '"', delimiter ',', header 0);
COPY fia_code_lookup FROM 'inst/extdata/treedb/fia_code_lookup.csv' (FORMAT 'csv', quote '"', delimiter ',', header 0);
COPY sapling_tree_defs FROM 'inst/extdata/treedb/sapling_tree_defs.csv' (FORMAT 'csv', quote '"', delimiter ',', header 0);
