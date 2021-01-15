

CREATE TABLE species_lookup(genus VARCHAR, species VARCHAR, render_key BOOLEAN);
CREATE TABLE genus_lookup(genus VARCHAR, render_key VARCHAR);
CREATE TABLE common_name_lookup(common_name VARCHAR, genus VARCHAR, species VARCHAR);
CREATE TABLE plants_code_lookup(genus VARCHAR, species VARCHAR, plants_code VARCHAR);
CREATE TABLE fia_code_lookup(fia_code DOUBLE, genus VARCHAR, species VARCHAR);
CREATE TABLE sapling_tree_defs(render_key VARCHAR, render_call VARCHAR);



