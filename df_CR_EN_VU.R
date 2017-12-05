#  Sara Williams
#  Novemeber 13, 2017
#  Constraining list of mammal, amphibian, and bird species to those that are in a 
#   IUCN threatened category (CR, EN, VU)


library(dplyr) 
library(stringr)
library(sf)



path <- "C:/Users/saraw/Documents/SEARRP"
path_spat_dat <- paste(path, "processed_spat_data", sep = "/")
path_df_dat <- paste(path, "processed_excel_data", sep = "/")
setwd(path)



#####  RAW RANGES ##### 	

#####  MAMMALS  #####
#  Load spatial data for spp (this is the set of spatial polygons that have been projected, cropped, and aggregated
#   to 1 polygon per spp.
load(file = paste(path_spat_dat, "spp_ranges_no_constraints/sf_mammals_CR_EN_VU.Rdata", sep = "/"))

#  Read in non-spatial, manually edited data and select only one record per spp .Some spp have multiple 
#   polygons of ranges but all records for a sinple spp contain the same info in the csv.
dat_f = paste(path_df_dat, "spp_data_w_constraint_info/df_mammals_CR_EN_VU_NT_DD.csv", sep = "/")

df_mammals_tmp <- read.csv(dat_f) %>%
	group_by(binomial) %>%
	slice(1) %>%
	arrange(binomial) %>%
	as.data.frame()
df_mammals_tmp$binomial <- as.factor(df_mammals_tmp$binomial)
	
#  Join to original spatial data object to reapply polygon/geometry/spatial info
sf_df_mammals <- left_join(sf_mammals_agg, df_mammals_tmp, by = "id_no") %>%
	dplyr::select(id_no, binomial = binomial.x, RL_code = RL_code.x, class = class.x,
		order = order.x, family = family.x, genus = genus.x, RL_year, compiler, source,
		shape_length, shape_area, forest_dep, forest_dep_notes, elev_min,         
		elev_max, elev_min_notes, elev_max_notes, water_dep, water_dep_notes,
		perm_notes, perm)

#  Create data frame (not sf) to save as excel file
df_mammals <- as.data.frame(sf_df_mammals) %>%
	dplyr::select( -geom)

#  Save 
# save(sf_df_mammals, file = paste(path_spat_dat, "spp_ranges_no_constraints/sf_df_mammals_CR_EN_VU.Rdata", sep = "/"))
# write.csv(df_mammals, file = paste(path_df_dat, "spp_data_w_constraint_info/df_mammals_CR_EN_VU.csv", sep = "/"))
#  Object name sf_df_mammals



#####  AMPHIBIANS  #####

#  Load spatial data for spp (this is the set of spatial polygons that have been projected, cropped, and aggregated
#   to 1 polygon per spp.
load(file = paste(path_spat_dat, "spp_ranges_no_constraints/sf_amphibians_CR_EN_VU.Rdata", sep = "/"))

#  Read in non-spatial, manually edited data and select only one record per spp .Some spp have multiple 
#   polygons of ranges but all records for a sinple spp contain the same info in the csv.
dat_f = paste(path_df_dat, "spp_data_w_constraint_info/df_amphibians_CR_EN_VU_NT_DD.csv", sep = "/")

df_amphibians_tmp <- read.csv(dat_f) %>%
	group_by(binomial) %>%
	slice(1) %>%
	arrange(binomial) %>%
	as.data.frame()
df_amphibians_tmp$binomial <- as.factor(df_amphibians_tmp$binomial)
	
#  Join to original spatial data object to reapply polygon/geometry/spatial info
sf_df_amphibians <- left_join(sf_amphibians_agg, df_amphibians_tmp, by = "id_no") %>%
	dplyr::select(id_no, binomial = binomial.x, RL_code = RL_code.x, class = class.x,
		order = order.x, family = family.x, genus = genus.x, RL_year, compiler, source,
		shape_length, shape_area, forest_dep, forest_dep_notes, elev_min,         
		elev_max, elev_min_notes, elev_max_notes, water_dep, water_dep_notes,
		perm_notes, perm)

#  Create data frame (not sf) to save as excel file
df_amphibians <- as.data.frame(sf_df_amphibians) %>%
	dplyr::select( -geometry)

#  Save 
#save(sf_df_amphibians, file = paste(path_spat_dat, "spp_ranges_no_constraints/sf_df_amphibians_CR_EN_VU.Rdata", sep = "/"))
#write.csv(df_amphibians, file = paste(path_df_dat, "spp_data_w_constraint_info/df_amphibians_CR_EN_VU.csv", sep = "/"))



#####  BIRDS  #####
#  Load spatial data for spp (this is the set of spatial polygons that have been projected, cropped, and aggregated
#   to 1 polygon per spp.
load(file = paste(path_spat_dat, "spp_ranges_no_constraints/sf_birds_CR_EN_VU.Rdata", sep = "/"))

#  Read in non-spatial, manually edited data and select only one record per spp .Some spp have multiple 
#   polygons of ranges but all records for a sinple spp contain the same info in the csv.
dat_f = paste(path_df_dat, "spp_data_w_constraint_info/df_birds_CR_EN_VU_NT_DD.csv", sep = "/")

df_birds_tmp <- read.csv(dat_f) %>%
	group_by(binomial) %>%
	slice(1) %>%
	arrange(binomial) %>%
	as.data.frame()
df_birds_tmp$binomial <- as.factor(df_birds_tmp$binomial)
	
#  Join to original spatial data object to reapply polygon/geometry/spatial info
sf_df_birds <- left_join(sf_birds_agg, df_birds_tmp, by = "id_no") %>%
	dplyr::select(id_no, binomial = binomial.x, RL_code = RL_code.x, class,
		order, family, genus, RL_year, compiler, source,
		shape_length, shape_area, forest_dep, forest_dep_notes, elev_min,         
		elev_max, elev_min_notes, elev_max_notes, water_dep, water_dep_notes,
		perm_notes, perm)

#  Create data frame (not sf) to save as excel file
df_birds <- as.data.frame(sf_df_birds) %>%
	dplyr::select( -geometry)

#  Save 
#save(sf_df_birds, file = paste(path_spat_dat, "spp_ranges_no_constraints/sf_df_birds_CR_EN_VU.Rdata", sep = "/"))
#write.csv(df_birds, file = paste(path_df_dat, "spp_data_w_constraint_info/df_birds_CR_EN_VU.csv", sep = "/"))




#####  ADJUSTED RANGES ##### 	

#####  MAMMALS  #####
#  Load spatial data for spp (this is the set of spatial polygons that have been projected, cropped, and aggregated
#   to 1 polygon per spp and had elevation adjustments taken into account.
load(file = paste(path_spat_dat, "spp_ranges_w_constraints/sf_mammals_CR_EN_VU_elev_adj.Rdata", sep = "/"))
#  Object name "sf_mammals_elev_adj"

#  Read in non-spatial, manually edited data and select only one record per spp .Some spp have multiple 
#   polygons of ranges but all records for a sinple spp contain the same info in the csv.
dat_f = paste(path_df_dat, "spp_data_w_constraint_info/df_mammals_CR_EN_VU_NT_DD.csv", sep = "/")

df_mammals_tmp <- read.csv(dat_f) %>%
	group_by(binomial) %>%
	slice(1) %>%
	arrange(binomial) %>%
	as.data.frame() %>%
	dplyr::filter(RL_code == "CR" | RL_code == "EN" | RL_code == "VU") %>%
	dplyr::filter(order_name != "CHIROPTERA") 
df_mammals_tmp$binomial <- as.factor(df_mammals_tmp$binomial)
	
#  Join to original spatial data object to reapply polygon/geometry/spatial info
sf_df_mammals_elev_adj <- bind_cols(sf_mammals_elev_adj, df_mammals_tmp) %>%
	dplyr::select(id_no, binomial, RL_code, class, order, family, genus, RL_year = RL_assess_year,
		shape_length = shape_Leng, shape_area = shape_Area, forest_dep, forest_dep_notes, elev_min,         
		elev_max, elev_min_notes, elev_max_notes, water_dep, water_dep_notes,
		perm_notes, perm)

#  Save 
save(sf_df_mammals_elev_adj, file = paste(path_spat_dat, "spp_ranges_w_constraints/sf_df_mammals_CR_EN_VU_elev_adj.Rdata", sep = "/"))



#####  AMPHIBIANS  #####
#  Load spatial data for spp (this is the set of spatial polygons that have been projected, cropped, and aggregated
#   to 1 polygon per spp and had elevation adjustments taken into account.
load(file = paste(path_spat_dat, "spp_ranges_w_constraints/sf_amphibians_CR_EN_VU_elev_adj.Rdata", sep = "/"))
#  Object name "sf_amphibians_elev_adj"

#  Read in non-spatial, manually edited data and select only one record per spp .Some spp have multiple 
#   polygons of ranges but all records for a sinple spp contain the same info in the csv.
dat_f = paste(path_df_dat, "spp_data_w_constraint_info/df_amphibians_CR_EN_VU_NT_DD.csv", sep = "/")

df_amphibians_tmp <- read.csv(dat_f) %>%
	group_by(binomial) %>%
	slice(1) %>%
	arrange(binomial) %>%
	as.data.frame() %>%
	dplyr::filter(RL_code == "CR" | RL_code == "EN" | RL_code == "VU") 
df_amphibians_tmp$binomial <- as.factor(df_amphibians_tmp$binomial)
	
#  Join to original spatial data object to reapply polygon/geometry/spatial info
sf_df_amphibians_elev_adj <- bind_cols(sf_amphibians_elev_adj, df_amphibians_tmp) %>%
	dplyr::select(id_no, binomial, RL_code, class, order, family, genus, RL_year = RL_assess_year,
		shape_length = shape_Leng, shape_area = shape_Area, forest_dep, forest_dep_notes, elev_min,         
		elev_max, elev_min_notes, elev_max_notes, water_dep, water_dep_notes,
		perm_notes, perm)

#  Save 
save(sf_df_amphibians_elev_adj, file = paste(path_spat_dat, "spp_ranges_w_constraints/sf_df_amphibians_CR_EN_VU_elev_adj.Rdata", sep = "/"))



#####  BIRDS  #####
#  Load spatial data for spp (this is the set of spatial polygons that have been projected, cropped, and aggregated
#   to 1 polygon per spp and had elevation adjustments taken into account.
load(file = paste(path_spat_dat, "spp_ranges_w_constraints/sf_birds_CR_EN_VU_elev_adj.Rdata", sep = "/"))
#  Object name "sf_birds_elev_adj"
st_crs(sf_birds_elev_adj) <- st_crs(sf_birds_agg) 
#  Read in non-spatial, manually edited data and select only one record per spp .Some spp have multiple 
#   polygons of ranges but all records for a sinple spp contain the same info in the csv.
dat_f = paste(path_df_dat, "spp_data_w_constraint_info/df_birds_CR_EN_VU_NT_DD.csv", sep = "/")

df_birds_tmp <- read.csv(dat_f) %>%
	group_by(binomial) %>%
	slice(1) %>%
	arrange(binomial) %>%
	as.data.frame() %>%
	dplyr::filter(RL_code == "CR" | RL_code == "EN" | RL_code == "VU") 
df_birds_tmp$binomial <- as.factor(df_birds_tmp$binomial)
	
#  Join to original spatial data object to reapply polygon/geometry/spatial info
sf_df_birds_elev_adj <- bind_cols(sf_birds_elev_adj, df_birds_tmp) %>%
	dplyr::select(id_no, binomial, RL_code, class, order, family, genus, RL_year = RL_assess_year,
		shape_length = shape_Leng, shape_area = shape_Area, forest_dep, forest_dep_notes, elev_min,         
		elev_max, elev_min_notes, elev_max_notes, water_dep, water_dep_notes,
		perm_notes, perm)

#  Save 
save(sf_df_birds_elev_adj, file = paste(path_spat_dat, "spp_ranges_w_constraints/sf_df_birds_CR_EN_VU_elev_adj.Rdata", sep = "/"))
