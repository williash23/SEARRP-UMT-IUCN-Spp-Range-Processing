#  Sara Williams
#  September 18, 2017; last updated Novemeber 15, 2017
#  Script to adjust spp ranges from raw IUCN spatial data download according to constraints 
#   from the text of the spp accounts pages. Outputs area:
#   1. Spp ranges formated as a RasterLayer within a RasterStack object. Cell values are 1 (spp range does
#   occur) and NA (spp range does not occur) per raster cell.
#   2. Spp ranges formated as an sf object (polygons).



#  Load packages
library(raster)
library(rgdal)
library(sf)
library(dplyr)
	#  Packages for running on multiple cores if wanted. If so, need to adjust out loop structure. 
	#   However, this causes spp to change order, so column binding is no longer possible. 
	# library(doParallel)
	# library(foreach)
	# registerDoParallel(7)



path <- "C:/Users/saraw/Documents/SEARRP"
path_spat_dat <- paste(path, "processed_spat_data", sep = "/")
path_df_dat <- paste(path, "processed_excel_data", sep = "/")



#  Load (resampled) elevation rasters (with matching resolutions).
elev_rs <- raster(paste(path_spat_dat, "trans_crop_proj/elev_250m_m.grd", sep = "/"))



#  Load polygonizer() function.
source("C:/Users/saraw/OneDrive/Documents/GitHub/SEARRP-UMT-Spatial-Data-Clean/raster_to_polygon_fun.R")



#####  MAMMALS  #####
#  Load saved spp data before running function if needed
load(paste(path_spat_dat, "spp_ranges_no_constraints/sf_df_mammals_CR_EN_VU.Rdata", sep = "/"))

#   Create list to hold spp ranges 	- sf
adj_range_list <- list()

#   Name for new sf file to be saved
adj_spp_sf = "sf_mammals_CR_EN_VU_elev_adj.Rdata"

for(i in 1:nrow(sf_df_mammals)){	
	sf_tmp <- sf_df_mammals %>%	
		dplyr::filter(row_number() == i)
	sp_tmp <-as(sf_tmp, "Spatial")

	#  Constrain by elevation - results in raster
	elev_tmp1 <- raster::crop(elev_rs, sp_tmp)
	elev_tmp2 <- raster::mask(elev_tmp1, sp_tmp)
	rc_val_elev <- c(-Inf, sf_tmp$elev_min, 0,
		sf_tmp$elev_min, sf_tmp$elev_max, 1,
		sf_tmp$elev_max, Inf, 0)
	rc_mat_elev <- matrix(rc_val_elev, 
		ncol = 3, 
		byrow = TRUE)
	elev_rc <- raster::reclassify(elev_tmp2, rc_mat_elev)
	elev_rc[elev_rc < 1] <- NA 
	
	#  Convert raster to polygon
	adj_range_sp_tmp <- polygonizer(elev_rc) 
	adj_range_sf_tmp <- st_as_sf(adj_range_sp_tmp)
	adj_range_sf <- adj_range_sf_tmp %>%
		dplyr::filter(DN == 1) 
	adj_range_sf_u <- st_union(adj_range_sf)
	ind <- as.numeric(st_bbox(adj_range_sf_u)[1])
	
	if(!is.na(ind)){
		adj_range_sp <-as(adj_range_sf_u, "Spatial")
		adj_range_sp@polygons[[1]]@ID <-  paste0("ID", i)
		id <- paste0("poly_", i)
		adj_range_list[[id]] <- adj_range_sp
			} else {
			adj_range_sp <- NULL
			id <- paste0("poly_", i)
			adj_range_list[[id]] <- NULL
		}
	}

#  Generate new spatial object that contains all new range polygons and convert to 
#   sf object
adj_mammals_ranges_sp <- SpatialPolygons(lapply(adj_range_list, function(x){x@polygons[[1]]}))
sf_mammals_elev_adj <- st_as_sf(adj_mammals_ranges_sp)
st_crs(sf_mammals_elev_adj) <- st_crs(sf_df_mammals)

#  Save sf obejct
save(sf_mammals_elev_adj, file = paste(path_spat_dat, "spp_ranges_w_constraints", adj_spp_sf, sep = "/"))
	
	

#####  AMPHIBIANS #####
#  Load saved spp data before running function if needed
load(paste(path_spat_dat, "spp_ranges_no_constraints/sf_df_amphibians_CR_EN_VU.Rdata", sep = "/"))

#   Create list to hold spp ranges 	- sf
adj_range_list <- list()

#   Name for new sf file to be saved
adj_spp_sf = "sf_amphibians_CR_EN_VU_elev_adj.Rdata"

for(i in 1:nrow(sf_df_amphibians)){	
	sf_tmp <- sf_df_amphibians %>%	
		dplyr::filter(row_number() == i)
	sp_tmp <-as(sf_tmp, "Spatial")

	#  Constrain by elevation - results in raster
	elev_tmp1 <- raster::crop(elev_rs, sp_tmp)
	elev_tmp2 <- raster::mask(elev_tmp1, sp_tmp)
	rc_val_elev <- c(-Inf, sf_tmp$elev_min, 0,
		sf_tmp$elev_min, sf_tmp$elev_max, 1,
		sf_tmp$elev_max, Inf, 0)
	rc_mat_elev <- matrix(rc_val_elev, 
		ncol = 3, 
		byrow = TRUE)
	elev_rc <- raster::reclassify(elev_tmp2, rc_mat_elev)
	elev_rc[elev_rc < 1] <- NA 

	#  Convert raster to polygon
	adj_range_sp_tmp <- polygonizer(elev_rc) 
	adj_range_sf_tmp <- st_as_sf(adj_range_sp_tmp)
	adj_range_sf <- adj_range_sf_tmp %>%
		dplyr::filter(DN == 1) 
	adj_range_sf_u <- st_union(adj_range_sf)
	ind <- as.numeric(st_bbox(adj_range_sf_u)[1])
	
	if(!is.na(ind)){
		adj_range_sp <-as(adj_range_sf_u, "Spatial")
		adj_range_sp@polygons[[1]]@ID <-  paste0("ID", i)
		id <- paste0("poly_", i)
		adj_range_list[[id]] <- adj_range_sp
			} else {
			adj_range_sp <- NULL
			id <- paste0("poly_", i)
			adj_range_list[[id]] <- NULL
		}
	}

#  Generate new spatial object that contains all new range polygons and convert to 
#   sf object
adj_amphibians_ranges_sp <- SpatialPolygons(lapply(adj_range_list, function(x){x@polygons[[1]]}))
sf_amphibians_elev_adj <- st_as_sf(adj_amphibians_ranges_sp)
st_crs(sf_amphibians_elev_adj) <- st_crs(sf_df_amphibians)

#  Save sf obejct
save(sf_amphibians_elev_adj, file = paste(path_spat_dat, "spp_ranges_w_constraints", adj_spp_sf, sep = "/"))



#####  BIRDS #####
#  Load saved spp data before running function if needed
load(paste(path_spat_dat, "spp_ranges_no_constraints/sf_df_birds_CR_EN_VU.Rdata", sep = "/"))

#   Create list to hold spp ranges 	- sf
adj_range_list <- list()

#   Name for new sf file to be saved
adj_spp_sf = "sf_birds_CR_EN_VU_elev_adj.Rdata"

for(i in 1:nrow(sf_df_birds)){	
	sf_tmp <- sf_df_birds %>%	
		dplyr::filter(row_number() == i)
	sp_tmp <-as(sf_tmp, "Spatial")

	#  Constrain by elevation - results in raster
	elev_tmp1 <- raster::crop(elev_rs, sp_tmp)
	elev_tmp2 <- raster::mask(elev_tmp1, sp_tmp)
	rc_val_elev <- c(-Inf, sf_tmp$elev_min, 0,
		sf_tmp$elev_min, sf_tmp$elev_max, 1,
		sf_tmp$elev_max, Inf, 0)
	rc_mat_elev <- matrix(rc_val_elev, 
		ncol = 3, 
		byrow = TRUE)
	elev_rc <- raster::reclassify(elev_tmp2, rc_mat_elev)
	elev_rc[elev_rc < 1] <- NA 

	#  Convert raster to polygon
	adj_range_sp_tmp <- polygonizer(elev_rc) 
	adj_range_sf_tmp <- st_as_sf(adj_range_sp_tmp)
	adj_range_sf <- adj_range_sf_tmp %>%
		dplyr::filter(DN == 1) 
	adj_range_sf_u <- st_union(adj_range_sf)
	ind <- as.numeric(st_bbox(adj_range_sf_u)[1])
	
	if(!is.na(ind)){
		adj_range_sp <-as(adj_range_sf_u, "Spatial")
		adj_range_sp@polygons[[1]]@ID <-  paste0("ID", i)
		id <- paste0("poly_", i)
		adj_range_list[[id]] <- adj_range_sp
			} else {
			adj_range_sp <- NULL
			id <- paste0("poly_", i)
			adj_range_list[[id]] <- NULL
		}
	}

#  Generate new spatial object that contains all new range polygons and convert to 
#   sf object
adj_birds_ranges_sp <- SpatialPolygons(lapply(adj_range_list, function(x){x@polygons[[1]]}))
sf_birds_elev_adj <- st_as_sf(adj_birds_ranges_sp)
st_crs(sf_birds_elev_adj) <- st_crs(sf_df_birds)

#  Save sf obejct
save(sf_birds_elev_adj, file = paste(path_spat_dat, "spp_ranges_w_constraints", adj_spp_sf, sep = "/"))

