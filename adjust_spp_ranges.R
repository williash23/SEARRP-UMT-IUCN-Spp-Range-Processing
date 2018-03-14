###############################################################################
#  Adjust species ranges.
#  September 18, 2017; last updated January 30, 2018
#  Script to adjust spp ranges from raw IUCN spatial data download according to constraints 
#   from the text of the spp accounts pages. Output is spp ranges formated as 
#   sf objects (polygons).
#  Sara Williams
###############################################################################



# =============================================================================
#  Load packages.
# =============================================================================
library(raster)
library(rgdal)
library(sf)
library(dplyr)

	# ----------------------
	#  Packages for running on multiple cores if wanted. If so, need to adjust out loop structure. 
	#   However, this causes spp to change order, so column binding is no longer possible. 
	# library(doParallel)
	# library(foreach)
	# registerDoParallel(7)



# =============================================================================
#  Load input data: this is the output from adjusting the spp ranges based on elevation
#  constraints, which was done using the script 'adjust_spp_ranges.R'. 
# =============================================================================

	# ----------------------
	# Global Multi-resolution Terrain Elevation raster
	elev <- raster("C:/Users/saraw/Documents/SEARRP_Analyses/processed_spat_data/trans_crop_proj/elev_250m_m.grd")

	# ----------------------
	#  Forest cover (aggregated to 900m x 900m resolution.
	cth_for <- raster("C:/Users/saraw/Documents/SEARRP_Analyses/optimization/cth_agg_rc_no_mang.grd")
	
	# ----------------------
	#  Polygonizer() function.
	source("C:/Users/saraw/OneDrive/Documents/GitHub/SEARRP-UMT-Spatial-Data-Clean/raster_to_polygon_fun.R")

	# ----------------------
	#  Mammal spp data
	load(file = "C:/Users/saraw/Documents/SEARRP_Analyses/processed_spat_data/spp_ranges_no_constraints/sf_df_mammals_CR_EN_VU.Rdata")

	# ----------------------
	#  Amphibian spp data
	load(file = "C:/Users/saraw/Documents/SEARRP_Analyses/processed_spat_data/spp_ranges_no_constraints/sf_df_amphibians_CR_EN_VU.Rdata")

	# ----------------------
	#  Bird spp data
	load(file = "C:/Users/saraw/Documents/SEARRP_Analyses/processed_spat_data/spp_ranges_no_constraints/sf_df_birds_CR_EN_VU.Rdata")

	
	
# =============================================================================
#  Resample elevation to match CTH forest cover.
# =============================================================================

	# ----------------------
	#  Resample
	elev_rs <- resample(elev, cth_for, "bilinear")

	
	
# =============================================================================
#  Run range adjustment steps for mammals.
# =============================================================================		

	# ----------------------
	#   Create list to hold spp ranges as sf objects
	adj_range_list <- list()

	# ----------------------
	#   Create raster stack  to hold spp ranges as rasters
	mammals_r <- stack()
	
	# ----------------------
	#  Loop over all spp in set
	for(i in 1:nrow(sf_df_mammals)){	
		sf_tmp <- sf_df_mammals %>%	
			dplyr::filter(row_number() == i)
		sp_tmp <-as(sf_tmp, "Spatial")

		# ----------------------
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
		
		# ----------------------
		#  Extend new range raster to same extent as forest cover.
		spp_r_tmp <- raster::extend(elev_rc, extent(cth_for), value = NA)

		# ----------------------
		#  Convert new raster to a list to avoid issues with NULL (empty) rasters.
		spp_r_ls <- list(spp_r_tmp)

		# ----------------------
		# Extract first element from each list item and save as a single list object.
		spp_r <- spp_r_ls[[1]]
	
		# ----------------------
		# Add new range raster to raster stack.
		mammals_r <- stack(mammals_r, spp_r)
	
		# ----------------------
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

	# ----------------------
	#  Generate new spatial object that contains all new range polygons and convert to 
	#   sf object
	adj_mammals_ranges_sp <- SpatialPolygons(lapply(adj_range_list, function(x){x@polygons[[1]]}))
	sf_mammals_elev_adj <- st_as_sf(adj_mammals_ranges_sp)
	st_crs(sf_mammals_elev_adj) <- st_crs(sf_df_mammals)

	# ----------------------
	#  Save sf object
	#save(sf_mammals_elev_adj, 
		#file = "C:/Users/saraw/Documents/SEARRP_Analyses/processed_spat_data/spp_ranges_w_constraints/sf_mammals_CR_EN_VU_elev_adj.Rdata")
	
	# ----------------------
	#  Save raster
	writeRaster(mammals_r, "C:/Users/saraw/Documents/SEARRP_Analyses/processed_spat_data/spp_ranges_w_constraints/mammals_r.grd")
	
	
	
# =============================================================================
#  Run adjustment steps for amphibians
# =============================================================================		

	# ----------------------
	#   Create list to hold spp ranges as sf objects
	adj_range_list <- list()
	
	# ----------------------
	#   Create raster stack  to hold spp ranges as rasters
	amphibians_r <- stack()

	# ----------------------
	#  Loop over all spp in set
	for(i in 1:nrow(sf_df_amphibians)){	
		sf_tmp <- sf_df_amphibians %>%	
			dplyr::filter(row_number() == i)
		sp_tmp <-as(sf_tmp, "Spatial")

		# ----------------------
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

		# ----------------------
		#  Extend new range raster to same extent as forest cover.
		spp_r_tmp <- raster::extend(elev_rc, extent(cth_for), value = NA)

		# ----------------------
		#  Convert new raster to a list to avoid issues with NULL (empty) rasters.
		spp_r_ls <- list(spp_r_tmp)

		# ----------------------
		# Extract first element from each list item and save as a single list object.
		spp_r <- spp_r_ls[[1]]
	
		# ----------------------
		# Add new range raster to raster stack.
		amphibians_r <- stack(amphibians_r, spp_r)
		
		# ----------------------
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

	# ----------------------
	#  Generate new spatial object that contains all new range polygons and convert to 
	#   sf object
	adj_amphibians_ranges_sp <- SpatialPolygons(lapply(adj_range_list, function(x){x@polygons[[1]]}))
	sf_amphibians_elev_adj <- st_as_sf(adj_amphibians_ranges_sp)
	st_crs(sf_amphibians_elev_adj) <- st_crs(sf_df_amphibians)

	# ----------------------
	#  Save sf object
	save(sf_amphibians_elev_adj, 
		file = "C:/Users/saraw/Documents/SEARRP_Analyses/processed_spat_data/spp_ranges_w_constraints/sf_amphibians_CR_EN_VU_elev_adj.Rdata")

	# ----------------------
	#  Save raster
	writeRaster(amphibians_r, "C:/Users/saraw/Documents/SEARRP_Analyses/processed_spat_data/spp_ranges_w_constraints/amphibians_r.grd")
	
	

# =============================================================================
#  Run adjustment steps for birds
# =============================================================================		

	# ----------------------
	#   Create list to hold spp ranges as sf objects
	adj_range_list <- list()

	# ----------------------
	#   Create raster stack  to hold spp ranges as rasters
	birds_r <- stack()
	
	# ----------------------
	#  Loop over all spp in set
	for(i in 1:nrow(sf_df_birds)){	
		sf_tmp <- sf_df_birds %>%	
			dplyr::filter(row_number() == i)
		sp_tmp <-as(sf_tmp, "Spatial")

		# ----------------------
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

		# ----------------------
		#  Extend new range raster to same extent as forest cover.
		spp_r_tmp <- raster::extend(elev_rc, extent(cth_for), value = NA)

		# ----------------------
		#  Convert new raster to a list to avoid issues with NULL (empty) rasters.
		spp_r_ls <- list(spp_r_tmp)

		# ----------------------
		# Extract first element from each list item and save as a single list object.
		spp_r <- spp_r_ls[[1]]
	
		# ----------------------
		# Add new range raster to raster stack.
		birds_r <- stack(birds_r, spp_r)
		
		# ----------------------
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

	# ----------------------
	#  Generate new spatial object that contains all new range polygons and convert to 
	#   sf object
	adj_birds_ranges_sp <- SpatialPolygons(lapply(adj_range_list, function(x){x@polygons[[1]]}))
	sf_birds_elev_adj <- st_as_sf(adj_birds_ranges_sp)
	st_crs(sf_birds_elev_adj) <- st_crs(sf_df_birds)

	# ----------------------
	#  Save sf object
	save(sf_birds_elev_adj, 
		file = "C:/Users/saraw/Documents/SEARRP_Analyses/processed_spat_data/spp_ranges_w_constraints/sf_birds_CR_EN_VU_elev_adj.Rdata")

	# ----------------------
	#  Save raster
	writeRaster(birds_r, "C:/Users/saraw/Documents/SEARRP_Analyses/processed_spat_data/spp_ranges_w_constraints/birds_r.grd")
	
	
	
# =============================================================================	
###############################################################################
