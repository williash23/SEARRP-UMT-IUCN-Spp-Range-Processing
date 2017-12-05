#  Sara Williams
#  August 16, 2017; November 16, 2017
#  Script to plot adjusted species distribution polygons with 
#  information on constraints such as elevation, forest, and water dependency.

library(sf)
library(sp)
library(raster)
library(dplyr) 
library(ggplot2) 
library(rasterVis)
library(RColorBrewer)
####  NOTE: Must install the development version of ggpot2 to use geom_sf().
####   devtools::install_github("tidyverse/ggplot2")



path <- "C:/Users/saraw/Documents/SEARRP"
path_spat_dat <- paste(path, "processed_spat_data", sep = "/")
path_df_dat <- paste(path, "processed_excel_data", sep = "/")



#  Load environmental/border data
load(file = paste(path_spat_dat, "trans_crop_proj/pa_rt_sf.Rdata", sep = "/"))
load(file = paste(path_spat_dat, "trans_crop_proj/border_sabah_d.Rdata", sep = "/"))
load(file = paste(path_spat_dat, "trans_crop_proj/border_sarawak_d.Rdata", sep = "/"))
load(file = paste(path_spat_dat, "trans_crop_proj/border_kali_d.Rdata", sep = "/"))
load(file = paste(path_spat_dat, "planning_units/locked_in_sf.Rdata", sep = "/"))



#   Generate required sf objects for borders and outlines
sabah_sf <- st_as_sf(border_sabah_d)
sarawak_sf <- st_as_sf(border_sarawak_d)
kali_sf <- st_as_sf(border_kali_d)



#####  BOTH RANGES ON SAME PLOT  ##### 	

#####  MAMMALS  #####
#  Load raw spp range polygons sf object
#load(paste(path_spat_dat, "spp_ranges_no_constraints/sf_df_mammals_CR_EN_VU.Rdata", sep = "/"))
#load(paste(path_spat_dat, "spp_ranges_w_constraints/CR_EN_VU/sf_adj_df_mammals_CR_EN_VU.Rdata", sep = "/"))
#  Object name is "sf_df_mammals"

# sf_df_mammals <- sf_df_mammals %>%
	# dplyr::filter(binomial != "Melogale everetti")

#   Loop over each spp (row)  
for (s in 1:nrow(sf_df_mammals)){

    dat <- sf_df_mammals %>%
		slice(s)
	nam <- dat$binomial
	
	dat_adj <- sf_mammals_elev_adj %>%
		slice(s)
		
	#  Plot
    range_p <- ggplot() +
		geom_sf(data = sabah_sf, colour = "grey50", fill = "grey80") +
		geom_sf(data = locked_in_sf, aes(fill = factor(status)), alpha = 0.5) +
			scale_fill_brewer(type = "qual", palette = "Greens",
			labels = c("	Class I - Protected Forest Reserve", "	Class VI - Virgin Jungle Reserve", "	Class VII - Wildlife Reserve", 
				"Parks", "Wildlife Sanctuary"),
			name = "Existing Protected Areas") +
		geom_sf(data = dat, colour = "transparent", fill = "darkorange2", alpha = 0.4) +
		geom_sf(data = dat_adj, colour = "transparent", fill = "darkred", alpha = 0.5) +
		geom_sf(data = sarawak_sf, colour = "grey50", fill = "grey80") +
		geom_sf(data = kali_sf, colour = "grey50", fill = "grey70") +
		coord_sf(crs = st_crs(32650)) +
		xlab("Latitude") +
		ylab("Longitude") +
		xlim(315000, 755000) +
		ylim(455000, 815000) +
		ggtitle(paste0("Raw and Adjusted Range", " - ", nam)) +
		theme_bw()
	
	#  Save to pdf
	ggsave(filename = paste(path_spat_dat, "/plots/mammals/", nam, "_raw_adj_range.jpeg", sep = ""), plot = range_p)
    }   


	
#####  AMPHIBIANS  #####
#  Load raw spp range polygons sf object
#load(paste(path_spat_dat, "spp_ranges_no_constraints/sf_df_amphibians_CR_EN_VU.Rdata", sep = "/"))

#   Loop over each spp (row)  
for (s in 1:nrow(sf_amphibians_elev_adj)){

    dat <- sf_df_amphibians %>%
		slice(s)
	nam <- dat$binomial
	
	dat_adj <- sf_amphibians_elev_adj %>%
		slice(s)
		
	#  Plot
    range_p <- ggplot() +
		geom_sf(data = sabah_sf, colour = "grey50", fill = "grey80") +
		geom_sf(data = locked_in_sf, aes(fill = factor(status)), alpha = 0.5) +
			scale_fill_brewer(type = "qual", palette = "Greens",
			labels = c("	Class I - Protected Forest Reserve", "	Class VI - Virgin Jungle Reserve", "	Class VII - Wildlife Reserve", 
				"Parks", "Wildlife Sanctuary"),
			name = "Existing Protected Areas") +
		geom_sf(data = dat, colour = "transparent", fill = "darkorange2", alpha = 0.4) +
		geom_sf(data = dat_adj, colour = "transparent", fill = "darkred", alpha = 0.5) +
		geom_sf(data = sarawak_sf, colour = "grey50", fill = "grey80") +
		geom_sf(data = kali_sf, colour = "grey50", fill = "grey70") +
		coord_sf(crs = st_crs(32650)) +
		xlab("Latitude") +
		ylab("Longitude") +
		xlim(315000, 755000) +
		ylim(455000, 815000) +
		ggtitle(paste0("Raw range", " - ", nam)) +
		theme_bw()
	
	#  Save to pdf
	ggsave(filename = paste(path_spat_dat, "/plots/amphibians/", nam, "_raw_adj_range.jpeg", sep = ""), plot = range_p)
    }   

	

#####  BIRDS  #####
#  Load raw spp range polygons sf object
#load(paste(path_spat_dat, "spp_ranges_no_constraints/sf_df_birds_CR_EN_VU.Rdata", sep = "/"))

#   Loop over each spp (row)  
for (s in 1:nrow(sf_df_birds)){

    dat <- sf_df_birds %>%
		slice(s)
	nam <- dat$binomial
	
	dat_adj <- sf_birds_elev_adj %>%
		slice(s)
		
	#  Plot
    range_p <- ggplot() +
		geom_sf(data = sabah_sf, colour = "grey50", fill = "grey80") +
		geom_sf(data = locked_in_sf, aes(fill = factor(status)), alpha = 0.5) +
			scale_fill_brewer(type = "qual", palette = "Greens",
			labels = c("	Class I - Protected Forest Reserve", "	Class VI - Virgin Jungle Reserve", "	Class VII - Wildlife Reserve", 
				"Parks", "Wildlife Sanctuary"),
			name = "Existing Protected Areas") +
		geom_sf(data = dat, colour = "transparent", fill = "darkorange2", alpha = 0.4) +
		geom_sf(data = dat_adj, colour = "transparent", fill = "darkred", alpha = 0.5) +
		geom_sf(data = sarawak_sf, colour = "grey50", fill = "grey80") +
		geom_sf(data = kali_sf, colour = "grey50", fill = "grey70") +
		coord_sf(crs = st_crs(32650)) +
		xlab("Latitude") +
		ylab("Longitude") +
		xlim(315000, 755000) +
		ylim(455000, 815000) +
		ggtitle(paste0("Raw range", " - ", nam)) +
		theme_bw()
	
	#  Save to pdf
	ggsave(filename = paste(path_spat_dat, "/plots/birds/", nam, "_raw_adj_range.jpeg", sep = ""), plot = range_p)
    }   



#####  RAW RANGES  ##### 	

#####  MAMMALS  #####
#  Load raw spp range polygons sf object
#load(paste(path_spat_dat, "spp_ranges_no_constraints/sf_df_mammals_CR_EN_VU.Rdata", sep = "/"))
#  Object name is "sf_df_mammals"

#   Loop over each spp (row)  
for (s in 1:nrow(sf_df_mammals)){

    dat <- sf_df_mammals %>%
		slice(s)
	nam <- dat$binomial
	
	dat_adj <- sf_mammals_elev_adj %>%
		slice(s)
		
	#  Plot
    range_p <- ggplot() +
		geom_sf(data = sabah_sf, colour = "grey50", fill = "grey80") +
		geom_sf(data = locked_in_sf, aes(fill = factor(status)), alpha = 0.5) +
			scale_fill_brewer(type = "qual", palette = "Greens",
			labels = c("	Class I - Protected Forest Reserve", "	Class VI - Virgin Jungle Reserve", "	Class VII - Wildlife Reserve", 
				"Parks", "Wildlife Sanctuary"),
			name = "Existing Protected Areas") +
		geom_sf(data = dat, colour = "transparent", fill = "darkorange2", alpha = 0.4) +
		geom_sf(data = dat_adj, colour = "transparent", fill = "darkred", alpha = 0.5) +
		geom_sf(data = sarawak_sf, colour = "grey50", fill = "grey80") +
		geom_sf(data = kali_sf, colour = "grey50", fill = "grey70") +
		coord_sf(crs = st_crs(32650)) +
		xlab("Latitude") +
		ylab("Longitude") +
		xlim(315000, 755000) +
		ylim(455000, 815000) +
		ggtitle(paste0("Raw and Adjusted Range", " - ", nam)) +
		theme_bw()
	
	#  Save to pdf
	ggsave(filename = paste(path_spat_dat, "/plots/mammals/", nam, "_raw_adj_range.jpeg", sep = ""), plot = range_p)
    }   


	
#####  AMPHIBIANS  #####
#  Load raw spp range polygons sf object
load(paste(path_spat_dat, "spp_ranges_no_constraints/sf_df_amphibians_CR_EN_VU.Rdata", sep = "/"))

#   Loop over each spp (row)  
for (s in 1:nrow(sf_amphibians_elev_adj)){

    dat <- sf_df_amphibians %>%
		slice(s)
	nam <- dat$binomial
	
	dat_adj <- sf_amphibians_elev_adj %>%
		slice(s)
		
	#  Plot
    range_p <- ggplot() +
		geom_sf(data = sabah_sf, colour = "grey50", fill = "grey80") +
		geom_sf(data = locked_in_sf, aes(fill = factor(status)), alpha = 0.5) +
			scale_fill_brewer(type = "qual", palette = "Greens",
			labels = c("	Class I - Protected Forest Reserve", "	Class VI - Virgin Jungle Reserve", "	Class VII - Wildlife Reserve", 
				"Parks", "Wildlife Sanctuary"),
			name = "Existing Protected Areas") +
		geom_sf(data = dat, colour = "transparent", fill = "darkorange2", alpha = 0.4) +
		geom_sf(data = dat_adj, colour = "transparent", fill = "darkred", alpha = 0.5) +
		geom_sf(data = sarawak_sf, colour = "grey50", fill = "grey80") +
		geom_sf(data = kali_sf, colour = "grey50", fill = "grey70") +
		coord_sf(crs = st_crs(32650)) +
		xlab("Latitude") +
		ylab("Longitude") +
		xlim(315000, 755000) +
		ylim(455000, 815000) +
		ggtitle(paste0("Raw range", " - ", nam)) +
		theme_bw()
	
	#  Save to pdf
	ggsave(filename = paste(path_spat_dat, "/plots/amphibians/", nam, "_raw_adj_range.pdf", sep = ""), plot = range_p)
    }   

	

#####  BIRDS  #####
#  Load raw spp range polygons sf object
load(paste(path_spat_dat, "spp_ranges_no_constraints/sf_df_birds_CR_EN_VU.Rdata", sep = "/"))

#   Loop over each spp (row)  
for (s in 1:nrow(sf_df_birds)){

    dat <- sf_df_birds %>%
		slice(s)
	nam <- dat$binomial
	
	dat_adj <- sf_birds_elev_adj %>%
		slice(s)
		
	#  Plot
    range_p <- ggplot() +
		geom_sf(data = sabah_sf, colour = "grey50", fill = "grey80") +
		geom_sf(data = locked_in_sf, aes(fill = factor(status)), alpha = 0.5) +
			scale_fill_brewer(type = "qual", palette = "Greens",
			labels = c("	Class I - Protected Forest Reserve", "	Class VI - Virgin Jungle Reserve", "	Class VII - Wildlife Reserve", 
				"Parks", "Wildlife Sanctuary"),
			name = "Existing Protected Areas") +
		geom_sf(data = dat, colour = "transparent", fill = "darkorange2", alpha = 0.4) +
		geom_sf(data = dat_adj, colour = "transparent", fill = "darkred", alpha = 0.5) +
		geom_sf(data = sarawak_sf, colour = "grey50", fill = "grey80") +
		geom_sf(data = kali_sf, colour = "grey50", fill = "grey70") +
		coord_sf(crs = st_crs(32650)) +
		xlab("Latitude") +
		ylab("Longitude") +
		xlim(315000, 755000) +
		ylim(455000, 815000) +
		ggtitle(paste0("Raw range", " - ", nam)) +
		theme_bw()
	
	#  Save to pdf
	ggsave(filename = paste(path_spat_dat, "/plots/birds/", nam, "_raw_adj_range.pdf", sep = ""), plot = range_p)
    }   


	
	
	
	
	
	
	
	
	
	
	
	
	

#####  ADJUSTED RANGES ##### 	
#  Plot adjusted spp ranges (elevational constraints) using sf objects 
	
#####  MAMMALS  ##### 
#  Load adjusted spp range polygons sf object and excel data
#   Object is called "sf_df_mammals_elev_adj"
#load(file = paste(path_spat_dat, "spp_ranges_w_constraints/sf_df_mammals_CR_EN_VU_elev_adj.Rdata", sep = "/"))
st_crs(sf_df_mammals_elev_adj) <- st_crs(sabah_sf)

#   Loop over each spp (row)  
for (s in 1:nrow(sf_df_mammals_elev_adj)){

    dat <- sf_df_mammals_elev_adj %>%
		slice(s)
	nam <- dat$binomial
	
	#  Plot
    range_p <- ggplot() +
		geom_sf(data = sabah_sf, colour = "grey50", fill = "grey80") +
		geom_sf(data = locked_in_sf, aes(fill = factor(status)), alpha = 0.5) +
			scale_fill_brewer(type = "qual", palette = "Greens",
			labels = c("	Class I - Protected Forest Reserve", "	Class VI - Virgin Jungle Reserve", "	Class VII - Wildlife Reserve", 
				"Parks", "Wildlife Sanctuary"),
			name = "Existing Protected Areas") +
		geom_sf(data = dat, colour = "transparent", fill = "darkorange2", alpha = 0.4) +
		geom_sf(data = sarawak_sf, colour = "grey50", fill = "grey80") +
		geom_sf(data = kali_sf, colour = "grey50", fill = "grey70") +
		coord_sf(crs = st_crs(32650)) +
		xlab("Latitude") +
		ylab("Longitude") +
		xlim(315000, 755000) +
		ylim(455000, 815000) +
		ggtitle(paste0("Adjusted range", " - ", nam)) +
		theme_bw()
	
	#  Save to pdf
	ggsave(filename =paste(path_spat_dat, "/plots/adj_spp_range/mammals/", nam, "_adj_range.pdf", sep = ""), plot = range_p)
    }   

	
#####  AMPHIBIANS  ##### 
#  Load adjusted spp range polygons sf object and excel data
#   sf object is called "sf_adj_df_amphibians_CR_EN_VU"
#load(file = paste(path_spat_dat, "spp_ranges_w_constraints/sf_df_amphibians_CR_EN_VU_elev_adj.Rdata", sep = "/"))
st_crs(sf_df_amphibians_elev_adj) <- st_crs(sabah_sf)

#   Loop over each spp (row)  
for (s in 1:nrow(sf_df_amphibians_elev_adj)){

    dat <- sf_df_amphibians_elev_adj %>%
		slice(s)
	nam <- dat$binomial
	
	#  Plot
    range_p <- ggplot() +
		geom_sf(data = sabah_sf, colour = "grey50", fill = "grey80") +
		geom_sf(data = locked_in_sf, aes(fill = factor(status)), alpha = 0.5) +
			scale_fill_brewer(type = "qual", palette = "Greens",
			labels = c("	Class I - Protected Forest Reserve", "	Class VI - Virgin Jungle Reserve", "	Class VII - Wildlife Reserve", 
				"Parks", "Wildlife Sanctuary"),
			name = "Existing Protected Areas") +
		geom_sf(data = dat, colour = "transparent", fill = "darkorange2", alpha = 0.4) +
		geom_sf(data = sarawak_sf, colour = "grey50", fill = "grey80") +
		geom_sf(data = kali_sf, colour = "grey50", fill = "grey70") +
		coord_sf(crs = st_crs(32650)) +
		xlab("Latitude") +
		ylab("Longitude") +
		xlim(315000, 755000) +
		ylim(455000, 815000) +
		ggtitle(paste0("Adjusted range", " - ", nam)) +
		theme_bw()
	
	#  Save to pdf
	ggsave(filename = paste(path_spat_dat, "/plots/adj_spp_range/amphibians/", nam, "_adj_range.pdf", sep = ""), plot = range_p)
    }   
	
#####  BIRDS  ##### 
#  Load adjusted spp range polygons sf object and excel data
#   sf object is called "sf_adj_df_birds_CR_EN_VU"
#load(file = paste(path_spat_dat, "spp_ranges_w_constraints/sf_df_birds_CR_EN_VU_elev_adj.Rdata", sep = "/"))
st_crs(sf_df_birds_elev_adj) <- st_crs(sabah_sf)

#   Loop over each spp (row)  
for (s in 1:nrow(sf_df_birds_elev_adj)){

    dat <- sf_df_birds_elev_adj %>%
		slice(s)
	nam <- dat$binomial
	
	#  Plot
    range_p <- ggplot() +
		geom_sf(data = sabah_sf, colour = "grey50", fill = "grey80") +
		geom_sf(data = locked_in_sf, aes(fill = factor(status)), alpha = 0.5) +
			scale_fill_brewer(type = "qual", palette = "Greens",
			labels = c("	Class I - Protected Forest Reserve", "	Class VI - Virgin Jungle Reserve", "	Class VII - Wildlife Reserve", 
				"Parks", "Wildlife Sanctuary"),
			name = "Existing Protected Areas") +
		geom_sf(data = dat, colour = "transparent", fill = "darkorange2", alpha = 0.4) +
		geom_sf(data = sarawak_sf, colour = "grey50", fill = "grey80") +
		geom_sf(data = kali_sf, colour = "grey50", fill = "grey70") +
		coord_sf(crs = st_crs(32650)) +
		xlab("Latitude") +
		ylab("Longitude") +
		xlim(315000, 755000) +
		ylim(455000, 815000) +
		ggtitle(paste0("Adjusted range", " - ", nam)) +
		theme_bw()
	
	#  Save to pdf
	ggsave(filename = paste(path_spat_dat, "/plots/adj_spp_range/birds/", nam, "_adj_range.pdf", sep = ""), plot = range_p)
    }   

	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	

#  Using rasterVis package
#   Set color palette theme
r_theme <- rasterTheme(rev(brewer.pal(8, "Spectral")))

plot_mams_r <- levelplot(mams_r, par.settings = r_theme, 
	main = "Threated mammal species ranges \n",
	xlab= "Longitude (UTM)",
	ylab="Latitude (UTM)") + # add margin = FALSE if no margins wanted
	layer(sp.polygons(border_sabah_d, lwd = 0.8, col = 'grey40')) + 
	layer(sp.polygons(border_sarawak_d, lwd = 0.8, col = 'grey40', fill = 'gray40')) +
	layer(sp.polygons(border_kali_d, lwd = 0.8, col = 'grey40', fill = 'gray40')) 
plot_mams_r

plot_amphs_r <- levelplot(amphs_r, par.settings = r_theme, 
	main = "Threated amphibian species ranges \n",
	xlab= "Longitude (UTM)",
	ylab="Latitude (UTM)") +
	layer(sp.polygons(border_sabah_d, lwd = 0.8, col = 'grey40')) + 
	layer(sp.polygons(border_sarawak_d, lwd = 0.8, col = 'grey40', fill = 'gray40')) +
	layer(sp.polygons(border_kali_d, lwd = 0.8, col = 'grey40', fill = 'gray40')) 
plot_amphs_r

plot_birds_r <- levelplot(birds_r, par.settings = r_theme, 
	main = "Threated bird species ranges \n",
	xlab= "Longitude (UTM)",
	ylab="Latitude (UTM)") +
	layer(sp.polygons(border_sabah_d, lwd = 0.8, col = 'grey40')) + 
	layer(sp.polygons(border_sarawak_d, lwd = 0.8, col = 'grey40', fill = 'gray40')) +
	layer(sp.polygons(border_kali_d, lwd = 0.8, col = 'grey40', fill = 'gray40')) 
plot_birds_r



#  Function and data prep to use ggplot with RasterLayer
raster_ggplot <- function(rastx) {
  require(SDMTools)
  stopifnot(class(rastx) == "RasterLayer")
  gfx_data <- getXYcoords(rastx)
  # lats need to be flipped
  gfx_data <- expand.grid(lons = gfx_data$x, lats = rev(gfx_data$y), 
                            stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
  gfx_data$n_spp <- rastx@data@values
  return (gfx_data)
}

#  Convert to data frame readily available for ggplot2
mams_spp_gg <- raster_ggplot(mams_r)
amphs_spp_gg <- raster_ggplot(amphs_r)
birds_spp_gg <- raster_ggplot(birds_r)
   
#  Plot raster-like objects
mams_r_p <- ggplot(mams_spp_gg) +
  geom_raster(aes(lons, lats, fill = n_spp)) +
  coord_equal() +
  geom_polygon(data = fortify(border_sabah_d), aes(long, lat, group=group)) +
  scale_fill_distiller(palette = "Spectral", name = "Number of spp") +
  xlab("Latitude") +
  ylab("Longitude") +
  ggtitle("Mammals - Threatened Species Ranges") +
  theme_bw()
mams_r_p


amphs_r_p <- ggplot() +
  geom_raster(data = amphs_spp_gg, aes(lons, lats, fill = n_spp)) +
  scale_fill_distiller(palette = "Spectral", name = "Number of spp") +
  xlab("Latitude") +
  ylab("Longitude") +
  ggtitle("Amphibians - Threatened Species Ranges") +
  theme_bw()
amphs_r_p 

birds_r_p <- ggplot(birds_spp_gg) +
  geom_raster(aes(lons, lats, fill = n_spp)) +
  scale_fill_distiller(palette = "Spectral", name = "Number of spp") +
  xlab("Latitude") +
  ylab("Longitude") +
  ggtitle("Birds - Threatened Species Ranges") +
  theme_bw()
birds_r_p

