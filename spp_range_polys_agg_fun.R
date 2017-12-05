#  Sara Williams
#  September 14, 2017
#  Function to adjust aggregate polygons within a sf object based on species name ("binomial").


#  Because some spp have more than 1 polygon (i.e., row) in an sf object, aggregate the polygons 

#  Load saved data file before running function
#   sf_f: file path to simple feature object of species spatial data 


agg_polys <- function(sf_n = NULL, sf_agg_n = NULL){ 
												 
	# sf_n: name of sf object loaded
	# sf_agg_n: simple feature object name to save aggregated sf object to
	
	sf_spp_threat <- sf_n %>%
		dplyr::select(binomial, id_no, geometry, shape_Leng) %>% 
		arrange(binomial, shape_Leng)
	sf_spp_threat$binomial <- as.factor(sf_spp_threat$binomial)
	
	sf_spp_threat$group_no <- group_indices(sf_spp_threat, binomial)
	groups = sf_spp_threat$group_no
    
	sf_spp_threat_agg = st_sf(
        geom = do.call(c,
            lapply(1:max(groups), function(g){
                st_union(sf_spp_threat[groups==g,])
            })
            )
        )
    sf_spp_threat_agg$group = 1:nrow(sf_spp_threat_agg)
    
	save(sf_spp_threat_agg, 
		file = paste(path_spat_dat, "spp_ranges_no_constraints", sf_agg_n, sep = "/"))
		
	}
	
