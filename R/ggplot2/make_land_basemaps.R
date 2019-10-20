make_maps <- function(island_name){
        # Capitalize first letter
        CapStr <- function(y) {
                c <- strsplit(y, " ")[[1]]
                paste(toupper(substring(c, 1,1)), substring(c, 2),
                      sep="", collapse=" ")
        }
        
        island_name_upperCase <- CapStr(island_name)
        # subset for shapefiles
        df <- readr::read_csv(here::here("data/shapefiles/", "shapefile_metadata.csv"))
        df %>% dplyr::filter(island == island_name) -> df

        ## load shapefiles
        
        # coast line
        coast_df <- df %>% dplyr::filter(geo == "coast_geo")
        coast_geo <- sf::read_sf(here::here("data/shapefiles/corrected_projections", coast_df$shapefile_name))

        # wetlands
        wetlands_df <- df %>% dplyr::filter(geo == "wetlands_geo")
        wetlands_geo <- sf::read_sf(here::here("data/shapefiles/corrected_projections", wetlands_df$shapefile_name))
        
        # moisture zones How TO DEAL WITH RELEVELING OF FACTORS?
        moisture_df <- df %>% dplyr::filter(geo == "moisture_geo")
        moisture_geo <- sf::read_sf(here::here("data/shapefiles/corrected_projections", moisture_df$shapefile_name))
        moisture_geo$MoistureZo <- as.factor(moisture_geo$MoistureZo)
        levels(moisture_geo$MoistureZo) <- moisture_vec
        
        # solar radiation
        solrad_df <- df %>% dplyr::filter(geo == "solrad_geo")
        solrad_geo <- sf::read_sf(here::here("data/shapefiles/corrected_projections", solrad_df$shapefile_name))
        solrad_geo$solar_cal <- as.factor(solrad_geo$solar_cal)
        levels(solrad_geo$solar_cal) <- solrad_vec
        
        # land cover
        agriculture_df <- df %>% dplyr::filter(geo == "agriculture_geo")
        agriculture_geo <- sf::read_sf(here::here("data/shapefiles/corrected_projections", agriculture_df$shapefile_name))
        agriculture_geo$Maj_LC <- as.factor(agriculture_geo$Maj_LC)
        levels(agriculture_geo$Maj_LC) <- agriculture_vec
        
        ## load rasters
        relief_100m <- data.table::fread(paste0(here::here("data/relief/100m"), "/", island_name, "_100m.csv"))
        relief_200m <- data.table::fread(paste0(here::here("data/relief/200m"), "/", island_name, "_200m.csv"))
        
        if(island_name == "hawaii" | island_name == "maui"){
                wetlands_df2 <- df %>% dplyr::filter(geo == "wetlands_geo2")
                wetlands_geo2 <- sf::read_sf(here::here("data/shapefiles/corrected_projections", wetlands_df2$shapefile_name))
                
                ## make the island basemap with no relief
                basemap <- make_island_map(coast_geo = coast_geo,
                                           wetlands_geo = wetlands_geo,
                                           subtitle = island_name_upperCase) +
                        geom_sf(data = wetlands_geo2,
                                fill = "#012F47",
                                color = "#012F47")
                saveRDS(basemap, paste0(here::here("figures/rds"), "/", island_name, "_land_basemap_noRelief.rds"))
                
                ## make the moisture basemap with no relief
                moisture_basemap <- make_moisture_map(moisture_geo = moisture_geo,
                                                      wetlands_geo = wetlands_geo,
                                                      subtitle = island_name_upperCase) +
                        geom_sf(data = wetlands_geo2,
                                fill = "#012F47",
                                color = "#012F47")
                saveRDS(moisture_basemap, paste0(here::here("figures/rds"), "/", island_name, "_moisture_basemap_noRelief.rds"))
                
                ## make the solar radiation basemap with no relief
                solrad_basemap <- make_solrad_map(solrad_geo = solrad_geo,
                                                  wetlands_geo = wetlands_geo,
                                                  subtitle = island_name_upperCase) +
                        geom_sf(data = wetlands_geo2,
                                fill = "#012F47",
                                color = "#012F47")
                saveRDS(solrad_basemap, paste0(here::here("figures/rds"), "/", island_name, "_solrad_basemap_noRelief.rds"))
                
                ## make the agriculture basemap with no relief
                agriculture_basemap <- make_agriculture_map(agriculture_geo = agriculture_geo,
                                                            wetlands_geo = wetlands_geo,
                                                            subtitle = island_name_upperCase) +
                        geom_sf(data = wetlands_geo2,
                                fill = "#012F47",
                                color = "#012F47")
                saveRDS(agriculture_basemap, paste0(here::here("figures/rds"), "/", island_name, "_agriculture_basemap_noRelief.rds"))
                
                ## make the 100m island basemap
                basemap <- make_island_map(coast_geo = coast_geo,
                                           wetlands_geo = wetlands_geo,
                                           subtitle = island_name_upperCase) +
                        geom_sf(data = wetlands_geo2,
                                fill = "#012F47",
                                color = "#012F47") +
                        geom_raster(
                                data = relief_100m,
                                inherit.aes = FALSE,
                                aes(
                                        x = x,
                                        y = y,
                                        alpha = value
                                )
                        )
                saveRDS(basemap, paste0(here::here("figures/rds"), "/", island_name, "_land_100m_basemap.rds"))
                
                ## make the 100m moisture basemap
                moisture_basemap <- make_moisture_map(moisture_geo = moisture_geo,
                                                      wetlands_geo = wetlands_geo,
                                                      subtitle = island_name_upperCase) +
                        geom_sf(data = wetlands_geo2,
                                fill = "#012F47",
                                color = "#012F47") +
                        geom_raster(
                                data = relief_100m,
                                inherit.aes = FALSE,
                                aes(
                                        x = x,
                                        y = y,
                                        alpha = value
                                )
                        )
                saveRDS(moisture_basemap, paste0(here::here("figures/rds"), "/", island_name, "_moisture_100m_basemap.rds"))
                
                ## make the 100m solar radiation basemap
                solrad_basemap <- make_solrad_map(solrad_geo = solrad_geo,
                                                  wetlands_geo = wetlands_geo,
                                                  subtitle = island_name_upperCase) +
                        geom_sf(data = wetlands_geo2,
                                fill = "#012F47",
                                color = "#012F47") +
                        geom_raster(
                                data = relief_100m,
                                inherit.aes = FALSE,
                                aes(
                                        x = x,
                                        y = y,
                                        alpha = value
                                )
                        )
                saveRDS(solrad_basemap, paste0(here::here("figures/rds"), "/", island_name, "_solrad_100m_basemap.rds"))
                
                ## make the 100m agriculture basemap
                agriculture_basemap <- make_agriculture_map(agriculture_geo = agriculture_geo,
                                                  wetlands_geo = wetlands_geo,
                                                  subtitle = island_name_upperCase) +
                        geom_sf(data = wetlands_geo2,
                                fill = "#012F47",
                                color = "#012F47") +
                        geom_raster(
                                data = relief_100m,
                                inherit.aes = FALSE,
                                aes(
                                        x = x,
                                        y = y,
                                        alpha = value
                                )
                        )
                saveRDS(agriculture_basemap, paste0(here::here("figures/rds"), "/", island_name, "_agriculture_100m_basemap.rds"))
                
                ## make the 200m island basemap
                basemap <- make_island_map(coast_geo = coast_geo,
                                           wetlands_geo = wetlands_geo,
                                           subtitle = island_name_upperCase) +
                        geom_sf(data = wetlands_geo2,
                                fill = "#012F47",
                                color = "#012F47") +
                        geom_raster(
                                data = relief_200m,
                                inherit.aes = FALSE,
                                aes(
                                        x = x,
                                        y = y,
                                        alpha = value
                                )
                        )
                
                saveRDS(basemap, paste0(here::here("figures/rds"), "/", island_name, "_land_200m_basemap.rds"))
                
                ## make the 200m moisture zones basemap
                moisture_basemap <- make_moisture_map(moisture_geo = moisture_geo,
                                                      wetlands_geo = wetlands_geo,
                                                      subtitle = island_name_upperCase) +
                        geom_sf(data = wetlands_geo2,
                                fill = "#012F47",
                                color = "#012F47") +
                        geom_raster(
                                data = relief_200m,
                                inherit.aes = FALSE,
                                aes(
                                        x = x,
                                        y = y,
                                        alpha = value
                                )
                        )
                
                saveRDS(moisture_basemap, paste0(here::here("figures/rds"), "/", island_name, "_moisture_200m_basemap.rds"))
                
                ## make the 200m solar radiation basemap
                solrad_basemap <- make_solrad_map(solrad_geo = solrad_geo,
                                                  wetlands_geo = wetlands_geo,
                                                  subtitle = island_name_upperCase) +
                        geom_sf(data = wetlands_geo2,
                                fill = "#012F47",
                                color = "#012F47") +
                        geom_raster(
                                data = relief_200m,
                                inherit.aes = FALSE,
                                aes(
                                        x = x,
                                        y = y,
                                        alpha = value
                                )
                        )
                
                saveRDS(solrad_basemap, paste0(here::here("figures/rds"), "/", island_name,  "_solrad_200m_basemap.rds"))
                
                ## make the 200m agriculture basemap
                agriculture_basemap <- make_agriculture_map(agriculture_geo = agriculture_geo,
                                                       wetlands_geo = wetlands_geo,
                                                       subtitle = NULL) +
                        geom_sf(data = wetlands_geo2,
                                fill = "#012F47",
                                color = "#012F47") +
                        geom_raster(
                                data = relief_200m,
                                inherit.aes = FALSE,
                                aes(
                                        x = x,
                                        y = y,
                                        alpha = value
                                )
                        )
                saveRDS(agriculture_basemap, paste0(here::here("figures/rds"), "/", island_name, "_agriculture_200m_basemap.rds"))
                
        } else {
                ## make the island basemap with no relief
                basemap <- make_island_map(coast_geo = coast_geo,
                                           wetlands_geo = wetlands_geo,
                                           subtitle = island_name_upperCase) +
                        
                
                saveRDS(basemap, paste0(here::here("figures/rds"), "/", island_name, "_land_basemap_noRelief.rds"))
                
                ## make the moisture basemap with no relief
                moisture_basemap <- make_moisture_map(moisture_geo = moisture_geo,
                                                      wetlands_geo = wetlands_geo,
                                                      subtitle = island_name_upperCase)
                
                saveRDS(moisture_basemap, paste0(here::here("figures/rds"), "/", island_name, "_moisture_basemap_noRelief.rds"))
                
                ## make the solar radiation basemap with no relief
                solrad_basemap <- make_solrad_map(solrad_geo = solrad_geo,
                                                  wetlands_geo = wetlands_geo,
                                                  subtitle = island_name_upperCase)
                
                saveRDS(solrad_basemap, paste0(here::here("figures/rds"), "/", island_name, "_solrad_basemap_noRelief.rds"))
                
                ## make the agriculture basemap with no relief
                agriculture_basemap <- make_agriculture_map(agriculture_geo = agriculture_geo,
                                                            wetlands_geo = wetlands_geo,
                                                            subtitle = island_name_upperCase)
                
                saveRDS(agriculture_basemap, paste0(here::here("figures/rds"), "/", island_name, "_agriculture_basemap_noRelief.rds"))
                
                ## make the 100m island basemap
                basemap <- make_island_map(coast_geo = coast_geo,
                                           wetlands_geo = wetlands_geo,
                                           subtitle = island_name_upperCase) +
                        geom_raster(
                                data = relief_100m,
                                inherit.aes = FALSE,
                                aes(
                                        x = x,
                                        y = y,
                                        alpha = value
                                )
                        )
                saveRDS(basemap, paste0(here::here("figures/rds"), "/", island_name, "_land_100m_basemap.rds"))
                
                ## make the 100m moisture basemap
                moisture_basemap <- make_moisture_map(moisture_geo = moisture_geo,
                                                      wetlands_geo = wetlands_geo,
                                                      subtitle = island_name_upperCase) +
                        geom_raster(
                                data = relief_100m,
                                inherit.aes = FALSE,
                                aes(
                                        x = x,
                                        y = y,
                                        alpha = value
                                )
                        )
                saveRDS(moisture_basemap, paste0(here::here("figures/rds"), "/", island_name, "_moisture_100m_basemap.rds"))
                
                ## make the 100m solar radiation basemap
                solrad_basemap <- make_solrad_map(solrad_geo = solrad_geo,
                                                  wetlands_geo = wetlands_geo,
                                                  subtitle = island_name_upperCase) +
                        geom_raster(
                                data = relief_100m,
                                inherit.aes = FALSE,
                                aes(
                                        x = x,
                                        y = y,
                                        alpha = value
                                )
                        )
                saveRDS(solrad_basemap, paste0(here::here("figures/rds"), "/", island_name, "_solrad_100m_basemap.rds"))
                
                ## make the 100m agriculture basemap
                agriculture_basemap <- make_agriculture_map(agriculture_geo = agriculture_geo,
                                                       wetlands_geo = wetlands_geo,
                                                       subtitle = island_name_upperCase) +
                        geom_raster(
                                data = relief_100m,
                                inherit.aes = FALSE,
                                aes(
                                        x = x,
                                        y = y,
                                        alpha = value
                                )
                        )
                saveRDS(agriculture_basemap, paste0(here::here("figures/rds"), "/", island_name, "_agriculture_100m_basemap.rds"))
                
                ## make the 200m island basemap
                basemap <- make_island_map(coast_geo = coast_geo,
                                           wetlands_geo = wetlands_geo,
                                           subtitle = island_name_upperCase) +
                        geom_raster(
                                data = relief_200m,
                                inherit.aes = FALSE,
                                aes(
                                        x = x,
                                        y = y,
                                        alpha = value
                                )
                        )
                
                saveRDS(basemap, paste0(here::here("figures/rds"), "/", island_name, "_land_200m_basemap.rds"))
                
                ## make the 200m moisture zones basemap
                moisture_basemap <- make_moisture_map(moisture_geo = moisture_geo,
                                                      wetlands_geo = wetlands_geo,
                                                      subtitle = island_name_upperCase) +
                        geom_raster(
                                data = relief_200m,
                                inherit.aes = FALSE,
                                aes(
                                        x = x,
                                        y = y,
                                        alpha = value
                                )
                        )
                saveRDS(moisture_basemap, paste0(here::here("figures/rds"), "/", island_name, "_moisture_200m_basemap.rds"))
                
                ## make the 200m solar radiation basemap
                solrad_basemap <- make_solrad_map(solrad_geo = solrad_geo,
                                                  wetlands_geo = wetlands_geo,
                                                  subtitle = island_name_upperCase) +
                        geom_raster(
                                data = relief_200m,
                                inherit.aes = FALSE,
                                aes(
                                        x = x,
                                        y = y,
                                        alpha = value
                                )
                        )
                saveRDS(solrad_basemap, paste0(here::here("figures/rds"), "/", island_name,  "_solrad_200m_basemap.rds"))
                
                ## make the 200m agriculture basemap
                agriculture_basemap <- make_agriculture_map(agriculture_geo = agriculture_geo,
                                                       wetlands_geo = wetlands_geo,
                                                       subtitle = island_name_upperCase) +
                        geom_raster(
                                data = relief_200m,
                                inherit.aes = FALSE,
                                aes(
                                        x = x,
                                        y = y,
                                        alpha = value
                                )
                        )
                saveRDS(agriculture_basemap, paste0(here::here("figures/rds"), "/", island_name, "_agriculture_200m_basemap.rds"))
        }
        
        
}