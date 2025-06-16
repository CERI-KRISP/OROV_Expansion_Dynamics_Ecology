# install.packages("devtools"); library(devtools)
# install_github("sdellicour/seraphim/unix_OS") # for Unix systems
## install_github("sdellicour/seraphim/windows") # for Windows systems
# install.packages("diagram")

library(diagram)
require(seraphim)
library(ggplot2)
library(ggnewscale)
library(rnaturalearth)
library(cowplot)

# 1. Loading MCC dispersal locations
mcc_tab=read.csv("Dispersal_locations/CladeS_constant_round2_cluster_MCC.csv") ### Change this based on which Clade plots are being made for. 

mcc_tab$startLon<-round(mcc_tab$startLon, digits = 0) 
mcc_tab$startLat<-round(mcc_tab$startLat, digits = 0)
mcc_tab$endLon<-round(mcc_tab$endLon, digits = 0)
mcc_tab$endLat<-round(mcc_tab$endLat, digits = 0)

# 2. Loading environmental layers



pop_rast = raster("Geospatial/Brazil_population_crop_mask.asc")
names(pop_rast) = "pop_2000_scaled_log"

elevation_rast = raster("Geospatial/Brazil_elevation_crop_mask.asc")
names(elevation_rast) = "elevation"

herbaceous_vegetation_rast = raster("Geospatial/Brazil_herbaceous_vegetation_earthenv.asc")
names(herbaceous_vegetation_rast) = "herbaceous_vegetation"

cultivated_managed_vegetation_rast = raster("Geospatial/Brazil_cultivated_managed_vegetation_earthenv.asc")
names(cultivated_managed_vegetation_rast) = "cultivated_managed_vegetation"

flooded_vegetation_rast = raster("Geospatial/Brazil_regularly_flooded_vegetation_earthenv.asc")
names(flooded_vegetation_rast) = "regularly_flooded_vegetation"

cocoa_rast = raster("Geospatial/Brazil_cocoa_harvested_area.asc")
names(cocoa_rast) = "cocoa"

soy_rast = raster("Geospatial/Brazil_soybean_harvested_area.asc")
names(soy_rast) = "soy"

cattle_rast = raster("Geospatial/Brazil_cattle_production.asc")
names(cattle_rast) = "cattle"

coffee_rast = raster("Geospatial/Brazil_coffee_harvested_area.asc")
names(coffee_rast) = "coffee"

maize_rast = raster("Geospatial/Brazil_maize_harvested_area.asc")
names(maize_rast) = "maize"

banana_rast = raster("Geospatial/Brazil_banana_harvested_area.asc")
names(banana_rast) = "banana"

sugarcane_rast = raster("Geospatial/Brazil_sugarcane_harvested_area.asc")
names(sugarcane_rast) = "sugarcane"

cassava_rast = raster("Geospatial/Brazil_cassava_harvested_area.asc")
names(cassava_rast) = "cassava"


culicoides_rast = raster("Geospatial/Brazil_culicoides_map.asc")
names(culicoides_rast) = "culicoides"



soil_moisture_AM = raster("Geospatial/Brazil_soil_moisture_AM.asc")


soil_moisture_PM = raster("Geospatial/Brazil_soil_moisture_PM.asc")



Brazil_forest_evergreen_broadleaf = raster("Geospatial/Brazil_evergreen_broadleaf_trees_earthenv.asc")
names(Brazil_forest_evergreen_broadleaf) = "evergreen_broadleaf"

Brazil_forest_decidious_broadleaf = raster("Geospatial/Brazil_deciduous_broadleaf_trees_earthenv.asc")
names(Brazil_forest_decidious_broadleaf) = "deciduous_broadleaf"

Brazil_forest_mixed_trees = raster("Geospatial/Brazil_mixed_trees_earthenv.asc")
names(Brazil_forest_mixed_trees) = "mixed_trees"

Brazil_forest_loss = raster("Geospatial/Brazil_forest_loss_medium_res_completearea_2020to2023.asc")
names(Brazil_forest_loss) = "forest_loss"


Brazil_grasslands = raster("Geospatial/Brazil_grasslands_crop_mask.asc")

Brazil_shrublands = raster("Geospatial/Brazil_shrubs_earthenv.asc")
names(Brazil_shrublands) = "shrublands"

Brazil_savannas = raster("Geospatial/Brazil_savannas_crop_mask.asc")

Brazil_woodysavannas = raster("Geospatial/Brazil_woody_savannas_crop_mask.asc")

Brazil_cropland_naturalvegetation = raster("Geospatial/Brazil_cropland_natural_vegetation_crop_mask.asc")

Brazil_urban = raster("Geospatial/Brazil_urban_built_up_earthenv.asc")
names(Brazil_urban) = "urban"

Brazil_water = raster("Geospatial/Brazil_water_occurrence.asc")
names(Brazil_water) = "water"

Brazil_precipitation = raster("Geospatial/Brazil_precipitation_mean_2023_crop_mask.asc")

Brazil_croplands = raster("Geospatial/Brazil_Croplands_2019.asc")

Brazil_temp = raster("Geospatial/Brazil_annual_mean_temp_2020_crop_mask.asc")


orov_suitability_cumulative = raster("Geospatial/Full_map_2024.tiff")
names(orov_suitability_cumulative) = "orov_suitability_cumulative"


orov_suitability_pre2024=raster("Geospatial/preExpansion_map_1.tiff")
names(orov_suitability_pre2024) = "orov_suitability_pre2024"

orov_suitability_premid2023 = raster("Geospatial/preExpansion_map_pre2024.tiff")
names(orov_suitability_premid2023) = "orov_suitability_premid2023"

# 3. Visualising spread maps and time series 
Brazil_forest_evergreen_broadleaf_df<-terra::as.data.frame(Brazil_forest_evergreen_broadleaf, xy = T, na.rm=TRUE)
pop_rast_df<-terra::as.data.frame(pop_rast, xy = T, na.rm=TRUE)
Brazil_urban_df<-terra::as.data.frame(Brazil_urban, xy = T, na.rm=TRUE)
banana_df<-terra::as.data.frame(banana_rast, xy = T, na.rm=TRUE)
cocoa_df<-terra::as.data.frame(cocoa_rast, xy = T, na.rm=TRUE)
elevation_df<-terra::as.data.frame(elevation_rast, xy = T, na.rm=TRUE)
water_df<-terra::as.data.frame(Brazil_water, xy = T, na.rm=TRUE)
temp_df<-terra::as.data.frame(Brazil_temp, xy = T, na.rm=TRUE)
orov_suitability_df<-terra::as.data.frame(orov_suitability_cumulative, xy = T, na.rm=TRUE)
orov_suitability_pre2024_df<-terra::as.data.frame(orov_suitability_pre2024, xy = T, na.rm=TRUE)
orov_suitability_premid2023_df<-terra::as.data.frame(orov_suitability_premid2023, xy = T, na.rm=TRUE)
culicoides_df<-terra::as.data.frame(culicoides_rast, xy = T, na.rm=TRUE)
#culex_df<-terra::as.data.frame(culex_rast, xy = T, na.rm=TRUE)
soil_moisture_AM_df<-terra::as.data.frame(soil_moisture_AM, xy = T, na.rm=TRUE)
soil_moisture_PM_df<-terra::as.data.frame(soil_moisture_PM, xy = T, na.rm=TRUE)



map1 <- ne_countries(type = "countries", country = "Brazil",
                     scale = "medium", returnclass = "sf")
map2 <- rnaturalearth::ne_states(country = "Brazil",
                                 returnclass = "sf")

evergreen_dispersal<-ggplot()+
  theme_void()+
  theme(legend.title=element_text(size=8),legend.text=element_text(size=7),
        legend.key.size = unit(0.1, "lines"),legend.box = "vertical")+
  theme(legend.position = c(0.25,0.32),legend.direction='vertical',
        legend.spacing = unit(0.1, 'cm'))+
  geom_raster(Brazil_forest_evergreen_broadleaf_df,mapping=aes(x,y,fill = evergreen_broadleaf),alpha=0.9,interpolate=T)+
  scale_fill_distiller(palette = 'YlGn', direction=1,name='Environmental values')+
  geom_sf(data=map1,mapping=aes(),fill=NA)+
  #geom_sf(data=map2,mapping=aes(),fill=NA,colour='white')+
  
  # new_scale_fill()+
  #scale_fill_manual(values=c('cadetblue3'))+
  scale_colour_manual(values=c('#264653','#ffb703','#d6ccc2'),name='Dispersal timing')+
  geom_count(data=subset(mcc_tab,endYear>=2024),
             shape=21,
             aes(startLon,startLat,colour='2024',shape='Local'))+
  geom_count(data=subset(mcc_tab,endYear>=2023.5 & endYear<2024),
             shape=21,
             aes(startLon,startLat,colour='After mid-2023',shape='Local'))+
  geom_count(data=subset(mcc_tab,endYear<2023.5),
             shape=21,
             aes(startLon,startLat,colour='Before mid-2023',shape='Local'))+
  #geom_count(data=subset(subset(mcc_tab_2024,startLon!=endLon),startLat==endLat),
   #         # shape=23,
   #          aes(startLon,startLat,colour='2024',shape='Inter-regional'))+
  #geom_count(data=subset(subset(mcc_tab_pre2024,startLon!=endLon),startLat==endLat),
            # shape=23,
   #          aes(startLon,startLat,colour='Before 2024',shape='Inter-regional'))+
  scale_size(range = c(1, 10),
             breaks=c(10,100),labels=c(10,100),name='Dispersal count')+
  #scale_shape_manual(values=c(23,21),name='Dispersal location')+
  coord_sf()+
  #guides(shape = guide_legend(override.aes = list(size = 0.5)))+
  guides(fill = guide_legend(override.aes = list(size = 0.2),ncol=5,order=3))+
  guides(colour = guide_legend(ncol=1,order=2))+
  guides(size = guide_legend(ncol=2,order=1))+
  ggtitle("Evergreen broadleaf forest cover")+
  theme(plot.title = element_text(hjust=0.5,size=10))
evergreen_dispersal

pop_dispersal<-ggplot()+
  theme_void()+
  theme(legend.title=element_text(size=8),legend.text=element_text(size=7),
        legend.key.size = unit(0.1, "lines"),legend.box = "vertical")+
  theme(legend.position = c(0.25,0.3),legend.direction='vertical',
        legend.spacing = unit(0.1, 'cm'))+
  geom_raster(pop_rast_df,mapping=aes(x,y,fill = pop_2000_scaled_log),alpha=0.9,interpolate=T)+
  scale_fill_distiller(palette = 'Purples', direction=1,name='Environmental values')+
  geom_sf(data=map1,mapping=aes(),fill=NA)+
  #geom_sf(data=map2,mapping=aes(),fill=NA,colour='white')+
  
  # new_scale_fill()+
  #scale_fill_manual(values=c('cadetblue3'))+
  scale_colour_manual(values=c('#264653','#ffb703','#d6ccc2'),name='Dispersal timing')+
  geom_count(data=subset(mcc_tab,endYear>=2024),
             shape=21,
             aes(startLon,startLat,colour='2024',shape='Local'))+
  geom_count(data=subset(mcc_tab,endYear>=2023.5 & endYear<2024),
             shape=21,
             aes(startLon,startLat,colour='After mid-2023',shape='Local'))+
  geom_count(data=subset(mcc_tab,endYear<2023.5),
             shape=21,
             aes(startLon,startLat,colour='Before mid-2023',shape='Local'))+
  #geom_count(data=subset(subset(mcc_tab_2024,startLon!=endLon),startLat==endLat),
  #         # shape=23,
  #          aes(startLon,startLat,colour='2024',shape='Inter-regional'))+
  #geom_count(data=subset(subset(mcc_tab_pre2024,startLon!=endLon),startLat==endLat),
  # shape=23,
  #          aes(startLon,startLat,colour='Before 2024',shape='Inter-regional'))+
  scale_size(range = c(1, 10),
             breaks=c(10,100),labels=c(10,100),name='Dispersal count')+
  #scale_shape_manual(values=c(23,21),name='Dispersal location')+
  coord_sf()+
  #guides(shape = guide_legend(override.aes = list(size = 0.5)))+
  guides(fill = guide_legend(override.aes = list(size = 0.2),ncol=3,order=3))+
  guides(colour = guide_legend(ncol=1,order=2))+
  guides(size = guide_legend(ncol=2,order=1))+
  ggtitle("Human population density")+
  theme(plot.title = element_text(hjust=0.5,size=10))
pop_dispersal


urban_dispersal<-ggplot()+
  theme_void()+
  theme(legend.title=element_text(size=8),legend.text=element_text(size=7),
        legend.key.size = unit(0.1, "lines"),legend.box = "vertical")+
  theme(legend.position = c(0.25,0.3),legend.direction='vertical',
        legend.spacing = unit(0.1, 'cm'))+
  geom_raster(Brazil_urban_df,mapping=aes(x,y,fill = urban),alpha=0.9,interpolate=T)+
  scale_fill_distiller(palette = 'Purples', direction=1,name='Environmental values')+
  geom_sf(data=map1,mapping=aes(),fill=NA)+
  #geom_sf(data=map2,mapping=aes(),fill=NA,colour='white')+
  
  # new_scale_fill()+
  #scale_fill_manual(values=c('cadetblue3'))+
  scale_colour_manual(values=c('#264653','#ffb703','#d6ccc2'),name='Dispersal timing')+
  geom_count(data=subset(mcc_tab,endYear>=2024),
             shape=21,
             aes(startLon,startLat,colour='2024',shape='Local'))+
  geom_count(data=subset(mcc_tab,endYear>=2023.5 & endYear<2024),
             shape=21,
             aes(startLon,startLat,colour='After mid-2023',shape='Local'))+
  geom_count(data=subset(mcc_tab,endYear<2023.5),
             shape=21,
             aes(startLon,startLat,colour='Before mid-2023',shape='Local'))+
  #geom_count(data=subset(subset(mcc_tab_2024,startLon!=endLon),startLat==endLat),
  #         # shape=23,
  #          aes(startLon,startLat,colour='2024',shape='Inter-regional'))+
  #geom_count(data=subset(subset(mcc_tab_pre2024,startLon!=endLon),startLat==endLat),
  # shape=23,
  #          aes(startLon,startLat,colour='Before 2024',shape='Inter-regional'))+
  scale_size(range = c(1, 10),
             breaks=c(10,100),labels=c(10,100),name='Dispersal count')+
  #scale_shape_manual(values=c(23,21),name='Dispersal location')+
  coord_sf()+
  #guides(shape = guide_legend(override.aes = list(size = 0.5)))+
  guides(fill = guide_legend(override.aes = list(size = 0.2),ncol=3,order=3))+
  guides(colour = guide_legend(ncol=1,order=2))+
  guides(size = guide_legend(ncol=2,order=1))+
  ggtitle("Urban extent")+
  theme(plot.title = element_text(hjust=0.5,size=10))
urban_dispersal

culicoides_dispersal<-ggplot()+
  theme_void()+
  theme(legend.title=element_text(size=8),legend.text=element_text(size=7),
        legend.key.size = unit(0.1, "lines"),legend.box = "vertical")+
  theme(legend.position = c(0.25,0.32),legend.direction='vertical',
        legend.spacing = unit(0.1, 'cm'))+
  geom_raster(culicoides_df,mapping=aes(x,y,fill = culicoides),alpha=0.9,interpolate=T)+
  scale_fill_distiller(palette = 'RdYlBu', direction=-1, na.value = 'white',name="Environmental values")+
  geom_sf(data=map1,mapping=aes(),fill=NA)+
  #geom_sf(data=map2,mapping=aes(),fill=NA,colour='white')+
  
  # new_scale_fill()+
  #scale_fill_manual(values=c('cadetblue3'))+
  scale_colour_manual(values=c('#264653','#ffb703','#d6ccc2'),name='Dispersal timing')+
  geom_count(data=subset(mcc_tab,endYear>=2024),
             shape=21,
             aes(startLon,startLat,colour='2024',shape='Local'))+
  geom_count(data=subset(mcc_tab,endYear>=2023.5 & endYear<2024),
             shape=21,
             aes(startLon,startLat,colour='After mid-2023',shape='Local'))+
  geom_count(data=subset(mcc_tab,endYear<2023.5),
             shape=21,
             aes(startLon,startLat,colour='Before mid-2023',shape='Local'))+
  #geom_count(data=subset(subset(mcc_tab_2024,startLon!=endLon),startLat==endLat),
  #         # shape=23,
  #          aes(startLon,startLat,colour='2024',shape='Inter-regional'))+
  #geom_count(data=subset(subset(mcc_tab_pre2024,startLon!=endLon),startLat==endLat),
  # shape=23,
  #          aes(startLon,startLat,colour='Before 2024',shape='Inter-regional'))+
  scale_size(range = c(1, 10),
             breaks=c(10,100),labels=c(10,100),name='Dispersal count')+
  #scale_shape_manual(values=c(23,21),name='Dispersal location')+
  coord_sf()+
  #guides(shape = guide_legend(override.aes = list(size = 0.5)))+
  guides(fill = guide_legend(override.aes = list(size = 0.2),ncol=5,order=3))+
  guides(colour = guide_legend(ncol=1,order=2))+
  guides(size = guide_legend(ncol=2,order=1))+
  #ggtitle("Culicoides Paraensis ecological suitability")+
  ggtitle(expression(paste("", italic("Culicoides paraensis"), " ecological suitability")))+
  theme(plot.title = element_text(hjust=0.5,size=10))

culicoides_dispersal


banana_dispersal<-ggplot()+
  theme_void()+
  theme(legend.title=element_text(size=8),legend.text=element_text(size=7),
        legend.key.size = unit(0.1, "lines"),legend.box = "vertical")+
  theme(legend.position = c(0.25,0.32),legend.direction='vertical',
        legend.spacing = unit(0.1, 'cm'))+
  geom_raster(banana_df,mapping=aes(x,y,fill = banana),alpha=0.9,interpolate=T)+
  scale_fill_distiller(palette = 'YlOrBr', direction=1, trans='log10',na.value = 'white', name="Environmental values")+
  geom_sf(data=map1,mapping=aes(),fill=NA)+
  #geom_sf(data=map2,mapping=aes(),fill=NA,colour='white')+
  
  # new_scale_fill()+
  #scale_fill_manual(values=c('cadetblue3'))+
  scale_colour_manual(values=c('#264653','#ffb703','#d6ccc2'),name='Dispersal timing')+
  geom_count(data=subset(mcc_tab,endYear>=2024),
             shape=21,
             aes(startLon,startLat,colour='2024',shape='Local'))+
  geom_count(data=subset(mcc_tab,endYear>=2023.5 & endYear<2024),
             shape=21,
             aes(startLon,startLat,colour='After mid-2023',shape='Local'))+
  geom_count(data=subset(mcc_tab,endYear<2023.5),
             shape=21,
             aes(startLon,startLat,colour='Before mid-2023',shape='Local'))+
  #geom_count(data=subset(subset(mcc_tab_2024,startLon!=endLon),startLat==endLat),
  #         # shape=23,
  #          aes(startLon,startLat,colour='2024',shape='Inter-regional'))+
  #geom_count(data=subset(subset(mcc_tab_pre2024,startLon!=endLon),startLat==endLat),
  # shape=23,
  #          aes(startLon,startLat,colour='Before 2024',shape='Inter-regional'))+
  scale_size(range = c(1, 10),
             breaks=c(10,100),labels=c(10,100),name='Dispersal count')+
  #scale_shape_manual(values=c(23,21),name='Dispersal location')+
  coord_sf()+
  #guides(shape = guide_legend(override.aes = list(size = 0.5)))+
  guides(fill = guide_legend(override.aes = list(size = 0.2),ncol=5,order=3))+
  guides(colour = guide_legend(ncol=1,order=2))+
  guides(size = guide_legend(ncol=2,order=1))+
  ggtitle("Banana harvested area")+
  theme(plot.title = element_text(hjust=0.5,size=10))



banana_dispersal


cocoa_dispersal<-ggplot()+
  theme_void()+
  theme(legend.title=element_text(size=8),legend.text=element_text(size=7),
        legend.key.size = unit(0.1, "lines"),legend.box = "vertical")+
  theme(legend.position = c(0.25,0.32),legend.direction='vertical',
        legend.spacing = unit(0.1, 'cm'))+
  geom_raster(cocoa_df,mapping=aes(x,y,fill = cocoa),alpha=0.9,interpolate=T)+
  scale_fill_distiller(palette = 'YlOrBr', direction=1, trans='log10',
                       breaks=c(1,10,100,1000),labels=c(1,10,100,1000),na.value = 'white', name="Environmental values")+
  geom_sf(data=map1,mapping=aes(),fill=NA)+
  #geom_sf(data=map2,mapping=aes(),fill=NA,colour='white')+
  
  # new_scale_fill()+
  #scale_fill_manual(values=c('cadetblue3'))+
  scale_colour_manual(values=c('#264653','#ffb703','#d6ccc2'),name='Dispersal timing')+
  geom_count(data=subset(mcc_tab,endYear>=2024),
             shape=21,
             aes(startLon,startLat,colour='2024',shape='Local'))+
  geom_count(data=subset(mcc_tab,endYear>=2023.5 & endYear<2024),
             shape=21,
             aes(startLon,startLat,colour='After mid-2023',shape='Local'))+
  geom_count(data=subset(mcc_tab,endYear<2023.5),
             shape=21,
             aes(startLon,startLat,colour='Before mid-2023',shape='Local'))+
  #geom_count(data=subset(subset(mcc_tab_2024,startLon!=endLon),startLat==endLat),
  #         # shape=23,
  #          aes(startLon,startLat,colour='2024',shape='Inter-regional'))+
  #geom_count(data=subset(subset(mcc_tab_pre2024,startLon!=endLon),startLat==endLat),
  # shape=23,
  #          aes(startLon,startLat,colour='Before 2024',shape='Inter-regional'))+
  scale_size(range = c(1, 10),
             breaks=c(10,100),labels=c(10,100),name='Dispersal count')+
  #scale_shape_manual(values=c(23,21),name='Dispersal location')+
  coord_sf()+
  #guides(shape = guide_legend(override.aes = list(size = 0.5)))+
  guides(fill = guide_legend(override.aes = list(size = 0.2),ncol=5,order=3))+
  guides(colour = guide_legend(ncol=1,order=2))+
  guides(size = guide_legend(ncol=2,order=1))+
  ggtitle("Cocoa harvested area")+
  theme(plot.title = element_text(hjust=0.5,size=10))

cocoa_dispersal


temp_dispersal<-ggplot()+
  theme_void()+
  theme(legend.title=element_text(size=8),legend.text=element_text(size=7),
        legend.key.size = unit(0.1, "lines"),legend.box = "vertical")+
  theme(legend.position = c(0.25,0.32),legend.direction='vertical',
        legend.spacing = unit(0.1, 'cm'))+
  
  geom_raster(temp_df,mapping=aes(x,y,fill = Brazil_annual_mean_temp_2020_crop_mask),alpha=0.9,interpolate=T)+
  scale_fill_distiller(palette = 'Reds', direction=1, na.value = 'white',name="Environmental values")+
  geom_sf(data=map1,mapping=aes(),fill=NA)+
  #geom_sf(data=map2,mapping=aes(),fill=NA,colour='white')+
  
  # new_scale_fill()+
  #scale_fill_manual(values=c('cadetblue3'))+
  scale_colour_manual(values=c('#264653','#ffb703','#d6ccc2'),name='Dispersal timing')+
  geom_count(data=subset(mcc_tab,endYear>=2024),
             shape=21,
             aes(startLon,startLat,colour='2024',shape='Local'))+
  geom_count(data=subset(mcc_tab,endYear>=2023.5 & endYear<2024),
             shape=21,
             aes(startLon,startLat,colour='After mid-2023',shape='Local'))+
  geom_count(data=subset(mcc_tab,endYear<2023.5),
             shape=21,
             aes(startLon,startLat,colour='Before mid-2023',shape='Local'))+
  #geom_count(data=subset(subset(mcc_tab_2024,startLon!=endLon),startLat==endLat),
  #         # shape=23,
  #          aes(startLon,startLat,colour='2024',shape='Inter-regional'))+
  #geom_count(data=subset(subset(mcc_tab_pre2024,startLon!=endLon),startLat==endLat),
  # shape=23,
  #          aes(startLon,startLat,colour='Before 2024',shape='Inter-regional'))+
  scale_size(range = c(1, 10),
             breaks=c(10,100),labels=c(10,100),name='Dispersal count')+
  #scale_shape_manual(values=c(23,21),name='Dispersal location')+
  coord_sf()+
  #guides(shape = guide_legend(override.aes = list(size = 0.5)))+
  guides(fill = guide_legend(override.aes = list(size = 0.2),ncol=5,order=3))+
  guides(colour = guide_legend(ncol=1,order=2))+
  guides(size = guide_legend(ncol=2,order=1))+
  ggtitle("Mean annual temperature")+
  theme(plot.title = element_text(hjust=0.5,size=10))


temp_dispersal



##plot spread values

cocoa_median_M<-read.csv("SpreadValues/M/cocoa_median_cocoa.csv")
cocoa_95HPD_M<-read.csv("SpreadValues/M/cocoa_95%HPD_cocoa.csv")

banana_median_M<-read.csv("SpreadValues/M/banana_median_banana.csv")
banana_95HPD_M<-read.csv("SpreadValues/M/banana_95%HPD_banana.csv")


temp_median_M<-read.csv("SpreadValues/M/Brazil_annual_mean_temp_2020_crop_mask_median_Brazil_annual_mean_temp_2020_crop_mask.csv")
temp_95HPD_M<-read.csv("SpreadValues/M/Brazil_annual_mean_temp_2020_crop_mask_95%HPD_Brazil_annual_mean_temp_2020_crop_mask.csv")


pop_median_M<-read.csv("SpreadValues/M/pop_2000_scaled_log_median_pop_2000_scaled_log.csv")
pop_95HPD_M<-read.csv("SpreadValues/M/pop_2000_scaled_log_95%HPD_pop_2000_scaled_log.csv")

urban_median_M<-read.csv("SpreadValues/M/urban_median_urban.csv")
urban_95HPD_M<-read.csv("SpreadValues/M/urban_95%HPD_urban.csv")


culicoides_median_M<-read.csv("SpreadValues/S/culicoides_median_culicoides.csv")
culicoides_95HPD_M<-read.csv("SpreadValues/S/culicoides_95%HPD_culicoides.csv")


evergreen_median_M<-read.csv("SpreadValues/M/evergreen_broadleaf_median_evergreen_broadleaf.csv")
evergreen_95HPD_M<-read.csv("SpreadValues/M/evergreen_broadleaf_95%HPD_evergreen_broadleaf.csv")

orov_pre2024_median_M<-read.csv("SpreadValues/M/orov_suitability_pre2024_median_orov_suitability_pre2024.csv")
orov_pre2024_95HPD_M<-read.csv("SpreadValues/M/orov_suitability_pre2024_95%HPD_orov_suitability_pre2024.csv")

orov_premid2023_median_M<-read.csv("SpreadValues/M/orov_suitability_premid2023_median_orov_suitability_premid2023.csv")
orov_premid2023_95HPD_M<-read.csv("SpreadValues/M/orov_suitability_premid2023_95%HPD_orov_suitability_premid2023.csv")

orov_cumulative_median_M<-read.csv("SpreadValues/M/orov_suitability_cumulative_median_orov_suitability_cumulative.csv")
orov_cumulative_95HPD_M<-read.csv("SpreadValues/M/orov_suitability_cumulative_95%HPD_orov_suitability_cumulative.csv")



orov_pre2024_values<-ggplot()+
  theme_minimal()+
  theme(axis.text=element_text(size=7),axis.title=element_text(size=8))+
  
  geom_ribbon(data=orov_pre2024_95HPD_M,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='turquoise4')+
  geom_line(data=orov_pre2024_median_M,
            aes(x=time,y=orov_suitability_pre2024-1),
            colour='turquoise4')+
  ylab("Orov suitability")+
  xlab("Date")+
  scale_x_continuous(limits=c(2022,2024.5),breaks=c(2022,2023,2024),labels=c(2022,2023,2024))
orov_pre2024_values


orov_premid2023_values<-ggplot()+
  theme_minimal()+
  theme(axis.text=element_text(size=7),axis.title=element_text(size=8))+
  
  geom_ribbon(data=orov_premid2023_95HPD_M,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='turquoise4')+
  geom_line(data=orov_premid2023_median_M,
            aes(x=time,y=orov_suitability_premid2023-1),
            colour='turquoise4')+
  ylab("Orov suitability")+
  xlab("Date")+
  scale_x_continuous(limits=c(2022,2024.5),breaks=c(2022,2023,2024),labels=c(2022,2023,2024))
orov_premid2023_values

orov_cumulative_values<-ggplot()+
  theme_minimal()+
  theme(axis.text=element_text(size=7),axis.title=element_text(size=8))+
  
  geom_ribbon(data=orov_cumulative_95HPD_M,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='turquoise4')+
  geom_line(data=orov_cumulative_median_M,
            aes(x=time,y=orov_suitability_cumulative-1),
            colour='turquoise4')+
  ylab("Environmental values")+
  xlab("Date")+
  scale_x_continuous(limits=c(2022,2024.5),breaks=c(2022,2023,2024),labels=c(2022,2023,2024))+
  geom_vline(xintercept=2023.5, colour='grey50',linetype='dashed')+
  geom_vline(xintercept=2024.0, colour='grey80',linetype='dashed')
orov_cumulative_values


temp_spread_values<-ggplot()+
  theme_minimal()+
  theme(axis.text=element_text(size=7),axis.title=element_text(size=8))+
  
  geom_ribbon(data=temp_95HPD_M,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='red3')+
  geom_line(data=temp_median_M,
            aes(x=time,y=Brazil_annual_mean_temp_2020_crop_mask-1),
            colour='red3')+
  ylab("Environmental values")+
  xlab("Date")+
  scale_x_continuous(limits=c(2022,2024.5),breaks=c(2022,2023,2024),labels=c(2022,2023,2024))+
  geom_vline(xintercept=2023.5, colour='grey50',linetype='dashed')+
  geom_vline(xintercept=2024.0, colour='grey80',linetype='dashed')
temp_spread_values


cocoa_spread_values<-ggplot()+
  theme_minimal()+
  theme(axis.text=element_text(size=7),axis.title=element_text(size=8))+
  
  geom_ribbon(data=cocoa_95HPD_M,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='tomato3')+
  geom_line(data=cocoa_median_M,
            aes(x=time,y=cocoa-1),
            colour='tomato3')+
  ylab("Environmental values")+
  xlab("Date")+
  scale_y_continuous(trans='log10',breaks=c(1,10,100),labels=c(1,10,100))+
  scale_x_continuous(limits=c(2022,2024.5),breaks=c(2022,2023,2024),labels=c(2022,2023,2024))+
  geom_vline(xintercept=2023.5, colour='grey50',linetype='dashed')+
  geom_vline(xintercept=2024.0, colour='grey80',linetype='dashed')
cocoa_spread_values

banana_spread_values<-ggplot()+
  theme_minimal()+
  theme(axis.text=element_text(size=7),axis.title=element_text(size=8))+
  
  geom_ribbon(data=banana_95HPD_M,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='orange3')+
  geom_line(data=banana_median_M,
            aes(x=time,y=banana-1),
            colour='orange3')+
  ylab("Environmental values")+
  xlab("Date")+
  scale_y_continuous(trans='log10',breaks=c(1,10,100),labels=c(1,10,100))+
  scale_x_continuous(limits=c(2022,2024.5),breaks=c(2022,2023,2024),labels=c(2022,2023,2024))+
  geom_vline(xintercept=2023.5, colour='grey50',linetype='dashed')+
  geom_vline(xintercept=2024.0, colour='grey80',linetype='dashed')
banana_spread_values

pop_spread_values<-ggplot()+
  theme_minimal()+  
  theme(axis.text=element_text(size=7),axis.title=element_text(size=8))+

  geom_ribbon(data=pop_95HPD_M,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='Purple4')+
  geom_line(data=pop_median_M,
            aes(x=time,y=pop_2000_scaled_log-1),
            colour='Purple4')+
  ylab("Environmental values")+
  xlab("Date")+
  scale_x_continuous(limits=c(2022,2024.5),breaks=c(2022,2023,2024),labels=c(2022,2023,2024))+
  geom_vline(xintercept=2023.5, colour='grey50',linetype='dashed')+
  geom_vline(xintercept=2024.0, colour='grey80',linetype='dashed')
pop_spread_values


urban_spread_values<-ggplot()+
  theme_minimal()+  
  theme(axis.text=element_text(size=7),axis.title=element_text(size=8))+
  
  geom_ribbon(data=urban_95HPD_M,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='Purple4')+
  geom_line(data=urban_median_M,
            aes(x=time,y=urban-1),
            colour='Purple4')+
  ylab("Environmental values")+
  xlab("Date")+
  scale_x_continuous(limits=c(2022,2024.5),breaks=c(2022,2023,2024),labels=c(2022,2023,2024))+
  geom_vline(xintercept=2023.5, colour='grey50',linetype='dashed')+
  geom_vline(xintercept=2024.0, colour='grey80',linetype='dashed')
urban_spread_values

culicoides_spread_values<-ggplot()+
  theme_minimal()+
  theme(axis.text=element_text(size=7),axis.title=element_text(size=8))+
  geom_ribbon(data=culicoides_95HPD_M,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='goldenrod2')+
  geom_line(data=culicoides_median_M,
            aes(x=time,y=culicoides-1),
            colour='goldenrod2')+
  ylab("Environmental values")+
  xlab("Date")+
  scale_x_continuous(limits=c(2022,2024.5),breaks=c(2022,2023,2024),labels=c(2022,2023,2024))+
  geom_vline(xintercept=2023.5, colour='grey50',linetype='dashed')+
  geom_vline(xintercept=2024.0, colour='grey80',linetype='dashed')

culicoides_spread_values


evergreen_spread_values<-ggplot()+
  theme_minimal()+
  theme(axis.text=element_text(size=7),axis.title=element_text(size=8))+
  geom_ribbon(data=evergreen_95HPD_M,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='Green4')+
  geom_line(data=evergreen_median_M,
            aes(x=time,y=evergreen_broadleaf-1),
            colour='Green4')+
  ylab("Environmental values")+
  xlab("Date")+
  #xlim(2022,2024.5)+
  scale_x_continuous(limits=c(2022,2024.5),breaks=c(2022,2023,2024),labels=c(2022,2023,2024))+
  geom_vline(xintercept=2023.5, colour='grey50',linetype='dashed')+
  geom_vline(xintercept=2024.0, colour='grey80',linetype='dashed')
evergreen_spread_values


#the panels below are assembled in powerpoint to make the final Figure 2 (segment M), Figure S4 (segment L) and Figure S5 (segment S)

pop<-plot_grid(pop_dispersal,pop_spread_values,ncol=1,rel_heights = c(0.7,0.3))
pop
urban<-plot_grid(urban_dispersal,urban_spread_values,ncol=1,rel_heights = c(0.7,0.3))
urban
banana<-plot_grid(banana_dispersal,banana_spread_values,ncol=1,rel_heights = c(0.7,0.3))
banana
temp<-plot_grid(temp_dispersal,temp_spread_values,ncol=1,rel_heights = c(0.7,0.3))
temp
cocoa<-plot_grid(cocoa_dispersal,cocoa_spread_values,ncol=1,rel_heights = c(0.7,0.3))
cocoa
culicoides<-plot_grid(culicoides_dispersal,culicoides_spread_values,ncol=1,rel_heights = c(0.7,0.3))
culicoides
elevation<-plot_grid(elevation_dispersal,elevation_spread_values,ncol=1,rel_heights = c(0.7,0.3))
water<-plot_grid(water_dispersal,water_spread_values,ncol=1,rel_heights = c(0.7,0.3))
evergreen<-plot_grid(evergreen_dispersal,evergreen_spread_values,ncol=1,rel_heights = c(0.7,0.3))
evergreen
