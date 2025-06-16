library(raster)
library(seraphim) # optional
#setwd("")
library(rworldmap)
library(sf)


### This codes plots Fig S2


map1 <- ne_countries(type = "countries", country = "Brazil",
                     scale = "medium", returnclass = "sf")
map2 <- rnaturalearth::ne_states(country = "Brazil",
                                 returnclass = "sf")

borders=map1$geometry
borders2=map2$geometry

pop_rast = raster("Geospatial/Brazil_population_crop_mask.asc")
elevation_rast = raster("Geospatial/Brazil_elevation_crop_mask.asc")
herbaceous_vegetation_rast = raster("Geospatial/Brazil_herbaceous_vegetation_earthenv.asc")
cultivated_managed_vegetation_rast = raster("Geospatial/Brazil_cultivated_managed_vegetation_earthenv.asc")
regularly_flooded_vegetation_rast = raster("Geospatial/Brazil_regularly_flooded_vegetation_earthenv.asc")

#forestation_loss_2000_2023 = raster("Geospatial/Brazil_forest_loss_medium_res.asc")
forestation_loss_2020_2023 = raster("Geospatial/Brazil_forest_loss_medium_res_completearea_2020to2023.asc")
#forestation_soy_loss_total_rast = raster("Geospatial/Brazil_deforestation_soy_better_resolution_reclassified.asc")
culicoides_rast = mask(crop(raster("Geospatial/Brazil_culicoides_map.asc"),map1),map1)
Brazil_forest_evergreen_broadleaf = raster("Geospatial/Brazil_evergreen_broadleaf_trees_earthenv.asc")
Brazil_forest_decidious_broadleaf = raster("Geospatial/Brazil_deciduous_broadleaf_trees_earthenv.asc")
Brazil_forest_mixed_trees = raster("Geospatial/Brazil_mixed_trees_earthenv.asc")
Brazil_grasslands = raster("Geospatial/Brazil_grasslands_crop_mask.asc")
Brazil_shrublands = raster("Geospatial/Brazil_shrubs_earthenv.asc")
#Brazil_closedshrublands = raster("Geospatial/Brazil_closed_shrublands_crop_mask.asc")
Brazil_savannas = raster("Geospatial/Brazil_savannas_crop_mask.asc")
Brazil_woody_savannas = raster("Geospatial/Brazil_woody_savannas_crop_mask.asc")
Brazil_cropland_naturalvegetation = raster("Geospatial/Brazil_cropland_natural_vegetation_crop_mask.asc")
Brazil_urban = raster("Geospatial/Brazil_urban_built_up_earthenv.asc")
Brazil_water = raster("Geospatial/Brazil_water_occurrence.asc")
Brazil_precipitation = raster("Geospatial/Brazil_precipitation_mean_2023_crop_mask.asc")
Brazil_croplands = raster("Geospatial/Brazil_Croplands_2019.asc")
Brazil_temp = raster("Geospatial/Brazil_annual_mean_temp_2020_crop_mask.asc")
#Brazil_banana = raster("Geospatial/Brazil_Banana_Crop_Area_2015_crop_mask.asc")
Brazil_banana = raster("Geospatial/Brazil_banana_harvested_area.asc")
#Brazil_stimulants = raster("Geospatial/Brazil_stimulants_harvested_area.asc")
Brazil_sugarcane = raster("Geospatial/Brazil_sugarcane_harvested_area.asc")
Brazil_soybean = raster("Geospatial/Brazil_soybean_harvested_area.asc")
Brazil_cassava = raster("Geospatial/Brazil_cassava_harvested_area.asc")
Brazil_maize = raster("Geospatial/Brazil_maize_harvested_area.asc")
Brazil_cattle = raster("Geospatial/Brazil_cattle_production.asc")
Brazil_coffee = raster("Geospatial/Brazil_coffee_harvested_area.asc")
Brazil_cocoa = raster("Geospatial/Brazil_cocoa_harvested_area.asc")



#par(mfrow = c(1, 1))

cols1=colorRampPalette(brewer.pal(9,"YlOrRd"))(130)[16:115]
cols2=rev(colorRampPalette(brewer.pal(11,"RdYlGn"))(130))[16:115]
cols3=colorRampPalette(brewer.pal(9,"Greens"))(130)[16:115]
cols4=rev(colorRampPalette(brewer.pal(11,"RdBu"))(130))[16:115]
cols5=colorRampPalette(brewer.pal(9,"Purples"))(130)[16:115]
cols6=colorRampPalette(brewer.pal(9,"YlGn"))(130)[16:115]
cols7=colorRampPalette(brewer.pal(9,"BuGn"))(130)[16:115]
cols8=colorRampPalette(brewer.pal(9,"YlOrBr"))(130)[16:115]
cols9=colorRampPalette(brewer.pal(9,"PuBuGn"))(130)[16:115]
cols10=colorRampPalette(brewer.pal(9,"Blues"))(130)[16:115]
cols11=colorRampPalette(brewer.pal(9,"PuBu"))(130)[16:115]
cols12=rev(colorRampPalette(brewer.pal(9,"RdYlBu"))(130))[16:115]
cols13=colorRampPalette(brewer.pal(9,"Reds"))(130)[16:115]
cols14=colorRampPalette(c("#f6fff8","#84a98c","#edafb8","#c9184a"))(130)
cols15=colorRampPalette(brewer.pal(9,"YlGnBu"))(130)[16:115]

pdf('Supplementary_Figure_Covariates_16June2025.pdf',width=14, height=15,bg="white")

par(mar=c(0,2,0,5), oma=c(0,0,1,2), mgp=c(0,5,0), lwd=1, bty="o",
    mfcol=c(6,5))

plot(pop_rast, col=cols1, colNA="white", box=F, axes=F, legend=T,
     interpolate=F, useRaster=T,
     legend.width=2,legend.args = list(text = 'Population Density', side = 4, 
                                       font = 1, line = -1.5, cex = 1))
plot(borders2, border='snow1',add=TRUE, lwd=0.2, interior = FALSE, col = "transparent", box=F, axes=F)
plot(borders, add=TRUE, lwd=0.5, interior = FALSE, col = "transparent", box=F, axes=F)

plot(Brazil_urban, col=cols13, colNA="white", box=F, axes=F, legend=T,
     interpolate=F, useRaster=T,
     legend.width=2,legend.args = list(text = 'Urban Areas', side = 4, 
                                       font = 1, line = -1.5, cex = 1))
plot(borders2, border='snow1',add=TRUE, lwd=0.2, interior = FALSE, col = "transparent", box=F, axes=F)
plot(borders, add=TRUE, lwd=0.5, interior = FALSE, col = "transparent", box=F, axes=F)

plot(elevation_rast,col=cols4, colNA="white", box=F, axes=F, legend=T,
     interpolate=F, useRaster=T,
     legend.width=2,legend.args = list(text = 'Elevation', side = 4, 
                                       font = 1, line = -1.5, cex = 1))
plot(borders2, border='snow1',add=TRUE, lwd=0.2, interior = FALSE, col = "transparent", box=F, axes=F)
plot(borders, add=TRUE, lwd=0.5, interior = FALSE, col = "transparent", box=F, axes=F)

plot(herbaceous_vegetation_rast,col=cols3, colNA="white", box=F, axes=F, legend=T,
     interpolate=F, useRaster=T,
     legend.width=2,legend.args = list(text = 'Vegetation (Herbaceous)', side = 4, 
                                       font = 1, line = -1.5, cex = 1))
plot(borders2, border='snow1',add=TRUE, lwd=0.2, interior = FALSE, col = "transparent", box=F, axes=F)
plot(borders, add=TRUE, lwd=0.5, interior = FALSE, col = "transparent", box=F, axes=F)

plot(cultivated_managed_vegetation_rast,col=cols3, colNA="white", box=F, axes=F, legend=T,
     interpolate=F, useRaster=T,
     legend.width=2,legend.args = list(text = 'Vegetation (Cultivated & Managed)', side = 4, 
                                       font = 1, line = -1.5, cex = 1))
plot(borders2, border='snow1',add=TRUE, lwd=0.2, interior = FALSE, col = "transparent", box=F, axes=F)
plot(borders, add=TRUE, lwd=0.5, interior = FALSE, col = "transparent", box=F, axes=F)

plot(regularly_flooded_vegetation_rast,col=cols3, colNA="white", box=F, axes=F, legend=T,
     interpolate=F, useRaster=T,
     legend.width=2,legend.args = list(text = 'Vegetation (Regularly Flooded)', side = 4, 
                                       font = 1, line = -1.5, cex = 1))
plot(borders2, border='snow1',add=TRUE, lwd=0.2, interior = FALSE, col = "transparent", box=F, axes=F)
plot(borders, add=TRUE, lwd=0.5, interior = FALSE, col = "transparent", box=F, axes=F)

plot(culicoides_rast, col=cols5, colNA="white",box=F, axes=F, legend=T, interpolate=F, useRaster=T,
     legend.width=2,legend.args = list(text = 'Culicoides Paraensis Occurrence', side = 4, 
                                       font = 1, line = -1.5, cex = 1))
plot(borders2, border='snow1',add=TRUE, lwd=0.2, interior = FALSE, col = "transparent", box=F, axes=F)
plot(borders, add=TRUE, lwd=0.5, interior = FALSE, col = "transparent", box=F, axes=F)



plot(Brazil_forest_evergreen_broadleaf, col=cols3, colNA="white",box=F, axes=F, legend=T, interpolate=F, useRaster=T,
     legend.width=2,legend.args = list(text = 'Evergreen Broadlead Forests', side = 4, 
                                       font = 1, line = -1.5, cex = 1))
plot(borders2, border='snow1',add=TRUE, lwd=0.2, interior = FALSE, col = "transparent", box=F, axes=F)
plot(borders, add=TRUE, lwd=0.5, interior = FALSE, col = "transparent", box=F, axes=F)

plot(Brazil_forest_decidious_broadleaf, col=cols3, colNA="white",box=F, axes=F, legend=T, interpolate=F, useRaster=T,
     legend.width=2,legend.args = list(text = 'Deciduous Broadlead Forests', side = 4, 
                                       font = 1, line = -1.5, cex = 1))
plot(borders2, border='snow1',add=TRUE, lwd=0.2, interior = FALSE, col = "transparent", box=F, axes=F)
plot(borders, add=TRUE, lwd=0.5, interior = FALSE, col = "transparent", box=F, axes=F)

plot(Brazil_forest_mixed_trees, col=cols3, colNA="white",box=F, axes=F, legend=T, interpolate=F, useRaster=T,
     legend.width=2,legend.args = list(text = 'Mixed Trees Forests', side = 4, 
                                       font = 1, line = -1.5, cex = 1))
plot(borders2, border='snow1',add=TRUE, lwd=0.2, interior = FALSE, col = "transparent", box=F, axes=F)
plot(borders, add=TRUE, lwd=0.5, interior = FALSE, col = "transparent", box=F, axes=F)


plot(Brazil_grasslands, col=cols6, colNA="white",box=F, axes=F, legend=T, interpolate=F, useRaster=T,
     legend.width=2,legend.args = list(text = 'Grasslands', side = 4, 
                                       font = 1, line = -1.5, cex = 1))
plot(borders2, border='snow1',add=TRUE, lwd=0.2, interior = FALSE, col = "transparent", box=F, axes=F)
plot(borders, add=TRUE, lwd=0.5, interior = FALSE, col = "transparent", box=F, axes=F)

plot(Brazil_shrublands, col=cols7, colNA="white",box=F, axes=F, legend=T, interpolate=F, useRaster=T,
     legend.width=2,legend.args = list(text = 'Shrublands', side = 4, 
                                       font = 1, line = -1.5, cex = 1))
plot(borders2, border='snow1',add=TRUE, lwd=0.2, interior = FALSE, col = "transparent", box=F, axes=F)
plot(borders, add=TRUE, lwd=0.5, interior = FALSE, col = "transparent", box=F, axes=F)

plot(Brazil_savannas, col=cols8, colNA="white",box=F, axes=F, legend=T, interpolate=F, useRaster=T,
     legend.width=2,legend.args = list(text = 'Savannas', side = 4, 
                                       font = 1, line = -1.5, cex = 1))
plot(borders2, border='snow1',add=TRUE, lwd=0.2, interior = FALSE, col = "transparent", box=F, axes=F)
plot(borders, add=TRUE, lwd=0.5, interior = FALSE, col = "transparent", box=F, axes=F)

plot(Brazil_woody_savannas, col=cols8, colNA="white",box=F, axes=F, legend=T, interpolate=F, useRaster=T,
     legend.width=2,legend.args = list(text = 'Woody Savannas', side = 4, 
                                       font = 1, line = -1.5, cex = 1))
plot(borders2, border='snow1',add=TRUE, lwd=0.2, interior = FALSE, col = "transparent", box=F, axes=F)
plot(borders, add=TRUE, lwd=0.5, interior = FALSE, col = "transparent", box=F, axes=F)


plot(Brazil_cropland_naturalvegetation, col=cols9, colNA="white",box=F, axes=F, legend=T, interpolate=F, useRaster=T,
     legend.width=2,legend.args = list(text = 'Croplands and Natural Vegetation', side = 4, 
                                       font = 1, line = -1.5, cex = 1))
plot(borders2, border='snow1',add=TRUE, lwd=0.2, interior = FALSE, col = "transparent", box=F, axes=F)
plot(borders, add=TRUE, lwd=0.5, interior = FALSE, col = "transparent", box=F, axes=F)

plot(Brazil_croplands, col=cols9, colNA="white",box=F, axes=F, legend=T, interpolate=F, useRaster=T,
     legend.width=2,legend.args = list(text = 'Croplands', side = 4, 
                                       font = 1, line = -1.5, cex = 1))
plot(borders2, border='snow1',add=TRUE, lwd=0.2, interior = FALSE, col = "transparent", box=F, axes=F)
plot(borders, add=TRUE, lwd=0.5, interior = FALSE, col = "transparent", box=F, axes=F)

plot(Brazil_water, col=cols10, colNA="white",box=F, axes=F, legend=T, interpolate=F, useRaster=T,
     legend.width=2,legend.args = list(text = 'Waterbodies', side = 4, 
                                       font = 1, line = -1.5, cex = 1))
plot(borders2, border='snow1',add=TRUE, lwd=0.2, interior = FALSE, col = "transparent", box=F, axes=F)
plot(borders, add=TRUE, lwd=0.5, interior = FALSE, col = "transparent", box=F, axes=F)

plot(Brazil_precipitation, col=cols11, colNA="white",box=F, axes=F, legend=T, interpolate=F, useRaster=T,
     legend.width=2,legend.args = list(text = 'Mean Monthly Precipitation', side = 4, 
                                       font = 1, line = -1.5, cex = 1))
plot(borders2, border='snow1',add=TRUE, lwd=0.2, interior = FALSE, col = "transparent", box=F, axes=F)
plot(borders, add=TRUE, lwd=0.5, interior = FALSE, col = "transparent", box=F, axes=F)

plot(Brazil_temp, col=cols4, colNA="white",box=F, axes=F, legend=T, interpolate=F, useRaster=T,
     legend.width=2,legend.args = list(text = 'Mean Monthly Temperature', side = 4, 
                                       font = 1, line = -1.5, cex = 1))
plot(borders2, border='snow1',add=TRUE, lwd=0.2, interior = FALSE, col = "transparent", box=F, axes=F)
plot(borders, add=TRUE, lwd=0.5, interior = FALSE, col = "transparent", box=F, axes=F)

#plot(forestation_loss_2000_2023, col=cols15, colNA="white",box=F, axes=F, legend=T, interpolate=F, useRaster=T,
#     legend.width=2,legend.args = list(text = 'Forestation Loss (2000-2023)', side = 4, 
 #                                      font = 1, line = -1.5, cex = 1))
#plot(borders2, border='snow1',add=TRUE, lwd=0.2, interior = FALSE, col = "transparent", box=F, axes=F)
#plot(borders, add=TRUE, lwd=0.5, interior = FALSE, col = "transparent", box=F, axes=F)

plot(forestation_loss_2020_2023, col=cols15, colNA="white",box=F, axes=F, legend=T, interpolate=F, useRaster=T,
     legend.width=2,legend.args = list(text = 'Forestation Loss (2020-2023)', side = 4, 
                                       font = 1, line = -1.5, cex = 1))
plot(borders2, border='snow1',add=TRUE, lwd=0.2, interior = FALSE, col = "transparent", box=F, axes=F)
plot(borders, add=TRUE, lwd=0.5, interior = FALSE, col = "transparent", box=F, axes=F)

plot(Brazil_cattle, col=cols14, colNA="white",box=F, axes=F, legend=T, interpolate=F, useRaster=T,
     legend.width=2,legend.args = list(text = 'Cattle Production (No.)', side = 4, 
                                       font = 1, line = -1.5, cex = 1))
plot(borders2, border='snow1',add=TRUE, lwd=0.2, interior = FALSE, col = "transparent", box=F, axes=F)
plot(borders, add=TRUE, lwd=0.5, interior = FALSE, col = "transparent", box=F, axes=F)

plot(Brazil_banana, col=cols14, colNA="white",box=F, axes=F, legend=T, interpolate=F, useRaster=T,
     legend.width=2,legend.args = list(text = 'Banana Harvested Area', side = 4, 
                                       font = 1, line = -1.5, cex = 1))
plot(borders2, border='snow1',add=TRUE, lwd=0.2, interior = FALSE, col = "transparent", box=F, axes=F)
plot(borders, add=TRUE, lwd=0.5, interior = FALSE, col = "transparent", box=F, axes=F)

plot(Brazil_sugarcane, col=cols14, colNA="white",box=F, axes=F, legend=T, interpolate=F, useRaster=T,
     legend.width=2,legend.args = list(text = 'Sugarcane Harvested Area', side = 4, 
                                       font = 1, line = -1.5, cex = 1))
plot(borders2, border='snow1',add=TRUE, lwd=0.2, interior = FALSE, col = "transparent", box=F, axes=F)
plot(borders, add=TRUE, lwd=0.5, interior = FALSE, col = "transparent", box=F, axes=F)

plot(Brazil_coffee, col=cols14, colNA="white",box=F, axes=F, legend=T, interpolate=F, useRaster=T,
     legend.width=2,legend.args = list(text = 'Coffee Harvested Area', side = 4, 
                                       font = 1, line = -1.5, cex = 1))
plot(borders2, border='snow1',add=TRUE, lwd=0.2, interior = FALSE, col = "transparent", box=F, axes=F)
plot(borders, add=TRUE, lwd=0.5, interior = FALSE, col = "transparent", box=F, axes=F)

plot(Brazil_cocoa, col=cols14, colNA="white",box=F, axes=F, legend=T, interpolate=F, useRaster=T,
     legend.width=2,legend.args = list(text = 'Cocoa Harvested Area', side = 4, 
                                       font = 1, line = -1.5, cex = 1))
plot(borders2, border='snow1',add=TRUE, lwd=0.2, interior = FALSE, col = "transparent", box=F, axes=F)
plot(borders, add=TRUE, lwd=0.5, interior = FALSE, col = "transparent", box=F, axes=F)


plot(Brazil_maize, col=cols14, colNA="white",box=F, axes=F, legend=T, interpolate=F, useRaster=T,
     legend.width=2,legend.args = list(text = 'Maize Harvested Area', side = 4, 
                                       font = 1, line = -1.5, cex = 1))
plot(borders2, border='snow1',add=TRUE, lwd=0.2, interior = FALSE, col = "transparent", box=F, axes=F)
plot(borders, add=TRUE, lwd=0.5, interior = FALSE, col = "transparent", box=F, axes=F)

plot(Brazil_soybean, col=cols14, colNA="white",box=F, axes=F, legend=T, interpolate=F, useRaster=T,
     legend.width=2,legend.args = list(text = 'Soybean Harvested Area', side = 4, 
                                       font = 1, line = -1.5, cex = 1))
plot(borders2, border='snow1',add=TRUE, lwd=0.2, interior = FALSE, col = "transparent", box=F, axes=F)
plot(borders, add=TRUE, lwd=0.5, interior = FALSE, col = "transparent", box=F, axes=F)

plot(Brazil_cassava, col=cols14, colNA="white",box=F, axes=F, legend=T, interpolate=F, useRaster=T,
     legend.width=2,legend.args = list(text = 'Cassava Harvested Area', side = 4, 
                                       font = 1, line = -1.5, cex = 1))
plot(borders2, border='snow1',add=TRUE, lwd=0.2, interior = FALSE, col = "transparent", box=F, axes=F)
plot(borders, add=TRUE, lwd=0.5, interior = FALSE, col = "transparent", box=F, axes=F)




a<-dev.off()