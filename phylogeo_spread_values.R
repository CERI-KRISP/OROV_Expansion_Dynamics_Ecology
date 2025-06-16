# install.packages("devtools"); library(devtools)
# install_github("sdellicour/seraphim/unix_OS") # for Unix systems
## install_github("sdellicour/seraphim/windows") # for Windows systems
# install.packages("diagram")

library(diagram)
require(seraphim)

# 1. Extracting the spatio-temporal information contained in posterior trees

treefile<- "L_constant/L_Global_edit_remove_outgroups.relaxed.CauchyRRW.constant.2bl.Combined.trees"

localTreesDirectory = "Tree_extractions_extended_round2_constant_L"
allTrees = scan(file=treefile, what="", sep="\n", quiet=T)
burnIn = 0 # no. of trees (=12685000 states)
randomSampling = FALSE
nberOfTreesToSample = 100
mostRecentSamplingDatum = 2024.356 

coordinateAttributeName = "Location"

treeExtractions(localTreesDirectory, allTrees, burnIn, randomSampling, nberOfTreesToSample, mostRecentSamplingDatum, coordinateAttributeName)


# 2. Extracting the spatio-temporal information embedded in the MCC tree

treefile<- "L_constant/L_Global_edit_remove_outgroups.relaxed.CauchyRRW.constant.2bl.Combined_MCC.tree"

source("mccExtractions.r")
mcc_tre = readAnnotatedNexus(treefile)
mcc_tab = mccExtractions(mcc_tre, mostRecentSamplingDatum)
write.csv(mcc_tab, "CladeL_constant_round2_cluster_MCC.csv", row.names=F, quote=F)

# 3. Estimating the HPD region for each time slice
mcc_tab=read.csv("CladeL_constant_round2_cluster_MCC.csv")
#mcc_tab=subset(mcc_tab,startYear>=2020.0)
nberOfExtractionFiles = nberOfTreesToSample
prob = 0.95; precision = 0.025
startDatum = min(mcc_tab[,"startYear"])

polygons = suppressWarnings(spreadGraphic2(localTreesDirectory, nberOfExtractionFiles, prob, startDatum, precision))


# 3. Loading environmental layers

pop_rast = raster("Geospatial/Brazil_population_crop_mask.asc")
names(pop_rast) = "pop_2000_scaled_log"
pop_rast[pop_rast[]<0] = 0
pop_rast[] = pop_rast[] + 1

elevation_rast = raster("Geospatial/Brazil_elevation_crop_mask.asc")
names(elevation_rast) = "elevation"
elevation_rast[elevation_rast[]<0] = 0
elevation_rast[] = elevation_rast[] + 1


herbaceous_vegetation_rast = raster("Geospatial/Brazil_herbaceous_vegetation_earthenv.asc")
names(herbaceous_vegetation_rast) = "herbaceous_vegetation"
herbaceous_vegetation_rast[herbaceous_vegetation_rast[]<0] = 0
herbaceous_vegetation_rast[] = herbaceous_vegetation_rast[] + 1

cultivated_managed_vegetation_rast = raster("Geospatial/Brazil_cultivated_managed_vegetation_earthenv.asc")
names(cultivated_managed_vegetation_rast) = "cultivated_managed_vegetation"
cultivated_managed_vegetation_rast[cultivated_managed_vegetation_rast[]<0] = 0
cultivated_managed_vegetation_rast[] = cultivated_managed_vegetation_rast[] + 1

flooded_vegetation_rast = raster("Geospatial/Brazil_regularly_flooded_vegetation_earthenv.asc")
names(flooded_vegetation_rast) = "regularly_flooded_vegetation"
flooded_vegetation_rast[flooded_vegetation_rast[]<0] = 0
flooded_vegetation_rast[] = flooded_vegetation_rast[] + 1

cocoa_rast = raster("Geospatial/Brazil_cocoa_harvested_area.asc")
names(cocoa_rast) = "cocoa"
cocoa_rast[cocoa_rast[]<0] = 0
cocoa_rast[] = cocoa_rast[] + 1


soy_rast = raster("Geospatial/Brazil_soybean_harvested_area.asc")
names(soy_rast) = "soy"
soy_rast[soy_rast[]<0] = 0
soy_rast[] = soy_rast[] + 1


cattle_rast = raster("Geospatial/Brazil_cattle_production.asc")
names(cattle_rast) = "cattle"
cattle_rast[cattle_rast[]<0] = 0
cattle_rast[] = cattle_rast[] + 1


coffee_rast = raster("Geospatial/Brazil_coffee_harvested_area.asc")
names(coffee_rast) = "coffee"
coffee_rast[coffee_rast[]<0] = 0
coffee_rast[] = coffee_rast[] + 1



maize_rast = raster("Geospatial/Brazil_maize_harvested_area.asc")
names(maize_rast) = "maize"
maize_rast[maize_rast[]<0] = 0
maize_rast[] = maize_rast[] + 1

banana_rast = raster("Geospatial/Brazil_banana_harvested_area.asc")
names(banana_rast) = "banana"
banana_rast[banana_rast[]<0] = 0
banana_rast[] = banana_rast[] + 1

sugarcane_rast = raster("Geospatial/Brazil_sugarcane_harvested_area.asc")
names(sugarcane_rast) = "sugarcane"
sugarcane_rast[sugarcane_rast[]<0] = 0
sugarcane_rast[] = sugarcane_rast[] + 1

cassava_rast = raster("Geospatial/Brazil_cassava_harvested_area.asc")
names(cassava_rast) = "cassava"
cassava_rast[cassava_rast[]<0] = 0
cassava_rast[] = cassava_rast[] + 1


mosquito_rast = raster("Geospatial/Brazil_aegypti_crop_mask.asc")
names(mosquito_rast) = "mosquito"
mosquito_rast[mosquito_rast[]<0] = 0
mosquito_rast[] = mosquito_rast[] + 1

Brazil_forest_evergreen_broadleaf = raster("Geospatial/Brazil_evergreen_broadleaf_trees_earthenv.asc")
names(Brazil_forest_evergreen_broadleaf) = "evergreen_broadleaf"
Brazil_forest_evergreen_broadleaf[Brazil_forest_evergreen_broadleaf[]<0] = 0
Brazil_forest_evergreen_broadleaf[] = Brazil_forest_evergreen_broadleaf[] + 1

Brazil_forest_decidious_broadleaf = raster("Geospatial/Brazil_deciduous_broadleaf_trees_earthenv.asc")
names(Brazil_forest_decidious_broadleaf) = "deciduous_broadleaf"
Brazil_forest_decidious_broadleaf[Brazil_forest_decidious_broadleaf[]<0] = 0
Brazil_forest_decidious_broadleaf[] = Brazil_forest_decidious_broadleaf[] + 1


Brazil_forest_mixed_trees = raster("Geospatial/Brazil_mixed_trees_earthenv.asc")
names(Brazil_forest_mixed_trees) = "mixed_trees"
Brazil_forest_mixed_trees[Brazil_forest_mixed_trees[]<0] = 0
Brazil_forest_mixed_trees[] = Brazil_forest_mixed_trees[] + 1

Brazil_forest_loss = raster("Geospatial/Brazil_forest_loss_medium_res_completearea_2020to2023.asc")
names(Brazil_forest_loss) = "forest_loss"
Brazil_forest_loss[Brazil_forest_loss[]<0] = 0
Brazil_forest_loss[] = Brazil_forest_loss[] + 1


Brazil_grasslands = raster("Geospatial/Brazil_grasslands_crop_mask.asc")
Brazil_grasslands[Brazil_grasslands[]<0] = 0
Brazil_grasslands[] = Brazil_grasslands[] + 1

Brazil_shrublands = raster("Geospatial/Brazil_shrubs_earthenv.asc")
names(Brazil_shrublands) = "shrublands"
Brazil_shrublands[Brazil_shrublands[]<0] = 0
Brazil_shrublands[] = Brazil_shrublands[] + 1


Brazil_savannas = raster("Geospatial/Brazil_savannas_crop_mask.asc")
Brazil_savannas[Brazil_savannas[]<0] = 0
Brazil_savannas[] = Brazil_savannas[] + 1

Brazil_woodysavannas = raster("Geospatial/Brazil_woody_savannas_crop_mask.asc")
Brazil_woodysavannas[Brazil_woodysavannas[]<0] = 0
Brazil_woodysavannas[] = Brazil_woodysavannas[] + 1


Brazil_cropland_naturalvegetation = raster("Geospatial/Brazil_cropland_natural_vegetation_crop_mask.asc")
Brazil_cropland_naturalvegetation[Brazil_cropland_naturalvegetation[]<0] = 0
Brazil_cropland_naturalvegetation[] = Brazil_cropland_naturalvegetation[] + 1

Brazil_urban = raster("Geospatial/Brazil_urban_built_up_earthenv.asc")
names(Brazil_urban) = "urban"
Brazil_urban[Brazil_urban[]<0] = 0
Brazil_urban[] = Brazil_urban[] + 1

Brazil_water = raster("Geospatial/Brazil_water_crop_mask.asc")
names(Brazil_water) = "water"
Brazil_water[Brazil_water[]<0] = 0
Brazil_water[] = Brazil_water[] + 1


Brazil_precipitation = raster("Geospatial/Brazil_precipitation_mean_2023_crop_mask.asc")
Brazil_precipitation[Brazil_precipitation[]<0] = 0
Brazil_precipitation[] = Brazil_precipitation[] + 1

Brazil_croplands = raster("Geospatial/Brazil_Croplands_2019.asc")
Brazil_croplands[Brazil_croplands[]<0] = 0
Brazil_croplands[] = Brazil_croplands[] + 1

Brazil_temp = raster("Geospatial/Brazil_annual_mean_temp_2020_crop_mask.asc")
Brazil_temp[Brazil_temp[]<0] = 0
Brazil_temp[] = Brazil_temp[] + 1

orov_suitability = stack("Geospatial/OROV_SuitabilityMaps.tiff")
orov_suitability_cumulative=orov_suitability[[2]]
names(orov_suitability_cumulative) = "orov_suitability_cumulative"
orov_suitability_cumulative[orov_suitability_cumulative[]<0] = 0
orov_suitability_cumulative[] = orov_suitability_cumulative[] + 1

orov_suitability_pre2024=orov_suitability[[1]]
names(orov_suitability_pre2024) = "orov_suitability_pre2024"
orov_suitability_pre2024[orov_suitability_pre2024[]<0] = 0
orov_suitability_pre2024[] = orov_suitability_pre2024[] + 1

orov_suitability_premid2023 = raster("Geospatial/preExpansion_map_half2023.tiff")
names(orov_suitability_premid2023) = "orov_suitability_premid2023"
orov_suitability_premid2023[orov_suitability_premid2023[]<0] = 0
orov_suitability_premid2023[] = orov_suitability_premid2023[] + 1




nberOfExtractionFiles = 100
envVariables_list = list(pop_rast,elevation_rast, herbaceous_vegetation_rast,
                         cultivated_managed_vegetation_rast,flooded_vegetation_rast,
                         cocoa_rast,coffee_rast,soy_rast,cattle_rast,maize_rast,
                         banana_rast,sugarcane_rast,cassava_rast,mixed_trees,mosquito_rast,
                    Brazil_forest_evergreen_broadleaf,
                    Brazil_forest_decidious_broadleaf,
                    Brazil_croplands,Brazil_shrublands,
                    Brazil_cropland_naturalvegetation,
                    Brazil_grasslands,Brazil_temp,
                    Brazil_precipitation,Brazil_water, Brazil_woodysavannas,
                    Brazil_savannas, Brazil_urban, Brazil_forest_loss)

pathModel = 0
fourCells = FALSE
nberOfRandomisations = 1
randomProcedure = 3
showingPlots = FALSE

for (x in envVariables_list) {
  print(names(x))
  spreadValues(localTreesDirectory = "Tree_extractions_extended_round2_constant_L",
               nberOfExtractionFiles = 100,
               envVariables=list(x),
               startTime=2018.0,
               endTime=2024.5,
               timeSlices = 100,
               slidingWindow = 1/12,
               showingPlots = FALSE, 
               outputName = paste0("SpreadValues/L/",names(x)),
               nberOfCores = 10)
}

