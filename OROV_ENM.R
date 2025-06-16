# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Environmental Niche Model - Oropouche Virus - Brazil 
# CERI 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# installing and loading the libraries needed: 
#install.packages("biomod2", repos="http://R-Forge.R-project.org")
library(maps)
library(geosphere)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(rworldmap)
library(data.table)
library(ggthemes)
library(airportr)
library(hrbrthemes)
library(ggplot2)
library(lubridate)
library(paletteer)
library(RColorBrewer)
library(data.table)
library(readr)
library(MMWRweek)
library(scales)
library(showtext)
library(sysfonts)
library(sf)
library(rnaturalearth)
library(janitor)
library(raster)
library(viridis)
library(ggnewscale)
library(ggrepel)
library(terra)
library(viridis)
LIB <- c("rgbif", "biomod2", "ggplot2", "gridExtra", "knitr", "raster", 
         "ade4", "rworldmap", "cleangeo", "maptools", "rasterVis", "rgdal")
#for(i in LIB) { install.packages(i) ; library(i, character.only=T) }
for(i in LIB) { library(i, character.only=T) }


#### Load_required_data
#data_to_model <- One of the following: 

Cumulative_df <- fread("OROV_occurrence_data/Cumulative_dataset.csv")
Pre_mid2023 <- fread("OROV_occurrence_data/Pre-mid2023.csv")
Pre_2024<- fread("OROV_occurrence_data/Pre-2024.csv")

## Brazil Shapefile
brazil.shp <- st_read("Shapefiles/BRAZIL_shp/gadm41_BRA_0.shp")

##########################################################################################
####################	    	 Data Formating               ##############################  
##########################################################################################

## Comment out the parts eval.resp. var to eval.resp.xy if you running the test on an independent dataset 

#data_to_model <- 


SPC_PresAbs <- BIOMOD_FormatingData(resp.var = data_to_model$occurrence ,
                                    expl.var = Brazil.cov.sel,
                                    resp.xy = data_to_model[,c('Longitude', 'Latitude')],
                                    resp.name = "OROV",
                                    #eval.resp.var= independent_data_2024_all_Occ$occurrence,
                                    #eval.expl.var = Brazil.cov.sel,
                                    #eval.resp.xy = independent_data_2024_all_Occ[,c('Longitude', 'Latitude')],
                                    na.rm=T)

SPC_PresAbs
plot(SPC_PresAbs)


##########################################################################################
####################	    	 BIOMOD MODELLING               ##############################  
##########################################################################################


# Set up non-default options:
# ...............................................
## Can also use default settings in biomod 
## but specified here because of MAXENT.Phillips 
## GBM = Boosted regression Trees (BRT)

user.RF <- list("_allData_allRun" = list(nodesize = 1,maxnodes = 50, ntree=500))
user.val = list(RF.binary)
MySpc_options <- bm_ModelingOptions( data.type = "binary",
                                     models = c("GLM","GAM","GBM", "RF","CTA","MAXENT","SRE"),
                                     strategy = "big.boss")



# Model the disease:

# when no independant dataset is available to evaluate the models, a repeated data 
# spliting procedure (block cross-validation) is carried out. 
# This entire procedure can be repeated X times (NbRunEval). 

# By default, each model will be evaluated according to `TSS` and `ROC` curve metrics.

MySpc_models <- BIOMOD_Modeling (bm.format = SPC_PresAbs,
                                 models = c("GLM","GAM","GBM", "RF","CTA","SRE", "MAXENT"),
                                 OPT.user  = MySpc_options,
                                 OPT.strategy	= 'bigboss',
                                 #OPT.user = "default",
                                 var.import = 5,
                                 metric.eval =c('TSS','ROC'),
                                 CV.do.full.models = F,
                                 modeling.id = "OROV")



############################################################################################
############################################################################################
####################	              MODEL EVALUATION                   #####################
############################################################################################
############################################################################################


### get models evaluation scores
MyModels_scores <- get_evaluations(MySpc_models)
MyModels_scores
bm_PlotEvalMean(MySpc_models)


############################################################################################
############################################################################################
####################	             Variable Importance                 #####################
############################################################################################
############################################################################################


MyModels_var_import <- get_variables_importance(MySpc_models)
MyModels_var_import
dimnames(MyModels_var_import)


MyModels_var_import %>% 
  filter(algo == "RF") %>% 
  ggplot()+
  geom_col(aes(x=algo, y=var.imp, fill= expl.var ), position = position_dodge())+
  theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  facet_wrap(~run)


############################################################################################
############################################################################################
###################       Map model prediction in Current climates        ##################
############################################################################################
############################################################################################

raster.files_proj <- list.files("Geospatial/", pattern = ".asc$", full.names=T)

Brazil.rast.list_proj <- lapply(raster.files_proj, rast)

## Select the environmental variables you want
Brazil.rast.list_proj <- Brazil.rast.list_proj[c(1,2,6,9,12,16,23,21,3)] ## Select the environmental variables you want

reference_raster <- Brazil.rast.list_proj[[3]]
# Align all rasters to the reference raster
Brazil.rast.aligned_proj <- lapply(Brazil.rast.list_proj, function(r) {
  resample(r, reference_raster, method = "bilinear")
})
Brazil.alighed.rasters_proj <- rast(Brazil.rast.aligned_proj)

## Model Projection
MySpc_models_proj_current <- BIOMOD_Projection( bm.mod = MySpc_models,
                                                new.env = Brazil.alighed.rasters_proj,
                                                proj.name = "current",
                                                selected.models = "all",
                                                binary.meth = "TSS",
                                                output.format = ".img",
                                                do.stack = F,
                                                build.clamping.mask = F)


## The output is in raster format (each layer representing a run/model)
Preds_Ind <- get_predictions(MySpc_models_proj_current)

############################################################################################
############################################################################################
############################################################################################
####################		 ENSEMBLE MODELLING
############################################################################################
############################################################################################
############################################################################################


MySpc_ensemble_models <- BIOMOD_EnsembleModeling( bm.mod = MySpc_models,
                                                  models.chosen = "all",
                                                  em.by = 'all', # combine all single models
                                                  #em.algo = "EMcv",
                                                  em.algo = "EMwmean",
                                                  EMwmean.decay = 2,
                                                  metric.select = "TSS",
                                                  metric.select.thresh = 0.6,
                                                  metric.eval = c('TSS','ROC'),
                                                  prob.mean = TRUE,
                                                  prob.cv = T, 
                                                  #committee.averaging = TRUE, 
                                                  prob.mean.weight = F) #Compute the weighted sum of probabilities across predictions


#save.image(file= "F.Model_Ensemble_Model.RData")


# check the scores of the models.
MySpc_ensemble_models_scores <- get_evaluations(MySpc_ensemble_models)
MySpc_ensemble_models_scores
# Represent evaluation scores
bm_PlotEvalMean(bm.out = MySpc_ensemble_models, dataset = 'evaluation')


# Ensemble model forecasts
# ...............................................
MySpc_ensemble_models_proj_current <- BIOMOD_EnsembleForecasting(
  bm.em = MySpc_ensemble_models,
  bm.proj = MySpc_models_proj_current,
  models.chosen = 'all',
  metric.binary = 'TSS',
  metric.filter = 'TSS')

## brazil shapefile
Brazil.admin1 <- st_read("Shapefiles/BRAZIL_shp/gadm41_BRA_1.shp")

## Plotting ensemble map 
## For reproducing the maps in Figure 3 - make sure you are using the correct input data Pre / post expansion
p2 <- ggplot()+
  geom_raster(data= preds_Ens$XXXX/1000, aes(x = x, y = y, fill = XXXX) )+ ## Replace XXXX as needed 
  scale_fill_gradientn(name = "Suitbaility Index", na.value = "transparent", colours=custom_palette,
                       guide = guide_colorbar(barwidth = 0.7, barheight = 15, title.position = "top"))+
  #geom_sf(data= Brazil.admin1, fill=NA, colour="white")+ # sometimes take a hile to load given the size of the shapefile
  coord_sf()+
  #theme_ipsum()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border=element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(size=20),
        plot.subtitle = element_text(size=18),
        legend.text = element_text(size=18),
        legend.title = element_text(size=18),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.box= "horizontal")+
  #coord_fixed()+
  xlab(" ") + ylab(" ")+
  labs(title = "Suitability Map of OROV" , subtitle = "__ Expansion")


## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Response Curves 
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

mods <- get_built_models(MySpc_models, algo = c("RF"))

bm_PlotResponseCurves(bm.out = MySpc_models, 
                      models.chosen = mods,
                      fixed.var = 'median')



############################################################################################
############################################################################################
############################################################################################
####################		Kernal Density Estimate for sampling 
############################################################################################
############################################################################################
############################################################################################
## Presence points would be the the disease presence locations (depending on the analysis)

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Step 1: Convert the extent of the population density raster to a SpatialPolygons object
# Convert your presence points to an `ppp` object
presence_points_ppp <- ppp(presence_dups_nodups$Longitude, presence_dups_nodups$Latitude, 
                           window = owin(c(min(presence_dups_nodups$Longitude), max(presence_dups_nodups$Longitude)), 
                                         c(min( presence_dups_nodups$Latitude), max( presence_dups_nodups$Latitude))))

# Perform kernel density estimation
kde <- density(presence_points_ppp)
# Assuming population_density is a raster layer
kde_raster <- raster(kde, crs=crs(pop_density_raster))
extent(kde_raster) <- extent(pop_density_raster)
kde_raster_masked <- mask(kde_raster, brazil.shp)
pop_density_raster2 <- resample(pop_density_raster,kde_raster_masked)
combined_density <- overlay(pop_density_raster2, kde_raster_masked, fun=function(x, y) {x * y})

## But we want to create a buffer of atleast 50 000 metres around those presence points 
# Convert presence points to an `sf` object
presence_points_sf <- st_as_sf(presence_dups_nodups, coords = c("Longitude", "Latitude"), crs = crs(pop_density_raster))


### Raster-Based Approach 
# Use a distance raster approach (optional step for sampling purposes)
distance_raster <- distanceFromPoints(kde_raster_masked, presence_points_sf)

# Create a mask for distances between 25,000 and 50,000 
mask_within_range <- (distance_raster > 50000) & (distance_raster < 300000)
plot(mask_within_range)

#mask_within_range <- resample(mask_within_range, combined_density, method="bilinear")
areaToSample <- combined_density*mask_within_range
plot(areaToSample)


# Step 1: Normalize the raster values to create a probability distribution
# Get the values of the raster
kde_values <- getValues(areaToSample)

# Remove NA values
kde_values[is.na(kde_values)] <- 0

# Normalize the raster values to sum to 1 (probabilities)
kde_probabilities <- kde_values / sum(kde_values)

# Step 2: Sample points based on the kernel density probabilities
# Number of points to sample
n_points <- 450

# Get indices of valid (non-NA) cells
valid_cells <- which(kde_probabilities > 0)

# Sample cell indices based on the probability weights
sampled_cells <- sample(valid_cells, size = n_points, prob = kde_probabilities[valid_cells], replace = TRUE)

# Convert the sampled cells to coordinates
sampled_coords <- xyFromCell(areaToSample, sampled_cells)

# Step 3: Convert the sampled coordinates to a SpatialPoints object (or sf object)
sampled_points <- SpatialPoints(sampled_coords, proj4string = crs(areaToSample))

# Step 4: Visualize the result
plot(areaToSample, main = "Kernel Density Raster with Sampled Points")
points(sampled_points, col = "red", pch = 20)

# Extract coordinates from SpatialPoints object
coords <- coordinates(sampled_points)
df <- data.frame(coords)# Convert coordinates to data frame
colnames(df) <- c("Longitude", "Latitude")# Optionally, you can add column names if needed


############################################################################################
############################################################################################
####################		Kernal Density Estimate for sampling 
############################################################################################
############################################################################################

library(ade4)
pca <- ade4::dudi.pca(Brazil.rast.df, scannf = F, nf = 2)

colnames(Brazil.rast.df) <- c()


#1. Visualise eigenvalues (Scree plot). Show percentage of variances explained by each principal component

fviz_eig(pca)

#Graph of variables. Variables with a similar profile are grouped together.
pca_plot <- fviz_pca_var(pca,
                         col.var = "contrib", # Color by the quality of representation
                         gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                         ggtheme = theme_minimal(),
                         repel = TRUE,
                         labelsize=6)   # Avoid text overlapping)

























