library(diagram)
library(HDInterval)
library(lubridate)
library(maptools)
library(MetBrewer)
library(RColorBrewer)
library(seraphim)
library(sf)
library(stringr)
library(terra)

source("Post_tree_extraction.r"); source("MCC_tree_extraction.r")
segments = c("L","M","S"); segment_names = c("OROV: segment L","OROV: segment M","OROV: segment S")
beast_analyses = c("L_Global_edit_remove_outgroups.relaxed.CauchyRRW.constant.2bl.Combined",
				   "M_Global_edit_remove_outgroups.relaxed.CauchyRRW.constant.2bl.Combined",
				   "S_Global_edit_remove_outgroups.relaxed.CauchyRRW.constant.2bl.Combined")
localTreesDirectories = c("Tree_extractions_segm_L","Tree_extractions_segm_M","Tree_extractions_segm_S"); nberOfExtractionFiles = 100
nberOfTreesToSample = nberOfExtractionFiles; mostRecentSamplingDates = c(2024.356,2024.356,2024.356)
colour_scale = met.brewer(name="Hokusai3", n=131, type="continuous")[1:101]
colour_scale = c(rep(colour_scale[1],100), colour_scale)

# 1. Loading the different administrative shapefiles and environmental rasters

GADM_BRA_0 = st_simplify(st_as_sf(shapefile("GADM_shapefile_BRA/GADM_BRA_0.shp")), dTolerance=1000)
GADM_BRA_1 = st_simplify(st_as_sf(shapefile("GADM_shapefile_BRA/GADM_BRA_1.shp")), dTolerance=1000)
e_studyArea = extent(GADM_BRA_0); e_studyArea[2] = -34
GADM_BRA_0 = st_crop(GADM_BRA_0, e_studyArea)
GADM_BRA_1 = st_crop(GADM_BRA_1, e_studyArea)
if (!file.exists("Background_rast.asc"))
	{
		background = raster("Environmental_rasters/Brazil_aegypti_crop_mask.asc")
		background[!is.na(background[])] = 0; writeRaster(background, "Background_rast.asc")
	}
background = raster("Background_rast.asc")
raster_files = list.files("Environmental_rasters"); envVariables = list()
raster_files = raster_files[which(!raster_files%in%c("Brazil_forest_loss_medium_res_2000-2023.asc"))]
raster_files = raster_files[c(19,26,12,16,10,21,1,11,13,18,15,23,22,28,8,9,3,4,6,7,17,24,25,5,14,27,20,2)]
for (i in 1:length(raster_files)) envVariables[[i]] = raster(paste0("Environmental_rasters/",raster_files[i]))
envVariables_mod = list(); templateRaster = envVariables[[17]] # resolution: 0.0833 x 0.0833
for (i in 1:length(envVariables))
	{
		if (res(envVariables[[i]])[1] < res(templateRaster)[1])
			{
				envVariables_mod[[i]] = raster(terra::project(rast(envVariables[[i]]), rast(templateRaster)))
			}	else		{
				envVariables_mod[[i]] = envVariables[[i]]
			}
	}
envVariableNames = gsub("Brazil_","",gsub("\\.asc","",gsub("_mask","",gsub("_crop_","_",raster_files))))
envVariableNames = gsub("alltypes","all types",gsub("aegypti","A. aegypti",gsub("_"," ",gsub("_total","",envVariableNames))))
envVariableNames = gsub("urban","urban areas",gsub("mean temp ","mean temperature ",gsub("medium res ","",envVariableNames)))
envVariableNames = gsub(" earthenv","",gsub("urban areas built up earthenv","urban & built-up areas",envVariableNames))
envVariableNames = gsub("shrubs","shrublands",gsub("water occurrence","waterbodies",envVariableNames))
envVariableNames = gsub("population","human population",gsub("natural vegetation","& natural vegetation",envVariableNames))
for (i in 1:length(envVariableNames))
	{
		envVariableName_words = unlist(strsplit(envVariableNames[i]," "))
		envVariableNames[i] = str_to_title(envVariableName_words[1])
		if (length(envVariableName_words) > 1)
			{
				for (j in 2:length(envVariableName_words)) envVariableNames[i] = paste(envVariableNames[i],envVariableName_words[j])
			}
	}
raster_names = rep(NA, length(raster_files)); colour_scales = list()
raster_names[1] = "Population density"; colour_scales[[1]] = colorRampPalette(brewer.pal(9,"BuPu"))(101)
raster_names[2] = "Urban & built-up areas (log)"; colour_scales[[2]] = colorRampPalette(brewer.pal(9,"BuPu"))(101)
raster_names[3] = "Elevation"; colour_scales[[3]] = colorRampPalette(brewer.pal(9,"YlOrRd"))(101)
raster_names[4] = "Herbaceous vegetation"; colour_scales[[4]] = colorRampPalette(brewer.pal(9,"Greens"))(131)[1:101]
raster_names[5] = "Cultivated managed vegetation"; colour_scales[[5]] = colorRampPalette(brewer.pal(9,"Greens"))(131)[1:101]
raster_names[6] = "Regularly flooded vegetation"; colour_scales[[6]] = colorRampPalette(brewer.pal(9,"Greens"))(131)[1:101]
raster_names[7] = "A. aegypti ecological suitability"; colour_scales[[7]] = rev(colorRampPalette(brewer.pal(11,"RdYlBu"))(101))
raster_names[8] = "Decidious broadleaf forests"; colour_scales[[8]] = colorRampPalette(brewer.pal(9,"YlGn"))(101)
raster_names[9] = "Evergreen broadleaf forests"; colour_scales[[9]] = colorRampPalette(brewer.pal(9,"YlGn"))(101)
raster_names[10] = "Mixed trees forests"; colour_scales[[10]] = colorRampPalette(brewer.pal(9,"YlGn"))(101)
raster_names[11] = "Grasslands"; colour_scales[[11]] = colorRampPalette(brewer.pal(9,"YlGn"))(101)
raster_names[12] = "Shrublands"; colour_scales[[12]] = colorRampPalette(brewer.pal(9,"YlGn"))(101)
raster_names[13] = "Savannas"; colour_scales[[13]] = colorRampPalette(brewer.pal(9,"YlGn"))(101)
raster_names[14] = "Woody savannas"; colour_scales[[14]] = colorRampPalette(brewer.pal(9,"YlGn"))(101)
raster_names[15] = "Croplands & natural vegetation"; colour_scales[[15]] = colorRampPalette(brewer.pal(9,"YlOrBr"))(111)[1:101]
raster_names[16] = "Croplands"; colour_scales[[16]] = colorRampPalette(brewer.pal(9,"YlOrBr"))(111)[1:101]
raster_names[17] = "Banana harvested area (log)"; colour_scales[[17]] = colorRampPalette(brewer.pal(9,"YlOrBr"))(111)[1:101]
raster_names[18] = "Cassava harvested area (log)"; colour_scales[[18]] = colorRampPalette(brewer.pal(9,"YlOrBr"))(111)[1:101]
raster_names[19] = "Cocoa harvested area (log)"; colour_scales[[19]] = colorRampPalette(brewer.pal(9,"YlOrBr"))(111)[1:101]
raster_names[20] = "Coffee harvested area (log)"; colour_scales[[20]] = colorRampPalette(brewer.pal(9,"YlOrBr"))(111)[1:101]
raster_names[21] = "Maize harvested area (log)"; colour_scales[[21]] = colorRampPalette(brewer.pal(9,"YlOrBr"))(111)[1:101]
raster_names[22] = "Soybean harvested area (log)"; colour_scales[[22]] = colorRampPalette(brewer.pal(9,"YlOrBr"))(111)[1:101]
raster_names[23] = "Sugarcane harvested area (log)"; colour_scales[[23]] = colorRampPalette(brewer.pal(9,"YlOrBr"))(111)[1:101]
raster_names[24] = "Cattle production"; colour_scales[[24]] = colorRampPalette(brewer.pal(9,"YlOrBr"))(111)[1:101]
raster_names[25] = "Forest loss 2020-2023"; colour_scales[[25]] = colorRampPalette(brewer.pal(9,"YlOrBr"))(111)[1:101]
raster_names[26] = "Waterbodies"; colour_scales[[26]] = colorRampPalette(brewer.pal(9,"Blues"))(101)
raster_names[27] = "Mean monthly precipitation"; colour_scales[[27]] = colorRampPalette(brewer.pal(9,"Blues"))(101)
raster_names[28] = "Mean monthly temperature"; colour_scales[[28]] = colorRampPalette(brewer.pal(9,"Reds"))(131)[1:101]

# 2. Plotting the different environmental rasters cropped on the Brazilian area

pdf(paste0("Environmental_rasts_NEW1.pdf"), width=9, height=6) # dev.new(width=9, height=6)
par(mfrow=c(4,7), oma=c(0,0,0.2,0), mar=c(0,0,0.6,0), mgp=c(0,0.1,0), lwd=0.3, bty="o", col="gray30")
for (i in 1:length(envVariables))
	{
		envVariable = envVariables[[i]]; colour_scale_i = paste0(colour_scales[[i]],"BF") # "BF" corresponds to 75% transparency
		if (raster_names[i] == "Population density (log)") envVariable[!is.na(envVariable[])] = log(envVariable[!is.na(envVariable[])]+1)
		if (raster_names[i] == "Urban & built-up areas (log)") envVariable[!is.na(envVariable[])] = log(envVariable[!is.na(envVariable[])]+1)
		if (grepl("harvested area",raster_names[i])) envVariable[!is.na(envVariable[])] = log(envVariable[!is.na(envVariable[])]+1)
		plot(GADM_BRA_0$geometry, col=NA, border=NA, ann=F, axes=F); plot(envVariable, axes=F, ann=F, legend=F, add=T, col=colour_scale_i)
		plot(GADM_BRA_0$geometry, col=NA, border="gray70", lwd=0.7, add=T); mtext(raster_names[i], line=-0.5, cex=0.5, col="gray30")
		plot(envVariable, legend.only=T, add=T, col=colour_scale_i, legend.width=0.5, legend.shrink=0.3, smallplot=c(0.09,0.13,0.1,0.4),
	 		 legend.args=list(text="", cex=0.7, col="gray30"), horizontal=F,
			 axis.args=list(cex.axis=0.7, lwd=0, lwd.tick=0.3, col.tick="gray30", tck=-0.8, col="gray30", col.axis="gray30", line=0, mgp=c(0,0.5,0)))
	}
dev.off()
	
pdf(paste0("Environmental_rasts_NEW2.pdf"), width=9, height=6) # not reported
par(mfrow=c(4,7), oma=c(0,0,0.2,0), mar=c(0,0,0.6,0), mgp=c(0,0.1,0), lwd=0.3, bty="o", col="gray30")
for (i in 1:length(envVariables))
	{
		envVariable = envVariables_mod[[i]]
		if (raster_names[i] == "Population density (log)") envVariable[!is.na(envVariable[])] = log(envVariable[!is.na(envVariable[])]+1)
		plot(GADM_BRA_0$geometry, col=NA, border=NA, ann=F, axes=F); plot(envVariable, axes=F, ann=F, legend=F, add=T, col=colour_scales[[i]])
		plot(GADM_BRA_0$geometry, col=NA, border="gray70", lwd=0.7, add=T); mtext(raster_names[i], line=-0.5, cex=0.5, col="gray30")
		plot(envVariable, legend.only=T, add=T, col=colour_scales[[i]], legend.width=0.5, legend.shrink=0.3, smallplot=c(0.09,0.13,0.1,0.4),
	 		 legend.args=list(text="", cex=0.7, col="gray30"), horizontal=F,
			 axis.args=list(cex.axis=0.7, lwd=0, lwd.tick=0.3, col.tick="gray30", tck=-0.8, col="gray30", col.axis="gray30", line=0, mgp=c(0,0.5,0)))
	}
dev.off()

# 3. Extract the spatio-temporal information embedded in MCC and 100 posterior trees

for (i in 1:length(beast_analyses))
	{
		if (!file.exists(paste0("Tree_extraction_files/",localTreesDirectories[i],".trees")))
			{
				txt1 = scan(paste0("BEAST_RRW_analyses/",beast_analyses[i],".trees"), what="", sep="\n", quiet=T, blank.lines.skip=F)
				if (txt1[length(txt1)] == "End;") txt1 = txt1[1:(length(txt1)-1)]
				indices1 = which(txt1=="\t\t;"); txt2 = txt1[1:indices1[2]]
				indices2 = c((indices1[2]+1):length(txt1)); txt2 = c(txt2, txt1[sample(indices2, 100, replace=F)])
				txt2 = c(txt2,"End;"); write(txt2, paste0("Tree_extraction_files/",localTreesDirectories[i],".trees"))
			}
		txt2 = scan(paste0("Tree_extraction_files/",localTreesDirectories[i],".trees"), what="", sep="\n", quiet=T, blank.lines.skip=F)
		indices = which(grepl("STATE",txt2)); treeIDs = rep(NA, length(indices)); options(scipens=9)
		for (j in 1:length(indices)) treeIDs[j] = gsub("STATE_","",unlist(strsplit(txt2[indices[j]]," "))[2])
		log1 = read.table(paste0("BEAST_RRW_analyses/",beast_analyses[i],".log"), head=T, sep="\t"); log2 = c()
		for (j in 1:length(treeIDs)) log2 = rbind(log2, log1[which(log1[,"state"]==as.numeric(treeIDs[j])),])
		write.table(log2, paste0("Tree_extraction_files/",localTreesDirectories[i],".log"), quote=F, row.names=F, sep="\t")
	}
for (i in 1:length(beast_analyses))
	{
		allTrees = readAnnotatedNexus(paste0("Tree_extraction_files/",localTreesDirectories[i],".trees"))
		for (j in 1:length(allTrees))
			{
				tab = Post_tree_extraction(allTrees[[j]], mostRecentSamplingDates[i])
				write.csv(tab, paste0("Tree_extraction_files/",localTreesDirectories[i],"_ext/TreeExtractions_",j,".csv"), row.names=F, quote=F)
			}
		mcc_tre = readAnnotatedNexus(paste0("Tree_extraction_files/",localTreesDirectories[i],".tree"))
		mcc_tab = MCC_tree_extraction(mcc_tre, mostRecentSamplingDates[i])
		write.csv(mcc_tab, paste0("Tree_extraction_files/",localTreesDirectories[i],".csv"), row.names=F, quote=F)
	}

# 4. Extracting the start and end Brazilian state for each phylogenetic branch

for (i in 1:length(beast_analyses))
	{
		for (j in 1:nberOfExtractionFiles)
			{
				tab = read.csv(paste0("Tree_extraction_files/",localTreesDirectories[i],"_ext/TreeExtractions_",j,".csv"), head=T)
				if (!"startState"%in%colnames(tab))
					{
						states = matrix(nrow=dim(tab)[1], ncol=2); colnames(states) = c("startState","endState")
						for (k in 1:dim(tab)[1])
							{
								c1 = 0; c2 = 0
								for (l in 1:length(GADM_BRA_1$geometry))
									{
										for (m in 1:length(GADM_BRA_1$geometry[[l]]))
											{
												if (point.in.polygon(tab[k,"startLon"],tab[k,"startLat"],GADM_BRA_1$geometry[[l]][[m]][[1]][,1],GADM_BRA_1$geometry[[l]][[m]][[1]][,2]) == 1)
													{
														states[k,"startState"] = GADM_BRA_1$HASC_1[l]; c1 = c1+1
													}
												if (point.in.polygon(tab[k,"endLon"],tab[k,"endLat"],GADM_BRA_1$geometry[[l]][[m]][[1]][,1],GADM_BRA_1$geometry[[l]][[m]][[1]][,2]) == 1)
													{
														states[k,"endState"] = GADM_BRA_1$HASC_1[l]; c2 = c2+1
													}
											}
									}
								if (c1 != 1) print(c(i,j,k,c1))
								if (c2 != 1) print(c(i,j,k,c2))
							}
						write.csv(cbind(tab, states), paste0("Tree_extraction_files/",localTreesDirectories[i],"_ext/TreeExtractions_",j,".csv"), row.names=F, quote=F)
					}
			}
	}

# 5. Estimating dispersal statistics and the evolution of the wavefront

timeSlices = 200; onlyTipBranches = F; showingPlots = F; nberOfCores = 10; slidingWindow = 1
for (i in 1:length(beast_analyses))
	{
		localTreesDirectory = paste0("Tree_extraction_files/",localTreesDirectories[i],"_ext")
		outputName = paste0("All_dispersal_statistics/OROV_segment",segments[i])
		spreadStatistics(localTreesDirectory, nberOfExtractionFiles, timeSlices, onlyTipBranches, showingPlots, outputName, nberOfCores, slidingWindow)
	}
for (i in 1:length(beast_analyses))
	{
		mat = read.table(paste0("All_dispersal_statistics/OROV_segment",segments[i],"_estimated_dispersal_statistics.txt"), head=T)
		vS1 = mat[,"weighted_diffusion_coefficient"]/365.25; HPD1 = round(HDInterval::hdi(vS1)[1:2],0)
		vS2 = mat[,"isolation_by_distance_signal_rP2"]; HPD2 = round(HDInterval::hdi(vS2)[1:2],3)
		cat("WDC = ",round(median(vS1),0)," km2/day (95% HPD = [",HPD1[1],", ",HPD1[2],"])\n",sep="")
			# WDC = 582 km2/day (95% HPD = [477, 672]) for segment L
			# WDC = 574 km2/day (95% HPD = [464, 677]) for segment M
			# WDC = 515 km2/day (95% HPD = [435, 675]) for segment S
		cat("IBD (rP2) = ",round(median(vS2),3)," (95% HPD = [",HPD2[1],", ",HPD2[2],"])\n",sep="")
			# IBD (rP2) = 0.470 (95% HPD = [0.360, 0.599]) for segment L
			# IBD (rP2) = 0.575 (95% HPD = [0.286, 0.607]) for segment M
			# IBD (rP2) = 0.684 (95% HPD = [0.326, 0.710]) for segment S
	}

# 6. Visualising the continuous phylogeographic reconstructions

mostRecentSamplingDatum = min(mostRecentSamplingDates)
minYears = matrix(nrow=nberOfExtractionFiles, ncol=length(beast_analyses))
for (i in 1:length(beast_analyses))
	{
		for (j in 1:nberOfExtractionFiles)
			{
				tab = read.csv(paste0("Tree_extraction_files/",localTreesDirectories[i],"_ext/TreeExtractions_",j,".csv"), head=T)
				if (mostRecentSamplingDatum < max(tab[,"endYear"])) mostRecentSamplingDatum = max(tab[,"endYear"])
				minYears[j,i] = min(tab[,"startYear"])
			}
	}
minYear = min(hdi(minYears)[1,], na.rm=T); maxYear = mostRecentSamplingDatum
polygons_list = list(); prob = 0.95; precision = 1/12; startDatum = minYear; croppingPolygons = FALSE
for (i in 1:length(beast_analyses))
	{
		localTreesDirectory = paste0("Tree_extraction_files/",localTreesDirectories[i],"_ext")
		polygons_list[[i]] = suppressWarnings(spreadGraphic2(localTreesDirectory, nberOfExtractionFiles, prob, startDatum, precision))
	}
states = c()
for (i in 1:length(beast_analyses))
	{
		for (j in 1:nberOfExtractionFiles)
			{
				tab = read.csv(paste0("Tree_extraction_files/",localTreesDirectories[i],"_ext/TreeExtractions_",j,".csv"), head=T)
				states = unique(c(states,tab[,"startState"],tab[,"endState"])); states = states[which(!is.na(states))]
			}
	}
states = states[order(states)]
states_centroids = matrix(nrow=length(states), ncol=2)
for (i in 1:length(states))
	{
		states_centroids[i,] = st_centroid(GADM_BRA_1$geometry)[which(GADM_BRA_1$HASC_1==states[i])][[1]][]
	}
states_gravity_points_list = list()
for (i in 1:length(beast_analyses))
	{
		states_gravity_points = matrix(nrow=length(states), ncol=2)
		for (j in 1:length(states))
			{
				state_gravity_points = matrix(nrow=nberOfExtractionFiles, ncol=2)
				for (k in 1:nberOfExtractionFiles)
					{
						tab = read.csv(paste0("Tree_extraction_files/",localTreesDirectories[i],"_ext/TreeExtractions_",k,".csv"), head=T)
						state_points = tab[which(tab[,"endState"]==states[j]),c("endLon","endLat")]
						state_gravity_points[k,] = cbind(mean(state_points[,"endLon"]),mean(state_points[,"endLat"])) 
					}
				states_gravity_points[j,] = cbind(mean(state_gravity_points[,1]),mean(state_gravity_points[,2])) 
			}
		states_gravity_points_list[[i]] = states_gravity_points
	}
states_cols = rep("gray95",length(GADM_BRA_1$HASC_1))
states_cols[which(GADM_BRA_1$HASC_1%in%states)] = "gray90"
states_cols_list = list()
for (i in 1:length(beast_analyses))
	{
		states_cols = rep("gray95",length(GADM_BRA_1$HASC_1))
		firstInvasionTime = matrix(nrow=nberOfExtractionFiles, ncol=length(states))
		for (j in 1:nberOfExtractionFiles)
			{
				tab = read.csv(paste0("Tree_extraction_files/",localTreesDirectories[i],"_ext/TreeExtractions_",j,".csv"), head=T)
				for (k in 1:length(states))
					{
						startYears = tab[which(tab[,"startState"]==states[k]),"startYear"]
						endYears = tab[which(tab[,"endState"]==states[k]),"endYear"]
						if (length(c(startYears,endYears)) > 0) firstInvasionTime[j,k] = min(c(startYears,endYears))
					}
			}
		for (j in 1:length(states))
			{
				if (!is.na(median(firstInvasionTime[,j],na.rm=T)))
					{
						index = (((median(firstInvasionTime[,j],na.rm=T)-minYear)/(maxYear-minYear))*(length(colour_scale)-1))+1
						states_cols[which(GADM_BRA_1$HASC_1==states[j])] = paste0(colour_scale[index],60)
					}	else	{
						states_cols[which(GADM_BRA_1$HASC_1==states[j])] = "gray95"
					}
			}
		states_cols_list[[i]] = states_cols
	}
within_state_transition_events_list = list()	
for (i in 1:length(beast_analyses))
	{
		within_state_transition_events = rep(NA, length(states))
		buffer = matrix(nrow=nberOfExtractionFiles, ncol=length(states))
		for (j in 1:nberOfExtractionFiles)
			{
				tab = read.csv(paste0("Tree_extraction_files/",localTreesDirectories[i],"_ext/TreeExtractions_",j,".csv"), head=T)
				for (k in 1:length(states))
					{
						buffer[j,k] = dim(tab[which((tab[,"startState"]==states[k])&(tab[,"endState"]==states[k])),])[1]
					}
			}
		for (j in 1:length(states)) within_state_transition_events[j] = median(buffer[,j])
		within_state_transition_events_list[[i]] = within_state_transition_events
	}
transitions_list = list()
for (i in 1:length(beast_analyses))
	{
		transitions = matrix(0, nrow=length(states), ncol=length(states))
		for (j in 1:nberOfExtractionFiles)
			{
				tab = read.csv(paste0("Tree_extraction_files/",localTreesDirectories[i],"_ext/TreeExtractions_",j,".csv"), head=T)
				for (k in 1:length(states))
					{
						for (l in 1:length(states))
							{
								transitions[k,l] = transitions[k,l] + length(which((tab[,"startState"]==states[k])&(tab[,"endState"]==states[l])))
							}
					}
			}
		transitions = transitions/nberOfExtractionFiles; transitions_list[[i]] = transitions; diag(transitions) = NA
		if (i == 1)
			{
				minTransition = min(transitions, na.rm=T); maxTransition = max(transitions, na.rm=T)
			}	else	{
				if (minTransition > min(transitions, na.rm=T)) minTransition = min(transitions, na.rm=T)
				if (maxTransition < max(transitions, na.rm=T)) maxTransition = max(transitions, na.rm=T)
			}
	}

pdf(paste0("Dispersal_history_1_NEW.pdf"), width=9, height=3)
par(mfrow=c(1,3), oma=c(0,0,0,0), mar=c(0,0,0,0), mgp=c(0,0.1,0), lwd=0.3, bty="o")
for (i in 1:length(beast_analyses))
	{
		mcc = read.csv(paste0("Tree_extraction_files/",localTreesDirectories[i],".csv"), head=T)
		mcc = mcc[order(mcc[,"startYear"]),]; mcc1 = mcc[1,]; mcc2 = mcc[c(2:dim(mcc)[1]),]
		mcc2 = mcc2[order(mcc2[,"endYear"]),]; mcc = rbind(mcc1,mcc2)
		endYears_indices = (((mcc[,"endYear"]-minYear)/(maxYear-minYear))*(length(colour_scale)-1))+1
		endYears_colours = colour_scale[endYears_indices]
		polygons_colours = rep(NA, length(polygons_list[[i]]))
		for (j in 1:length(polygons_list[[i]]))
			{
				date = as.numeric(names(polygons_list[[i]][[j]]))
				polygon_index = round((((date-minYear)/(maxYear-minYear))*(length(colour_scale)-1))+1)
				polygons_colours[j] = paste0(colour_scale[polygon_index],"20")
			}
		plot(GADM_BRA_0$geometry, col="gray90", border=NA, ann=F, axes=F)
		plot(GADM_BRA_1$geometry, col=NA, border="white", lwd=0.3, add=T)
		plot(GADM_BRA_0$geometry, col=NA, border="gray70", lwd=0.7, add=T)
		mtext(segment_names[i], side=3, cex=0.55, col="gray30", at=-68.2, line=-13.3)
		if (i == length(beast_analyses))
			{
				rast = raster(matrix(nrow=1, ncol=2)); rast[1] = minYear; rast[2] = maxYear
				plot(rast, legend.only=T, add=T, col=colour_scale, legend.width=0.5, legend.shrink=0.3, smallplot=c(0.052,0.313,0.385,0.400),
	 				 legend.args=list(text="", cex=0.7, col="gray30"), horizontal=T,
					 axis.args=list(cex.axis=0.8, lwd=0, lwd.tick=0.3, col.tick="gray30", tck=-1.0, col="gray30", col.axis="gray30", line=0, mgp=c(0,0.24,0), at=seq(2016,2024,4)))
			}
		for (j in 1:length(polygons_list[[i]]))
			{
				for (k in 1:length(polygons_list[[i]][[j]]@polygons))
					{
						# polygons_list[[i]][[j]]@polygons[[k]] = checkPolygonsHoles(polygons_list[[i]][[j]]@polygons[[k]])
					}
				pol = polygons_list[[i]][[j]]; crs(pol) = crs(GADM_BRA_0)
				if (croppingPolygons == TRUE) pol = crop(pol, GADM_BRA_0)
				plot(pol, axes=F, col=polygons_colours[j], add=T, border=NA)
			}
		for (j in 1:dim(mcc)[1])
			{
				curvedarrow(cbind(mcc[j,"startLon"],mcc[j,"startLat"]), cbind(mcc[j,"endLon"],mcc[j,"endLat"]), arr.length=0,
				  		  	arr.width=0, lwd=0.3, lty=1, lcol="gray30", arr.col=NA, arr.pos=F, curve=0.1, dr=NA, endhead=F)
			}
		for (j in dim(mcc)[1]:1)
			{
				if (!mcc[j,"node1"]%in%mcc[,"node2"])
					{
						startYears_index = (((mcc[j,"startYear"]-minYear)/(maxYear-minYear))*(length(colour_scale)-1))+1
						points(mcc[j,"startLon"], mcc[j,"startLat"], pch=16, col=colour_scale[startYears_index], cex=0.8)
						points(mcc[j,"startLon"], mcc[j,"startLat"], pch=1, col="gray30", lwd=0.3, cex=0.8)
					}
				points(mcc[j,"endLon"], mcc[j,"endLat"], pch=16, col=endYears_colours[j], cex=0.8)
				points(mcc[j,"endLon"], mcc[j,"endLat"], pch=1, col="gray30", lwd=0.3, cex=0.8)
			}
	}
dev.off()

pdf(paste0("Dispersal_history_2_NEW.pdf"), width=9, height=3)
par(mfrow=c(1,3), oma=c(0,0,0,0), mar=c(0,0,0,0), mgp=c(0,0.1,0), lwd=0.3, bty="o")
multiplier1 = 100; multiplier2 = 2; multiplier3 = 0.0075
for (i in 1:length(beast_analyses))
	{
		states_cols = states_cols_list[[i]]; states_cols[states_cols=="gray95"] = NA
		plot(GADM_BRA_1$geometry, col="gray90", border=NA, ann=F, axes=F)
		plot(GADM_BRA_1$geometry, col=states_cols, border="white", lwd=0.3, add=T)
		plot(GADM_BRA_0$geometry, col=NA, border="gray70", lwd=0.7, add=T)
		mtext(segment_names[i], side=3, cex=0.55, col="gray30", at=-68.2, line=-13.3)
		if (i == length(beast_analyses))
			{
				rast = raster(matrix(nrow=1, ncol=2)); rast[1] = minYear; rast[2] = maxYear
				plot(rast, legend.only=T, add=T, col="gray90", legend.width=0.5, legend.shrink=0.3, smallplot=c(0.052,0.313,0.385,0.400),
	 				 legend.args=list(text="", cex=0.7, col=NA), horizontal=T,
					 axis.args=list(cex.axis=0.8, lwd=0, lwd.tick=0, col.tick=NA, tck=0, col=NA, col.axis=NA, line=0, mgp=c(0,0.24,0), at=c()))
				plot(rast, legend.only=T, add=T, col=paste0(colour_scale,60), legend.width=0.5, legend.shrink=0.3, smallplot=c(0.052,0.313,0.385,0.400),
	 				 legend.args=list(text="", cex=0.7, col="gray30"), horizontal=T,
					 axis.args=list(cex.axis=0.8, lwd=0, lwd.tick=0.3, col.tick="gray30", tck=-1.0, col="gray30", col.axis="gray30", line=0, mgp=c(0,0.24,0), at=seq(2016,2024,4)))
			}
		CEX = sqrt((multiplier1*(within_state_transition_events_list[[i]]/150))/pi)
		points(states_gravity_points_list[[i]], cex=CEX, pch=16, col="#4D4D4D50") # "#DE432750"
		if (i == 1)
			{
				CEX = sqrt((multiplier1*(50/150))/pi); points(cbind(-65,-20), cex=CEX, pch=16, col="#4D4D4D50") # "#DE432750"
				CEX = sqrt((multiplier1*(200/150))/pi); points(cbind(-65,-20), cex=CEX, pch=16, col="#4D4D4D50") # "#DE432750"
				CEX = sqrt((multiplier1*(500/150))/pi); points(cbind(-65,-20), cex=CEX, pch=16, col="#4D4D4D50") # "#DE432750"
			}
		transitions = transitions_list[[i]]
		for (j in 1:dim(transitions)[1])
			{
				for (k in 1:dim(transitions)[2])
					{
						LWD = (((transitions[j,k]-minTransition)/(maxTransition-minTransition))*multiplier2)+0.5
						arrow = (multiplier3*(transitions[j,k]/maxTransition))+0.1
						if ((j != k)&(transitions[j,k] >= 0.25))
							{
								if (transitions[j,k] < 0.95) LTY = 2
								if (transitions[j,k] >= 0.95) LTY = 1
								curvedarrow(cbind(states_gravity_points_list[[i]][j,1],states_gravity_points_list[[i]][j,2]), cbind(states_gravity_points_list[[i]][k,1],states_gravity_points_list[[i]][k,2]),
											arr.length=0.1, arr.width=arrow, lwd=LWD, lty=LTY, lcol="black", arr.col="black", arr.pos=0.5, curve=0.15, dr=NA, endhead=F, arr.type="triangle")	
							}
					}
			}
	}
dev.off()

selected_variables = which(raster_names%in%c("Evergreen broadleaf forests","Population density","A. aegypti ecological suitability",
											 "Banana harvested area (log)","Cocoa harvested area (log)","Mean monthly temperature"))
pdf(paste0("Dispersal_history_3_NEW.pdf"), width=9, height=6)
par(mfrow=c(2,3), oma=c(0,0,0,0), mar=c(0,0,0,0), mgp=c(0,0.1,0), lwd=0.3, bty="o", col="gray30")
I = 2; multiplier1 = 100; multiplier2 = 2; multiplier3 = 0.0075
for (i in 1:length(selected_variables))
	{
		plot(GADM_BRA_1$geometry, col=NA, border=NA, ann=F, axes=F); envVariable = envVariables[[selected_variables[i]]]
		if (raster_names[selected_variables[i]] == "Population density (log)") envVariable[!is.na(envVariable[])] = log(envVariable[!is.na(envVariable[])]+1)
		if (raster_names[selected_variables[i]] == "Urban & built-up areas (log)") envVariable[!is.na(envVariable[])] = log(envVariable[!is.na(envVariable[])]+1)
		if (grepl("harvested area",raster_names[selected_variables[i]])) envVariable[!is.na(envVariable[])] = log(envVariable[!is.na(envVariable[])]+1)
		colour_scale_i = paste0(colour_scales[[selected_variables[i]]],"BF") # "8C" and "BF" corresponds to 65% and 75% transparency, repectively
		plot(envVariable, axes=F, ann=F, legend=F, add=T, col=colour_scale_i)
		plot(envVariable, legend.only=T, add=T, col=colour_scale_i, legend.width=0.5, legend.shrink=0.3, smallplot=c(0.150,0.165,0.1,0.4),
	 		 legend.args=list(text="", cex=0.9, col="gray30"), horizontal=F,
			 axis.args=list(cex.axis=0.9, lwd=0, lwd.tick=0.3, col.tick="gray30", tck=-1, col="gray30", col.axis="gray30", line=0, mgp=c(0,0.5,0)))
		plot(GADM_BRA_1$geometry, col=NA, border="white", lwd=0.3, add=T); plot(GADM_BRA_0$geometry, col=NA, border="gray70", lwd=0.7, add=T)
		CEX = sqrt((multiplier1*(within_state_transition_events_list[[I]]/150))/pi)
		points(states_gravity_points_list[[I]], cex=CEX, pch=16, col="#4D4D4D80") # "#DE432750"
		transitions = transitions_list[[I]]
		for (j in 1:dim(transitions)[1])
			{
				for (k in 1:dim(transitions)[2])
					{
						LWD = (((transitions[j,k]-minTransition)/(maxTransition-minTransition))*multiplier2)+0.5
						arrow = (multiplier3*(transitions[j,k]/maxTransition))+0.1
						if ((j != k)&(transitions[j,k] >= 0.25))
							{
								if (transitions[j,k] < 0.95) LTY = 2
								if (transitions[j,k] >= 0.95) LTY = 1
								curvedarrow(cbind(states_gravity_points_list[[I]][j,1],states_gravity_points_list[[I]][j,2]), cbind(states_gravity_points_list[[I]][k,1],states_gravity_points_list[[I]][k,2]),
											arr.length=0.1, arr.width=arrow, lwd=LWD, lty=LTY, lcol="black", arr.col="black", arr.pos=0.5, curve=0.15, dr=NA, endhead=F, arr.type="triangle")	
							}
					}
			}
	}
dev.off()

pdf(paste0("Dispersal_history_4_NEW.pdf"), width=9, height=5)
par(mfrow=c(3,3), oma=c(0,0,0,0), mar=c(2.5,2.5,0,0), mgp=c(0,0.1,0), lwd=0.3, bty="o", col="gray30")
for (i in 1:length(beast_analyses))
	{
		swf_median = read.table(paste0("All_dispersal_statistics/OROV_segment",segments[i],"_median_spatial_wavefront_distance.txt"), header=T)
		swf_95pHPD = read.table(paste0("All_dispersal_statistics/OROV_segment",segments[i],"_95%HPD_spatial_wavefront_distance.txt"), header=T)
		swf = cbind(swf_median, swf_95pHPD[,2:3]); colnames(swf) = c("time","median","95pHDP_lower","95pHDP_upper") # "swf" is for "spatial wavefront"
		swf = swf[which((swf[,"time"]>=minYear)&(swf[,"time"]<maxYear)),]
		xMin = min(swf[,"time"]); xMax = max(swf[,"time"]); timeSlice = swf[1,"time"]-swf[2,"time"]; xMin = 2020.9
		yMin = min(swf[,"95pHDP_lower"], na.rm=T); yMax = max(swf[,"95pHDP_upper"], na.rm=T); yMin = 0; yMax = 4000
		colours = paste0(colour_scale[(((swf[,c("time")]-minYear)/(maxYear-minYear))*(length(colour_scale)-1))+1],60)
		plot(swf[,"time"], swf[,"median"], lwd=0.7, type="l", cex.axis=0.8, cex.lab=0.8, col="gray30", axes=F, xlab=NA, ylab=NA, xlim=c(xMin,xMax), ylim=c(yMin,yMax))
		xx_l = c(swf[,c("time")],rev(swf[,c("time")])); yy_l = c(swf[,"95pHDP_lower"],rev(swf[,"95pHDP_upper"]))
		getOption("scipen"); opt = options("scipen"=20); polygon(xx_l,yy_l,col=rgb(187/255,187/255,187/255,0.25),border=0)
		for (j in 1:length(swf[,"time"]))
			{
				x1 = swf[j,"time"]-(timeSlice/2); x2 = swf[j,"time"]+(timeSlice/2)
				y1 = swf[j,"95pHDP_lower"]-2000; y2 = swf[j,"95pHDP_upper"]+2000
				polygon(c(x1,x2,x2,x1), c(y1,y1,y2,y2), col="gray90", border=NA)
				polygon(c(x1,x2,x2,x1), c(y1,y1,y2,y2), col=colours[j], border=NA)
			}
		getOption("scipen"); opt = options("scipen"=20); polygon(xx_l,yy_l,col=NA,border="gray30")
		lines(swf[,"time"], swf[,"median"], lwd=1.0, type="l", cex.axis=0.8, cex.lab=0.8, col="gray30")
		for (j in c(2021:2024)) abline(v=j, lty=2, col="gray30", lwd=0.3)
		axis(side=1, lwd.tick=0.3, cex.axis=1.0, lwd=0.3, tck=-0.030, col="gray30", col.axis="gray30", col.tick="gray30", mgp=c(0,0.35,0), at=seq(2020,2025))
		axis(side=2, lwd.tick=0.3, cex.axis=1.0, lwd=0.3, tck=-0.030, col="gray30", col.axis="gray30", col.tick="gray30", mgp=c(1,0.35,0), at=seq(0,4500,1500))
		if (i == 1) mtext("Distance from epidemic origin (km)", side=2, col="gray30", cex=0.7, line=1.2, las=3)
	}
for (i in 1:length(beast_analyses))
	{
		wdc_median = read.table(paste0("All_dispersal_statistics/OROV_segment",segments[i],"_median_weighted_diffusion_coefficient.txt"), header=T)[,1:2]
		wdc_95pHPD = read.table(paste0("All_dispersal_statistics/OROV_segment",segments[i],"_95%HPD_weighted_diffusion_coefficient.txt"), header=T)
		wdc = cbind(wdc_median, wdc_95pHPD[,2:3]); colnames(wdc) = c("time","median","95pHDP_lower","95pHDP_upper") # "wdc" is for "weighted diffusion coefficient"
		wdc[,2:dim(wdc)[2]] = wdc[,2:dim(wdc)[2]]/365.25 # to get the WDC estimates in km2/day and not per year
		wdc = wdc[which((wdc[,"time"]>=minYear)&(wdc[,"time"]<maxYear)),]
		xMin = min(wdc[,"time"]); xMax = max(wdc[,"time"]); timeSlice = wdc[1,"time"]-wdc[2,"time"]; xMin = 2020.9
		yMin = min(wdc[,"95pHDP_lower"], na.rm=T); yMax = max(wdc[,"95pHDP_upper"], na.rm=T); yMin = 0; yMax = 35000
		colours = paste0(colour_scale[(((wdc[,c("time")]-minYear)/(maxYear-minYear))*(length(colour_scale)-1))+1],60)
		plot(wdc[,"time"], wdc[,"median"], lwd=0.7, type="l", cex.axis=0.8, cex.lab=0.8, col="gray30", axes=F, xlab=NA, ylab=NA, xlim=c(xMin,xMax), ylim=c(yMin,yMax))
		xx_l = c(wdc[,c("time")],rev(wdc[,c("time")])); yy_l = c(wdc[,"95pHDP_lower"],rev(wdc[,"95pHDP_upper"]))
		getOption("scipen"); opt = options("scipen"=20); polygon(xx_l,yy_l,col=rgb(187/255,187/255,187/255,0.25),border=0)
		for (j in 1:length(wdc[,"time"]))
			{
				x1 = wdc[j,"time"]-(timeSlice/2); x2 = wdc[j,"time"]+(timeSlice/2)
				y1 = wdc[j,"95pHDP_lower"]-(2500000/365.25); y2 = wdc[j,"95pHDP_upper"]+(2500000/365.25)
				polygon(c(x1,x2,x2,x1), c(y1,y1,y2,y2), col="gray90", border=NA)
				polygon(c(x1,x2,x2,x1), c(y1,y1,y2,y2), col=colours[j], border=NA)
			}
		getOption("scipen"); opt = options("scipen"=20); polygon(xx_l,yy_l,col=NA,border="gray30")
		lines(wdc[,"time"], wdc[,"median"], lwd=1.0, type="l", cex.axis=0.8, cex.lab=0.8, col="gray30")
		for (j in c(2021:2024)) abline(v=j, lty=2, col="gray30", lwd=0.3)
		axis(side=1, lwd.tick=0.3, cex.axis=1.0, lwd=0.3, tck=-0.030, col="gray30", col.axis="gray30", col.tick="gray30", mgp=c(0,0.35,0), at=seq(2020,2025))
		axis(side=2, lwd.tick=0.3, cex.axis=1.0, lwd=0.3, tck=-0.030, col="gray30", col.axis="gray30", col.tick="gray30", mgp=c(1,0.35,0), at=c(0,15000,30000,45000))
		if (i == 1) mtext("Weighted diffusion coefficient (km2/day)", side=2, col="gray30", cex=0.7, line=1.2, las=3)
	}
for (i in 1:length(beast_analyses))
	{
		three_colours = c("#FFFFFF",paste0(colorRampPalette(brewer.pal(6,"YlOrBr"))(6)[c(3:5)]))
		three_colours = c("#FFFFFF",paste0(colorRampPalette(brewer.pal(6,"Blues"))(6)[c(4:6)]))
		localTreesDirectory = paste0("Tree_extraction_files/",localTreesDirectories[i],"_ext")
		geo_dists = rep(NA, nrow=dim(tab)[1]*100); durations = rep(NA, nrow=dim(tab)[1]*100)
		for (j in 1:100) # to only consider 100 posterior trees
			{
				tab = read.csv(paste0(localTreesDirectory,"/TreeExtractions_",j,".csv"), header=T)
				dists_geo = rdist.earth(tab[,c("startLon","startLat")],tab[,c("endLon","endLat")], miles=F)			
				geo_dists[((dim(tab)[1]*(j-1))+1):((dim(tab)[1]*(j-1))+dim(tab)[1])] = diag(rdist.earth(tab[,c("startLon","startLat")],tab[,c("endLon","endLat")],miles=F))
				durations[((dim(tab)[1]*(j-1))+1):((dim(tab)[1]*(j-1))+dim(tab)[1])] = tab[,"length"]
			}
		geo_dists[which(geo_dists==0)] = 0.001
		H = Hpi(cbind(log(geo_dists), log(((geo_dists^2)/(4*durations)))))
		kde = kde(cbind(log(geo_dists), log(((geo_dists^2)/(4*durations)))), H=H)
		# H = Hpi(cbind(log(geo_dists)+5, log(((geo_dists^2)/(4*durations)))+5))
		# kde = kde(cbind(log(geo_dists)+5, log(((geo_dists^2)/(4*durations)))+5), H=H)
		# kde$x = kde$x-5; kde$eval.points[[1]] = kde$eval.points[[1]]-5; kde$eval.points[[2]] = kde$eval.points[[2]]-5
		xLab = "geographic distance (km, log)"; yLab = "branch diffusion coeffcient (km2/day, log)"
		plot(kde, display="filled.contour2", cont=c(50,75,95), col=three_colours, axes=F, ann=F, xlim=c(-4.5,8.3), ylim=c(-6.2,17.5))
		axis(side=1, lwd.tick=0.3, cex.axis=1.0, lwd=0.3, tck=-0.030, col="gray30", col.axis="gray30", col.tick="gray30", mgp=c(0,0.35,0), at=(seq(-5,15,2)))
		axis(side=2, lwd.tick=0.3, cex.axis=1.0, lwd=0.3, tck=-0.030, col="gray30", col.axis="gray30", col.tick="gray30", mgp=c(1,0.35,0), at=(seq(-10,20,5)))
		title(xlab=xLab, cex.lab=0.9, mgp=c(1.4,0,0), col.lab="gray30"); # title(ylab=yLab, cex.lab=0.9, mgp=c(1.5,0,0), col.lab="gray30")
		if (i == 3)
			{
				legend(x=4.5, y=7, c("","95% HPD","75% HPD","50% HPD"), text.col="gray30", pch=16, pt.cex=2, col=three_colours, box.lty=0, cex=1, y.intersp=1.1)
				legend(x=4.5, y=7, c("","95% HPD","75% HPD","50% HPD"), text.col=rgb(0,0,0,0), pch=1, pt.cex=2, col="gray30", box.lty=0, cex=1, y.intersp=1.1)
			}
	}
dev.off()

# 7. Visualising the evolution of the environmental values in which viral lineages are evolving

startTime = minYear; endTime = maxYear; timeSlices = 200; slidingWindow = 1/12; showingPlots = F; nberOfCores = 10
for (i in 1:length(beast_analyses))
	{
		localTreesDirectory = paste0("Tree_extraction_files/",localTreesDirectories[i],"_ext")
		outputName = paste0("Environmental_values/OROV_",segments[i])
		for (j in 1:length(envVariables_mod))
			{
				if (!file.exists(paste0("Environmental_values/OROV_",segments[i],"_mean_",names(envVariables_mod[[j]]),".csv")))
					{
						envVariables_list = list(envVariables_mod[[j]])
						spreadValues(localTreesDirectory, nberOfExtractionFiles=100, envVariables_list, startTime, endTime,
									 timeSlices, slidingWindow, showingPlots, outputName, nberOfCores, simulations=F)
					}
			}
	}
xMins = c(2020.9, 2020.9, 2020.9); xPos = xMins-0.065
selected_variables = which(raster_names%in%c("Evergreen broadleaf forests","Population density","A. aegypti ecological suitability",
											 "Banana harvested area (log)","Cocoa harvested area (log)","Mean monthly temperature"))
for (i in 2:2) # 1:length(beast_analyses))
	{
		pdf(paste0("OROV_",segments[i],"_envValues_NEW.pdf"), width=9, height=2.5); # dev.new(width=9, height=8)
		par(mfrow=c(2,3), oma=c(0,0,0.5,1), mar=c(2.0,2.5,1.5,0), mgp=c(0,0.1,0), lwd=0.3, bty="o", col="gray30")
		yMins = rep(9999999, length(envVariables)); yMaxs = rep(-9999999, length(envVariables))
		for (j in c(selected_variables))
			{
				tab1 = read.csv(paste0("Environmental_values/OROV_",segments[i],"_median_",names(envVariables[[j]]),".csv"), header=T)
				tab2 = read.csv(paste0("Environmental_values/OROV_",segments[i],"_95%HPD_",names(envVariables[[j]]),".csv"), header=T)
				tab2 = tab2[which(!is.na(tab1[,2])),]; tab1 = tab1[which(!is.na(tab1[,2])),]
				slicedTimes = tab1[,"time"]; environmentalMedianValue = tab1[,2]; lower = tab2[,2]; upper = tab2[,3]
				xx_l = c(slicedTimes,rev(slicedTimes)); yy_l = c(lower,rev(upper))			
				if (yMins[j] > min(tab2[,2],na.rm=T)) yMins[j] = min(tab2[,2],na.rm=T)
				if (yMaxs[j] < max(tab2[,3],na.rm=T)) yMaxs[j] = max(tab2[,3],na.rm=T)
				col1 = colour_scales[[j]][100]; col2 = paste0(col1,"30")
				plot(cbind(slicedTimes,environmentalMedianValue), lwd=0.7, type="l", cex.axis=0.8, cex.lab=0.8, 
					 col.axis="gray30", col=col1, axes=F, xlab=NA, ylab=NA, xlim=c(xMins[i], mostRecentSamplingDates[i]), ylim=c(yMins[j],yMaxs[j]))
				for (k in c(2021,2024)) abline(v=k, lty=2, col="gray30", lwd=0.3)
				mtext(raster_names[j], side=3, col="gray30", cex=0.65, line=0.2, las=1, at=xMins[i]+((mostRecentSamplingDates[i]-xMins[i])/2))
				getOption("scipen"); opt = options("scipen"=20); polygon(xx_l, yy_l, col=col2, border=0) # col = rgb(187/255,187/255,187/255,0.25)
				axis(side=1, lwd.tick=0.3, cex.axis=0.9, lwd=0.3, tck=-0.060, col.axis="gray30", col="gray30", mgp=c(1,0.25,0), pos=yMins[j]-((yMaxs[j]-yMins[j])/10), at=c(2020:2025))
				ticks = axTicks(2, axp=NULL, usr=NULL, log=NULL, nintLog=NULL)
				if (length(ticks) > 3) ticks = ticks[seq(1,length(ticks),2)]
				dif = ticks[2]-ticks[1]; ats = c(ticks[1]-dif,ticks,ticks[length(ticks)]+dif)
				axis(side=2, lwd.tick=0.3, cex.axis=0.9, lwd=0.3, tck=-0.060, col.axis="gray30", col="gray30", mgp=c(1,0.30,0), pos=xPos[i], at=ats)
			}
		dev.off()
	}

# 8. Generating a null dispersal model (either through a randomisation or simulation procedure)

nberOfSimulations = 100; nberOfRandomisations = nberOfSimulations
for (i in 1:length(beast_analyses)) # to randomise the branch positions within the study area (delimitated by a minimum convex hull polygon)
	{
		localTreesDirectory = paste0("Tree_extraction_files/",localTreesDirectories[i],"_ext"); randomProcedure = 3; nberOfCores = 1
		treesRandomisation(localTreesDirectory, nberOfRandomisations, list(background), randomProcedure, nberOfCores)
	}
for (i in 3:length(beast_analyses))
	{
		localTreesDirectory = paste0("Tree_extraction_files/",localTreesDirectories[i],"_ext2")
		for (j in 1:nberOfRandomisations)
			{
				pdf(paste0(localTreesDirectory,"/TreeRandomisation_",j,".pdf"), width=5, height=5)
				par(oma=c(0,0,0,0), mar=c(0,0,0,0), mgp=c(0,0.1,0), lwd=0.2, bty="o")
				plot(GADM_BRA_0$geometry, col="gray90", border=NA, ann=F, axes=F)
				plot(GADM_BRA_1$geometry, col=NA, border="white", lwd=0.3, add=T)
				plot(GADM_BRA_0$geometry, col=NA, border="gray70", lwd=0.7, add=T)
				tab = read.csv(paste0(localTreesDirectory,"/TreeRandomisation_",j,".csv"), head=T)
				for (k in 1:dim(tab)[1])
					{
						curvedarrow(cbind(tab[k,"startLon"],tab[k,"startLat"]), cbind(tab[k,"endLon"],tab[k,"endLat"]), arr.length=0,
				  				  	arr.width=0, lwd=0.3, lty=1, lcol="gray30", arr.col=NA, arr.pos=F, curve=0.1, dr=NA, endhead=F)
					}
				endYears_indices = (((tab[,"endYear"]-minYear)/(maxYear-minYear))*(length(colour_scale)-1))+1
				endYears_colours = colour_scale[endYears_indices]
				for (k in dim(tab)[1]:1)
					{
						if (!tab[k,"node1"]%in%tab[,"node2"])
							{
								startYears_index = (((tab[k,"startYear"]-minYear)/(maxYear-minYear))*(length(colour_scale)-1))+1
								points(tab[k,"startLon"], tab[k,"startLat"], pch=16, col=colour_scale[startYears_index], cex=0.6)
								points(tab[k,"startLon"], tab[k,"startLat"], pch=1, col="gray30", lwd=0.3, cex=0.6)
							}
						points(tab[k,"endLon"], tab[k,"endLat"], pch=16, col=endYears_colours[k], cex=0.6)
						points(tab[k,"endLon"], tab[k,"endLat"], pch=1, col="gray30", lwd=0.3, cex=0.6)
					}
				rect(-11, 41.5, 29, 65, lwd=0.2, border="gray30")
				dev.off()
			}
	}
for (i in 1:length(beast_analyses)) # to conduct RRW simulation along the phylogenetic branches (still within a minimum convex hull polygon)
	{
		localTreesDirectory = paste0("Tree_extraction_files/",localTreesDirectories[i],"_ext")
		trees = readAnnotatedNexus(paste0("Tree_extraction_files/",localTreesDirectories[i],".trees"))
		log = read.table(paste0("Tree_extraction_files/",localTreesDirectories[i],".log"), head=T)
		for (j in 1:nberOfSimulations)
			{
				tab = read.csv(paste0(localTreesDirectory,"/TreeExtractions_",j,".csv"), header=T)
				all_rates = c(); geoDists = matrix(nrow=dim(tab)[1], ncol=1)
				for (k in 1:length(trees[[j]]$annotations))
					{
						all_rates = c(all_rates, trees[[j]]$annotations[[k]]$Location.rate)
					}
				for (k in 1:dim(tab)[1])
					{
						x1 = cbind(tab[k,"startLon"], tab[k,"startLat"])
						x2 = cbind(tab[k,"endLon"], tab[k,"endLat"])
						geoDists[k,1] = rdist.earth(x1, x2, miles=F, R=NULL)
					}
				ancestID = which(!tab[,"node1"]%in%tab[,"node2"])[1]
				ancestPosition = c(tab[ancestID,"startLon"], tab[ancestID,"startLat"])
				col11 = log[j,"treeLengthPrecision1"]
				col12 = log[j,"treeLengthPrecision3"]
				col22 = log[j,"treeLengthPrecision2"]
				my_prec = c(col11, col12, col12, col22)
				reciprocalRates = TRUE; n1 = 100; n2 = 100
				showingPlots = TRUE; newPlot = TRUE
				showingPlots = FALSE; newPlot = FALSE
				my_var = solve(matrix(my_prec,nrow=2))
				sigma1 = sqrt(my_var[1,1]); sigma2 = sqrt(my_var[2,2])
				sigmas = c(sigma1, sigma2); # source("simulatorRRW1_m.r")
				cor = my_var[1,2]/(sqrt(my_var[1,1])*sqrt(my_var[2,2]))
				tree = trees[[j]]; rates = all_rates
				if (!file.exists(paste0(localTreesDirectory,"/TreeSimulations_",j,".csv")))
					{
						output = simulatorRRW1(tree, rates, sigmas, cor, list(background), mostRecentSamplingDates[i],
											   ancestPosition, reciprocalRates, n1, n2, showingPlots, newPlot)
						write.csv(output, paste0(localTreesDirectory,"/TreeSimulations_",j,".csv"), row.names=F, quote=F)
						phytools::writeNexus(tree, paste0(localTreesDirectory,"/TreeSimulations_",j,".tree"))
						if (i == 3)
							{
								pdf(paste0(localTreesDirectory,"/TreeSimulations_",j,".pdf"), width=5, height=5)
								par(oma=c(0,0,0,0), mar=c(0,0,0,0), mgp=c(0,0.1,0), lwd=0.2, bty="o")
								plot(GADM_BRA_0$geometry, col="gray90", border=NA, ann=F, axes=F)
								plot(GADM_BRA_1$geometry, col=NA, border="white", lwd=0.3, add=T)
								plot(GADM_BRA_0$geometry, col=NA, border="gray70", lwd=0.7, add=T)
								tab = read.csv(paste0(localTreesDirectory,"/TreeSimulations_",j,".csv"), head=T)
								for (k in 1:dim(tab)[1])
									{
										curvedarrow(cbind(tab[k,"startLon"],tab[k,"startLat"]), cbind(tab[k,"endLon"],tab[k,"endLat"]), arr.length=0,
										  		  	arr.width=0, lwd=0.3, lty=1, lcol="gray30", arr.col=NA, arr.pos=F, curve=0.1, dr=NA, endhead=F)
									}
								endYears_indices = (((tab[,"endYear"]-minYear)/(maxYear-minYear))*(length(colour_scale)-1))+1
								endYears_colours = colour_scale[endYears_indices]
								for (k in dim(tab)[1]:1)
									{
										if (!tab[k,"node1"]%in%tab[,"node2"])
											{
												startYears_index = (((tab[k,"startYear"]-minYear)/(maxYear-minYear))*(length(colour_scale)-1))+1
												points(tab[k,"startLon"], tab[k,"startLat"], pch=16, col=colour_scale[startYears_index], cex=0.6)
												points(tab[k,"startLon"], tab[k,"startLat"], pch=1, col="gray30", lwd=0.3, cex=0.6)
											}
										points(tab[k,"endLon"], tab[k,"endLat"], pch=16, col=endYears_colours[k], cex=0.6)
										points(tab[k,"endLon"], tab[k,"endLat"], pch=1, col="gray30", lwd=0.3, cex=0.6)
									}
								rect(-11, 41.5, 29, 65, lwd=0.2, border="gray30")
								dev.off()
							}
					}
			}
	}

# 9. Testing the impact of environmental factors on the lineage dispersal direction/position

nberOfSimulations = 100; nberOfRandomisations = nberOfSimulations
for (i in 1:length(beast_analyses)) # to extract the environmental values at the "end" position of each phylogenetic branch
	{
		localTreesDirectory = paste0("Tree_extraction_files/",localTreesDirectories[i],"_ext")
		for (j in 1:nberOfSimulations)
			{
				obs = read.csv(paste0(localTreesDirectory,"/TreeExtractions_",j,".csv"), header=T)
				ran = read.csv(paste0(localTreesDirectory,"/TreeRandomisation_",j,".csv"), header=T)
				sim = read.csv(paste0(localTreesDirectory,"/TreeSimulations_",j,".csv"), header=T)
				envValues_obs = matrix(nrow=dim(obs)[1], ncol=length(envVariables))
				envValues_ran = matrix(nrow=dim(ran)[1], ncol=length(envVariables))
				envValues_sim = matrix(nrow=dim(sim)[1], ncol=length(envVariables))
				colnames(envValues_obs) = gsub(" ","_",gsub("A\\.","A",envVariableNames))
				colnames(envValues_ran) = gsub(" ","_",gsub("A\\.","A",envVariableNames))
				colnames(envValues_sim) = gsub(" ","_",gsub("A\\.","A",envVariableNames))
				for (k in 1:length(envVariables))
					{
						envValues_obs[,k] = raster::extract(envVariables[[k]], SpatialPoints(obs[,c("endLon","endLat")]))
						envValues_ran[,k] = raster::extract(envVariables[[k]], SpatialPoints(ran[,c("endLon","endLat")]))
						envValues_sim[,k] = raster::extract(envVariables[[k]], SpatialPoints(sim[,c("endLon","endLat")]))
					}
				write.csv(envValues_obs, paste0(localTreesDirectory,"/EnvValues_obs_",j,".csv"), row.names=F, quote=F)
				write.csv(envValues_ran, paste0(localTreesDirectory,"/EnvValues_ran_",j,".csv"), row.names=F, quote=F)
				write.csv(envValues_sim, paste0(localTreesDirectory,"/EnvValues_sim_",j,".csv"), row.names=F, quote=F)
			}
	}
for (i in 1:length(beast_analyses)) # to compute the difference between the environmental values extracted at the "end" and "start" node of each branch
	{
		localTreesDirectory = paste0("Tree_extraction_files/",localTreesDirectories[i],"_ext")
		for (j in 1:nberOfSimulations)
			{
				obs = read.csv(paste0(localTreesDirectory,"/TreeExtractions_",j,".csv"), header=T)
				ran = read.csv(paste0(localTreesDirectory,"/TreeRandomisation_",j,".csv"), header=T)
				sim = read.csv(paste0(localTreesDirectory,"/TreeSimulations_",j,".csv"), header=T)
				envDifferences_obs = matrix(nrow=dim(obs)[1], ncol=length(envVariables))
				envDifferences_ran = matrix(nrow=dim(ran)[1], ncol=length(envVariables))
				envDifferences_sim = matrix(nrow=dim(sim)[1], ncol=length(envVariables))
				colnames(envDifferences_obs) = gsub(" ","_",gsub("A\\.","A",envVariableNames))
				colnames(envDifferences_ran) = gsub(" ","_",gsub("A\\.","A",envVariableNames))
				colnames(envDifferences_sim) = gsub(" ","_",gsub("A\\.","A",envVariableNames))
				for (k in 1:length(envVariables))
					{
				envDifferences_obs[,k] = raster::extract(envVariables[[k]], SpatialPoints(obs[,c("endLon","endLat")]))-raster::extract(envVariables[[k]], SpatialPoints(obs[,c("startLon","startLat")]))
				envDifferences_ran[,k] = raster::extract(envVariables[[k]], SpatialPoints(ran[,c("endLon","endLat")]))-raster::extract(envVariables[[k]], SpatialPoints(ran[,c("startLon","startLat")]))
				envDifferences_sim[,k] = raster::extract(envVariables[[k]], SpatialPoints(sim[,c("endLon","endLat")]))-raster::extract(envVariables[[k]], SpatialPoints(sim[,c("startLon","startLat")]))
					}
				write.csv(envDifferences_obs, paste0(localTreesDirectory,"/Differences_obs_",j,".csv"), row.names=F, quote=F)
				write.csv(envDifferences_ran, paste0(localTreesDirectory,"/Differences_ran_",j,".csv"), row.names=F, quote=F)
				write.csv(envDifferences_sim, paste0(localTreesDirectory,"/Differences_sim_",j,".csv"), row.names=F, quote=F)
			}
	}
for (i in 1:length(beast_analyses)) # to perform the D statistic tests, which are actually testing to what extent the environmental value trends are consistent across sampled posterior trees
	{
		localTreesDirectory = paste0("Tree_extraction_files/",localTreesDirectories[i],"_ext")
		BFs = matrix(nrow=length(envVariableNames), ncol=2); colnames(BFs) = c("lower","higher")
		row.names(BFs) = gsub(" ","_",gsub("A\\.","A",envVariableNames))
		for (j in 1:length(envVariableNames))
			{
				lowerMeanEnvValuesAfter2024 = 0; higherMeanEnvValuesAfter2024 = 0
				for (k in 1:nberOfSimulations)
					{
						obs = read.csv(paste0(localTreesDirectory,"/TreeExtractions_",k,".csv"), header=T)
						envVariableName = gsub("-",".",gsub("&",".",gsub(" ","_",gsub("\\. "," ",envVariableNames[j]))))
						envValues_obs = read.csv(paste0(localTreesDirectory,"/EnvValues_obs_",k,".csv"))[,envVariableName]
						meanEnvValuesBefore2024 = mean(envValues_obs[which(obs[,"endYear"]<2024)], na.rm=T)
						meanEnvValuesAfter2024 = mean(envValues_obs[which(obs[,"endYear"]>=2024)], na.rm=T)
						if (meanEnvValuesBefore2024 > meanEnvValuesAfter2024) lowerMeanEnvValuesAfter2024 = lowerMeanEnvValuesAfter2024+1
						if (meanEnvValuesBefore2024 < meanEnvValuesAfter2024) higherMeanEnvValuesAfter2024 = higherMeanEnvValuesAfter2024+1				
					}
				p = lowerMeanEnvValuesAfter2024/nberOfSimulations; BFs[j,"lower"] = round((p/(1-p))/(0.5/(1-0.5)),1)
				p = higherMeanEnvValuesAfter2024/nberOfSimulations; BFs[j,"higher"] = round((p/(1-p))/(0.5/(1-0.5)),1)
			}
		write.csv(BFs, paste0("All_seraphim_analyses/OROV_",segments[i],"_D_tests.csv"), quote=F)
	}
for (i in 1:length(beast_analyses))
	{
		localTreesDirectory = paste0("Tree_extraction_files/",localTreesDirectories[i],"_ext")
		for (j in 1:nberOfSimulations)
			{
				obs = read.csv(paste0(localTreesDirectory,"/TreeExtractions_",j,".csv"), header=T)
				ran = read.csv(paste0(localTreesDirectory,"/TreeRandomisation_",j,".csv"), header=T)
				sim = read.csv(paste0(localTreesDirectory,"/TreeSimulations_",j,".csv"), header=T)
				envValues_obs = read.csv(paste0(localTreesDirectory,"/EnvValues_obs_",j,".csv"))
				envValues_ran = read.csv(paste0(localTreesDirectory,"/envValues_ran_",j,".csv"))
				envValues_sim = read.csv(paste0(localTreesDirectory,"/envValues_sim_",j,".csv"))
				if (dim(obs)[1] != dim(envValues_obs)[1]) print(c(i,j,1))
				if (dim(ran)[1] != dim(envValues_ran)[1]) print(c(i,j,2))
				if (dim(sim)[1] != dim(envValues_sim)[1]) print(c(i,j,3))
			}
	}
for (i in 1:length(beast_analyses)) # to perform the E statistic tests over the entire period of time and for the two distinct time periods considered (< and >01/01/2024)
	{
		localTreesDirectory = paste0("Tree_extraction_files/",localTreesDirectories[i],"_ext")
		BFs = matrix(nrow=length(envVariableNames), ncol=4); colnames(BFs) = c("lower_randomisations","higher_randomisations","lower_simulations","higher_simulations")
		row.names(BFs) = gsub(" ","_",gsub("A\\.","A",envVariableNames)); meanEnvValues_obs_list = list(); meanEnvValues_ran_list = list(); meanEnvValues_sim_list = list()
		for (j in 1:length(envVariableNames))
			{
				meanEnvValues_obs = rep(NA, nberOfSimulations); meanEnvValues_ran = rep(NA, nberOfSimulations); meanEnvValues_sim = rep(NA, nberOfSimulations)
				lowerEnvValues_randomisations = 0; lowerEnvValues_simulations = 0
				for (k in 1:nberOfSimulations)
					{
						envVariableName = gsub("-",".",gsub("&",".",gsub(" ","_",gsub("\\. "," ",envVariableNames[j]))))
						envValues_obs1 = read.csv(paste0(localTreesDirectory,"/EnvValues_obs_",k,".csv"))[,envVariableName]
						envValues_ran1 = read.csv(paste0(localTreesDirectory,"/envValues_ran_",k,".csv"))[,envVariableName]
						envValues_sim1 = read.csv(paste0(localTreesDirectory,"/envValues_sim_",k,".csv"))[,envVariableName]
						indices = which(!is.na(envValues_obs1)); envValues_obs2 = envValues_obs1[indices]; meanEnvValues_obs[k] = mean(envValues_obs2, na.rm=T)
						indices = which(!is.na(envValues_ran1)); envValues_ran2 = envValues_ran1[indices]; meanEnvValues_ran[k] = mean(envValues_ran2, na.rm=T)
						indices = which(!is.na(envValues_sim1)); envValues_sim2 = envValues_sim1[indices]; meanEnvValues_sim[k] = mean(envValues_sim2, na.rm=T)
						if (meanEnvValues_obs[k] < meanEnvValues_ran[k]) lowerEnvValues_randomisations = lowerEnvValues_randomisations + 1
						if (meanEnvValues_obs[k] < meanEnvValues_sim[k]) lowerEnvValues_simulations = lowerEnvValues_simulations + 1				
					}
				p = lowerEnvValues_randomisations/nberOfSimulations; BFs[j,"lower_randomisations"] = round((p/(1-p))/(0.5/(1-0.5)),1)
				p = (1-lowerEnvValues_randomisations/nberOfSimulations); BFs[j,"higher_randomisations"] = round((p/(1-p))/(0.5/(1-0.5)),1)
				p = lowerEnvValues_simulations/nberOfSimulations; BFs[j,"lower_simulations"] = round((p/(1-p))/(0.5/(1-0.5)),1)
				p = (1-lowerEnvValues_simulations/nberOfSimulations); BFs[j,"higher_simulations"] = round((p/(1-p))/(0.5/(1-0.5)),1)
				meanEnvValues_obs_list[[j]] = meanEnvValues_obs; meanEnvValues_ran_list[[j]] = meanEnvValues_ran; meanEnvValues_sim_list[[j]] = meanEnvValues_sim
			}
		write.csv(BFs, paste0("All_seraphim_analyses/OROV_",segments[i],"_E_tests.csv"), quote=F)
		BFs = matrix(nrow=length(envVariableNames), ncol=4); colnames(BFs) = c("lower_randomisations","higher_randomisations","lower_simulations","higher_simulations")
		row.names(BFs) = gsub(" ","_",gsub("A\\.","A",envVariableNames)); meanEnvValues_obs_list = list(); meanEnvValues_ran_list = list(); meanEnvValues_sim_list = list()
		for (j in 1:length(envVariableNames))
			{
				meanEnvValues_obs = rep(NA, nberOfSimulations); meanEnvValues_ran = rep(NA, nberOfSimulations); meanEnvValues_sim = rep(NA, nberOfSimulations)
				lowerEnvValues_randomisations = 0; lowerEnvValues_simulations = 0
				for (k in 1:nberOfSimulations)
					{
						obs = read.csv(paste0(localTreesDirectory,"/TreeExtractions_",k,".csv"), header=T)
						ran = read.csv(paste0(localTreesDirectory,"/TreeRandomisation_",k,".csv"), header=T)
						sim = read.csv(paste0(localTreesDirectory,"/TreeSimulations_",k,".csv"), header=T)
						envVariableName = gsub("-",".",gsub("&",".",gsub(" ","_",gsub("\\. "," ",envVariableNames[j]))))
						envValues_obs1 = read.csv(paste0(localTreesDirectory,"/EnvValues_obs_",k,".csv"))[,envVariableName]
						envValues_ran1 = read.csv(paste0(localTreesDirectory,"/envValues_ran_",k,".csv"))[,envVariableName]
						envValues_sim1 = read.csv(paste0(localTreesDirectory,"/envValues_sim_",k,".csv"))[,envVariableName]
						indices = which(!is.na(envValues_obs1)&(obs[,"endYear"]<decimal_date(ymd("2023-07-01")))); envValues_obs2 = envValues_obs1[indices] 
						indices = which(!is.na(envValues_ran1)&(ran[,"endYear"]<decimal_date(ymd("2023-07-01")))); envValues_ran2 = envValues_ran1[indices]
						indices = which(!is.na(envValues_sim1)&(sim[,"endYear"]<decimal_date(ymd("2023-07-01")))); envValues_sim2 = envValues_sim1[indices]
						meanEnvValues_obs[k] = mean(envValues_obs2, na.rm=T); meanEnvValues_ran[k] = mean(envValues_ran2, na.rm=T); meanEnvValues_sim[k] = mean(envValues_sim2, na.rm=T)
						if (meanEnvValues_obs[k] < meanEnvValues_ran[k]) lowerEnvValues_randomisations = lowerEnvValues_randomisations + 1
						if (meanEnvValues_obs[k] < meanEnvValues_sim[k]) lowerEnvValues_simulations = lowerEnvValues_simulations + 1				
					}
				p = lowerEnvValues_randomisations/nberOfSimulations; BFs[j,"lower_randomisations"] = round((p/(1-p))/(0.5/(1-0.5)),1)
				p = (1-lowerEnvValues_randomisations/nberOfSimulations); BFs[j,"higher_randomisations"] = round((p/(1-p))/(0.5/(1-0.5)),1)
				p = lowerEnvValues_simulations/nberOfSimulations; BFs[j,"lower_simulations"] = round((p/(1-p))/(0.5/(1-0.5)),1)
				p = (1-lowerEnvValues_simulations/nberOfSimulations); BFs[j,"higher_simulations"] = round((p/(1-p))/(0.5/(1-0.5)),1)
				meanEnvValues_obs_list[[j]] = meanEnvValues_obs; meanEnvValues_ran_list[[j]] = meanEnvValues_ran; meanEnvValues_sim_list[[j]] = meanEnvValues_sim
			}
		write.csv(BFs, paste0("All_seraphim_analyses/OROV_",segments[i],"_E_b23-07.csv"), quote=F)
		BFs = matrix(nrow=length(envVariableNames), ncol=4); colnames(BFs) = c("lower_randomisations","higher_randomisations","lower_simulations","higher_simulations")
		row.names(BFs) = gsub(" ","_",gsub("A\\.","A",envVariableNames)); meanEnvValues_obs_list = list(); meanEnvValues_ran_list = list(); meanEnvValues_sim_list = list()
		for (j in 1:length(envVariableNames))
			{
				meanEnvValues_obs = rep(NA, nberOfSimulations); meanEnvValues_ran = rep(NA, nberOfSimulations); meanEnvValues_sim = rep(NA, nberOfSimulations)
				lowerEnvValues_randomisations = 0; lowerEnvValues_simulations = 0
				for (k in 1:nberOfSimulations)
					{
						obs = read.csv(paste0(localTreesDirectory,"/TreeExtractions_",k,".csv"), header=T)
						ran = read.csv(paste0(localTreesDirectory,"/TreeRandomisation_",k,".csv"), header=T)
						sim = read.csv(paste0(localTreesDirectory,"/TreeSimulations_",k,".csv"), header=T)
						envVariableName = gsub("-",".",gsub("&",".",gsub(" ","_",gsub("\\. "," ",envVariableNames[j]))))
						envValues_obs1 = read.csv(paste0(localTreesDirectory,"/EnvValues_obs_",k,".csv"))[,envVariableName]
						envValues_ran1 = read.csv(paste0(localTreesDirectory,"/envValues_ran_",k,".csv"))[,envVariableName]
						envValues_sim1 = read.csv(paste0(localTreesDirectory,"/envValues_sim_",k,".csv"))[,envVariableName]
						indices = which(!is.na(envValues_obs1)&(obs[,"endYear"]>decimal_date(ymd("2023-07-01")))); envValues_obs2 = envValues_obs1[indices] 
						indices = which(!is.na(envValues_ran1)&(ran[,"endYear"]>decimal_date(ymd("2023-07-01")))); envValues_ran2 = envValues_ran1[indices]
						indices = which(!is.na(envValues_sim1)&(sim[,"endYear"]>decimal_date(ymd("2023-07-01")))); envValues_sim2 = envValues_sim1[indices]
						meanEnvValues_obs[k] = mean(envValues_obs2, na.rm=T); meanEnvValues_ran[k] = mean(envValues_ran2, na.rm=T); meanEnvValues_sim[k] = mean(envValues_sim2, na.rm=T)
						if (meanEnvValues_obs[k] < meanEnvValues_ran[k]) lowerEnvValues_randomisations = lowerEnvValues_randomisations + 1
						if (meanEnvValues_obs[k] < meanEnvValues_sim[k]) lowerEnvValues_simulations = lowerEnvValues_simulations + 1				
					}
				p = lowerEnvValues_randomisations/nberOfSimulations; BFs[j,"lower_randomisations"] = round((p/(1-p))/(0.5/(1-0.5)),1)
				p = (1-lowerEnvValues_randomisations/nberOfSimulations); BFs[j,"higher_randomisations"] = round((p/(1-p))/(0.5/(1-0.5)),1)
				p = lowerEnvValues_simulations/nberOfSimulations; BFs[j,"lower_simulations"] = round((p/(1-p))/(0.5/(1-0.5)),1)
				p = (1-lowerEnvValues_simulations/nberOfSimulations); BFs[j,"higher_simulations"] = round((p/(1-p))/(0.5/(1-0.5)),1)
				meanEnvValues_obs_list[[j]] = meanEnvValues_obs; meanEnvValues_ran_list[[j]] = meanEnvValues_ran; meanEnvValues_sim_list[[j]] = meanEnvValues_sim
			}
		write.csv(BFs, paste0("All_seraphim_analyses/OROV_",segments[i],"_E_a23-07.csv"), quote=F)
		BFs = matrix(nrow=length(envVariableNames), ncol=4); colnames(BFs) = c("lower_randomisations","higher_randomisations","lower_simulations","higher_simulations")
		row.names(BFs) = gsub(" ","_",gsub("A\\.","A",envVariableNames)); meanEnvValues_obs_list = list(); meanEnvValues_ran_list = list(); meanEnvValues_sim_list = list()
		for (j in 1:length(envVariableNames))
			{
				meanEnvValues_obs = rep(NA, nberOfSimulations); meanEnvValues_ran = rep(NA, nberOfSimulations); meanEnvValues_sim = rep(NA, nberOfSimulations)
				lowerEnvValues_randomisations = 0; lowerEnvValues_simulations = 0
				for (k in 1:nberOfSimulations)
					{
						obs = read.csv(paste0(localTreesDirectory,"/TreeExtractions_",k,".csv"), header=T)
						ran = read.csv(paste0(localTreesDirectory,"/TreeRandomisation_",k,".csv"), header=T)
						sim = read.csv(paste0(localTreesDirectory,"/TreeSimulations_",k,".csv"), header=T)
						envVariableName = gsub("-",".",gsub("&",".",gsub(" ","_",gsub("\\. "," ",envVariableNames[j]))))
						envValues_obs1 = read.csv(paste0(localTreesDirectory,"/EnvValues_obs_",k,".csv"))[,envVariableName]
						envValues_ran1 = read.csv(paste0(localTreesDirectory,"/envValues_ran_",k,".csv"))[,envVariableName]
						envValues_sim1 = read.csv(paste0(localTreesDirectory,"/envValues_sim_",k,".csv"))[,envVariableName]
						indices = which(!is.na(envValues_obs1)&(obs[,"endYear"]<decimal_date(ymd("2024-01-01")))); envValues_obs2 = envValues_obs1[indices] 
						indices = which(!is.na(envValues_ran1)&(ran[,"endYear"]<decimal_date(ymd("2024-01-01")))); envValues_ran2 = envValues_ran1[indices]
						indices = which(!is.na(envValues_sim1)&(sim[,"endYear"]<decimal_date(ymd("2024-01-01")))); envValues_sim2 = envValues_sim1[indices]
						meanEnvValues_obs[k] = mean(envValues_obs2, na.rm=T); meanEnvValues_ran[k] = mean(envValues_ran2, na.rm=T); meanEnvValues_sim[k] = mean(envValues_sim2, na.rm=T)
						if (meanEnvValues_obs[k] < meanEnvValues_ran[k]) lowerEnvValues_randomisations = lowerEnvValues_randomisations + 1
						if (meanEnvValues_obs[k] < meanEnvValues_sim[k]) lowerEnvValues_simulations = lowerEnvValues_simulations + 1				
					}
				p = lowerEnvValues_randomisations/nberOfSimulations; BFs[j,"lower_randomisations"] = round((p/(1-p))/(0.5/(1-0.5)),1)
				p = (1-lowerEnvValues_randomisations/nberOfSimulations); BFs[j,"higher_randomisations"] = round((p/(1-p))/(0.5/(1-0.5)),1)
				p = lowerEnvValues_simulations/nberOfSimulations; BFs[j,"lower_simulations"] = round((p/(1-p))/(0.5/(1-0.5)),1)
				p = (1-lowerEnvValues_simulations/nberOfSimulations); BFs[j,"higher_simulations"] = round((p/(1-p))/(0.5/(1-0.5)),1)
				meanEnvValues_obs_list[[j]] = meanEnvValues_obs; meanEnvValues_ran_list[[j]] = meanEnvValues_ran; meanEnvValues_sim_list[[j]] = meanEnvValues_sim
			}
		write.csv(BFs, paste0("All_seraphim_analyses/OROV_",segments[i],"_E_b24-01.csv"), quote=F)
		BFs = matrix(nrow=length(envVariableNames), ncol=4); colnames(BFs) = c("lower_randomisations","higher_randomisations","lower_simulations","higher_simulations")
		row.names(BFs) = gsub(" ","_",gsub("A\\.","A",envVariableNames)); meanEnvValues_obs_list = list(); meanEnvValues_ran_list = list(); meanEnvValues_sim_list = list()
		for (j in 1:length(envVariableNames))
			{
				meanEnvValues_obs = rep(NA, nberOfSimulations); meanEnvValues_ran = rep(NA, nberOfSimulations); meanEnvValues_sim = rep(NA, nberOfSimulations)
				lowerEnvValues_randomisations = 0; lowerEnvValues_simulations = 0
				for (k in 1:nberOfSimulations)
					{
						obs = read.csv(paste0(localTreesDirectory,"/TreeExtractions_",k,".csv"), header=T)
						ran = read.csv(paste0(localTreesDirectory,"/TreeRandomisation_",k,".csv"), header=T)
						sim = read.csv(paste0(localTreesDirectory,"/TreeSimulations_",k,".csv"), header=T)
						envVariableName = gsub("-",".",gsub("&",".",gsub(" ","_",gsub("\\. "," ",envVariableNames[j]))))
						envValues_obs1 = read.csv(paste0(localTreesDirectory,"/EnvValues_obs_",k,".csv"))[,envVariableName]
						envValues_ran1 = read.csv(paste0(localTreesDirectory,"/envValues_ran_",k,".csv"))[,envVariableName]
						envValues_sim1 = read.csv(paste0(localTreesDirectory,"/envValues_sim_",k,".csv"))[,envVariableName]
						indices = which(!is.na(envValues_obs1)&(obs[,"endYear"]>decimal_date(ymd("2024-01-01")))); envValues_obs2 = envValues_obs1[indices] 
						indices = which(!is.na(envValues_ran1)&(ran[,"endYear"]>decimal_date(ymd("2024-01-01")))); envValues_ran2 = envValues_ran1[indices]
						indices = which(!is.na(envValues_sim1)&(sim[,"endYear"]>decimal_date(ymd("2024-01-01")))); envValues_sim2 = envValues_sim1[indices]
						meanEnvValues_obs[k] = mean(envValues_obs2, na.rm=T); meanEnvValues_ran[k] = mean(envValues_ran2, na.rm=T); meanEnvValues_sim[k] = mean(envValues_sim2, na.rm=T)
						if (meanEnvValues_obs[k] < meanEnvValues_ran[k]) lowerEnvValues_randomisations = lowerEnvValues_randomisations + 1
						if (meanEnvValues_obs[k] < meanEnvValues_sim[k]) lowerEnvValues_simulations = lowerEnvValues_simulations + 1				
					}
				p = lowerEnvValues_randomisations/nberOfSimulations; BFs[j,"lower_randomisations"] = round((p/(1-p))/(0.5/(1-0.5)),1)
				p = (1-lowerEnvValues_randomisations/nberOfSimulations); BFs[j,"higher_randomisations"] = round((p/(1-p))/(0.5/(1-0.5)),1)
				p = lowerEnvValues_simulations/nberOfSimulations; BFs[j,"lower_simulations"] = round((p/(1-p))/(0.5/(1-0.5)),1)
				p = (1-lowerEnvValues_simulations/nberOfSimulations); BFs[j,"higher_simulations"] = round((p/(1-p))/(0.5/(1-0.5)),1)
				meanEnvValues_obs_list[[j]] = meanEnvValues_obs; meanEnvValues_ran_list[[j]] = meanEnvValues_ran; meanEnvValues_sim_list[[j]] = meanEnvValues_sim
			}
		write.csv(BFs, paste0("All_seraphim_analyses/OROV_",segments[i],"_E_a24-01.csv"), quote=F)
	}
tab_E_ran = matrix(nrow=length(envVariableNames), ncol=12); tab_E_sim = matrix(nrow=length(envVariableNames), ncol=12)
colnames(tab_E_ran) = c("L_lower_b23-07_ran","L_lower_a23-07_ran","L_upper_b23-07_ran","L_upper_a23-07_ran",
						"M_lower_b23-07_ran","M_lower_a23-07_ran","M_upper_b23-07_ran","M_upper_a23-07_ran",
						"S_lower_b23-07_ran","S_lower_a23-07_ran","S_upper_b23-07_ran","S_upper_a23-07_ran")
colnames(tab_E_sim) = c("L_lower_b23-07_sim","L_lower_a23-07_sim","L_upper_b23-07_sim","L_upper_a23-07_sim",
						"M_lower_b23-07_sim","M_lower_a23-07_sim","M_upper_b23-07_sim","M_upper_a23-07_sim",
						"S_lower_b23-07_sim","S_lower_a23-07_sim","S_upper_b23-07_sim","S_upper_a23-07_sim")
for (i in 1:length(beast_analyses))
	{
		tab = read.csv(paste0("All_seraphim_analyses/OROV_",segments[i],"_E_b23-07.csv"), head=T)
		tab_E_ran[,((i-1)*4)+1] = tab[,"lower_randomisations"]; tab_E_ran[,((i-1)*4)+3] = tab[,"higher_randomisations"]
		tab = read.csv(paste0("All_seraphim_analyses/OROV_",segments[i],"_E_a23-07.csv"), head=T)
		tab_E_ran[,((i-1)*4)+2] = tab[,"lower_randomisations"]; tab_E_ran[,((i-1)*4)+4] = tab[,"higher_randomisations"]	
		tab = read.csv(paste0("All_seraphim_analyses/OROV_",segments[i],"_E_b23-07.csv"), head=T)
		tab_E_sim[,((i-1)*4)+1] = tab[,"lower_simulations"]; tab_E_sim[,((i-1)*4)+3] = tab[,"higher_simulations"]
		tab = read.csv(paste0("All_seraphim_analyses/OROV_",segments[i],"_E_a23-07.csv"), head=T)
		tab_E_sim[,((i-1)*4)+2] = tab[,"lower_simulations"]; tab_E_sim[,((i-1)*4)+4] = tab[,"higher_simulations"]
	}
write.table(cbind(tab_E_ran,tab_E_sim), "All_seraphim_analyses/OROV_all_E_23-07.txt", row.names=F, quote=F, sep="\t")
tab_E_ran = matrix(nrow=length(envVariableNames), ncol=12); tab_E_sim = matrix(nrow=length(envVariableNames), ncol=12)
colnames(tab_E_ran) = c("L_lower_b24-01_ran","L_lower_a24-01_ran","L_upper_b24-01_ran","L_upper_a24-01_ran",
						"M_lower_b24-01_ran","M_lower_a24-01_ran","M_upper_b24-01_ran","M_upper_a24-01_ran",
						"S_lower_b24-01_ran","S_lower_a24-01_ran","S_upper_b24-01_ran","S_upper_a24-01_ran")
colnames(tab_E_sim) = c("L_lower_b24-01_sim","L_lower_a24-01_sim","L_upper_b24-01_sim","L_upper_a24-01_sim",
						"M_lower_b24-01_sim","M_lower_a24-01_sim","M_upper_b24-01_sim","M_upper_a24-01_sim",
						"S_lower_b24-01_sim","S_lower_a24-01_sim","S_upper_b24-01_sim","S_upper_a24-01_sim")
for (i in 1:length(beast_analyses))
	{
		tab = read.csv(paste0("All_seraphim_analyses/OROV_",segments[i],"_E_b24-01.csv"), head=T)
		tab_E_ran[,((i-1)*4)+1] = tab[,"lower_randomisations"]; tab_E_ran[,((i-1)*4)+3] = tab[,"higher_randomisations"]
		tab = read.csv(paste0("All_seraphim_analyses/OROV_",segments[i],"_E_a24-01.csv"), head=T)
		tab_E_ran[,((i-1)*4)+2] = tab[,"lower_randomisations"]; tab_E_ran[,((i-1)*4)+4] = tab[,"higher_randomisations"]	
		tab = read.csv(paste0("All_seraphim_analyses/OROV_",segments[i],"_E_b24-01.csv"), head=T)
		tab_E_sim[,((i-1)*4)+1] = tab[,"lower_simulations"]; tab_E_sim[,((i-1)*4)+3] = tab[,"higher_simulations"]
		tab = read.csv(paste0("All_seraphim_analyses/OROV_",segments[i],"_E_a24-01.csv"), head=T)
		tab_E_sim[,((i-1)*4)+2] = tab[,"lower_simulations"]; tab_E_sim[,((i-1)*4)+4] = tab[,"higher_simulations"]
	}
write.table(cbind(tab_E_ran,tab_E_sim), "All_seraphim_analyses/OROV_all_E_24-01.txt", row.names=F, quote=F, sep="\t")
for (i in 1:length(beast_analyses))) # to perform the R statistic tests; not relevant (here) as all branches - even all the small recent ones - get the same weight in the analysis
	{
		localTreesDirectory = paste0("Tree_extraction_files/",localTreesDirectories[i],"_ext")
		BFs = matrix(nrow=length(envVariableNames), ncol=4); colnames(BFs) = c("lower_randomisations","higher_randomisations","lower_simulations","higher_simulations")
		row.names(BFs) = gsub(" ","_",gsub("A\\.","A",envVariableNames))
		for (j in 1:length(envVariableNames))
			{
				meanPositiveDifferences_obs = rep(NA, nberOfSimulations); meanNegativeDifferences_obs = rep(NA, nberOfSimulations)
				meanPositiveDifferences_ran = rep(NA, nberOfSimulations); meanNegativeDifferences_ran = rep(NA, nberOfSimulations)
				for (k in 1:nberOfSimulations)
					{
						envVariableName = gsub("-",".",gsub("&",".",gsub(" ","_",gsub("\\. "," ",envVariableNames[j]))))
						envDifferences_obs1 = read.csv(paste0(localTreesDirectory,"/Differences_obs_",k,".csv"))[,envVariableName]
						envDifferences_ran1 = read.csv(paste0(localTreesDirectory,"/Differences_ran_",k,".csv"))[,envVariableName]
						indices = which(!is.na(envDifferences_obs1)); envDifferences_obs2 = envDifferences_obs1[indices]
						indices = which(!is.na(envDifferences_ran1)); envDifferences_ran2 = envDifferences_ran1[indices]
						meanPositiveDifferences_obs[k] = sum(envDifferences_obs2>0)/length(envDifferences_obs2)
						meanNegativeDifferences_obs[k] = sum(envDifferences_obs2<0)/length(envDifferences_obs2)
						meanPositiveDifferences_ran[k] = sum(envDifferences_ran2>0)/length(envDifferences_ran2)
						meanNegativeDifferences_ran[k] = sum(envDifferences_ran2<0)/length(envDifferences_ran2)
					}
				p = sum(meanNegativeDifferences_obs<meanNegativeDifferences_ran)/nberOfSimulations; BFs[j,"lower_randomisations"] = round((p/(1-p))/(0.5/(1-0.5)),1)
				p = sum(meanPositiveDifferences_obs>meanPositiveDifferences_ran)/nberOfSimulations; BFs[j,"higher_randomisations"] = round((p/(1-p))/(0.5/(1-0.5)),1)
			}
		for (j in 1:length(envVariableNames))
			{
				meanPositiveDifferences_obs = rep(NA, nberOfSimulations); meanNegativeDifferences_obs = rep(NA, nberOfSimulations)
				meanPositiveDifferences_sim = rep(NA, nberOfSimulations); meanNegativeDifferences_sim = rep(NA, nberOfSimulations)
				for (k in 1:nberOfSimulations)
					{
						envVariableName = gsub("-",".",gsub("&",".",gsub(" ","_",gsub("\\. "," ",envVariableNames[j]))))
						envDifferences_obs1 = read.csv(paste0(localTreesDirectory,"/Differences_obs_",k,".csv"))[,envVariableName]
						envDifferences_sim1 = read.csv(paste0(localTreesDirectory,"/Differences_sim_",k,".csv"))[,envVariableName]
						indices = which(!is.na(envDifferences_obs1)); envDifferences_obs2 = envDifferences_obs1[indices]
						indices = which(!is.na(envDifferences_sim1)); envDifferences_sim2 = envDifferences_sim1[indices]
						meanPositiveDifferences_obs[k] = sum(envDifferences_obs2>0)/length(envDifferences_obs2)
						meanNegativeDifferences_obs[k] = sum(envDifferences_obs2<0)/length(envDifferences_obs2)
						meanPositiveDifferences_sim[k] = sum(envDifferences_sim2>0)/length(envDifferences_sim2)
						meanNegativeDifferences_sim[k] = sum(envDifferences_sim2<0)/length(envDifferences_sim2)
					}
				p = sum(meanNegativeDifferences_obs<meanNegativeDifferences_sim)/nberOfSimulations; BFs[j,"lower_simulations"] = round((p/(1-p))/(0.5/(1-0.5)),1)
				p = sum(meanPositiveDifferences_obs>meanPositiveDifferences_sim)/nberOfSimulations; BFs[j,"higher_simulations"] = round((p/(1-p))/(0.5/(1-0.5)),1)
			}
		write.csv(BFs, paste0("All_seraphim_analyses/OROV_",segments[i],"_R_tests.csv"), quote=F)
	}
for (i in 1:length(beast_analyses))) # to perform the R statistic tests; not relevant (here) as all branches - even all the small recent ones - get the same weight in the analysis
	{
		localTreesDirectory = paste0("Tree_extraction_files/",localTreesDirectories[i],"_ext")
		BFs = matrix(nrow=length(envVariableNames), ncol=4); colnames(BFs) = c("lower_randomisations","higher_randomisations","lower_simulations","higher_simulations")
		row.names(BFs) = gsub(" ","_",gsub("A\\.","A",envVariableNames))
		for (j in 1:length(envVariableNames))
			{
				meanPositiveDifferences_obs = rep(NA, nberOfSimulations); meanNegativeDifferences_obs = rep(NA, nberOfSimulations)
				meanPositiveDifferences_ran = rep(NA, nberOfSimulations); meanNegativeDifferences_ran = rep(NA, nberOfSimulations)
				for (k in 1:nberOfSimulations)
					{
						obs = read.csv(paste0(localTreesDirectory,"/TreeExtractions_",k,".csv"), header=T)
						ran = read.csv(paste0(localTreesDirectory,"/TreeRandomisation_",k,".csv"), header=T)
						dists_obs = rdist.earth(obs[,c("startLon","startLat")], obs[,c("endLon","endLat")], miles=F); dists_obs = diag(dists_obs)
						dists_ran = rdist.earth(ran[,c("startLon","startLat")], ran[,c("endLon","endLat")], miles=F); dists_ran = diag(dists_ran)
						envVariableName = gsub("-",".",gsub("&",".",gsub(" ","_",gsub("\\. "," ",envVariableNames[j]))))
						envDifferences_obs1 = read.csv(paste0(localTreesDirectory,"/Differences_obs_",k,".csv"))[,envVariableName]
						envDifferences_ran1 = read.csv(paste0(localTreesDirectory,"/Differences_ran_",k,".csv"))[,envVariableName]
						indices = which((!is.na(envDifferences_obs1))&(dists_obs>500)); envDifferences_obs2 = envDifferences_obs1[indices]
						indices = which((!is.na(envDifferences_ran1))&(dists_ran>500)); envDifferences_ran2 = envDifferences_ran1[indices]
						meanPositiveDifferences_obs[k] = sum(envDifferences_obs2>0)/length(envDifferences_obs2)
						meanNegativeDifferences_obs[k] = sum(envDifferences_obs2<0)/length(envDifferences_obs2)
						meanPositiveDifferences_ran[k] = sum(envDifferences_ran2>0)/length(envDifferences_ran2)
						meanNegativeDifferences_ran[k] = sum(envDifferences_ran2<0)/length(envDifferences_ran2)
					}
				p = sum(meanNegativeDifferences_obs<meanNegativeDifferences_ran)/nberOfSimulations; BFs[j,"lower_randomisations"] = round((p/(1-p))/(0.5/(1-0.5)),1)
				p = sum(meanPositiveDifferences_obs>meanPositiveDifferences_ran)/nberOfSimulations; BFs[j,"higher_randomisations"] = round((p/(1-p))/(0.5/(1-0.5)),1)
			}
		for (j in 1:length(envVariableNames))
			{
				meanPositiveDifferences_obs = rep(NA, nberOfSimulations); meanNegativeDifferences_obs = rep(NA, nberOfSimulations)
				meanPositiveDifferences_sim = rep(NA, nberOfSimulations); meanNegativeDifferences_sim = rep(NA, nberOfSimulations)
				for (k in 1:nberOfSimulations)
					{
						obs = read.csv(paste0(localTreesDirectory,"/TreeExtractions_",k,".csv"), header=T)
						sim = read.csv(paste0(localTreesDirectory,"/TreeSimulations_",k,".csv"), header=T)
						dists_obs = rdist.earth(obs[,c("startLon","startLat")], obs[,c("endLon","endLat")], miles=F); dists_obs = diag(dists_obs)
						dists_sim = rdist.earth(sim[,c("startLon","startLat")], sim[,c("endLon","endLat")], miles=F); dists_sim = diag(dists_sim)
						envVariableName = gsub("-",".",gsub("&",".",gsub(" ","_",gsub("\\. "," ",envVariableNames[j]))))
						envDifferences_obs1 = read.csv(paste0(localTreesDirectory,"/Differences_obs_",k,".csv"))[,envVariableName]
						envDifferences_sim1 = read.csv(paste0(localTreesDirectory,"/Differences_sim_",k,".csv"))[,envVariableName]
						indices = which((!is.na(envDifferences_obs1))&(dists_obs>500)); envDifferences_obs2 = envDifferences_obs1[indices]
						indices = which((!is.na(envDifferences_sim1))&(dists_sim>500)); envDifferences_sim2 = envDifferences_sim1[indices]
						meanPositiveDifferences_obs[k] = sum(envDifferences_obs2>0)/length(envDifferences_obs2)
						meanNegativeDifferences_obs[k] = sum(envDifferences_obs2<0)/length(envDifferences_obs2)
						meanPositiveDifferences_sim[k] = sum(envDifferences_sim2>0)/length(envDifferences_sim2)
						meanNegativeDifferences_sim[k] = sum(envDifferences_sim2<0)/length(envDifferences_sim2)
					}
				p = sum(meanNegativeDifferences_obs<meanNegativeDifferences_sim)/nberOfSimulations; BFs[j,"lower_simulations"] = round((p/(1-p))/(0.5/(1-0.5)),1)
				p = sum(meanPositiveDifferences_obs>meanPositiveDifferences_sim)/nberOfSimulations; BFs[j,"higher_simulations"] = round((p/(1-p))/(0.5/(1-0.5)),1)
			}
		write.csv(BFs, paste0("All_seraphim_analyses/OROV_",segments[i],"_R_500k.csv"), quote=F)
	}

