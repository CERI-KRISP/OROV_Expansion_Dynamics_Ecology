library(ggplot2)
library(cowplot)
library(diagram)
require(seraphim)
library(rworldmap)
library(raster)
library(rnaturalearth)
library(readxl)
library(paletteer)
library(ggnewscale)
library(patchwork)
library(dplyr)
library(lubridate)



###### Supplementary figure S1

amazonas <- c("AC","AP","AM","MA","MT","PA","RO","RR","TO")
'%!in%' <- function(x,y)!('%in%'(x,y))

cols<-paletteer_c("grDevices::YlOrBr", 9)
cols1<-paletteer_c("grDevices::Heat", 9)
cols2<-paletteer_c("grDevices::PuBuGn", 19)
cols3<-paletteer_c("ggthemes::Green-Blue Diverging", 19)



cases<-read_xlsx("OROPOUCHE_cases.xlsx")
cases$date<-paste0(cases$ANO,cases$SEMANA_EPI,"1")
cases$date<-as.Date(cases$date,format="%Y%W%u")
cases$Amazon<-0
cases$Amazon[cases$UF_RESID %in%amazonas] <- "Amazon"
cases$Amazon[cases$UF_RESID %!in%amazonas] <- "Outside Amazon"

p1<-ggplot()+
  theme_bw()+
  geom_bar(data=subset(cases,UF_RESID %in% amazonas), mapping=aes(date, fill=UF_RESID),colour='black',size=0.1,position='stack')+
  scale_fill_manual(values=cols,name='Amazonian states')+
  guides(fill = guide_legend(ncol=6,order=1))+
  
  new_scale_fill()+
  geom_bar(data=subset(cases,UF_RESID %!in% amazonas), mapping=aes(date, fill=UF_RESID),position='stack',colour='black',size=0.1,alpha=0.9)+
  scale_fill_manual(values=cols2,name='Non-Amazonian states')+
  theme(legend.position = c(0.3,0.25))+
  ylab("Cases")+xlab("")+
  scale_x_date(date_labels = "%b-%Y")+
  guides(fill = guide_legend(ncol=6,order=2))+
  theme(legend.key.size = unit(0.3,"line"),legend.background = element_blank())+
  facet_wrap(~Amazon,ncol=1)
p1





cases<-dplyr::left_join(cases,map2, by=c('UF_RESID'='postal'))
#centroids <- do.call("rbind.data.frame", centroids)  # Convert to Data Frame

cases<-cases %>% dplyr::group_by(UF_RESID) %>% dplyr::mutate(UF_lat=mean(latitude), UF_lon=mean(longitude)) %>%  dplyr::mutate(case_count=dplyr::n()) 
p2<-ggplot()+
  theme_void()+
  geom_sf(data=map1,mapping=aes(),fill=NA)+
  geom_sf(data=subset(map2,postal %in% amazonas),mapping=aes(fill=postal),show.legend = FALSE,colour='white')+
  
  scale_fill_manual(values=cols,name='Amazonian states')+
  new_scale_fill()+
  geom_sf(data=subset(map2,postal %!in% amazonas),mapping=aes(fill=postal),show.legend = FALSE,colour='white')+
  scale_fill_manual(values=cols2,name='Other states')+
  geom_count(cases,mapping=aes(UF_lon,UF_lat),shape=21,fill='white',alpha=0.5)+
  #geom_text(cases,mapping=aes(UF_lon,UF_lat,label=case_count),size=2,colour='white')+

  #scale_size_area(0,10)+
  scale_size(range = c(0, 10),breaks=c(10,100,1000),labels=c(10,100,1000))+
  theme(legend.position = c(0.15,0.35),legend.title = element_blank())+
  guides(size = guide_legend(ncol=1))
  
p2
plot_grid(p1,p2,ncol=2,rel_widths = c(0.8,0.2))
  
p1 +
  inset_element(p2, left = 0.01, bottom = 0.55, right = 0.6, top = 1)


# Figure 3E & 3F

orov_pre2024_median_M<-read.csv("SpreadValues/M/orov_suitability_pre2024_median_orov_suitability_pre2024.csv")
orov_pre2024_95HPD_M<-read.csv("SpreadValues/M/orov_suitability_pre2024_95%HPD_orov_suitability_pre2024.csv")

orov_premid2023_median_M<-read.csv("SpreadValues/M/orov_suitability_premid2023_median_orov_suitability_premid2023.csv")
orov_premid2023_95HPD_M<-read.csv("SpreadValues/M/orov_suitability_premid2023_95%HPD_orov_suitability_premid2023.csv")

orov_cumulative_median_M<-read.csv("SpreadValues/M/orov_suitability_cumulative_median_orov_suitability_cumulative.csv")
orov_cumulative_95HPD_M<-read.csv("SpreadValues/M/orov_suitability_cumulative_95%HPD_orov_suitability_cumulative.csv")


cparaensis_median_M<-read.csv("SpreadValues/M/culicoides_median_culicoides.csv")
cparaensis_cumulative_95HPD_M<-read.csv("SpreadValues/M/culicoides_95%HPD_culicoides.csv")

cases$decimal_date<-decimal_date(cases$date)


cparaensis_values<-ggplot()+
  theme_minimal()+
  theme(axis.text=element_text(size=8),axis.title=element_text(size=10),
        legend.text = element_text(size=8),legend.title = element_text(size=8),legend.spacing = unit(0, 'cm'))+
  
  geom_bar(data=cases, mapping=aes(decimal_date,fill=Amazon),colour='grey40',size=0.05)+
  scale_fill_manual(values=c('seashell2',"grey95"),name="Cases")+
  guides(
    fill = guide_legend(order = 2),
    colour=guide_legend(order = 2),
  )+
  #guides(fill = guide_legend(override.aes = list(size = 0.2)))+
  theme(legend.key.size = unit(0.5,"line"))+
  new_scale_fill()+
  geom_vline(xintercept=2023.5,linetype='dashed',colour='grey50')+
  geom_vline(xintercept=2024.0,linetype='dashed',colour='grey80')+
  geom_ribbon(data=cparaensis_cumulative_95HPD_M,
              aes(x=time,ymin=(X95.HPD_lower_value-1)*500,ymax=(X95.HPD_higher_value-1)*500,fill='C.Paraensis'),
              alpha=0.2)+
  #geom_ribbon(data=culex_q_cumulative_95HPD_M,
   #           aes(x=time,ymin=(X95.HPD_lower_value-1)*500,ymax=(X95.HPD_higher_value-1)*500,fill='Culex Q'),
   #           alpha=0.2)+
  #geom_ribbon(data=aegypti_cumulative_95HPD_M,
  #            aes(x=time,ymin=(X95.HPD_lower_value-1)*500,ymax=(X95.HPD_higher_value-1)*500,fill='Ae.aegypti'),
  #            alpha=0.2)+
  #geom_ribbon(data=orov_premid2023_95HPD_M,
   #           aes(x=time,ymin=(X95.HPD_lower_value-1)*500,ymax=(X95.HPD_higher_value-1)*500,fill='Pre-mid-2023'),
  #            alpha=0.2)+
  #geom_ribbon(data=orov_pre2024_95HPD_M,
  #            aes(x=time,ymin=(X95.HPD_lower_value-1)*500,ymax=(X95.HPD_higher_value-1)*500,fill='Pre-2024'),
  #            alpha=0.2)+
  #geom_ribbon(data=orov_cumulative_95HPD_M,
  #            aes(x=time,ymin=(X95.HPD_lower_value-1)*500,ymax=(X95.HPD_higher_value-1)*500,fill='OROV'),
  #            alpha=0.2)+
  geom_line(data=cparaensis_median_M,
            aes(x=time,y=(culicoides-1)*500,
                colour='C.Paraensis'))+
  #geom_line(data=culex_q_median_M,
  #          aes(x=time,y=(culex-1)*500,
   #             colour='Culex Q'))+
  #geom_line(data=aegypti_median_M,
   #         aes(x=time,y=(mosquito-1)*500,
   #             colour='Ae.aegypti'))+
  #geom_line(data=orov_premid2023_median_M,
  #          aes(x=time,y=(orov_suitability_premid2023-1)*500,
   #             colour='Pre-mid-2023'))+
  #geom_line(data=orov_pre2024_median_M,
  #          aes(x=time,y=(orov_suitability_pre2024-1)*500,
   #             colour='Pre-2024'))+
  #geom_line(data=orov_cumulative_median_M,
  #          aes(x=time,y=(orov_suitability_cumulative-1)*500,
   #             colour='OROV'))+
  ylab("Weekly cases")+
  xlab("Date")+
  theme(legend.position = "top",legend.box = 'vertical',legend.spacing = unit(0, 'cm'))+
  scale_fill_manual(values=c("orange3","black"),name='Vector ENM model')+
  scale_colour_manual(values=c("orange3","black"),name='Vector ENM model')+
  guides(
    fill = guide_legend(order = 1),
    colour = guide_legend(order = 1)
  )+
  scale_x_continuous(limits=c(2022,2024.5),breaks=c(2022,2023,2024),labels=c(2022,2023,2024))+
  scale_y_continuous(
    "Cases", 
    sec.axis = sec_axis(~ . / 500, name = "Suitability index of dispersal locations")
  )+
  theme(legend.key.size = unit(0.5,"line"))

cparaensis_values


orov_values<-ggplot()+
  theme_minimal()+
  theme(axis.text=element_text(size=8),axis.title=element_text(size=10),
        legend.text = element_text(size=8),legend.title = element_text(size=8),legend.spacing = unit(0, 'cm'))+
  
  geom_bar(data=cases, mapping=aes(decimal_date,fill=Amazon),colour='grey40',size=0.05)+
  scale_fill_manual(values=c('seashell2',"grey95"),name="Cases")+
  #guides(fill = guide_legend(override.aes = list(size = 0.2)))+
  theme(legend.key.size = unit(0.5,"line"))+
  new_scale_fill()+
  geom_vline(xintercept=2023.5,linetype='dashed',colour='grey50')+
  geom_vline(xintercept=2024.0,linetype='dashed',colour='grey80')+
  
  geom_ribbon(data=orov_premid2023_95HPD_M,
              aes(x=time,ymin=(X95.HPD_lower_value-1)*500,ymax=(X95.HPD_higher_value-1)*500,fill='Pre-mid-2023'),
              alpha=0.2)+
  geom_ribbon(data=orov_pre2024_95HPD_M,
              aes(x=time,ymin=(X95.HPD_lower_value-1)*500,ymax=(X95.HPD_higher_value-1)*500,fill='Pre-2024'),
              alpha=0.2)+
  geom_ribbon(data=orov_cumulative_95HPD_M,
              aes(x=time,ymin=(X95.HPD_lower_value-1)*500,ymax=(X95.HPD_higher_value-1)*500,fill='Full'),
              alpha=0.2)+
  geom_line(data=orov_premid2023_median_M,
            aes(x=time,y=(orov_suitability_premid2023-1)*500,
                colour='Pre-mid-2023'))+
  geom_line(data=orov_pre2024_median_M,
            aes(x=time,y=(orov_suitability_pre2024-1)*500,
                colour='Pre-2024'))+
  geom_line(data=orov_cumulative_median_M,
            aes(x=time,y=(orov_suitability_cumulative-1)*500,
                colour='Full'))+
  ylab("Weekly cases")+
  xlab("Date")+
  theme(legend.position = "top",legend.box = 'vertical',legend.spacing = unit(0, 'cm'))+
  scale_fill_manual(values=c("#bf0603","#1f7a8c","#00a896"),name='OROV ENM model')+
  scale_colour_manual(values=c("#bf0603","#1f7a8c","#00a896"),name='OROV ENM model')+
  scale_x_continuous(limits=c(2022,2024.5),breaks=c(2022,2023,2024),labels=c(2022,2023,2024))+
  scale_y_continuous(
    "Cases", 
    sec.axis = sec_axis(~ . / 500, name = "Suitability index of dispersal locations")
  )+
  theme(legend.key.size = unit(0.5,"line"))

orov_values

plot_grid(orov_values,cparaensis_values,ncol=2)


######## Supplementary Figure S3

precipitation_median_S<-read.csv("SpreadValues/S/Brazil_precipitation_mean_2023_crop_mask_median_Brazil_precipitation_mean_2023_crop_mask.csv")
precipitation_95HPD_S<-read.csv("SpreadValues/S/Brazil_precipitation_mean_2023_crop_mask_95%HPD_Brazil_precipitation_mean_2023_crop_mask.csv")
precipitation_median_M<-read.csv("SpreadValues/M/Brazil_precipitation_mean_2023_crop_mask_median_Brazil_precipitation_mean_2023_crop_mask.csv")
precipitation_95HPD_M<-read.csv("SpreadValues/M/Brazil_precipitation_mean_2023_crop_mask_95%HPD_Brazil_precipitation_mean_2023_crop_mask.csv")
precipitation_median_L<-read.csv("SpreadValues/L/Brazil_precipitation_mean_2023_crop_mask_median_Brazil_precipitation_mean_2023_crop_mask.csv")
precipitation_95HPD_L<-read.csv("SpreadValues/L/Brazil_precipitation_mean_2023_crop_mask_95%HPD_Brazil_precipitation_mean_2023_crop_mask.csv")

temp_median_S<-read.csv("SpreadValues/S/Brazil_annual_mean_temp_2020_crop_mask_median_Brazil_annual_mean_temp_2020_crop_mask.csv")
temp_95HPD_S<-read.csv("SpreadValues/S/Brazil_annual_mean_temp_2020_crop_mask_95%HPD_Brazil_annual_mean_temp_2020_crop_mask.csv")
temp_median_M<-read.csv("SpreadValues/M/Brazil_annual_mean_temp_2020_crop_mask_median_Brazil_annual_mean_temp_2020_crop_mask.csv")
temp_95HPD_M<-read.csv("SpreadValues/M/Brazil_annual_mean_temp_2020_crop_mask_95%HPD_Brazil_annual_mean_temp_2020_crop_mask.csv")
temp_median_L<-read.csv("SpreadValues/L/Brazil_annual_mean_temp_2020_crop_mask_median_Brazil_annual_mean_temp_2020_crop_mask.csv")
temp_95HPD_L<-read.csv("SpreadValues/L/Brazil_annual_mean_temp_2020_crop_mask_95%HPD_Brazil_annual_mean_temp_2020_crop_mask.csv")


pop_median_S<-read.csv("SpreadValues/S/pop_2000_scaled_log_median_pop_2000_scaled_log.csv")
pop_95HPD_S<-read.csv("SpreadValues/S/pop_2000_scaled_log_95%HPD_pop_2000_scaled_log.csv")
pop_median_M<-read.csv("SpreadValues/M/pop_2000_scaled_log_median_pop_2000_scaled_log.csv")
pop_95HPD_M<-read.csv("SpreadValues/M/pop_2000_scaled_log_95%HPD_pop_2000_scaled_log.csv")
pop_median_L<-read.csv("SpreadValues/L/pop_2000_scaled_log_median_pop_2000_scaled_log.csv")
pop_95HPD_L<-read.csv("SpreadValues/L/pop_2000_scaled_log_95%HPD_pop_2000_scaled_log.csv")

pop2020_median_S<-read.csv("SpreadValues/S/pop_2020_raw_median_pop_2020_raw.csv")
pop2020_95HPD_S<-read.csv("SpreadValues/S/pop_2020_raw_95%HPD_pop_2020_raw.csv")
pop2020_median_M<-read.csv("SpreadValues/M/pop_2020_raw_median_pop_2020_raw.csv")
pop2020_95HPD_M<-read.csv("SpreadValues/M/pop_2020_raw_95%HPD_pop_2020_raw.csv")
pop2020_median_L<-read.csv("SpreadValues/L/pop_2020_raw_median_pop_2020_raw.csv")
pop2020_95HPD_L<-read.csv("SpreadValues/L/pop_2020_raw_95%HPD_pop_2020_raw.csv")


elevation_median_S<-read.csv("SpreadValues/S/elevation_median_elevation.csv")
elevation_95HPD_S<-read.csv("SpreadValues/S/elevation_95%HPD_elevation.csv")
elevation_median_M<-read.csv("SpreadValues/M/elevation_median_elevation.csv")
elevation_95HPD_M<-read.csv("SpreadValues/M/elevation_95%HPD_elevation.csv")
elevation_median_L<-read.csv("SpreadValues/L/elevation_median_elevation.csv")
elevation_95HPD_L<-read.csv("SpreadValues/L/elevation_95%HPD_elevation.csv")

evergreen_median_S<-read.csv("SpreadValues/S/evergreen_broadleaf_median_evergreen_broadleaf.csv")
evergreen_95HPD_S<-read.csv("SpreadValues/S/evergreen_broadleaf_95%HPD_evergreen_broadleaf.csv")
evergreen_median_M<-read.csv("SpreadValues/M/evergreen_broadleaf_median_evergreen_broadleaf.csv")
evergreen_95HPD_M<-read.csv("SpreadValues/M/evergreen_broadleaf_95%HPD_evergreen_broadleaf.csv")
evergreen_median_L<-read.csv("SpreadValues/L/evergreen_broadleaf_median_evergreen_broadleaf.csv")
evergreen_95HPD_L<-read.csv("SpreadValues/L/evergreen_broadleaf_95%HPD_evergreen_broadleaf.csv")

deciduous_median_S<-read.csv("SpreadValues/S/deciduous_broadleaf_median_deciduous_broadleaf.csv")
deciduous_95HPD_S<-read.csv("SpreadValues/S/deciduous_broadleaf_95%HPD_deciduous_broadleaf.csv")
deciduous_median_M<-read.csv("SpreadValues/M/deciduous_broadleaf_median_deciduous_broadleaf.csv")
deciduous_95HPD_M<-read.csv("SpreadValues/M/deciduous_broadleaf_95%HPD_deciduous_broadleaf.csv")
deciduous_median_L<-read.csv("SpreadValues/L/deciduous_broadleaf_median_deciduous_broadleaf.csv")
deciduous_95HPD_L<-read.csv("SpreadValues/L/deciduous_broadleaf_95%HPD_deciduous_broadleaf.csv")

croplands_median_S<-read.csv("SpreadValues/S/Brazil_Croplands_2019_median_Brazil_Croplands_2019.csv")
croplands_95HPD_S<-read.csv("SpreadValues/S/Brazil_Croplands_2019_95%HPD_Brazil_Croplands_2019.csv")
croplands_median_M<-read.csv("SpreadValues/M/Brazil_Croplands_2019_median_Brazil_Croplands_2019.csv")
croplands_95HPD_M<-read.csv("SpreadValues/M/Brazil_Croplands_2019_95%HPD_Brazil_Croplands_2019.csv")
croplands_median_L<-read.csv("SpreadValues/L/Brazil_Croplands_2019_median_Brazil_Croplands_2019.csv")
croplands_95HPD_L<-read.csv("SpreadValues/L/Brazil_Croplands_2019_95%HPD_Brazil_Croplands_2019.csv")

croplands_natural_vegetation_median_S<-read.csv("SpreadValues/S/Brazil_cropland_natural_vegetation_crop_mask_median_Brazil_cropland_natural_vegetation_crop_mask.csv")
croplands_natural_vegetation_95HPD_S<-read.csv("SpreadValues/S/Brazil_cropland_natural_vegetation_crop_mask_95%HPD_Brazil_cropland_natural_vegetation_crop_mask.csv")
croplands_natural_vegetation_median_M<-read.csv("SpreadValues/M/Brazil_cropland_natural_vegetation_crop_mask_median_Brazil_cropland_natural_vegetation_crop_mask.csv")
croplands_natural_vegetation_95HPD_M<-read.csv("SpreadValues/M/Brazil_cropland_natural_vegetation_crop_mask_95%HPD_Brazil_cropland_natural_vegetation_crop_mask.csv")
croplands_natural_vegetation_median_L<-read.csv("SpreadValues/L/Brazil_cropland_natural_vegetation_crop_mask_median_Brazil_cropland_natural_vegetation_crop_mask.csv")
croplands_natural_vegetation_95HPD_L<-read.csv("SpreadValues/L/Brazil_cropland_natural_vegetation_crop_mask_95%HPD_Brazil_cropland_natural_vegetation_crop_mask.csv")


savannas_median_S<-read.csv("SpreadValues/S/Brazil_savannas_crop_mask_median_Brazil_savannas_crop_mask.csv")
savannas_95HPD_S<-read.csv("SpreadValues/S/Brazil_savannas_crop_mask_95%HPD_Brazil_savannas_crop_mask.csv")
savannas_median_M<-read.csv("SpreadValues/M/Brazil_savannas_crop_mask_median_Brazil_savannas_crop_mask.csv")
savannas_95HPD_M<-read.csv("SpreadValues/M/Brazil_savannas_crop_mask_95%HPD_Brazil_savannas_crop_mask.csv")
savannas_median_L<-read.csv("SpreadValues/L/Brazil_savannas_crop_mask_median_Brazil_savannas_crop_mask.csv")
savannas_95HPD_L<-read.csv("SpreadValues/L/Brazil_savannas_crop_mask_95%HPD_Brazil_savannas_crop_mask.csv")

woody_savannas_median_S<-read.csv("SpreadValues/S/Brazil_woody_savannas_crop_mask_median_Brazil_woody_savannas_crop_mask.csv")
woody_savannas_95HPD_S<-read.csv("SpreadValues/S/Brazil_woody_savannas_crop_mask_95%HPD_Brazil_woody_savannas_crop_mask.csv")
woody_savannas_median_M<-read.csv("SpreadValues/M/Brazil_woody_savannas_crop_mask_median_Brazil_woody_savannas_crop_mask.csv")
woody_savannas_95HPD_M<-read.csv("SpreadValues/M/Brazil_woody_savannas_crop_mask_95%HPD_Brazil_woody_savannas_crop_mask.csv")
woody_savannas_median_L<-read.csv("SpreadValues/L/Brazil_woody_savannas_crop_mask_median_Brazil_woody_savannas_crop_mask.csv")
woody_savannas_95HPD_L<-read.csv("SpreadValues/L/Brazil_woody_savannas_crop_mask_95%HPD_Brazil_woody_savannas_crop_mask.csv")


grasslands_median_S<-read.csv("SpreadValues/S/Brazil_grasslands_crop_mask_median_Brazil_grasslands_crop_mask.csv")
grasslands_95HPD_S<-read.csv("SpreadValues/S/Brazil_grasslands_crop_mask_95%HPD_Brazil_grasslands_crop_mask.csv")
grasslands_median_M<-read.csv("SpreadValues/M/Brazil_grasslands_crop_mask_median_Brazil_grasslands_crop_mask.csv")
grasslands_95HPD_M<-read.csv("SpreadValues/M/Brazil_grasslands_crop_mask_95%HPD_Brazil_grasslands_crop_mask.csv")
grasslands_median_L<-read.csv("SpreadValues/L/Brazil_grasslands_crop_mask_median_Brazil_grasslands_crop_mask.csv")
grasslands_95HPD_L<-read.csv("SpreadValues/L/Brazil_grasslands_crop_mask_95%HPD_Brazil_grasslands_crop_mask.csv")

shrublands_median_S<-read.csv("SpreadValues/S/shrublands_median_shrublands.csv")
shrublands_95HPD_S<-read.csv("SpreadValues/S/shrublands_95%HPD_shrublands.csv")
shrublands_median_M<-read.csv("SpreadValues/M/shrublands_median_shrublands.csv")
shrublands_95HPD_M<-read.csv("SpreadValues/M/shrublands_95%HPD_shrublands.csv")
shrublands_median_L<-read.csv("SpreadValues/L/shrublands_median_shrublands.csv")
shrublands_95HPD_L<-read.csv("SpreadValues/L/shrublands_95%HPD_shrublands.csv")

water_median_S<-read.csv("SpreadValues/S/water_median_water.csv")
water_95HPD_S<-read.csv("SpreadValues/S/water_95%HPD_water.csv")
water_median_M<-read.csv("SpreadValues/M/water_median_water.csv")
water_95HPD_M<-read.csv("SpreadValues/M/water_95%HPD_water.csv")
water_median_L<-read.csv("SpreadValues/L/water_median_water.csv")
water_95HPD_L<-read.csv("SpreadValues/L/water_95%HPD_water.csv")

urban_median_S<-read.csv("SpreadValues/S/urban_median_urban.csv")
urban_95HPD_S<-read.csv("SpreadValues/S/urban_95%HPD_urban.csv")
urban_median_M<-read.csv("SpreadValues/M/urban_median_urban.csv")
urban_95HPD_M<-read.csv("SpreadValues/M/urban_95%HPD_urban.csv")
urban_median_L<-read.csv("SpreadValues/L/urban_median_urban.csv")
urban_95HPD_L<-read.csv("SpreadValues/L/urban_95%HPD_urban.csv")


culicoides_median_S<-read.csv("SpreadValues/S/culicoides_median_culicoides.csv")
culicoides_95HPD_S<-read.csv("SpreadValues/S/culicoides_95%HPD_culicoides.csv")
culicoides_median_M<-read.csv("SpreadValues/M/culicoides_median_culicoides.csv")
culicoides_95HPD_M<-read.csv("SpreadValues/M/culicoides_95%HPD_culicoides.csv")
culicoides_median_L<-read.csv("SpreadValues/L/culicoides_median_culicoides.csv")
culicoides_95HPD_L<-read.csv("SpreadValues/L/culicoides_95%HPD_culicoides.csv")


soilmoisture_median_S<-read.csv("SpreadValues/S/Brazil_soil_moisture_PM_median_Brazil_soil_moisture_PM.csv")
soilmoisture_95HPD_S<-read.csv("SpreadValues/S/Brazil_soil_moisture_PM_95%HPD_Brazil_soil_moisture_PM.csv")
soilmoisture_median_M<-read.csv("SpreadValues/M/Brazil_soil_moisture_PM_median_Brazil_soil_moisture_PM.csv")
soilmoisture_95HPD_M<-read.csv("SpreadValues/M/Brazil_soil_moisture_PM_95%HPD_Brazil_soil_moisture_PM.csv")
soilmoisture_median_L<-read.csv("SpreadValues/L/Soil_Moisture_Americas_PM_median_Soil_Moisture_Americas_PM.csv")
soilmoisture_95HPD_L<-read.csv("SpreadValues/L/Soil_Moisture_Americas_PM_95%HPD_Soil_Moisture_Americas_PM.csv")


herbaceous_vegetation_median_S<-read.csv("SpreadValues/S/herbaceous_vegetation_median_herbaceous_vegetation.csv")
herbaceous_vegetation_95HPD_S<-read.csv("SpreadValues/S/herbaceous_vegetation_95%HPD_herbaceous_vegetation.csv")
herbaceous_vegetation_median_M<-read.csv("SpreadValues/M/herbaceous_vegetation_median_herbaceous_vegetation.csv")
herbaceous_vegetation_95HPD_M<-read.csv("SpreadValues/M/herbaceous_vegetation_95%HPD_herbaceous_vegetation.csv")
herbaceous_vegetation_median_L<-read.csv("SpreadValues/L/herbaceous_vegetation_median_herbaceous_vegetation.csv")
herbaceous_vegetation_95HPD_L<-read.csv("SpreadValues/L/herbaceous_vegetation_95%HPD_herbaceous_vegetation.csv")


cultivated_managed_vegetation_median_S<-read.csv("SpreadValues/S/cultivated_managed_vegetation_median_cultivated_managed_vegetation.csv")
cultivated_managed_vegetation_95HPD_S<-read.csv("SpreadValues/S/cultivated_managed_vegetation_95%HPD_cultivated_managed_vegetation.csv")
cultivated_managed_vegetation_median_M<-read.csv("SpreadValues/M/cultivated_managed_vegetation_median_cultivated_managed_vegetation.csv")
cultivated_managed_vegetation_95HPD_M<-read.csv("SpreadValues/M/cultivated_managed_vegetation_95%HPD_cultivated_managed_vegetation.csv")
cultivated_managed_vegetation_median_L<-read.csv("SpreadValues/L/cultivated_managed_vegetation_median_cultivated_managed_vegetation.csv")
cultivated_managed_vegetation_95HPD_L<-read.csv("SpreadValues/L/cultivated_managed_vegetation_95%HPD_cultivated_managed_vegetation.csv")

regularly_flooded_vegetation_median_S<-read.csv("SpreadValues/S/regularly_flooded_vegetation_median_regularly_flooded_vegetation.csv")
regularly_flooded_vegetation_95HPD_S<-read.csv("SpreadValues/S/regularly_flooded_vegetation_95%HPD_regularly_flooded_vegetation.csv")
regularly_flooded_vegetation_median_M<-read.csv("SpreadValues/M/regularly_flooded_vegetation_median_regularly_flooded_vegetation.csv")
regularly_flooded_vegetation_95HPD_M<-read.csv("SpreadValues/M/regularly_flooded_vegetation_95%HPD_regularly_flooded_vegetation.csv")
regularly_flooded_vegetation_median_L<-read.csv("SpreadValues/L/regularly_flooded_vegetation_median_regularly_flooded_vegetation.csv")
regularly_flooded_vegetation_95HPD_L<-read.csv("SpreadValues/L/regularly_flooded_vegetation_95%HPD_regularly_flooded_vegetation.csv")


cocoa_median_S<-read.csv("SpreadValues/S/cocoa_median_cocoa.csv")
cocoa_95HPD_S<-read.csv("SpreadValues/S/cocoa_95%HPD_cocoa.csv")
cocoa_median_M<-read.csv("SpreadValues/M/cocoa_median_cocoa.csv")
cocoa_95HPD_M<-read.csv("SpreadValues/M/cocoa_95%HPD_cocoa.csv")
cocoa_median_L<-read.csv("SpreadValues/L/cocoa_median_cocoa.csv")
cocoa_95HPD_L<-read.csv("SpreadValues/L/cocoa_95%HPD_cocoa.csv")

soy_median_S<-read.csv("SpreadValues/S/soy_median_soy.csv")
soy_95HPD_S<-read.csv("SpreadValues/S/soy_95%HPD_soy.csv")
soy_median_M<-read.csv("SpreadValues/M/soy_median_soy.csv")
soy_95HPD_M<-read.csv("SpreadValues/M/soy_95%HPD_soy.csv")
soy_median_L<-read.csv("SpreadValues/L/soy_median_soy.csv")
soy_95HPD_L<-read.csv("SpreadValues/L/soy_95%HPD_soy.csv")

cattle_median_S<-read.csv("SpreadValues/S/cattle_median_cattle.csv")
cattle_95HPD_S<-read.csv("SpreadValues/S/cattle_95%HPD_cattle.csv")
cattle_median_M<-read.csv("SpreadValues/M/cattle_median_cattle.csv")
cattle_95HPD_M<-read.csv("SpreadValues/M/cattle_95%HPD_cattle.csv")
cattle_median_L<-read.csv("SpreadValues/L/cattle_median_cattle.csv")
cattle_95HPD_L<-read.csv("SpreadValues/L/cattle_95%HPD_cattle.csv")

coffee_median_S<-read.csv("SpreadValues/S/coffee_median_coffee.csv")
coffee_95HPD_S<-read.csv("SpreadValues/S/coffee_95%HPD_coffee.csv")
coffee_median_M<-read.csv("SpreadValues/M/coffee_median_coffee.csv")
coffee_95HPD_M<-read.csv("SpreadValues/M/coffee_95%HPD_coffee.csv")
coffee_median_L<-read.csv("SpreadValues/L/coffee_median_coffee.csv")
coffee_95HPD_L<-read.csv("SpreadValues/L/coffee_95%HPD_coffee.csv")

soy_median_S<-read.csv("SpreadValues/S/soy_median_soy.csv")
soy_95HPD_S<-read.csv("SpreadValues/S/soy_95%HPD_soy.csv")
soy_median_M<-read.csv("SpreadValues/M/soy_median_soy.csv")
soy_95HPD_M<-read.csv("SpreadValues/M/soy_95%HPD_soy.csv")
soy_median_L<-read.csv("SpreadValues/L/soy_median_soy.csv")
soy_95HPD_L<-read.csv("SpreadValues/L/soy_95%HPD_soy.csv")

bananas_median_S<-read.csv("SpreadValues/S/banana_median_banana.csv")
bananas_95HPD_S<-read.csv("SpreadValues/S/banana_95%HPD_banana.csv")
bananas_median_M<-read.csv("SpreadValues/M/banana_median_banana.csv")
bananas_95HPD_M<-read.csv("SpreadValues/M/banana_95%HPD_banana.csv")
bananas_median_L<-read.csv("SpreadValues/L/banana_median_banana.csv")
bananas_95HPD_L<-read.csv("SpreadValues/L/banana_95%HPD_banana.csv")

maize_median_S<-read.csv("SpreadValues/S/maize_median_maize.csv")
maize_95HPD_S<-read.csv("SpreadValues/S/maize_95%HPD_maize.csv")
maize_median_M<-read.csv("SpreadValues/M/maize_median_maize.csv")
maize_95HPD_M<-read.csv("SpreadValues/M/maize_95%HPD_maize.csv")
maize_median_L<-read.csv("SpreadValues/L/maize_median_maize.csv")
maize_95HPD_L<-read.csv("SpreadValues/L/maize_95%HPD_maize.csv")

cassava_median_S<-read.csv("SpreadValues/S/cassava_median_cassava.csv")
cassava_95HPD_S<-read.csv("SpreadValues/S/cassava_95%HPD_cassava.csv")
cassava_median_M<-read.csv("SpreadValues/M/cassava_median_cassava.csv")
cassava_95HPD_M<-read.csv("SpreadValues/M/cassava_95%HPD_cassava.csv")
cassava_median_L<-read.csv("SpreadValues/L/cassava_median_cassava.csv")
cassava_95HPD_L<-read.csv("SpreadValues/L/cassava_95%HPD_cassava.csv")

sugarcane_median_S<-read.csv("SpreadValues/S/sugarcane_median_sugarcane.csv")
sugarcane_95HPD_S<-read.csv("SpreadValues/S/sugarcane_95%HPD_sugarcane.csv")
sugarcane_median_M<-read.csv("SpreadValues/M/sugarcane_median_sugarcane.csv")
sugarcane_95HPD_M<-read.csv("SpreadValues/M/sugarcane_95%HPD_sugarcane.csv")
sugarcane_median_L<-read.csv("SpreadValues/L/sugarcane_median_sugarcane.csv")
sugarcane_95HPD_L<-read.csv("SpreadValues/L/sugarcane_95%HPD_sugarcane.csv")

mixed_trees_median_S<-read.csv("SpreadValues/S/mixed_trees_median_mixed_trees.csv")
mixed_trees_95HPD_S<-read.csv("SpreadValues/S/mixed_trees_95%HPD_mixed_trees.csv")
mixed_trees_median_M<-read.csv("SpreadValues/M/mixed_trees_median_mixed_trees.csv")
mixed_trees_95HPD_M<-read.csv("SpreadValues/M/mixed_trees_95%HPD_mixed_trees.csv")
mixed_trees_median_L<-read.csv("SpreadValues/L/mixed_trees_median_mixed_trees.csv")
mixed_trees_95HPD_L<-read.csv("SpreadValues/L/mixed_trees_95%HPD_mixed_trees.csv")

forest_loss_median_S<-read.csv("SpreadValues/S/forest_loss_median_forest_loss.csv")
forest_loss_95HPD_S<-read.csv("SpreadValues/S/forest_loss_95%HPD_forest_loss.csv")
forest_loss_median_M<-read.csv("SpreadValues/M/forest_loss_median_forest_loss.csv")
forest_loss_95HPD_M<-read.csv("SpreadValues/M/forest_loss_95%HPD_forest_loss.csv")
forest_loss_median_L<-read.csv("SpreadValues/L/forest_loss_median_forest_loss.csv")
forest_loss_95HPD_L<-read.csv("SpreadValues/L/forest_loss_95%HPD_forest_loss.csv")


temp_values<-ggplot()+
  ggtitle("Mean Annual Temperature")+
  theme_classic()+theme(axis.title = element_blank(),plot.title = element_text(size=6),axis.text = element_text(size=6))+
  geom_ribbon(data=temp_95HPD_L,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#bf0603')+
  geom_ribbon(data=temp_95HPD_M,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#3d405b')+
  geom_ribbon(data=temp_95HPD_S,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#40916c')+
  geom_line(data=temp_median_L,
            aes(x=time,y=Brazil_annual_mean_temp_2020_crop_mask-1),
            colour='#bf0603')+
  geom_line(data=temp_median_M,
            aes(x=time,y=Brazil_annual_mean_temp_2020_crop_mask-1),
            colour='#3d405b')+
  geom_line(data=temp_median_S,
            aes(x=time,y=Brazil_annual_mean_temp_2020_crop_mask-1),
            colour='#40916c')+
  geom_vline(xintercept = 2023.5, linetype="dashed",color = "grey50", size=0.5)+
  geom_vline(xintercept = 2024, linetype="dashed",color = "grey80", size=0.5)
  
  temp_values


precipitation_values<-ggplot()+
  ggtitle("Mean monthly precipitation (mm)")+
  theme_classic()+theme(axis.title = element_blank(),plot.title = element_text(size=6),axis.text = element_text(size=6))+
  geom_ribbon(data=precipitation_95HPD_L,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#bf0603')+
  geom_ribbon(data=precipitation_95HPD_M,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#3d405b')+
  geom_ribbon(data=precipitation_95HPD_S,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#40916c')+
  geom_line(data=precipitation_median_L,
            aes(x=time,y=Brazil_precipitation_mean_2023_crop_mask-1),
            colour='#bf0603')+
  geom_line(data=precipitation_median_M,
            aes(x=time,y=Brazil_precipitation_mean_2023_crop_mask-1),
            colour='#3d405b')+
  geom_line(data=precipitation_median_S,
            aes(x=time,y=Brazil_precipitation_mean_2023_crop_mask-1),
            colour='#40916c')+
  geom_vline(xintercept = 2023.5, linetype="dashed",color = "grey50", size=0.5)+
  geom_vline(xintercept = 2024, linetype="dashed",color = "grey80", size=0.5)+
  xlim(2021,2024.5)
precipitation_values


pop_values<-ggplot()+
  ggtitle("Population density (per km2)")+
  theme_classic()+theme(axis.title = element_blank(),plot.title = element_text(size=6),axis.text = element_text(size=6))+
  geom_ribbon(data=pop_95HPD_L,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#bf0603')+
  geom_ribbon(data=pop_95HPD_M,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#3d405b')+
  geom_ribbon(data=pop_95HPD_S,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#40916c')+
  geom_line(data=pop_median_L,
            aes(x=time,y=pop_2000_scaled_log-1),
            colour='#bf0603')+
  geom_line(data=pop_median_M,
            aes(x=time,y=pop_2000_scaled_log-1),
            colour='#3d405b')+
  geom_line(data=pop_median_S,
            aes(x=time,y=pop_2000_scaled_log-1),
            colour='#40916c')+
  geom_vline(xintercept = 2023.5, linetype="dashed",color = "grey50", size=0.5)+
  geom_vline(xintercept = 2024, linetype="dashed",color = "grey80", size=0.5)+
  xlim(2021,2024.5)

pop_values



pop2020_values<-ggplot()+
  ggtitle("Population Density (2020)")+
  theme_classic()+theme(axis.title = element_blank(),plot.title = element_text(size=6),axis.text = element_text(size=6))+
  geom_ribbon(data=pop2020_95HPD_L,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#bf0603')+
  geom_ribbon(data=pop2020_95HPD_M,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#3d405b')+
  geom_ribbon(data=pop2020_95HPD_S,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#40916c')+
  geom_line(data=pop2020_median_L,
            aes(x=time,y=pop_2020_raw-1),
            colour='#bf0603')+
  geom_line(data=pop2020_median_M,
            aes(x=time,y=pop_2020_raw-1),
            colour='#3d405b')+
  geom_line(data=pop2020_median_S,
            aes(x=time,y=pop_2020_raw-1),
            colour='#40916c')+
  geom_vline(xintercept = 2023.5, linetype="dashed",color = "black", size=0.5)+
  geom_vline(xintercept = 2024, linetype="dashed",color = "black", size=0.5)
pop2020_values


elevation_values<-ggplot()+
  ggtitle("Elevation (m)")+
  theme_classic()+theme(axis.title = element_blank(),plot.title = element_text(size=6),axis.text = element_text(size=6))+
  geom_ribbon(data=elevation_95HPD_L,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#bf0603')+
  geom_ribbon(data=elevation_95HPD_M,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#3d405b')+
  geom_ribbon(data=elevation_95HPD_S,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#40916c')+
  geom_line(data=elevation_median_L,
            aes(x=time,y=elevation-1),
            colour='#bf0603')+
  geom_line(data=elevation_median_M,
            aes(x=time,y=elevation-1),
            colour='#3d405b')+
  geom_line(data=elevation_median_S,
            aes(x=time,y=elevation-1),
            colour='#40916c')+
  geom_vline(xintercept = 2023.5, linetype="dashed",color = "grey50", size=0.5)+
  geom_vline(xintercept = 2024, linetype="dashed",color = "grey80", size=0.5)+
  xlim(2021,2024.5)


elevation_values


forest_loss_values<-ggplot()+
  ggtitle("Forest loss (2020-2023)")+
  theme_classic()+theme(axis.title = element_blank(),plot.title = element_text(size=6),axis.text = element_text(size=6))+
  geom_ribbon(data=forest_loss_95HPD_L,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#bf0603')+
  geom_ribbon(data=forest_loss_95HPD_M,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#3d405b')+
  geom_ribbon(data=forest_loss_95HPD_S,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#40916c')+
  geom_line(data=forest_loss_median_L,
            aes(x=time,y=forest_loss-1),
            colour='#bf0603')+
  geom_line(data=forest_loss_median_M,
            aes(x=time,y=forest_loss-1),
            colour='#3d405b')+
  geom_line(data=forest_loss_median_S,
            aes(x=time,y=forest_loss-1),
            colour='#40916c')+
  xlim(2021,2024.5)+
  geom_vline(xintercept = 2023.5, linetype="dashed",color = "grey50", size=0.5)+
  geom_vline(xintercept = 2024, linetype="dashed",color = "grey80", size=0.5)
forest_loss_values


mixed_trees_values<-ggplot()+
  ggtitle("Mixed trees forest cover (%)")+
  theme_classic()+theme(axis.title = element_blank(),plot.title = element_text(size=6),axis.text = element_text(size=6))+
  geom_ribbon(data=mixed_trees_95HPD_L,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#bf0603')+
  geom_ribbon(data=mixed_trees_95HPD_M,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#3d405b')+
  geom_ribbon(data=mixed_trees_95HPD_S,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#40916c')+
  geom_line(data=mixed_trees_median_L,
            aes(x=time,y=mixed_trees-1),
            colour='#bf0603')+
  geom_line(data=mixed_trees_median_M,
            aes(x=time,y=mixed_trees-1),
            colour='#3d405b')+
  geom_line(data=mixed_trees_median_S,
            aes(x=time,y=mixed_trees-1),
            colour='#40916c')+
  geom_vline(xintercept = 2023.5, linetype="dashed",color = "grey50", size=0.5)+
  geom_vline(xintercept = 2024, linetype="dashed",color = "grey80", size=0.5)+
  xlim(2021,2024.5)
mixed_trees_values


evergreen_broadleaf_values<-ggplot()+
  ggtitle("Evergreen broadleaf forest cover (%)")+
  theme_classic()+theme(axis.title = element_blank(),plot.title = element_text(size=6),axis.text = element_text(size=6))+
  geom_ribbon(data=evergreen_95HPD_L,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#bf0603')+
  geom_ribbon(data=evergreen_95HPD_M,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#3d405b')+
  geom_ribbon(data=evergreen_95HPD_S,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#40916c')+
  geom_line(data=evergreen_median_L,
            aes(x=time,y=evergreen_broadleaf-1),
            colour='#bf0603')+
  geom_line(data=evergreen_median_M,
            aes(x=time,y=evergreen_broadleaf-1),
            colour='#3d405b')+
  geom_line(data=evergreen_median_S,
            aes(x=time,y=evergreen_broadleaf-1),
            colour='#40916c')+
  geom_vline(xintercept = 2023.5, linetype="dashed",color = "grey50", size=0.5)+
  geom_vline(xintercept = 2024, linetype="dashed",color = "grey80", size=0.5)+
  xlim(2021,2024.5)

evergreen_broadleaf_values


deciduous_broadleaf_values<-ggplot()+
  ggtitle("Deciduous broadleaf forest cover (%)")+
  theme_classic()+theme(axis.title = element_blank(),plot.title = element_text(size=6),axis.text = element_text(size=6))+
  geom_ribbon(data=deciduous_95HPD_L,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#bf0603')+
  geom_ribbon(data=deciduous_95HPD_M,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#3d405b')+
  geom_ribbon(data=deciduous_95HPD_S,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#40916c')+
  geom_line(data=deciduous_median_L,
            aes(x=time,y=deciduous_broadleaf-1),
            colour='#bf0603')+
  geom_line(data=deciduous_median_M,
            aes(x=time,y=deciduous_broadleaf-1),
            colour='#3d405b')+
  geom_line(data=deciduous_median_S,
            aes(x=time,y=deciduous_broadleaf-1),
            colour='#40916c')+
  geom_vline(xintercept = 2023.5, linetype="dashed",color = "grey50", size=0.5)+
  geom_vline(xintercept = 2024, linetype="dashed",color = "grey80", size=0.5)+
  xlim(2021,2024.5)

deciduous_broadleaf_values



croplands_values<-ggplot()+
  ggtitle("Croplands cover (%)")+
  theme_classic()+theme(axis.title = element_blank(),plot.title = element_text(size=6),axis.text = element_text(size=6))+
  geom_ribbon(data=croplands_95HPD_L,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#bf0603')+
  geom_ribbon(data=croplands_95HPD_M,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#3d405b')+
  geom_ribbon(data=croplands_95HPD_S,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#40916c')+
  geom_line(data=croplands_median_L,
            aes(x=time,y=Brazil_Croplands_2019-1),
            colour='#bf0603')+
  geom_line(data=croplands_median_M,
            aes(x=time,y=Brazil_Croplands_2019-1),
            colour='#3d405b')+
  geom_line(data=croplands_median_S,
            aes(x=time,y=Brazil_Croplands_2019-1),
            colour='#40916c')+
  geom_vline(xintercept = 2023.5, linetype="dashed",color = "grey50", size=0.5)+
  geom_vline(xintercept = 2024, linetype="dashed",color = "grey80", size=0.5)+
  xlim(2021,2024.5)

croplands_values



croplands_natural_vegetation_values<-ggplot()+
  ggtitle("Croplands and natural vegetation cover (%)")+
  theme_classic()+theme(axis.title = element_blank(),plot.title = element_text(size=6),axis.text = element_text(size=6))+
  geom_ribbon(data=croplands_natural_vegetation_95HPD_L,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#bf0603')+
  geom_ribbon(data=croplands_natural_vegetation_95HPD_M,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#3d405b')+
  geom_ribbon(data=croplands_natural_vegetation_95HPD_S,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#40916c')+
  geom_line(data=croplands_natural_vegetation_median_L,
            aes(x=time,y=Brazil_cropland_natural_vegetation_crop_mask-1),
            colour='#bf0603')+
  geom_line(data=croplands_natural_vegetation_median_M,
            aes(x=time,y=Brazil_cropland_natural_vegetation_crop_mask-1),
            colour='#3d405b')+
  geom_line(data=croplands_natural_vegetation_median_S,
            aes(x=time,y=Brazil_cropland_natural_vegetation_crop_mask-1),
            colour='#40916c')+
  geom_vline(xintercept = 2023.5, linetype="dashed",color = "grey50", size=0.5)+
  geom_vline(xintercept = 2024, linetype="dashed",color = "grey80", size=0.5)+
  xlim(2021,2024.5)

croplands_natural_vegetation_values


grasslands_values<-ggplot()+
  ggtitle("Grasslands cover (%)")+
  theme_classic()+theme(axis.title = element_blank(),plot.title = element_text(size=6),axis.text = element_text(size=6))+
  geom_ribbon(data=grasslands_95HPD_L,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#bf0603')+
  geom_ribbon(data=grasslands_95HPD_M,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#3d405b')+
  geom_ribbon(data=grasslands_95HPD_S,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#40916c')+
  geom_line(data=grasslands_median_L,
            aes(x=time,y=Brazil_grasslands_crop_mask-1),
            colour='#bf0603')+
  geom_line(data=grasslands_median_M,
            aes(x=time,y=Brazil_grasslands_crop_mask-1),
            colour='#3d405b')+
  geom_line(data=grasslands_median_S,
            aes(x=time,y=Brazil_grasslands_crop_mask-1),
            colour='#40916c')+
  geom_vline(xintercept = 2023.5, linetype="dashed",color = "grey50", size=0.5)+
  geom_vline(xintercept = 2024, linetype="dashed",color = "grey80", size=0.5)+
  xlim(2021,2024.5)

grasslands_values



savannas_values<-ggplot()+
  ggtitle("Savannas cover (%)")+
  theme_classic()+theme(axis.title = element_blank(),plot.title = element_text(size=6),axis.text = element_text(size=6))+
  geom_ribbon(data=savannas_95HPD_L,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#bf0603')+
  geom_ribbon(data=savannas_95HPD_M,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#3d405b')+
  geom_ribbon(data=savannas_95HPD_S,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#40916c')+
  geom_line(data=savannas_median_L,
            aes(x=time,y=Brazil_savannas_crop_mask-1),
            colour='#bf0603')+
  geom_line(data=savannas_median_M,
            aes(x=time,y=Brazil_savannas_crop_mask-1),
            colour='#3d405b')+
  geom_line(data=savannas_median_S,
            aes(x=time,y=Brazil_savannas_crop_mask-1),
            colour='#40916c')+
  geom_vline(xintercept = 2023.5, linetype="dashed",color = "grey50", size=0.5)+
  geom_vline(xintercept = 2024, linetype="dashed",color = "grey80", size=0.5)+
  xlim(2021,2024.5)

savannas_values


woodysavannas_values<-ggplot()+
  ggtitle("Woody savannas cover (%)")+
  theme_classic()+theme(axis.title = element_blank(),plot.title = element_text(size=6),axis.text = element_text(size=6))+
  geom_ribbon(data=woody_savannas_95HPD_L,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#bf0603')+
  geom_ribbon(data=woody_savannas_95HPD_M,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#3d405b')+
  geom_ribbon(data=woody_savannas_95HPD_S,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#40916c')+
  geom_line(data=woody_savannas_median_L,
            aes(x=time,y=Brazil_woody_savannas_crop_mask-1),
            colour='#bf0603')+
  geom_line(data=woody_savannas_median_M,
            aes(x=time,y=Brazil_woody_savannas_crop_mask-1),
            colour='#3d405b')+
  geom_line(data=woody_savannas_median_S,
            aes(x=time,y=Brazil_woody_savannas_crop_mask-1),
            colour='#40916c')+
  geom_vline(xintercept = 2023.5, linetype="dashed",color = "grey50", size=0.5)+
  geom_vline(xintercept = 2024, linetype="dashed",color = "grey80", size=0.5)+
  xlim(2021,2024.5)

woodysavannas_values

shrublands_values<-ggplot()+
  ggtitle("Shrublands cover (%)")+
  theme_classic()+theme(axis.title = element_blank(),plot.title = element_text(size=6),axis.text = element_text(size=6))+
  geom_ribbon(data=shrublands_95HPD_L,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#bf0603')+
  geom_ribbon(data=shrublands_95HPD_M,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#3d405b')+
  geom_ribbon(data=shrublands_95HPD_S,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#40916c')+
  geom_line(data=shrublands_median_L,
            aes(x=time,y=shrublands-1),
            colour='#bf0603')+
  geom_line(data=shrublands_median_M,
            aes(x=time,y=shrublands-1),
            colour='#3d405b')+
  geom_line(data=shrublands_median_S,
            aes(x=time,y=shrublands-1),
            colour='#40916c')+
  geom_vline(xintercept = 2023.5, linetype="dashed",color = "grey50", size=0.5)+
  geom_vline(xintercept = 2024, linetype="dashed",color = "grey80", size=0.5)+
  xlim(2021,2024.5)

shrublands_values


culicoides_values<-ggplot()+
  ggtitle(substitute(paste(italic("Culicoides paraensis")," suitability")))+
  theme_classic()+theme(axis.title = element_blank(),plot.title = element_text(size=6),axis.text = element_text(size=6))+
  geom_ribbon(data=culicoides_95HPD_L,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#bf0603')+
  geom_ribbon(data=culicoides_95HPD_M,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#3d405b')+
  geom_ribbon(data=culicoides_95HPD_S,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#40916c')+
  geom_line(data=culicoides_median_L,
            aes(x=time,y=culicoides-1),
            colour='#bf0603')+
  geom_line(data=culicoides_median_M,
            aes(x=time,y=culicoides-1),
            colour='#3d405b')+
  geom_line(data=culicoides_median_S,
            aes(x=time,y=culicoides-1),
            colour='#40916c')+
  geom_vline(xintercept = 2023.5, linetype="dashed",color = "grey50", size=0.5)+
  geom_vline(xintercept = 2024, linetype="dashed",color = "grey80", size=0.5)+
  xlim(2021,2024.5)

culicoides_values



soilmoisture_values<-ggplot()+
  ggtitle("Soil Moisture")+
  theme_classic()+theme(axis.title = element_blank(),plot.title = element_text(size=6),axis.text = element_text(size=6))+
  geom_ribbon(data=soilmoisture_95HPD_L,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#bf0603')+
  geom_ribbon(data=soilmoisture_95HPD_M,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#3d405b')+
  geom_ribbon(data=soilmoisture_95HPD_S,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#40916c')+
  geom_line(data=soilmoisture_median_L,
            aes(x=time,y=Soil_Moisture_Americas_PM-1),
            colour='#bf0603')+
  geom_line(data=soilmoisture_median_M,
            aes(x=time,y=Brazil_soil_moisture_PM-1),
            colour='#3d405b')+
  geom_line(data=soilmoisture_median_S,
            aes(x=time,y=Brazil_soil_moisture_PM-1),
            colour='#40916c')+
  geom_vline(xintercept = 2023.5, linetype="dashed",color = "grey50", size=0.5)+
  geom_vline(xintercept = 2024, linetype="dashed",color = "grey80", size=0.5)+
  xlim(2021,2024.5)

soilmoisture_values

herbaceous_vegetation_values<-ggplot()+
  ggtitle("Herbaceous vegetation cover (%)")+
  theme_classic()+theme(axis.title = element_blank(),plot.title = element_text(size=6),axis.text = element_text(size=6))+
  geom_ribbon(data=herbaceous_vegetation_95HPD_L,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#bf0603')+
  geom_ribbon(data=herbaceous_vegetation_95HPD_M,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#3d405b')+
  geom_ribbon(data=herbaceous_vegetation_95HPD_S,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#40916c')+
  geom_line(data=herbaceous_vegetation_median_L,
            aes(x=time,y=herbaceous_vegetation-1),
            colour='#bf0603')+
  geom_line(data=herbaceous_vegetation_median_M,
            aes(x=time,y=herbaceous_vegetation-1),
            colour='#3d405b')+
  geom_line(data=herbaceous_vegetation_median_S,
            aes(x=time,y=herbaceous_vegetation-1),
            colour='#40916c')+
  geom_vline(xintercept = 2023.5, linetype="dashed",color = "grey50", size=0.5)+
  geom_vline(xintercept = 2024, linetype="dashed",color = "grey80", size=0.5)+
  xlim(2021,2024.5)

herbaceous_vegetation_values


cultivated_managed_vegetation_values<-ggplot()+
  ggtitle("Cultivated and managed vegetation cover (%)")+
  theme_classic()+theme(axis.title = element_blank(),plot.title = element_text(size=6),axis.text = element_text(size=6))+
  geom_ribbon(data=cultivated_managed_vegetation_95HPD_L,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#bf0603')+
  geom_ribbon(data=cultivated_managed_vegetation_95HPD_M,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#3d405b')+
  geom_ribbon(data=cultivated_managed_vegetation_95HPD_S,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#40916c')+
  geom_line(data=cultivated_managed_vegetation_median_L,
            aes(x=time,y=cultivated_managed_vegetation-1),
            colour='#bf0603')+
  geom_line(data=cultivated_managed_vegetation_median_M,
            aes(x=time,y=cultivated_managed_vegetation-1),
            colour='#3d405b')+
  geom_line(data=cultivated_managed_vegetation_median_S,
            aes(x=time,y=cultivated_managed_vegetation-1),
            colour='#40916c')+
  geom_vline(xintercept = 2023.5, linetype="dashed",color = "grey50", size=0.5)+
  geom_vline(xintercept = 2024, linetype="dashed",color = "grey80", size=0.5)+
  xlim(2021,2024.5)

cultivated_managed_vegetation_values

regularly_flooded_vegetation_values<-ggplot()+
  ggtitle("Regularly flooded vegetation cover (%)")+
  theme_classic()+theme(axis.title = element_blank(),plot.title = element_text(size=6),axis.text = element_text(size=6))+
  geom_ribbon(data=regularly_flooded_vegetation_95HPD_L,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#bf0603')+
  geom_ribbon(data=regularly_flooded_vegetation_95HPD_M,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#3d405b')+
  geom_ribbon(data=regularly_flooded_vegetation_95HPD_S,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#40916c')+
  geom_line(data=regularly_flooded_vegetation_median_L,
            aes(x=time,y=regularly_flooded_vegetation-1),
            colour='#bf0603')+
  geom_line(data=regularly_flooded_vegetation_median_M,
            aes(x=time,y=regularly_flooded_vegetation-1),
            colour='#3d405b')+
  geom_line(data=regularly_flooded_vegetation_median_S,
            aes(x=time,y=regularly_flooded_vegetation-1),
            colour='#40916c')+
  geom_vline(xintercept = 2023.5, linetype="dashed",color = "grey50", size=0.5)+
  geom_vline(xintercept = 2024, linetype="dashed",color = "grey80", size=0.5)+
  xlim(2021,2024.5)

regularly_flooded_vegetation_values

water_values<-ggplot()+
  ggtitle("Water occurrence")+
  theme_classic()+theme(axis.title = element_blank(),plot.title = element_text(size=6),axis.text = element_text(size=6))+
  geom_ribbon(data=water_95HPD_L,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#bf0603')+
  geom_ribbon(data=water_95HPD_M,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#3d405b')+
  geom_ribbon(data=water_95HPD_S,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#40916c')+
  geom_line(data=water_median_L,
            aes(x=time,y=water-1),
            colour='#bf0603')+
  geom_line(data=water_median_M,
            aes(x=time,y=water-1),
            colour='#3d405b')+
  geom_line(data=water_median_S,
            aes(x=time,y=water-1),
            colour='#40916c')+
  geom_vline(xintercept = 2023.5, linetype="dashed",color = "grey50", size=0.5)+
  geom_vline(xintercept = 2024, linetype="dashed",color = "grey80", size=0.5)+
  xlim(2021,2024.5)

water_values


urban_values<-ggplot()+
  ggtitle("Urban and built-up cover (%)")+
  theme_classic()+theme(axis.title = element_blank(),plot.title = element_text(size=6),axis.text = element_text(size=6))+
  geom_ribbon(data=urban_95HPD_L,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#bf0603')+
  geom_ribbon(data=urban_95HPD_M,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#3d405b')+
  geom_ribbon(data=urban_95HPD_S,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#40916c')+
  geom_line(data=urban_median_L,
            aes(x=time,y=urban-1),
            colour='#bf0603')+
  geom_line(data=urban_median_M,
            aes(x=time,y=urban-1),
            colour='#3d405b')+
  geom_line(data=urban_median_S,
            aes(x=time,y=urban-1),
            colour='#40916c')+
  geom_vline(xintercept = 2023.5, linetype="dashed",color = "grey50", size=0.5)+
  geom_vline(xintercept = 2024, linetype="dashed",color = "grey80", size=0.5)+
  xlim(2021,2024.5)

urban_values


cocoa_values<-ggplot()+
  ggtitle("Cocoa harvested area (ha)")+
  theme_classic()+theme(axis.title = element_blank(),plot.title = element_text(size=6),axis.text = element_text(size=6))+
  geom_ribbon(data=cocoa_95HPD_L,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#bf0603')+
  geom_ribbon(data=cocoa_95HPD_M,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#3d405b')+
  geom_ribbon(data=cocoa_95HPD_S,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#40916c')+
  geom_line(data=cocoa_median_L,
            aes(x=time,y=cocoa-1),
            colour='#bf0603')+
  geom_line(data=cocoa_median_M,
            aes(x=time,y=cocoa-1),
            colour='#3d405b')+
  geom_line(data=cocoa_median_S,
            aes(x=time,y=cocoa-1),
            colour='#40916c')+
  geom_vline(xintercept = 2023.5, linetype="dashed",color = "grey50", size=0.5)+
  geom_vline(xintercept = 2024, linetype="dashed",color = "grey80", size=0.5)+
  xlim(2021,2024.5)

cocoa_values


soy_values<-ggplot()+
  ggtitle("Soy harvested area (ha)")+
  theme_classic()+theme(axis.title = element_blank(),plot.title = element_text(size=6),axis.text = element_text(size=6))+
  geom_ribbon(data=soy_95HPD_L,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#bf0603')+
  geom_ribbon(data=soy_95HPD_M,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#3d405b')+
  geom_ribbon(data=soy_95HPD_S,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#40916c')+
  geom_line(data=soy_median_L,
            aes(x=time,y=soy-1),
            colour='#bf0603')+
  geom_line(data=soy_median_M,
            aes(x=time,y=soy-1),
            colour='#3d405b')+
  geom_line(data=soy_median_S,
            aes(x=time,y=soy-1),
            colour='#40916c')+
  geom_vline(xintercept = 2023.5, linetype="dashed",color = "grey50", size=0.5)+
  geom_vline(xintercept = 2024, linetype="dashed",color = "grey80", size=0.5)+
  xlim(2021,2024.5)

soy_values

sugarcane_values<-ggplot()+
  ggtitle("Sugarcane harvested area (ha)")+
  theme_classic()+theme(axis.title = element_blank(),plot.title = element_text(size=6),axis.text = element_text(size=6))+
  geom_ribbon(data=sugarcane_95HPD_L,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#bf0603')+
  geom_ribbon(data=sugarcane_95HPD_M,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#3d405b')+
  geom_ribbon(data=sugarcane_95HPD_S,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#40916c')+
  geom_line(data=sugarcane_median_L,
            aes(x=time,y=sugarcane-1),
            colour='#bf0603')+
  geom_line(data=sugarcane_median_M,
            aes(x=time,y=sugarcane-1),
            colour='#3d405b')+
  geom_line(data=sugarcane_median_S,
            aes(x=time,y=sugarcane-1),
            colour='#40916c')+
  geom_vline(xintercept = 2023.5, linetype="dashed",color = "grey50", size=0.5)+
  geom_vline(xintercept = 2024, linetype="dashed",color = "grey80", size=0.5)+
  xlim(2021,2024.5)

sugarcane_values

cattle_values<-ggplot()+
  ggtitle("Cattle farm area (ha)")+
  theme_classic()+theme(axis.title = element_blank(),plot.title = element_text(size=6),axis.text = element_text(size=6))+
  geom_ribbon(data=cattle_95HPD_L,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#bf0603')+
  geom_ribbon(data=cattle_95HPD_M,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#3d405b')+
  geom_ribbon(data=cattle_95HPD_S,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#40916c')+
  geom_line(data=cattle_median_L,
            aes(x=time,y=cattle-1),
            colour='#bf0603')+
  geom_line(data=cattle_median_M,
            aes(x=time,y=cattle-1),
            colour='#3d405b')+
  geom_line(data=cattle_median_S,
            aes(x=time,y=cattle-1),
            colour='#40916c')+
  geom_vline(xintercept = 2023.5, linetype="dashed",color = "grey50", size=0.5)+
  geom_vline(xintercept = 2024, linetype="dashed",color = "grey80", size=0.5)+
  xlim(2021,2024.5)

cattle_values


coffee_values<-ggplot()+
  ggtitle("Coffee harvested area (ha)")+
  theme_classic()+theme(axis.title = element_blank(),plot.title = element_text(size=6),axis.text = element_text(size=6))+
  geom_ribbon(data=coffee_95HPD_L,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#bf0603')+
  geom_ribbon(data=coffee_95HPD_M,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#3d405b')+
  geom_ribbon(data=coffee_95HPD_S,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#40916c')+
  geom_line(data=coffee_median_L,
            aes(x=time,y=coffee-1),
            colour='#bf0603')+
  geom_line(data=coffee_median_M,
            aes(x=time,y=coffee-1),
            colour='#3d405b')+
  geom_line(data=coffee_median_S,
            aes(x=time,y=coffee-1),
            colour='#40916c')+
  geom_vline(xintercept = 2023.5, linetype="dashed",color = "grey50", size=0.5)+
  geom_vline(xintercept = 2024, linetype="dashed",color = "grey80", size=0.5)+
  xlim(2021,2024.5)

coffee_values



maize_values<-ggplot()+
  ggtitle("Maize harvested area (ha)")+
  theme_classic()+theme(axis.title = element_blank(),plot.title = element_text(size=6),axis.text = element_text(size=6))+
  geom_ribbon(data=maize_95HPD_L,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#bf0603')+
  geom_ribbon(data=maize_95HPD_M,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#3d405b')+
  geom_ribbon(data=maize_95HPD_S,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#40916c')+
  geom_line(data=maize_median_L,
            aes(x=time,y=maize-1),
            colour='#bf0603')+
  geom_line(data=maize_median_M,
            aes(x=time,y=maize-1),
            colour='#3d405b')+
  geom_line(data=maize_median_S,
            aes(x=time,y=maize-1),
            colour='#40916c')+
  geom_vline(xintercept = 2023.5, linetype="dashed",color = "grey50", size=0.5)+
  geom_vline(xintercept = 2024, linetype="dashed",color = "grey80", size=0.5)+
  xlim(2021,2024.5)

maize_values

cassava_values<-ggplot()+
  ggtitle("Cassava harvested area (ha)")+
  theme_classic()+theme(axis.title = element_blank(),plot.title = element_text(size=6),axis.text = element_text(size=6))+
  geom_ribbon(data=cassava_95HPD_L,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#bf0603')+
  geom_ribbon(data=cassava_95HPD_M,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#3d405b')+
  geom_ribbon(data=cassava_95HPD_S,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#40916c')+
  geom_line(data=cassava_median_L,
            aes(x=time,y=cassava-1),
            colour='#bf0603')+
  geom_line(data=cassava_median_M,
            aes(x=time,y=cassava-1),
            colour='#3d405b')+
  geom_line(data=cassava_median_S,
            aes(x=time,y=cassava-1),
            colour='#40916c')+
  geom_vline(xintercept = 2023.5, linetype="dashed",color = "grey50", size=0.5)+
  geom_vline(xintercept = 2024, linetype="dashed",color = "grey80", size=0.5)+
  xlim(2021,2024.5)

cassava_values

bananas_values<-ggplot()+
  ggtitle("Banana harvested area (ha)")+
  theme_classic()+theme(axis.title = element_blank(),plot.title = element_text(size=6),axis.text = element_text(size=6))+
  geom_ribbon(data=bananas_95HPD_L,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#bf0603')+
  geom_ribbon(data=bananas_95HPD_M,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#3d405b')+
  geom_ribbon(data=bananas_95HPD_S,
              aes(x=time,ymin=X95.HPD_lower_value-1,ymax=X95.HPD_higher_value-1),
              alpha=0.2,fill='#40916c')+
  geom_line(data=bananas_median_L,
            aes(x=time,y=banana-1),
            colour='#bf0603')+
  geom_line(data=bananas_median_M,
            aes(x=time,y=banana-1),
            colour='#3d405b')+
  geom_line(data=bananas_median_S,
            aes(x=time,y=banana-1),
            colour='#40916c')+
  geom_vline(xintercept = 2023.5, linetype="dashed",color = "grey50", size=0.5)+
  geom_vline(xintercept = 2024, linetype="dashed",color = "grey80", size=0.5)+
  xlim(2021,2024.5)

bananas_values

title <- ggdraw() + draw_label("Environmental values associated with branch dispersal locations", fontface='bold',size=8)

figure<-plot_grid(pop_values,urban_values,elevation_values,herbaceous_vegetation_values,
                   cultivated_managed_vegetation_values,regularly_flooded_vegetation_values,
                   culicoides_values, soilmoisture_values, evergreen_broadleaf_values,deciduous_broadleaf_values,mixed_trees_values ,grasslands_values,
                   shrublands_values,savannas_values, woodysavannas_values,
                   croplands_natural_vegetation_values,croplands_values,water_values,
                   precipitation_values,temp_values,
                   cocoa_values,soy_values,cattle_values,bananas_values,cassava_values,
                   coffee_values,sugarcane_values,maize_values,
                   forest_loss_values,
                   ncol=4)
figure

plot_grid(title,figure,ncol=1,rel_heights = c(0.05,0.95))
