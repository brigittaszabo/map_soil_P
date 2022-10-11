# --------------------------------------------------------------------------
#        Derive soil P content map for SWAT+ modelling
#             
#             author of script: Szab?, B., Kassai, P. 
#                 first version: 10.12.2021.
#                 last changes: 17.01.2022.            
# --------------------------------------------------------------------------


# install necessary packages ----
packages = c("raster", "mapview", "dplyr", "sf", "rgdal", "fasterize", "tidyverse", "readxl")
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

suppressPackageStartupMessages(library(mapview))

# if a function of a package does not work please install the latest version of that package

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
dir()

pathxls <- readxl::read_excel(path = file.path(file.choose()))

CS_name <- pathxls[[1,5]]
dir.create(CS_name)
fileout <- paste0( CS_name, "/")

# 1. load input data ---- 
# 1.1 load shape of the catchment ----
cs <- st_read(paste0(tools::file_path_sans_ext(pathxls[[2,5]]), ".shp"), geometry_column="geometry",quiet=TRUE)

# 1.2 load relevant LUCAS Topsoil Survey data, based on the SWAT modelling period ----
# LUCAS Topsoil data sets are available from the UFZ cloud
lucas_topsoil <- read.csv2(paste0(tools::file_path_sans_ext(pathxls[[3,5]]), ".csv"))

# 1.3 load relevant LUCAS land cover data, based on the SWAT modelling period ----
# LUCAS land use data sets are available from the UFZ cloud
lucas_harmo_uf <- read.csv(paste0(tools::file_path_sans_ext(pathxls[[8,5]]), ".csv"))
# criteria to select samples from LUCAS Topsoil database based on NUTS region (level 0 or 1 or 2)
nuts_r1 <- pathxls[[10,5]]
nuts_r2 <- pathxls[[11,5]]
nuts_r3 <- pathxls[[12,5]]
nuts_r4 <- pathxls[[13,5]]
nuts_r5 <- pathxls[[14,5]]
nuts_r6 <- pathxls[[15,5]]
nuts_r7 <- pathxls[[16,5]]
nuts_r8 <- pathxls[[17,5]]

# 1.4 load agro-climate zones map ----
cz_map <- raster(paste0(tools::file_path_sans_ext(pathxls[[18,5]]), ".tif"))
# criteria to select samples from LUCAS Topsoil database based on climate
climate1 <- pathxls[[19,5]]
climate2 <- pathxls[[20,5]]
climate3 <- pathxls[[21,5]]
climate4 <- pathxls[[22,5]]
climate5 <- pathxls[[23,5]]

# 1.5 load relevant CORINE, based on the SWAT modelling period ----
# not needed, if local land use map is available
# CORINE data sets are available from the UFZ cloud
if (!is.na(pathxls[[4,5]])){
corine <- raster(paste0(tools::file_path_sans_ext(pathxls[[4,5]]), ".tif"))
}

# 1.6 load local land use map ----
if (!is.na(pathxls[[5,5]])){
l_lu_map <- raster(paste0(tools::file_path_sans_ext(pathxls[[5,5]]), ".tif"))
}

# 1.7 load table to recode local land use map categories into CORINE level 2 categories ----
# this table has to be created by the CS leader for each CS
if (!is.na(pathxls[[7,5]])){
recode_l_lu_map_corine <- readxl::read_excel(paste0(tools::file_path_sans_ext(pathxls[[7,5]]), ".xls"))
}

# 1.8 load table to recode LUCAS land cover/ land use categories into CORINE level 2 categories ----
# this table is available from the UFZ cloud
recode_lucas_lc1_corine <- read.csv2(paste0(tools::file_path_sans_ext(pathxls[[9,5]]), ".csv"))

# 1.9 load soil P content data measured in the catchment, IF AVAILABLE ----
if (!is.na(pathxls[[6,5]])){
p_meas <- st_read(paste0(tools::file_path_sans_ext(pathxls[[6,5]]), ".shp"), geometry_column="geometry",quiet=TRUE)
}

# 2. add LUCAS land use and land cover categories, climate zone codes to LUCAS Topsoil dataset ----
lucas <- merge (lucas_topsoil, lucas_harmo_uf[, c("point_id", "lc1", "nuts0", "nuts1", "nuts2")], by.x = "point_id", by.y = "point_id")
summary(as.factor(lucas$lc1))

lucas_eu <- st_as_sf(lucas, coords = c("x_laea", "y_laea"))
st_crs(lucas_eu) <- 3035
st_crs(lucas_eu)
# mapview(lucas_eu)

# reproject lucas_eu into crs of cz_map
st_crs(cz_map)
mapview(cz_map)
lucas_eu <- st_transform(lucas_eu, st_crs(cz_map))
st_crs(lucas_eu)

# add climate zone codes with meaning of codes
lucas_eu$cz <- raster::extract(cz_map, lucas_eu, df=TRUE, factors=TRUE)

# 3. recode LUCAS land use and land cover categories into CORINE level 2 categories ----
lucas_eu_recode <- merge (lucas_eu, recode_lucas_lc1_corine, by="lc1")
summary(as.factor(lucas_eu_recode$corine_2_lc1))

# 4. compute geometric mean by derived CORINE categories (corine_2_lc1) on lucas_eu ----
geom_mean <- function(data) {
  log_data <- log(data)
  gm <- round(exp(mean(log_data[is.finite(log_data)])), 2)
  return(gm)}

g_mean_lucas_eu <- as.data.frame(with(lucas_eu_recode, tapply(X = lucas_eu_recode$Olsen_P, INDEX = lucas_eu_recode$corine_2_lc1, FUN = geom_mean)))
g_mean_lucas_eu$corine_2_lc1 <- rownames(g_mean_lucas_eu)
names(g_mean_lucas_eu)[1] <- "g_mean"
g_mean_lucas_eu$n <- summary(as.factor(lucas_eu_recode$corine_2_lc1))
g_mean_lucas_eu

g_mean_lucas_eu <- g_mean_lucas_eu %>% add_row(corine_2_lc1 = c("13", "14", "24", "52"))
g_mean_lucas_eu <- g_mean_lucas_eu[order(g_mean_lucas_eu$corine_2_lc1),]
rownames(g_mean_lucas_eu)<- NULL
g_mean_lucas_eu

# CORINE 11 (urban fabric), 12 (industrial, commercial and transport units) categories: multiply it with 0.49
g_mean_lucas_eu[(g_mean_lucas_eu$corine_2_lc1 == 11), 1] <- round(g_mean_lucas_eu$g_mean[(g_mean_lucas_eu$corine_2_lc1 == 11)]*0.49, 2)
g_mean_lucas_eu[(g_mean_lucas_eu$corine_2_lc1 == 12), 1] <- round(g_mean_lucas_eu$g_mean[(g_mean_lucas_eu$corine_2_lc1 == 12)]*0.49, 2)

g_mean_lucas_eu


# 5. select relevant samples from LUCAS Topsoil dataset for the CS ----
# 5.1 based on NUTS region ----
if (!is.na(nuts_r1)){
  lucas_sel <- dplyr::filter(lucas_eu_recode, (grepl(nuts_r1, nuts2)  | grepl(nuts_r2, nuts2)| grepl(nuts_r3, nuts2)| grepl(nuts_r4, nuts2) | grepl(nuts_r5, nuts2) | grepl(nuts_r6, nuts2) | grepl(nuts_r7, nuts2) | grepl(nuts_r8, nuts2)))
}
mapview(lucas_sel[1])
summary(lucas_sel)

# 5.2 based on climate zone ----
if (!is.na(climate1)){
  lucas_sel <- subset(lucas_sel, lucas_sel$cz$category == climate1 | lucas_sel$cz$category == climate2 | lucas_sel$cz$category == climate3 | lucas_sel$cz$category == climate4)
}

mapview(lucas_sel[1])
summary(lucas_sel)
summary(as.factor(lucas_sel$cz$category))
n <- summary(as.factor(lucas_sel$corine_2_lc1))
n

# 6. compute geometric mean of Olsen P by CORINE level 2 categories ----
g_mean_lucas_sel <- as.data.frame(with(lucas_sel, tapply(X = lucas_sel$Olsen_P, INDEX = lucas_sel$corine_2_lc1, FUN = geom_mean)))
g_mean_lucas_sel$corine_2_lc1 <- rownames(g_mean_lucas_sel)
names(g_mean_lucas_sel)[1] <- "g_mean"
g_mean_lucas_sel$n <- summary(as.factor(lucas_sel$corine_2_lc1))
g_mean_lucas_sel
check_table <- g_mean_lucas_sel


# 6.1. add the geometric mean Olsen P value to those CORINE level 2 categories which are missing from the LUCAS selection ----
dif <- setdiff(g_mean_lucas_eu$corine_2_lc1, g_mean_lucas_sel$corine_2_lc1)
dif
g_mean_lucas_sel <- g_mean_lucas_sel %>% add_row(corine_2_lc1 = c(dif, 51, 33))
g_mean_lucas_sel <- g_mean_lucas_sel[order(g_mean_lucas_sel$corine_2_lc1),]
rownames(g_mean_lucas_sel)<- NULL
g_mean_lucas_sel

# 6.2 overwrite geometric mean values for underrepresented non-fertilized land use categories ----
# CORINE category 11, 12
g_mean_lucas_sel[g_mean_lucas_sel$corine_2_lc1 == 11, 1] <- g_mean_lucas_eu[g_mean_lucas_eu$corine_2 == 11, 1] 
g_mean_lucas_sel[g_mean_lucas_sel$corine_2_lc1 == 12, 1] <- g_mean_lucas_eu[g_mean_lucas_eu$corine_2 == 12, 1]

# CORINE category 13, 14
lucas_eu_artificial <- subset(lucas_eu_recode, lucas_eu_recode$corine_2_lc1 < 20)
summary(lucas_eu_artificial)
g_mean_artificial <- geom_mean(lucas_eu_artificial$Olsen_P)
g_mean_artificial
g_mean_lucas_sel[g_mean_lucas_sel$corine_2_lc1 == 13 | g_mean_lucas_sel$corine_2_lc1 == 14, 1] <- g_mean_artificial
g_mean_lucas_sel

# CORINE category 24 
lucas_sel_arable <- subset(lucas_sel, lucas_sel$corine_2_lc1 > 20 & lucas_sel$corine_2_lc1 < 40)
summary(lucas_sel_arable)
g_mean_arable <- geom_mean(lucas_sel_arable$Olsen_P)
g_mean_arable
g_mean_lucas_sel[g_mean_lucas_sel$corine_2_lc1 == 24, 1] <- g_mean_arable
g_mean_lucas_sel

# CORINE category 32, 33, 51, 52
lucas_eu_32_33_5 <- subset(lucas_eu_recode, lucas_eu_recode$corine_2_lc1 > 31 & lucas_eu_recode$corine_2_lc1 < 40 | lucas_eu_recode$corine_2_lc1 > 50)
summary(lucas_eu_32_33_5)
summary(as.factor(lucas_eu_32_33_5$corine_2_lc1))
summary(as.factor(lucas_eu_32_33_5$iso_countr))
g_mean_32_33_5 <- geom_mean(lucas_eu_32_33_5$Olsen_P)
g_mean_32_33_5
g_mean_lucas_sel[((g_mean_lucas_sel$corine_2_lc1 > 31 & g_mean_lucas_sel$corine_2_lc1 < 40) | (g_mean_lucas_sel$corine_2_lc1 > 50)), 1] <- g_mean_32_33_5 
g_mean_lucas_sel

# CORINE 41 and 42 
lucas_eu_4 <- subset(lucas_eu_recode, lucas_eu_recode$corine_2_lc1 > 40 & lucas_eu_recode$corine_2_lc1 < 50)
summary(lucas_eu_4)
summary(as.factor(lucas_eu_4$corine_2_lc1))
summary(as.factor(lucas_eu_4$iso_countr))
g_mean_4 <- geom_mean(lucas_eu_4$Olsen_P)
g_mean_4
g_mean_lucas_sel[g_mean_lucas_sel$corine_2_lc1 > 40 & g_mean_lucas_sel$corine_2_lc1 < 50, 1] <- g_mean_4 

g_mean_lucas_sel
g_mean_lucas_eu

str(g_mean_lucas_sel)
g_mean_lucas_sel$corine_2_lc1 <- as.numeric(g_mean_lucas_sel$corine_2_lc1)

rewritten_table <- g_mean_lucas_sel



# 7. add mean Olsen P values to the local land use map ----

# a) if local land use map is not available --> CORINE map is used ----

if (!exists("l_lu_map")) {
  crs(cs)
  st_crs(corine)
  cs_p <- st_transform(cs, st_crs(corine))
  mapview(cs_p)
  corine_cs <- crop(corine, cs_p)
  mapview(corine_cs) + mapview(cs_p)
  hist(corine_cs)
  
  corine_cs_spdf <- as(corine_cs, "SpatialPixelsDataFrame")
  hist(corine_cs_spdf@data[,1])
  corine_cs_spdf@data$corine_2_local <- recode(corine_cs_spdf@data[,1],`1` = 11, `2` = 11, `3` = 12, `4` = 12, `5` = 12, `6` = 12, `7` = 13, `8` = 13, `9` = 13, `10` = 14, `11` = 14, `12` = 21, `13` = 21, `14` = 21, `15` = 22, `16` = 22, `17` = 22, `18` = 23, `19` = 24, `20` = 24, `21` = 24, `22` = 24, `23` = 31, `24` = 31, `25` = 31, `26` = 32, `27` = 32, `28` = 32, `29` = 32, `30` = 33, `31` = 33, `32` = 33, `33` = 33, `34` = 33, `35` = 41, `36` = 41, `37` = 42, `38` = 42, `39` = 42, `40` = 51, `41` = 51, `42` = 52, `43` = 52, `44` = 52, `48` = 999)  
  hist(corine_cs_spdf@data[,2])
  summary(as.factor(corine_cs_spdf@data[,2]))
  corine_cs_spdf@data$seq <- c(1:nrow(corine_cs_spdf@data))
  c_r_m2 <- merge(corine_cs_spdf@data, g_mean_lucas_sel, by.x="corine_2_local", by.y="corine_2_lc1", all.x =TRUE)
  c_r_m2 <- c_r_m2[order(c_r_m2$seq),]
  corine_cs_spdf@data$g_mean_Olsen_P <- c_r_m2$g_mean
  mapview(corine_cs_spdf["g_mean_Olsen_P"])
  
  c_r_Olsen_P_spdf <- raster(corine_cs_spdf["g_mean_Olsen_P"])
  crs(cs_p)
  crs(c_r_Olsen_P_spdf)
  c_r_Olsen_P_spdf_cs <- projectRaster(c_r_Olsen_P_spdf, crs = crs(cs_p), res = res(c_r_Olsen_P_spdf))
  crs(c_r_Olsen_P_spdf_cs)
  crop <- crop(c_r_Olsen_P_spdf_cs, extent(cs_p))
  Olsen_P_lucas <- mask(crop, cs_p)
  mapview(Olsen_P_lucas)
  summary(Olsen_P_lucas)
  hist(Olsen_P_lucas)
  
  writeRaster(Olsen_P_lucas, filename=paste0(fileout, "Olsen_P_lucas_corine.tif"), format="GTiff", overwrite=TRUE)
  
  # reproject to EPSG:3035 for upload to the cloud and create metadata .xml
  c_Olsen_P_lucas_3035 <- projectRaster(Olsen_P_lucas, crs = "+init=epsg:3035", res = res(Olsen_P_lucas))
  writeRaster(c_Olsen_P_lucas_3035, filename=paste0(fileout, "Olsen_P_lucas_corine_3035.tif"), format="GTiff", overwrite=TRUE)
}



# b) if local land use map is available ----

if (exists("l_lu_map")) {
  # crop local land use map with catchment shape
  mapview(cs)
  crs(cs)
  mapview(l_lu_map)
  st_crs(l_lu_map)
  cs_p <- st_transform(cs, st_crs(l_lu_map))
  mapview(cs_p)
  l_lu_map_cs <- crop(l_lu_map, cs_p)
  mapview(l_lu_map_cs) + mapview(cs_p) # not all pixels are shown by mapview to decrease memory needed for plotting
  
  # 7.1 recode local land use categories into CORINE level 2 categories ----
  l_lu_map_cs_spdf <- as(l_lu_map_cs, "SpatialPixelsDataFrame")
  hist(l_lu_map_cs_spdf@data[,1])
  summary(as.factor(l_lu_map_cs_spdf@data[,1]))
  r <- l_lu_map_cs_spdf@data 
  r$seq <- c(1:nrow(r))
  r$l_lu_map_code <- r[,1]
  r_m <- merge (r, recode_l_lu_map_corine, by="l_lu_map_code", all.x =TRUE)
  
  # add geometric mean of Olsen P to the map based on CORINE level 2 
  r_m2 <- merge (r_m, g_mean_lucas_sel, by.x="corine_2_local", by.y="corine_2_lc1", all.x =TRUE)
  r_m2 <- r_m2[order(r_m2$seq),]
  summary(r_m2)
  l_lu_map_cs_spdf@data$g_mean_Olsen_P <- r_m2$g_mean
  l_lu_map_cs_spdf@data$corine_2_local <- r_m2$corine_2_local
  mapview(l_lu_map_cs_spdf["g_mean_Olsen_P"])
  summary(l_lu_map_cs_spdf["g_mean_Olsen_P"])
  hist(l_lu_map_cs_spdf@data$g_mean_Olsen_P)
  
  # check mapped Olsen P values by CORINE level 2 categories
  result <- round(with(l_lu_map_cs_spdf@data, tapply(X = l_lu_map_cs_spdf@data$g_mean_Olsen_P, INDEX = l_lu_map_cs_spdf@data$corine_2_local, FUN = mean)), 2)
  result
  
  # crop
  r_Olsen_P_spdf <- raster(l_lu_map_cs_spdf["g_mean_Olsen_P"])
  crs(cs)
  crs(r_Olsen_P_spdf)
  r_Olsen_P_spdf_cs <- projectRaster(r_Olsen_P_spdf, crs = crs(cs), res = res(r_Olsen_P_spdf))
  crs(r_Olsen_P_spdf_cs)
  crop <- crop(r_Olsen_P_spdf_cs, extent(cs))
  Olsen_P_lucas <- mask(crop, cs)
  mapview(Olsen_P_lucas)
  
  writeRaster(Olsen_P_lucas, filename=paste0(fileout, "Olsen_P_lucas.tif"), format="GTiff", overwrite=TRUE)
  
  # reproject to EPSG:3035 for upload to the cloud and create metadata .xml
  Olsen_P_lucas_3035 <- projectRaster(Olsen_P_lucas, crs = "+init=epsg:3035", res = res(Olsen_P_lucas))
  writeRaster(Olsen_P_lucas_3035, filename=paste0(fileout, "Olsen_P_lucas_3035.tif"), format="GTiff", overwrite=TRUE)
  
  
}







# 8. add locally measured soil P content data (IF AVAILABLE) ----
if (exists("p_meas")) {

  # reproject p_meas into CRS of derived Olsen_P_lucas map
  mapview(p_meas)
  st_crs(p_meas)
  p_meas_p <- st_transform(p_meas, crs(Olsen_P_lucas))
  
  st_crs(p_meas_p)
  mapview(p_meas_p)
  summary(p_meas_p$P_1) # ADAPT ---
  
  # compute AL-P content from AL-P2O5
  p_meas_p$AL_P <- p_meas_p$P_1*0.4365 # ADAPT if needed ----
  summary(p_meas_p)
  
  # convert AL-P into Olsen P based on:
  # S?rdi, K., Csath?, P., Osztoics, E. 2009. Evaluation of soil phosphorus contents in long-term experiments from environmental aspects. Proceedings of the 51st Georgikon Scientific Conference, Keszthely, Hungary, 807-815.
  p_meas_p$Olsen_P <- p_meas_p$AL_P * 0.5722 - 1.0939 # ADAPT based on table of transfer functions in guideline ----
  geom_mean(p_meas_p$Olsen_P)
  summary(p_meas_p)
  
  # merge local measured Olsen P with Olsen P content by land use map
  # rasterize the vector object
  r_p_meas <- fasterize(p_meas_p, Olsen_P_lucas, field = "Olsen_P", fun="max")
  
  mapview(r_p_meas)
  mapview(Olsen_P_lucas) + mapview(r_p_meas)
  
  p_corine_meas <- merge(r_p_meas, Olsen_P_lucas, overlap = TRUE)
  mapview(p_corine_meas)
  
  # clip/crop and mask raster with the CS polygon
  crop_meas <- crop(p_corine_meas, extent(cs))
  Olsen_P_lucas_meas <- mask(crop_meas, cs)
  mapview(Olsen_P_lucas_meas)
  
  writeRaster(Olsen_P_lucas_meas, filename=paste0(fileout, "Olsen_P_lucas_meas.tif"), format="GTiff", overwrite=TRUE)
  
  # reproject to EPSG:3035 for upload to the cloud and create metadata .xml
  Olsen_P_lucas_meas_3035 <- projectRaster(Olsen_P_lucas_meas, crs = "+init=epsg:3035", res = res(Olsen_P_lucas_meas))
  writeRaster(Olsen_P_lucas_meas_3035, filename=paste0(fileout, "Olsen_P_lucas_meas_3035.tif"), format="GTiff", overwrite=TRUE)
  
}


# 9. check number of samples used for the computation and created maps ----
suppressPackageStartupMessages(library(mapview))

check_table
rewritten_table
nrow(lucas_sel)
lucas_sel_map <- mapview(lucas_sel[1])
lucas_sel_map

# view final maps
pal <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(5, "YlGnBu"))

# no measured P value added
Olsen_P_lucas_map <- mapview(Olsen_P_lucas, col.regions = pal(100), legend = TRUE, na.color = "transparent")
Olsen_P_lucas_map

# measured P value added, shown if measured P value was available
if (exists("Olsen_P_lucas_meas")){
  p_max <- cellStats(Olsen_P_lucas_meas, stat='max', na.rm=TRUE, asSample=TRUE)
  Olsen_P_lucas_meas_map <- mapview(Olsen_P_lucas_meas, col.regions = pal(100), at = seq(0, p_max+5, 5), legend = TRUE, na.color = "transparent")
}

Olsen_P_lucas_meas_map

# Olsen_P_lucas_map_recol <- mapview(Olsen_P_lucas, col.regions = pal(100), at = seq(0, p_max+5, 5), legend = TRUE, na.color = "transparent")
# Olsen_P_lucas_map_recol
