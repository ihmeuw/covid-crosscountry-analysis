library('seegSDM')
library('rgdal')
library('raster')
in_dir<-'FILEPATH'

IUCN_ranges <- shapefile(paste0(in_dir,"/MAMMALS_TERRESTRIAL_ONLY.shp"))
crs <- CRS(paste0(proj4string(IUCN_ranges))) # define common crs 
configs<- read.csv(paste0(in_dir,'/bats/config.csv'), as.is=T)
buff_dir <- paste0(in_dir,'/bats/buffers/')
setwd(buff_dir)
dummy_raster<-raster(paste0(in_dir,'/land.tif'))

#Remove species that don't have IUCN range
configs<-configs[!(Sp %in% c("Rhinolophus imaizumii","Pteropus brunneus",'Pipistrellus tenuis','Paracoelops megalotis',
                             'Myonycteris angolensis',"Hypsugo bodenheimeri","Harpiocephalus mordax"))]

#Rename species to match IUCN range

for (i in 1:nrow(configs)) {
  print(i)
  species <- gsub("_", " ", configs$index[i])
  if (species == "Dobsonia magna") species <- "Dobsonia moluccensis"
  if (species == "Arielulus aureocollaris") species <- "Thainycteris aureocollaris"
  if (species == "Artibeus phaeotis") species <- "Dermanura phaeotis"
  if (species == "Chaerephon leucogaster") species <- "Mops leucogaster"
  if (species == "Eptesicus matroka") species <- "Neoromicia matroka"
  if (species == "Eptesicus nasutus") species <- "Rhyneptesicus nasutus"
  if (species == "Falsistrellus affinis") species <- "Hypsugo affinis"
  if (species == "Hipposideros commersoni") species <- "Macronycteris commersoni"
  if (species == "Megaderma lyra") species <- "Lyroderma lyra"
  if (species == "Murina grisea") species <- "Harpiola grisea"
  if (species == "Murina silvatica") species <- "Murina ussuriensis"
  if (species == "Myotis hosonoi") species <- "Myotis ikonnikovi"
  if (species == "Myotis oxygnathus") species <- "Myotis blythii"
  if (species == "Myotis ozensis") species <- "Myotis ikonnikovi"
  if (species == "Myotis ricketti") species <- "Myotis pilosus"
  if (species == "Myotis yesoensis") species <- "Myotis ikonnikovi"
  if (species == "Neoromicia somalicus") species <- "Neoromicia malagasyensis"
  if (species == "Pipistrellus deserti") species <- "Pipistrellus kuhlii"
  if (species == "Pipistrellus subflavus") species <- "Perimyotis subflavus"
  if (species == "Pteropus leucopterus") species <- "Desmalopex leucopterus"
  if (species == "Rousettus bidens") species <- "Boneia bidens"
  if (species == "Scotoecus albigula") species <- "Scotoecus hirundo"
  if (species == "Scotoecus hindei") species <- "Scotoecus hirundo"
  species_out <- gsub(" ", "_", species)
  species_ranges <- gUnaryUnion(IUCN_ranges[which(IUCN_ranges$binomial %in% species),])
  species_raster<-rasterize(species_ranges, dummy_raster)
  outpath<-paste0(buff_dir,species_out,'.tif')
  writeRaster(species_raster, outpath,format="GTiff", overwrite=TRUE)
}

raster<-raster(paste0(in_dir,"/bats/buffers/Murina_silvatica.tif") #sample raster for matching extents
raster<-extend(raster, extent(dummy_raster), value=0)
ext_master<-extent(raster)

raster_list<-list()
for(l in list.files(buff_dir, full.names = T)){
  raster<-raster(l)
  print(l)
  ext<-extent(raster)
  print(ext)
  raster<-extend(raster, extent(dummy_raster), value=0)
  extent(raster)<-ext_master
  print(extent(raster))
  raster[is.na(raster)]<-0
  coarse_raster <- aggregate(raster,fact=4,fun=max) 
  raster_list<-c(raster_list, coarse_raster)
}

raster_list_n<-raster_list

r_stack = stack(raster_list)
species<-names(r_stack)

datasum<-overlay(r_stack, fun=sum, unstack=TRUE, forcefun=FALSE)

writeRaster(datasum,paste0(in_dir,'/summed_bat_species.tif'), overwrite=TRUE)
globe_shp_path <- paste0(in_dir,'/covid_simp_2.shp')
map_shp <- readOGR(globe_shp_path)
bat_extract<-extract(datasum,map_shp, fun=mean, df=TRUE) 
bat_shp <- data.frame(location_id=map_shp$loc_id,bat_extract)

#log transform with pad of 5% of median
bat_df_s[,summed_bat_species:=log(summed_bat_species+0.575)]
write.csv(bat_df_s, paste0(in_dir,'/bats/bat_covariate.csv', row.names=F))
