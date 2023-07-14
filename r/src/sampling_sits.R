library(cluster)
library(sits)
library(sitsdata)
library(dplyr)
library(factoextra)
library(data.table)
library(sf)


# =============== Functions ====================================================

# This function allows filter a time series data set ("S2-SEN2COR_10_16D_STK-1")
# based in a specific date, label, tile, value, at NDVI band. The objective is 
# to search for possible outliers in data set. 
filter_vband <- function(input_data, obs_date, min_value, max_value, label_target, tile_target){
  print(tile_target)
  pre_filter <- filter(input_data, label == label_target, tile == tile_target)
  return(
    filter(pre_filter, unlist(
      lapply(pre_filter$time_series, 
             function(x) 
               (any(  x$Index == as.Date(obs_date) &
                        x$NDVI > min_value & 
                        x$NDVI < max_value))))))
  
}


test<-head(input_data.tb, 5)
test2 <- filter_by_value_date(test, "2017-08-13", 0.0, 0.68, "Floresta")

# Example: a<-find_outliers(input_data.tb, "2018-06-26", -0.81, 0.1, Desmatamento, 079093)

# This function adds the tile column in a S2-SEN2COR_10_16D_STK-1 cube data set.
add_tile <- function(data.df, shapefile){
  nc <- st_read(shapefile)
  points <- mapply(function(x,y) st_point(c(x,y), 4326),
                   data.df$longitude,data.df$latitude,SIMPLIFY = FALSE)
  points <- st_sfc(points, crs=4326)
  
  return(data.df %>% mutate(tile = case_when(
    (st_within(points,nc[nc$tile == "077093",],sparse = FALSE)) ~ "077093",
    (st_within(points,nc[nc$tile == "077094",],sparse = FALSE)) ~ "077094",
    (st_within(points,nc[nc$tile == "077095",],sparse = FALSE)) ~ "077095",
    (st_within(points,nc[nc$tile == "078093",],sparse = FALSE)) ~ "078093",
    (st_within(points,nc[nc$tile == "078094",],sparse = FALSE)) ~ "078094",
    (st_within(points,nc[nc$tile == "078095",],sparse = FALSE)) ~ "078095",
    (st_within(points,nc[nc$tile == "079093",],sparse = FALSE)) ~ "079093",
    (st_within(points,nc[nc$tile == "079094",],sparse = FALSE)) ~ "079094",
    (st_within(points,nc[nc$tile == "079095",],sparse = FALSE)) ~ "079095"
    
  )))
  
}
add_tile_1 <- function(data.df) {
  return(
    data.df %>% mutate(tile =
                         case_when(
                           (latitude <= -8.968547882953816 & latitude >= -10.003125268820506) & (longitude <= -63.645285163429676 & longitude >= -65.23432383171814) ~ "077094", 
                           (latitude <= -9.947158110497963 & latitude >= -10.981203537048179) & (longitude <= -63.680330341114505 & longitude >= -65.27527994811312) ~ "077095",
                           (latitude <= -7.988961804961198 & latitude >= -9.024359680174772) & (longitude <= -63.610492715308375 & longitude >= -65.19366401670857) ~ "077093", 
                           (latitude <= -8.04463477904458 & latitude >= -9.071876595685131) & (longitude <= -62.067103113531694 & longitude >= -63.645285163429676) ~ "078093", 
                           (latitude <= -8.092032760400222 & latitude >= -9.111095461705832) & (longitude <= -60.52322216034859 & longitude >= -62.09631492244112) ~ "079093", 
                           (latitude <= -9.024359680174772 & latitude >= -10.05077521190122) & (longitude <= -62.09631492244112 & longitude >= -63.68033034111452) ~ "078094", 
                           (latitude <= -10.003125268820506 & latitude >= -11.029000806386007) & (longitude <= -62.12573899841475 & longitude >= -63.71563101099978) ~ "078095", 
                           (latitude <= -9.071876595685131 & latitude >= -10.090104398980037) & (longitude <= -60.546847976099244 & longitude >= -62.12573899841475) ~ "079094", 
                           (latitude <= -10.05077521190122 & latitude >= -11.068452127014156) & (longitude <= -60.57064551836915 & longitude >= -62.15537766288313) ~ "079095")
    ))
}

# This function select a specific image in sits cube
select_raster <- function(cube, date_view, tile_){
  return(cube %>% 
           dplyr::filter(tile == tile_) %>% 
           mutate(file_info = lapply(file_info, 
                                     function(x) filter(x, date == date_view))))
  
}
# Example: select_raster(raster_plot, "2017-08-13", "077095")

# This function create the plot of time series data set
save_plot_ts <- function(input_data, band, tile_, dir_work, label_){
  new_path <- paste(dir_work, band, sep = "")
  if (!dir.exists(new_path)){
    dir.create(new_path)
  }
  new_path_band <- paste(new_path, label_, sep = "/")
  if (!dir.exists(new_path_band)){
    dir.create(new_path_band)
  }
  
  path_png <- paste(new_path_band, tile_, sep = "/")
  path_png <- paste(path_png, ".png", sep = "")
  png(filename=path_png)
  s.df <- input_data[input_data$label == label_,]
  s.df[s.df$tile == tile_,] %>% sits_select(bands = band)%>% plot()
  dev.off()
  
}
# Example: 
# save_plot_ts(input_data.tb, "NDVI", tiles, "../data/imagens/","Desmatamento")

# calculate some statistics over time series data set.
create_iqr <- function(dt, band) {
  V1 <- NULL # to avoid setting global variable
  
  data.table::setnames(dt, band, "V1")
  dt_med <- dt[, stats::median(V1), by = Index]
  data.table::setnames(dt_med, "V1", "med")
  dt_qt25 <- dt[, stats::quantile(V1, 0.25), by = Index]
  data.table::setnames(dt_qt25, "V1", "qt25")
  dt_qt75 <- dt[, stats::quantile(V1, 0.75), by = Index]
  data.table::setnames(dt_qt75, "V1", "qt75")
  dt_var <- dt[, stats::var(V1), by = Index]
  data.table::setnames(dt_var, "V1", "var")
  dt_qts <- merge(dt_med, dt_qt25)
  dt_qts <- merge(dt_qts, dt_qt75)
  dt_qts <- merge(dt_qts, dt_var)
  data.table::setnames(dt, "V1", band)
  return(dt_qts)
}

find_sample <- function(data, long, lat){
  return(filter(data, (longitude > long - 0.0003 & longitude < long + 0.0003)
                , (latitude > lat - 0.0003 & latitude < lat + 0.0003)))
}

download_raster <- function(cube, date_, bands_, tile_, dir) {
  new_path <- paste(dir, tile_, sep = "/")
  if (!dir.exists(new_path)){
    dir.create(new_path)
  }
  
  file_info_tile <-cube[cube$tile == tile_,]$file_info[[1]]
  
  for (b in bands_){
    path <- filter(file_info_tile, date==date_, band==b)$path
    print(path)
    url <- strsplit(path, "l/")[[1]][2] 
    splited_url <- strsplit(url, "/")
    name_file <- tail(splited_url[[1]], n=1)
    name_file <- strsplit(name_file, ".tif")[[1]][1]
    name_file <- paste(name_file, ".tif", sep="")
    path_file <- paste(new_path, name_file, sep="/")
    download.file(url, path_file)
  }
}




# =============== Execution ====================================================

# Read a samples csv file. 
samples.df <- read.csv(file = '/home/leonardo.vieira/git/Time-Series-Analysis/data/csv/samples_def2.csv')


# Create a S2-SEN2COR_10_16D_STK-1 sits cube.
sent_cube <- sits_cube(
  source        = "BDC",
  collection    = "S2-SEN2COR_10_16D_STK-1",
  bands         = c("B01", "B02","B03","B04","B05","B06",
                    "B07","B08","B09","B11", "B12", "NDVI", "EVI", "CLOUD"),
  tiles         = c("077093","077094","077095","078093","078094",
                    "078095","079093","079094","079095"), 
  start_date    = "2017-08-06", 
  end_date      = "2019-07-31" 
)



# Get the time series from sits cube
input_data.tb <- sits_get_data(
  cube = sent_cube, 
  samples = "/home/leonardo.vieira/git/Time-Series-Analysis/data/csv/samples_def2.csv",
  bands      = c("B01", "B02","B03","B04","B05","B06",
                 "B07","B08","B09","B11", "B12", "NDVI", "EVI", "CLOUD"),
  multicores = 16,
  output_dir = "/home/leonardo.vieira/git/Time-Series-Analysis/data/temps"
)

# Add tile information on data frame
input_data.tb <- add_tile(input_data.tb, "../data/shp/roi/roi.shp")

# Save the data set in a rds file
saveRDS(input_data.tb, file = "../data/rds/samples_update.rds")

saveRDS(samples, file = "../data/rds/samples.rds")

# Read previous data saved
input_data.tb <- readRDS("../data/rds/samples_update.rds")


# Create shape file from samples
test_sf<-sits_as_sf(test_out)
st_write(test_sf, "../data/shp/test.shp")

# found image to analysis

test$file_info %>% lapply(function(x) filter(x, date == "2017-08-13"))

# Get rasters
dates <- c("2017-09-14", "2017-08-13", "2018-06-10")
for (d in dates){
  for(tile in tiles){
    download_raster(sent_cube, d, c("B12"), "078094", "../data/imagens/raster/")  
  }  
}

for (row in head(input_data.tb, 1)){
  print(row)
}



# ====================== Statistics ============================================
# Deforestation
band_tb <- sits_select(input_data.tb[input_data.tb$label == "Desmatamento",], "NDVI")
ts <- band_tb$time_series
dt_byrows <- data.table::data.table(dplyr::bind_rows(ts))
stats_def_NDVI <- create_iqr(dt_byrows, "NDVI")

# Forest
band_tb <- sits_select(input_data.tb[input_data.tb$label == "Desmatamento",], "NDVI")
ts <- band_tb$time_series
dt_byrows <- data.table::data.table(dplyr::bind_rows(ts))
stats_for_NDVI <- create_iqr(dt_byrows, "NDVI")


stats_def_NDVI
stats_for_NDVI

# ================================= Steps to Analyse ===========================
# retirar floresta -0.7 0.65 desmatamento .865 1
a <- filter(input_all_filtered, tile == "078094", label == "Desmatamento")
for (tile in tiles){
  # Forest
  forest_out<-sits_as_sf(
    find_outliers(input_data.tb, "2017-09-14", -0.1, 0.5, "Floresta", "078094"))
  st_write(forest_out, "../data/shp/out_2.shp")
  
}
st_write(sits_as_sf(a),"../data/shp/out_4.shp") 

forest_filtered <- filter_by_value_date(
  input_data.tb, "2017-08-13", 0.65, 1.0, "Floresta")
deforestation_filtered <- filter_by_value_date(
  input_data.tb, "2017-08-13", 0.0, 0.856, "Desmatamento")

input_data.filtered <- rbind(forest_filtered, deforestation_filtered)
dates <- input_data.tb$time_series[[1]]$Index

list_samples <- list()

floresta.tb <- filter(input_data.tb, label == "Floresta")

input_all_filtered <- input_data.tb[0, ] 

for (tile in tiles){
  input_forest <- filter_vband(input_data.tb, "2017-08-13", 0.65, 1,"Floresta", tile)
  input_deforestation <- filter_vband(input_data.tb, "2017-08-13", 0, 0.865,"Desmatamento", tile)
  for (i in 2:46){
    input_forest <- filter_vband(input_forest, dates[i], 0.7, 1,"Floresta", tile)
    
    input_deforestation <- filter_vband(input_deforestation, dates[i], 0, 0.865,"Desmatamento", tile)
  } 
  input_all_filtered <- rbind(input_all_filtered, input_forest)
  input_all_filtered <- rbind(input_all_filtered, input_deforestation)
}

input_all_filtered
filter(floresta.tb, sample_ok == TRUE)

# Plot time series * Not work yet *
tiles = c("077095", "078095", "079095", "077094", "078094",
          "079094", "077093", "078093", "079093" )

for (t in tiles) {
  save_plot_ts(input_all_filtered, "NDVI", t, "../data/imagens/all_filtered/","Floresta")
}

for (t in tiles) {
  save_plot_ts(input_all_filtered, "NDVI", t, "../data/imagens/all_filtered/","Desmatamento")
}

samples$label[samples$label == 'Floresta'] <- 'Forest'
samples$label[samples$label == 'Deforestation'] <- 'Deforested'
head(samples)
test <- head(input_data.filtered, 5)

test
