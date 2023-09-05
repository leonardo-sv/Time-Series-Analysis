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
  print(as.Date(obs_date))
  pre_filter <- filter(input_data, label == label_target, tile == tile_target)
  
  if(obs_date %in% input_data$time_series[[1]]$Index){
    return(filtered <- filter(pre_filter, unlist(
      lapply(pre_filter$time_series, 
             function(x) 
               (any(  x$Index == as.Date(obs_date) &
                        x$NDVI > min_value & 
                        x$NDVI < max_value))))))
    }
  
  
  
  return(input_data)
    
  
}

filter_band <- function(input_data, obs_date, min_value, max_value, label_target){
  print(tile_target)
  pre_filter <- filter(input_data, label == label_target)
  return(
    filter(pre_filter, unlist(
      lapply(pre_filter$time_series, 
             function(x) 
               (any(  x$Index == as.Date(obs_date) &
                        x$NDVI > min_value & 
                        x$NDVI < max_value))))))
  
}


find_outiers <- function(samples){
  forest_samples <- samples[samples$label == "Forest",]
  ts <- forest_samples$time_series
  dt <- data.table::data.table(dplyr::bind_rows(ts))
  f_summary <- summary(dt)
  
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
    (st_within(points,nc[nc$tile == "012014",],sparse = FALSE)) ~ "012014",
    (st_within(points,nc[nc$tile == "012015",],sparse = FALSE)) ~ "012015",
    (st_within(points,nc[nc$tile == "012016",],sparse = FALSE)) ~ "012016",
    (st_within(points,nc[nc$tile == "013014",],sparse = FALSE)) ~ "013014",
    (st_within(points,nc[nc$tile == "013015",],sparse = FALSE)) ~ "013015",
    (st_within(points,nc[nc$tile == "013016",],sparse = FALSE)) ~ "013016",
    (st_within(points,nc[nc$tile == "014014",],sparse = FALSE)) ~ "014014",
    (st_within(points,nc[nc$tile == "014015",],sparse = FALSE)) ~ "014015",
    (st_within(points,nc[nc$tile == "014016",],sparse = FALSE)) ~ "014016"
    
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
  data.table::setnames(dt_med, "V1", "Median")
  dt_qt25 <- dt[, stats::quantile(V1, 0.25), by = Index]
  data.table::setnames(dt_qt25, "V1", "quantile-25")
  dt_qt75 <- dt[, stats::quantile(V1, 0.75), by = Index]
  data.table::setnames(dt_qt75, "V1", "quantile-75")
  dt_mean <- dt[, mean(V1), by = Index]
  data.table::setnames(dt_sd, "V1", "Mean")
  dt_var <- dt[, stats::var(V1), by = Index]
  data.table::setnames(dt_var, "V1", "Variance")
  dt_sd <- dt[, stats::sd(V1), by = Index]
  data.table::setnames(dt_sd, "V1", "Std Deviation")
  
  
  dt_qts <- merge(dt_med, dt_qt25)
  dt_qts <- merge(dt_qts, dt_qt75)
  dt_qts <- merge(dt_qts, dt_mean)
  dt_qts <- merge(dt_qts, dt_var)
  dt_qts <- merge(dt_qts, dt_sd)
  data.table::setnames(dt, "V1", band)
  return(dt_qts)
}

stats_ts <- function(ts_tibble) {
  V1 <- NULL
  dt_stats <- data.table(
    "Index" = structure(numeric(0), class = "Date"),
    "Value" = numeric(),
    "Stats" = character(),
    "StatsBand" = character(),
    "label" = character(),
    "band" = character()
    
  )
  labels <- sits_labels(ts_tibble)
  bands <- sits_bands(ts_tibble)
  
  for (label in labels){
    dt_bylabel <- ts_tibble[ts_tibble$label == label,]
    ts <- dt_bylabel$time_series
    dt <- data.table::data.table(dplyr::bind_rows(ts))
    
    for(band in bands){
      dt_qts <- data.table(
        Index = structure(numeric(0), class = "Date"),
        "Value" = numeric(),
        "Stats" = character(), 
        "StatsBand" = character()
      )
      data.table::setnames(dt, band, "V1")
      dt_med <- dt[, stats::median(V1), by = Index]
      data.table::setnames(dt_med, "V1", "Value")
      dt_med[ , `:=` (StatsBand = paste(band, "Median)", sep="("),
                      Stats ="Median")]
      
      dt_qt25 <- dt[, stats::quantile(V1, 0.25), by = Index]
      data.table::setnames(dt_qt25, "V1", "Value")   
      dt_qt25[ , `:=` (StatsBand = paste(band, "Q25)", sep="("),
               Stats ="Q25")]
      
      dt_qt75 <- dt[, stats::quantile(V1, 0.75), by = Index]
      data.table::setnames(dt_qt75, "V1","Value")
      dt_qt75[ , `:=` (StatsBand = paste(band, "Q75)", sep="("),
               Stats ="Q75")]
      
      dt_var <- dt[, stats::var(V1), by = Index]
      data.table::setnames(dt_var, "V1", "Value")
      dt_var[ , `:=` (StatsBand = paste(band, "Variance)", sep="("),
              Stats ="Variance")]
      
      dt_mean <- dt[, mean(V1), by = Index]
      data.table::setnames(dt_mean, "V1", "Value")
      dt_mean[ , `:=` (StatsBand = paste(band, "Mean)", sep="("),
                      Stats ="Mean")]
      
      dt_sd <- dt[, stats::sd(V1), by = Index]
      data.table::setnames(dt_sd, "V1", "Value")
      dt_sd[ , `:=` (StatsBand = paste(band, "Standard Deviation)", sep="("),
                      Stats ="Standard Deviation")]
      
      dt_qts <- rbind(dt_qts, dt_med)
      dt_qts <- rbind(dt_qts, dt_qt25)
      dt_qts <- rbind(dt_qts, dt_qt75)
      dt_qts <- rbind(dt_qts, dt_var)
      dt_qts <- rbind(dt_qts, dt_mean)
      dt_qts <- rbind(dt_qts, dt_sd)
      dt_qts[ , `:=` (band=band, label=label)]
      data.table::setnames(dt, "V1", band)
      dt_stats <- rbind(dt_stats, dt_qts)
    }
  }
  data.table::setnames(dt_stats, "Index", "Time")
  return(dt_stats)
  
}

stats_ts2 <- function(ts_tibble) {
  V1 <- NULL
  dt_stats <- data.table(
    "Index" = structure(numeric(0), class = "Date"),
    "Q25" = numeric(),
    "Median" = numeric(),
    "Q75" = numeric(),
    "Variance" = numeric(),
    "label" = character(),
    "band" = character()
  )
  labels <- sits_labels(ts_tibble)
  bands <- sits_bands(ts_tibble)
  
  for (label in labels){
    dt_bylabel <- ts_tibble[ts_tibble$label == label,]
    ts <- dt_bylabel$time_series
    dt <- data.table::data.table(dplyr::bind_rows(ts))
    
    for(band in bands){
      dt_qts <- data.table(
        Index = ts_tibble$time_series[[1]]$Index
      )
      data.table::setnames(dt, band, "V1")
      dt_med <- dt[, stats::median(V1), by = Index]
      dt_qt25 <- dt[, stats::quantile(V1, 0.25), by = Index]
      dt_qt75 <- dt[, stats::quantile(V1, 0.75), by = Index]
      dt_var <- dt[, stats::var(V1), by = Index]
      data.table::setnames(dt_qt25, "V1", "Q25")
      data.table::setnames(dt_med, "V1", "Median")
      data.table::setnames(dt_var, "V1", "Variance")
      data.table::setnames(dt_qt75, "V1","Q75")
      dt_qts <- merge(dt_qts, dt_med)
      dt_qts <- merge(dt_qts, dt_qt25)
      dt_qts <- merge(dt_qts, dt_qt75)
      dt_qts <- merge(dt_qts, dt_var)
      dt_qts[ , `:=` (band=band, label=label)]
      data.table::setnames(dt, "V1", band)
      dt_stats <- rbind(dt_stats, dt_qts)
    }
  }
  return(dt_stats)
  
}

plot_box <- function(dt){
  
  plot.df <- purrr::pmap_dfr(
    list(dt$label, dt$time_series),
    function(label, ts) {
      lb <- as.character(label)
      # extract the time series and convert
      df <- tibble::tibble(Time = ts$Index, ts[-1], Band = lb)
      return(df)
    }
  )
  
  
  plot.df <- tidyr::pivot_longer(plot.df, cols = sits_bands(dt))
  
  gp <- ggplot(plot.df, aes(x=name, y=value, color=name)) +
    geom_boxplot()  + ggplot2::facet_wrap(~Band)
  
  p <- graphics::plot(gp)
  
  return(invisible(p))
  
}

corr_classes <- function(ts_tibble){
  bands <- sits_bands(patterns)
  df <- data.frame(Correlation=numeric(0),band=character(0))
  labels <- patterns %>% dplyr::pull(label)

  ts <- ts_tibble$time_series
  
  for (b in bands){
    df_corr <- data.frame(
      Correlation=round(cor(ts[[1]][b], ts[[2]][b])[1,1],3),
      Band = b
    )
    df <- rbind(df, df_corr)
  }
  return(df)
}

ggplot(data=df, aes(x=Band, y=Correlation)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=Correlation), vjust=-0.3, size=3.5)+
  theme_minimal()

corr_ts <- function(ts_tibble){
  
  col_names <- c("Var1", "Var2", "Value", "label")
  df <- read.table(text = "",
                   col.names = col_names)
  labels <- ts_tibble %>% dplyr::pull(label)
  for (l in labels){
    dt_bylabel <- ts_tibble[ts_tibble$label == l,]
    ts <- dt_bylabel$time_series
    cormat <- round(cor(ts[[1]][2:14]),2)
    melted_cormat <- melt(cormat)
    melted_cormat$label <- l
    df <- rbind(df, melted_cormat)
  }
  return(df)
}



plot_corr_matrix <- function(corr){
  
  gp <- ggplot(data = corr, aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile() + ggplot2::facet_wrap(~label) + 
    #geom_text(aes(label = value), color = "white", size = 4)
  
  p <- graphics::plot(gp)
  
  return(invisible(p))
  
    
}
plot_stats <- function(dt, stats, bands){
  dt <- dt[band %in% bands & Stats %in% stats]
  
  gp <- ggplot2::ggplot(data = dt, ggplot2::aes(
    x = .data[["Time"]],
    y = .data[["Value"]],
    color = .data[["band"]],
    linetype= .data[["Stats"]]
    
  )) +  ggplot2::facet_wrap(~label) +
    ggplot2::geom_line()
  
  gp <- gp + ggplot2::facet_wrap(~label)
  
  gp <- gp +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::guides(colour = ggplot2::guide_legend(title = "Bands")) +
    ggplot2::ylab("Value")
  
  p <- graphics::plot(gp)
  
  return(invisible(p))
  
}
plot_patterns(patterns)
plot_patterns <- function(x, year_grid = FALSE){
  plot.df <- purrr::pmap_dfr(
    list(x$label, x$time_series),
    function(label, ts) {
      lb <- as.character(label)
      # extract the time series and convert
      df <- tibble::tibble(Time = ts$Index, ts[-1], Pattern = lb)
      return(df)
    }
  )

  
  
  plot.df <- tidyr::pivot_longer(plot.df, cols = sits_bands(x))
  
    # Plot temporal patterns
  gp <- ggplot2::ggplot(plot.df, ggplot2::aes(
    x = .data[["Time"]],
    y = .data[["value"]],
    colour = .data[["name"]]
  )) +
    ggplot2::geom_line()
  
  if (year_grid)
    gp <- gp + ggplot2::facet_grid(year ~ Pattern)
  else
    gp <- gp + ggplot2::facet_wrap(~Pattern)
  
  gp <- gp +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::guides(colour = ggplot2::guide_legend(title = "Bands")) +
    ggplot2::ylab("Value")
  
  p <- graphics::plot(gp)
  
  return(invisible(p))
}

plot_stats <- function(band, list_stats){
  
  
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
  collection    = "S2-16D-2",
  bands         = c("B01", "B02","B03","B04","B05","B06",
                    "B07","B08", "B8A", "B09","B11", "B12", "NDVI", "EVI", "CLOUD"),
  tiles         = c("012014", "012015", "012016", "013014", "013015", "013016"
                    ,"014014", "014015", "014016"), 
  start_date    = "2018-07-24", 
  end_date      = "2021-07-16" 
)



# Get the time series from sits cube
input_data.tb <- sits_get_data(
  cube = sent_cube, 
  samples = "/home/leonardo.vieira/git/Time-Series-Analysis/data/csv/S2-16D/samples_S2-16D-2-p1.csv",
  bands      = c("B01", "B02","B03","B04","B05","B06",
                 "B07","B08","B8A","B09","B11", "B12", "NDVI", "EVI", "CLOUD"),
  multicores = 16,
  output_dir = "/home/leonardo.vieira/git/Time-Series-Analysis/data/temps"
)

# Add tile information on data frame
input_data.tb <- add_tile(input_data.tb, "/home/leonardo.vieira/git/Time-Series-Analysis/data/shp/roi.shp")

# Save the data set in a rds file
saveRDS(input_data.tb, file = "../data/rds/samples_update.rds")

saveRDS(samples, file = "../data/rds/samples.rds")

# Read previous data saved
input_data.tb <- readRDS("../data/rds/samples_S2-16D.rds")
samples <- readRDS("../data/rds/samples_S2-16D.rds")
patterns <- sits_patterns(samples)

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

library(dplyr)
nrow(samples[samples$label == 'Forest', ])
nrow(samples[samples$label == 'Deforested', ])

# ====================== Statistics ============================================
# Deforestation
band_tb <- sits_select(samples[samples$label == "Deforested",], "NDVI")
ts <- band_tb$time_series
dt_byrows <- data.table::data.table(dplyr::bind_rows(ts))
stats_def_NDVI <- create_iqr(dt_byrows, "NDVI")

# Forest
band_tb <- sits_select(input_data.tb[input_data.tb$label == "Desmatamento",], "NDVI")
ts <- band_tb$time_series
dt_byrows <- data.table::data.table(dplyr::bind_rows(ts))

dt_byrows[,x:= list(list(cor(dt_byrows))), by = Index]
stats_for_NDVI <- create_iqr(dt_byrows, "NDVI")

length(dt_byrows$B01)
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


tiles <- sent_cube$tile

input_all_filtered <- samples[0, ] 

for (tile in tiles){
  row <- dplyr::filter(sent_cube, tile == t)
  dates <-  (row$file_info[[1]]$date[row$file_info[[1]]$cloud_cover < 5.0 & row$file_info[[1]]$band == "NDVI"])
  input_forest <- filter_vband(samples, dates[1], 0.60, 1,"Floresta", tile)
  input_deforestation <- filter_vband(samples, dates[1], -1, 0.865,"Desmatamento", tile)
  for (i in 2:length(dates)){
    input_forest <- filter_vband(input_forest, dates[i], 0.60, 1,"Floresta", tile)
    
    input_deforestation <- filter_vband(input_deforestation, dates[i], -1, 0.865,"Desmatamento", tile)
  } 
  input_all_filtered <- rbind(input_all_filtered, input_forest)
  input_all_filtered <- rbind(input_all_filtered, input_deforestation)
}

input_all_filtered
patterns2 <- sits_patterns(samples)
plot_patterns(patterns2)
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
samples$label[samples$label == 'Desmatamento'] <- 'Deforestation'
head(samples)
test <- head(input_data.filtered, 5)
my.table[, lapply(.SD, mean), by=gbc]
# helper

# merge two tibbles in one bind_rows(tibble1, tibble 2)

saveRDS(input_all_filtered, file = "../data/rds/samples.rds")
gp <- ggplot2::ggplot(test, ggplot2::aes(x = Index, y = c(med_NDVI, qt75_NDVI, var_NDVI))) +
  ggplot2::geom_line()

graphics::plot(gp)


patterns

stats <- stats_ts(samples)

plot.df <- purrr::pmap_dfr(
  list(test$label, test$time_series),
  function(label, ts) {
    lb <- as.character(label)
    # extract the time series and convert
    df <- tibble::tibble(Time = ts$Index, ts[-1], Pattern = lb)
    return(df)
  }
)


patterns$time_series[[1]]$Index

plot(sent_cube,
     red = "B04", blue = "B02", green = "B03",
     date = "2018-07-28"
)

get_ts <- function(path){
  df <- head(samples,0)
  count <- 2
  save_path <- "/home/leonardo.vieira/git/Time-Series-Analysis/data/rds/"
  for(f in list.files(path)){
    file_dir <- paste(path, f, sep="")
    count <- count + 1
    print(file_dir)
    input_data.tb <- sits_get_data(
      cube = sent_cube, 
      samples = file_dir,
      bands      = c("B01", "B02","B03","B04","B05","B06",
                     "B07","B08","B8A","B09","B11", "B12", "NDVI", "EVI", "CLOUD"),
      multicores = 16,
      output_dir = "/home/leonardo.vieira/git/Time-Series-Analysis/data/temps"
    )
    df <- rbind(df, input_data.tb)
    save_file <- paste(save_path, as.character(count), sep ="")
    saveRDS(input_data.tb, file = paste(save_file,".rds", sep="") )
    
  }
  saveRDS(df, file = "/home/leonardo.vieira/git/Time-Series-Analysis/data/rds/samples-S2-16D.rds")
}

get_ts("/home/leonardo.vieira/git/Time-Series-Analysis/data/csv/S2-16D/")



for(t in sent_cube$tile){
  row <- dplyr::filter(sent_cube, tile == t)
  date <- (row$file_info[[1]]$date[row$file_info[[1]]$cloud_cover
                                   == min(
                                     row$file_info[[1]]$cloud_cover)
                                   & row$file_info[[1]]$band == "NDVI"])
  download_raster(sent_cube, date[[1]], c("B02","B03","B04","NDVI"), t, "../data/imagens/raster/")
}

