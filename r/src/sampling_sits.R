source("./src/analysis_functions.R")

test <- read.csv("../data/csv/predicted/B04-B07-B12-NDVI-EVI/cluster_B12-NDVI-2020-07-27-2021-07-12.csv")


list.files("../data/rds/samples/2019-2021")

my_tibble <- tibble(
  longitude = numeric(),
  latitude = numeric(),
  start_date = date(),
  end_date = character(),
  label = character(),
  cube = character(),
  time_series = list()
)
path <- "../data/rds/samples/2019-2021"
for(file in list.files(path)){
  path_load <- paste(path, file, sep = "/")
  s <- readRDS(path_load)
  my_tibble <- rbind(my_tibble, s)


}
# =============== Execution ====================================================

# Read a samples csv file.
score_metrics2 <- read.csv(file =
    '../data/csv/metrics_score.csv')


# Create a S2-SEN2COR_10_16D_STK-1 sits cube.
sent_cube <- sits_cube(
  source        = "BDC",
  collection    = "S2-16D-2",
  bands         = c("B01", "B02","B03","B04","B05","B06",
                    "B07","B08", "B8A", "B09","B11", "B12", "NDVI", "EVI", "CLOUD"),
  tiles         = c("011013","011014","011015","011016","011017","012013",
                    "012014", "012015", "012016","012017","013013","013014",
                    "013015", "013016","013017","014013","014017","014014",
                    "014015", "014016","015013", "015014","015015","015016",
                    "015017"),
  start_date    = "2018-07-24",
  end_date      = "2021-07-16"
)

path <- "/home/leonardo.vieira/git/Time-Series-Analysis/data/csv/samples_D-2019-2021.csv"
input_data.tb <- sits_get_data(
  cube = sent_cube,
  samples = path,
  bands      = c("B01", "B02","B03","B04","B05","B06",
                 "B07","B08","B8A","B09","B11", "B12", "NDVI", "EVI", "CLOUD"),
  multicores = 16,
  output_dir = "/home/leonardo.vieira/git/Time-Series-Analysis/data/temps"
)



path <- "/home/leonardo.vieira/git/Time-Series-Analysis/data/csv/samples/samples-2019-2021"

list.files(path)
count <- 7
for(file in list.files(path)){
  path_save <- "/home/leonardo.vieira/git/Time-Series-Analysis/data/rds/samples/2019-2021"
  path_samples <- paste(path, file, sep="/")
  print(path_samples)
  input_data.tb <- sits_get_data(
    cube = sent_cube,
    samples = path_samples,
    bands      = c("B01", "B02","B03","B04","B05","B06",
                   "B07","B08","B8A","B09","B11", "B12", "NDVI", "EVI", "CLOUD"),
    multicores = 16,
    output_dir = "/home/leonardo.vieira/git/Time-Series-Analysis/data/temps"
  )
  to_save <- paste(path_save, count, sep = "/")
  to_save <- paste(to_save, "rds", sep = ".")
  saveRDS(input_data.tb, file = to_save)
  count <- count + 1
}


path <- "/home/leonardo.vieira/git/Time-Series-Analysis/data/csv/samples/samples-2019-2021"

list.files(path)
my_tibble <- tibble(
  longitude = numeric(),
  latitude = numeric(),
  start_date = date(),
  end_date = character(),
  label = character(),
  cube = character(),
  time_series = list()
)
path <- "../data/rds/samples/2020-2021"
for(file in list.files(path)){
  print(file)
  path_load <- paste(path, file, sep = "/")
  s <- readRDS(path_load)
  my_tibble <- rbind(my_tibble, s)
}

saveRDS(my_tibble, file = "../data/rds/samples/2019-2021/dsamples-2020-2021.rds")

files <- list("../data/rds/samples/2020-2021/dsamples-2020-2021.rds",
              "../data/rds/samples/2019-2021/dsamples-2019-2021.rds")

for(f in files){
  samples <- readRDS(f)

  samples <- add_tile(samples,
                            "/home/leonardo.vieira/git/Time-Series-Analysis/data/shp/bdc_sm/bdc_sm_4326.shp")

  samples$label[samples$label == 'Floresta'] <- 'Forest'
  samples$label[samples$label == 'Desmatamento'] <- 'Deforestation'

  saveRDS(samples, file = f)

}
s1 <- readRDS("../data/rds/samples/2020-2021/dsamples-2020-2021.rds")
s2 <- readRDS("../data/rds/samples/2019-2021/dsamples-2019-2021.rds")

input_data.tb <- sits_get_data(
  cube = sent_cube,
  samples = path,
  bands      = c("B01", "B02","B03","B04","B05","B06",
                 "B07","B08","B8A","B09","B11", "B12", "NDVI", "EVI", "CLOUD"),
  multicores = 16,
  output_dir = "/home/leonardo.vieira/git/Time-Series-Analysis/data/temps"
)

saveRDS(hclusters, file = "../data/rds/list_clusters.rds")



# Add tile information on data frame
input_data.tb <- add_tile(input_data.tb,
              "/home/leonardo.vieira/git/Time-Series-Analysis/data/shp/roi.shp")

# Save the data set in a rds file
saveRDS(hclusters, file = "../data/rds/list_clusters.rds")

# Filter by NDVI
tiles <- sent_cube$tile

input_all_filtered <- samples[0, ]

for (tile in tiles){
  row <- dplyr::filter(sent_cube, tile == tile)
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
saveRDS(best_clusters, file = "../data/rds/B04-B07-B12-NDVI-EVI/best_clusters.rds")


# Get rasters

tiles <- sent_cube$tile

path <- "../data/imagens/raster/SD-16D/"

tiles <- c("012017","013013","013017","014013","014017",
           "011013","011014","011015","011016","011017",
           "015013","015014","015015","015016","015017")
tiles <- c("012013")

for(t in tiles){
  row <- dplyr::filter(sent_cube, tile == t)
  date <- (row$file_info[[1]]$date[row$file_info[[1]]$cloud_cover
                                   == min(
                                     row$file_info[[1]]$cloud_cover)
                                   & row$file_info[[1]]$band == "NDVI"])
  download_raster(sent_cube, date[[1]], c("B02","B03","B04","NDVI"), t, path)
}

# Read previous data saved
samples <- readRDS("../data/rds/samples.rds")
samples$label[samples$label == 'Floresta'] <- 'Forest'
samples$label[samples$label == 'Desmatamento'] <- 'Deforestation'

patterns <- sits_patterns(samples)

# Create shape file from samples
test_sf<-sits_as_sf(test_out)
st_write(test_sf, "../data/shp/test.shp")

# found image to analysis
test$file_info %>% lapply(function(x) filter(x, date == "2017-08-13"))

# Get rasters

for(t in sent_cube$tile){
  row <- dplyr::filter(sent_cube, tile == t)
  date <- (row$file_info[[1]]$date[row$file_info[[1]]$cloud_cover
                                   == min(
                                     row$file_info[[1]]$cloud_cover)
                                   & row$file_info[[1]]$band == "NDVI"])
  download_raster(sent_cube, date[[1]], c("B02","B03","B04","NDVI"), t, "../data/imagens/raster/")
}

library(dplyr)
nrow(samples[samples$label == 'Forest', ])
nrow(samples[samples$label == 'Deforested', ])


# ====================== Statistics ============================================
# Deforestation

best_clusters <- readRDS("../data/rds/best_clusters_list.rds")


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

tiles <- sent_cube$tile

input_all_filtered <- samples[0, ]

for (tile in tiles){
  row <- dplyr::filter(sent_cube, tile == tile)
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
saveRDS(input_all_filtered, file = "../data/rds/samples_FILTER.rds")

# ================================= CLUSTERING =================================
samples_013015 <- samples[samples$tile == "013015",]
samples_013015_1y <-subset_by_date (samples_013015, "2018-07-28","2019-07-12")
values <- sits_values(samples_013015_1y, "NDVI", format = "cases_dates_bands")
values <- sits_values(samples, comb_bands, format = "cases_dates_bands")
bands <- c("B05", "B07","B11","NDVI", "EVI")

hclusters <- band_analysis_cluster(samples_013015_1y, bands, 5)
gt <- factor(samples_013015_1y$label)
vi_evaluators <- cvi_evaluators("valid", ground.truth = gt)
score_fun <- vi_evaluators$score

hclusters[[1]]$cluster$bands <- bands

metrics <- score_fun(hclusters)
ACC <- sapply(hclusters,accuracy_model,gt=gt)
metrics <- cbind(metrics, ACC)
score <- as.data.frame(metrics)
cbands <- c()
n_bands <- c()
inte <- 0
for(nf in 1:5){
  comb <-combn(bands,nf)
  for(col in 1:ncol(comb)){
    cbands <- append(cbands, paste(comb[,col],collapse='/'))
    n_bands <- append(n_bands, nf)
  }
}
score <- cbind(score, cbands)
score <- cbind(score, n_bands)

score <- best_model_voting(score)
score <- best_model_voting(score,1)
score <- best_model_voting(score,2)
score <- best_model_voting(score,3)
score <- best_model_voting(score,4)
score <- best_model_voting(score,5)


df <- data.frame (labels = gt,
                  cluster = predict_clust
)

unique(df$labels)
unique(df$cluster)

agg_tbl <- df %>% group_by(cluster, labels) %>%
  summarise(total_count=n(),
            .groups = 'drop')

df %>% group_by(cluster, labels) %>%
  summarise(total_count=n()) %>%
  mutate(freq_by_cluster = total_count / sum(total_count))


frequency  %>%
  filter(freq_by_cluster == max(freq_by_cluster))

cluster_dendro_1 <- hclusters

ACC <- sapply(hclusters,accuracy_model,gt=gt)
ACC <- acc
accuracy_model

metrics <- cbind(metrics, ACC)
write.csv(metrics, "/home/leonardo.vieira/git/Time-Series-Analysis/data/csv/metrics.csv", row.names=TRUE)


select(score,contains("best"))
x <- data
xx$indexOfMax <- apply(x, 1, which.max)
x$colName <- colnames(x)[x$indexOfMax]

score[which.max(score$best_model_1),]$cbands


plot.df <- purrr::pmap_dfr(
  list(samples$label, samples$time_series),
  function(label, ts) {
    lb <- as.character(label)
    # extract the time series and convert
    df <- tibble::tibble(Time = ts$Index, ts[-1], Band = lb)
    return(df)
  }
)


plot.df <- tidyr::pivot_longer(plot.df, cols = sits_bands(samples))

p <- ggplot(plot.df, aes(x=name, y=value, color=name)) +
  geom_boxplot()  + ggplot2::facet_wrap(~Band)

png("../data/imagens/papper/box_plot_samples.png")
print(p)
dev.off()

ggsave("../data/imagens/papper/blox_plot_samples.pdf")
ggsave("../data/imagens/papper/blox_plot_samples.png")


gt <- factor(samples_013015_1y$label)
vi_evaluators <- cvi_evaluators("valid", ground.truth = gt)
score_fun <- vi_evaluators$score

metrics <- score_fun(hclusters)

ACC <- sapply(hclusters,accuracy_model,gt=gt)
metrics <- cbind(metrics, ACC)
score <- as.data.frame(metrics)
cbands <- c()
n_bands <- c()
inte <- 0
for(nf in 1:5){
  comb <-combn(bands,nf)
  for(col in 1:ncol(comb)){
    cbands <- append(cbands, paste(comb[,col],collapse='/'))
    n_bands <- append(n_bands, nf)
  }
}
score <- cbind(score, cbands)
score <- cbind(score, n_bands)

score <- best_model_voting(score)
score <- best_model_voting(score,1)
score <- best_model_voting(score,2)
score <- best_model_voting(score,3)
score <- best_model_voting(score,4)
score <- best_model_voting(score,5)

score
write.csv(acc2_tiles, "../data/csv/acc.csv", row.names=TRUE)
write.csv(acc_tiles, "../data/csv/acc2.csv", row.names=TRUE)


cm <- table(predicted, actual)

ACC <- sum(cm[1], cm[4]) / sum(cm[1:4])
PREC <- cm[4] / sum(cm[4], cm[2])
SENS <- cm[4] / sum(cm[4], cm[3])
F1 <- (2 * (sensitivity * precision))/(sensitivity + precision)
SPEC <- cm[1] / sum(cm[1], cm[2])
acc_Forest <-  cm["Forest", "Forest"]/ sum(cm["Forest",])
acc_Deforestation <-  cm["Deforestation", "Deforestation"] / sum(cm["Deforestation",])

majority <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

metrics


best_clusters <- readRDS("../data/rds/models/B04-B07-B12-NDVI-EVI/best_clusters.rds")

train_test_eval <- list()

for(i in 1:length(best_clusters)){
  pred <- prediction_test(samples,
                  best_clusters[[i]],
                  "2018-07-28",
                  "2019-07-12",
                  "013015",
                  "../../data/csv/predicted/B04-B07-B12-NDVI-EVI")

  train_test_eval <- list.append(train_test_eval, pred)
}


federative_units <- st_read("../data/shp/uf_2020/uf_2020.shp")
roi <- st_read("../data/shp/roi_5x5/roi.shp")
brazilian_amazon <- st_read("../data/shp/brazilian_amazon/brazilian_legal_amazon.shp")
grid <- st_read("../data/shp/bdc_sm/bdc_sm_4326.shp")


ggplot() +
  geom_sf(data=federative_units) +
  geom_sf(data=grid, alpha=1/10) +
  geom_sf(data = brazilian_amazon, fill = "green",alpha = 1/5) +
  geom_sf(data = roi, fill = "red",alpha = 1/1.5)


render("./rmd/statisticalanalysis.Rmd", "./rmd/statiscal.pdf")

# rmarkdown::render('/home/leonardo.vieira/git/Time-Series-Analysis/r/rmd/to_pdf/sampling_analysis2.Rmd',
#                   output_file = '/home/leonardo.vieira/git/Time-Series-Analysis/r/rmd/to_pdf/sampling_analysis2.pdf', clean = FALSE)
