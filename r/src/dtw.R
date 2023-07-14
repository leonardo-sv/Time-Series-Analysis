library(cluster)
library(sits)
library(sitsdata)
library(dplyr)
library(factoextra)
library(data.table)
library(sf)
library(dtwSat)
library(dtw)

# Studies dtw and sits

subset_by_nsteps <- function(input_data, first, last) {
  new_end_date <- input_data$time_series[[1]][last,1][[1]]
  new_start_date <- input_data$time_series[[1]][first,1][[1]]
  return(
    input_data %>%
      mutate(
        start_date = new_start_date ,
        end_date = new_end_date,
        time_series
             = lapply(input_data$time_series, '[', first:last,)) %>%
      dplyr::select(latitude,longitude,start_date,end_date,label,cube,tile,time_series,)
  )
}

subset_by_nsteps <- function(input_data, first, last) {
  return(
    input_data %>%
      mutate(end_date = input_data$time_series[[first]][last,first], time_series
             = lapply(input_data$time_series, '[', first:last,)) %>%
      dplyr::select(latitude,longitude,start_date,end_date,label,cube, tile, time_series)
  )
}

# -----------------------------------------------------------------------------
samples <- readRDS("../data/rds/samples_def_filtered.rds")
samples_078094 <- samples[samples$tile == "078094",]
samples_078094_1y <- subset_by_nsteps(samples_078094, 1, 23)
samples_patterns.tb <- sits_patterns(samples_078094_1y)
samples_patterns.tb %>% plot()

samples_test <- head(samples,n=5)
samples_patterns_test<- sits_patterns(samples_test)
samples_patterns_test %>% plot()

reference <-sits_values(samples_patterns_test, format = "bands_cases_dates")
query<-sits_values(samples_test, format = "bands_cases_dates")

alignment<-dtw(query$NDVI[1,],reference$NDVI[1,],keep=TRUE)

plot(alignment,type="threeway")

alignment$normalizedDistance



