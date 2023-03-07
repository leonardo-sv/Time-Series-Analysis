library(cluster)
library(sits)
library(sitsdata)
library(dplyr)
library(factoextra)


# samples.df <- read.csv(file = '../data/samples_12000.csv')


sent_cube <- sits_cube(
  source        = "BDC",
  collection    = "S2-SEN2COR_10_16D_STK-1",
  name          = "sentinel_cube",
  bands         = c("NDVI", "EVI",  "CLOUD"),
  tiles         = c("077095", "078095", "079095", "077094", "078094",
                    "078094", "077093", "078093", "079093" ), 
  start_date    = "2017-08-06", 
  end_date      = "2019-07-31" 
)



input_data.tb <- sits_get_data(
  cube = sent_cube, 
  file = "../data/csv/samples.csv",
  bands      = c("NDVI", "EVI", "CLOUD"),
  multicores = 12
)

saveRDS(input_data.tb, file = "../data/rds/samples.rds")
