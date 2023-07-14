sent_cube %>%
  dplyr::filter(tile == "077094", file_info$date)

# Plot Tests
plot(raster_plot, band = "NDVI", palette = "RdYlGn")
plot(raster_plot, band = "NDVI", palette = "RdYlGn", reset=FALSE)




layout_matrix_1 <- matrix(1:9, ncol = 3)       # Define position matrix
layout_matrix_1 

layout(layout_matrix_1) 


test_sf3<-sits_as_sf(
  find_outliers(input_data.tb, "2018-06-10", 0.4, 0.6, "Floresta", tile))
file <- "../data/shp/"
file <- paste(file, "Floresta", sep="")
file <- paste(file, "_2018-06-10_", sep="")
file <- paste(file, tile, sep="")
file <- paste(file, ".shp", sep="")
st_write(test_sf3, file)

# Deforestation
test_sf1<-sits_as_sf(
  find_outliers(input_data.tb, "2017-09-14", 0.1, 0.4, "Floresta", tile))
file <- "../data/shp/"
file <- paste(file, "Floresta", sep="")
file <- paste(file, "_2017-09-14_", sep="")
file <- paste(file, tile, sep="")
file <- paste(file, ".shp", sep="")
st_write(test_sf1, file)
test_sf4<-sits_as_sf(
  find_outliers(input_data.tb, "2017-09-14", 0.8, 0.9, "Desmatamento", "078094"))
file <- "../data/shp/"
file <- paste(file, "Desmatamento", sep="")
file <- paste(file, "_2017-09-14_test", sep="")
file <- paste(file, "078094", sep="")
file <- paste(file, ".shp", sep="")
st_write(test_sf4, file)

test_sf5<-sits_as_sf(
  find_outliers(input_data.tb, "2017-08-13", -0.7, 0.15, "Desmatamento", tile))
file <- "../data/shp/"
file <- paste(file, "Desmatamento", sep="")
file <- paste(file, "_2017-08-13_", sep="")
file <- paste(file, tile, sep="")
file <- paste(file, ".shp", sep="")
st_write(test_sf5, file)

test_sf6<-sits_as_sf(
  find_outliers(input_data.tb, "2018-06-10", 0.7, 0.9, "Desmatamento", tile))
file <- "../data/shp/"
file <- paste(file, "Desmatamento", sep="")
file <- paste(file, "_2018-06-10_", sep="")
file <- paste(file, tile, sep="")
file <- paste(file, ".shp", sep="")
st_write(test_sf6, file)



```{r  echo=F ,results='asis' }

tile_file <- function(local_file){
  return(substr(tail(strsplit(local_file,"/")[[1]], n=1),1,6))
  
}
# print(list.files(directories[d], pattern="*.png"))
# cat('<h1>', paste0('TABLE: ', sub('figures//', '', directories[d])), '</h1> <p>')

plots <- list.files('../data/imagens/NDVI/DEFOREST', pattern="*.png")
plots <- paste0('../data/imagens/NDVI/DEFOREST', '/', plots)


if(length(plots > 0)){
  # #create an html table with float left/right, whatever....
  for(i in seq(1,length(plots),3)){
    cat('<table border="0">
    <tr>
    <td height="250">',tile_file(plots[i]),'<img src=',plots[i],' height="300" width="300"/></td>
    <td height="250">',tile_file(plots[i+1]),'<img src=',plots[i+1],' height="300" width="300"/></td>
    <td height="250">',tile_file(plots[i+2]),'<img src=',plots[i+2],' height="300" width="300"/></td>
    </tr>
    </table>')
  }
  
  # }
}

```

# Notebook
# ```{r  echo=F ,results='asis' }
# 
# tile_file <- function(local_file){
#   return(substr(tail(strsplit(local_file,"/")[[1]], n=1),1,6))
#   
# }
# # print(list.files(directories[d], pattern="*.png"))
# # cat('<h1>', paste0('TABLE: ', sub('figures//', '', directories[d])), '</h1> <p>')
# 
# plots <- list.files('../data/imagens/NDVI/FOREST', pattern="*.png")
# plots <- paste0('../data/imagens/NDVI/FOREST', '/', plots)
# 
# 
# if(length(plots > 0)){
#   # #create an html table with float left/right, whatever....
#   for(i in seq(1,length(plots),3)){
#     cat('<table border="0">
#     <tr>
#     <td height="250">',tile_file(plots[i]),'<img src=',plots[i],' height="300" width="300"/></td>
#     <td height="250">',tile_file(plots[i+1]),'<img src=',plots[i+1],' height="300" width="300"/></td>
#     <td height="250">',tile_file(plots[i+2]),'<img src=',plots[i+2],' height="300" width="300"/></td>
#     </tr>
#     </table>')
#   }
#   
#   # }
# }
# 
# ```


# find_outlier <- function(samples, timestep, value){
#   samples %>% lapply(input_data$time_series, 
#                      function(x) filter(
#                        x, Index >= 
#                          as.Date("2017-08-13"), 
#                        Index <= as.Date("2018-06-26")))
#   
#   input_data %>%
#     filter(start_date = as.Date("2017-08-13"), 
#            end_date = as.Date("2018-06-26"),
#            time_series = lapply(input_data$time_series, 
#                                 function(x) filter(
#                                   x, Index >= 
#                                     as.Date("2017-08-13"), 
#                                   Index <= as.Date("2018-06-26")))
#            
# }



# return(
#   input_data %>%
#     mutate(start_date = as.Date("2017-08-13"), 
#            end_date = as.Date("2018-06-26"),
#            time_series = lapply(input_data$time_series, 
#                                 function(x) filter(
#                                   x, Index >= 
#                                     as.Date("2017-08-13"), 
#                                   Index <= as.Date("2018-06-26")))
#            
#            band_tb <- sits_select(samples.df, "NDVI")
#            ts <- band_tb$time_series
#            dt_byrows <- data.table::data.table(dplyr::bind_rows(ts))
#            
#            
#            dt_byrows
#            
#            
#            
#            dt_qts <- create_iqr(dt_byrows, "NDVI")
#            
#            dt_ts <- data.table::data.table(ts)
#            dt_acf <- dt[, stats::acf(ts[[1]]), by = Index]
#            ts[[1]]
#            
#            test$file_info %>% lapply(function(x) filter(x, date == "2017-07-28"))
#            
#            dplyr::filter(date == "2017-07-28")
#            
#            # sf_object <- sits_as_sf(input_data.tb)
#            sf_object
