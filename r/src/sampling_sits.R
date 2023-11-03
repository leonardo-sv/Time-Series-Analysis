library(cluster)
library(sits)
library(sitsdata)
library(dplyr)
library(factoextra)
library(data.table)
library(sf)
library(dtwclust)


# =============== Functions ====================================================

# This function allows filter a time series data set ("S2-SEN2COR_10_16D_STK-1")
# based in a specific date, label, tile, value, at NDVI band. The objective is 
# to search for possible outliers in data set. 
filter_vband <- function(input_data,
                         obs_date,
                         min_value,
                         max_value,
                         label_target,
                         tile_target){
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
# Function that realize a subset of a time-series tibble. It split the time-series
# considering a start and end dates generating a new data.
subset_by_date <- function(input_data, start_date_, end_date_) {
  start_pos <- which(input_data$time_series[[1]]$Index == start_date_)
  end_pos <- which(input_data$time_series[[1]]$Index == end_date_)
  
  return(
    input_data %>%
      dplyr::mutate(start_date = start_date_, end_date = end_date_, time_series
             = lapply(input_data$time_series, '[', start_pos:end_pos,)) %>%
      dplyr::select(latitude,longitude,start_date,
                    end_date,label,cube, tile,time_series)
  )
}

# Function that realize a filter using a minimum and maximum limit of the band
# value.
filter_band <- function(input_data, obs_date, min_value,
                        max_value, label_target){
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

# Example: a<-find_outliers(input_data.tb, "2018-06-26", -0.81, 0.1, Desmatamento, 079093)

# This function adds the tile column in a S2-SEN2COR_10_16D_STK-1 cube data set.
add_tile <- function(data, shapefile){
  nc <- st_read(shapefile)
  print("points")
  points <- mapply(function(x,y) st_point(c(x,y), 4326),
                   data$longitude,data$latitude,SIMPLIFY = FALSE)
  print("lp")
  lp <- lapply(points, st_within, nc, sparse=FALSE)
  print("wlp")
  wlp <- lapply(lp, which)
  print("ltiles")
  ltiles <- lapply(wlp, function(s) nc$tile[s])
  tiles <- rapply(ltiles, function(x) head(x, 1))
  return(data %>% mutate(tile=tiles))
  
}
add_tile_2 <- function(data.df, shapefile){
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

within_pos <- function(point, geom){
  lp <- lapply(point, st_within, geom, sparse=FALSE)
  
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
  #return(p)
  
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



corr_ts <- function(ts_tibble){
  
  col_names <- c("Var1", "Var2", "Value", "label")
  df <- read.table(text = "",
                   col.names = col_names)
  labels <- ts_tibble %>% dplyr::pull(label)
  for (l in labels){
    dt_bylabel <- ts_tibble[ts_tibble$label == l,]
    ts <- dt_bylabel$time_series
    cormat <- round(cor(ts[[1]][2:15]),2)
    melted_cormat <- melt(cormat)
    melted_cormat$label <- l
    df <- rbind(df, melted_cormat)
  }
  return(df)

}

plot_corr_matrix <- function(corr){
  
  gp <- ggplot(data = corr, aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile(color = "white",
              lwd = 0.5,
              linetype = 1) + ggplot2::facet_wrap(~label) +
    scale_fill_gradient2(low = "blue", high = "red", mid="white")
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


find_sample <- function(data, long, lat){
  return(filter(data, (longitude > long - 0.0003 & longitude < long + 0.0003)
                , (latitude > lat - 0.0003 & latitude < lat + 0.0003)))
}

download_raster <- function(cube, date_, bands_, tile_, dir) {
  new_path <- paste(dir, tile_, sep = "")
  if (!dir.exists(new_path)){
    dir.create(new_path)
    print(new_path)
  }
  
  file_info_tile <-cube[cube$tile == tile_,]$file_info[[1]]
  
  for (b in bands_){
    path <- filter(file_info_tile, date==date_, band==b)$path
    #print(path)
    url <- strsplit(path, "l/")[[1]][2] 
    splited_url <- strsplit(url, "/")
    name_file <- tail(splited_url[[1]], n=1)
    name_file <- strsplit(name_file, ".tif")[[1]][1]
    name_file <- paste(name_file, ".tif", sep="")
    path_file <- paste(new_path, name_file, sep="/")
    print(path_file)
    print(url)
    download.file(url, path_file)
  }
}

voting_nbands <- function(score, n_bands, fuzzy_score = FALSE, no_hierarchical = FALSE){
  score_eval <- score[lengths(strsplit(score$bands, '/')) == n_bands,]
  best_model <- voting_best(score_eval, fuzzy_score, no_hierarchical)
  bands <- score_eval[best_model,]$bands
  return(which(score$bands == bands))
}

voting_best <- function(score, fuzzy_score = FALSE, no_hierarchical = FALSE){
  
  eval_metrics <- c("VI","COP",  "RI", "ARI", "J", 
                    "FM", "NMIM", "Sil", "D", 
                    "ACC", "PREC", "SENS", "F1", "SPEC")
  fuzzy_metrics <- c("MPC", "K", "T", "SC", "PBMF")
  no_hierarchical_metrics <- c("DB", "DBstar", "CH", "SF")
  
  if (fuzzy_score){
    eval_metrics <- c(eval_metrics, fuzzy_metrics)
  }
  
  if (no_hierarchical){
    eval_metrics <- c(eval_metrics, no_hierarchical_metrics)
  }
  
  score_eval <- score[, which(names(score) %in% eval_metrics)]
  
  
  best_by_metrics <- apply(score_eval, 2L, which.max)
  
  majority <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  majority(best_by_metrics)
  
}

best_model_voting <- function(score, n_bands = "all"){
  best_model <- numeric(nrow(score))
  index <- c(1:nrow(score))
  score <- cbind(score, best_model)
  score <- cbind(score, index)
  
  minimized <- c("VI","COP", "DB", "DBstar", "K", "T")
  maximized <- c("RI", "ARI", "J", "FM", "NMIM", "Sil", "D", "CH", "SF", "MPC",
                 "SC", "PBMF", "ACC", "PREC", "SENS", "F1", "SPEC")
  
  if(is.numeric(n_bands)){
    compute_score <- score[score$n_bands == n_bands,]
  }
  else{
    compute_score <- score
  }
  
  for(i in colnames(score)){
    if (i %in% maximized){
      index <-compute_score[which.max(compute_score[i][,1]),]$index
      score[score$index == index,]$best_model <- 
        score[score$index == index,]$best_model +1
      
    }
    else if (i %in% minimized){
      index <-compute_score[which.min(compute_score[i][,1]),]$index
      score[score$index == index,]$best_model <- 
        score[score$index == index,]$best_model +1
      
    }
    
  }
  colnames(score)[which(names(score) == "best_model")] <- 
    paste("best_model", n_bands, sep="_")
  
  return(subset(score, select = -index))
  
  
}

define_label_cluster <- function(predicted, ground_truth){
  df <- data.frame (labels = ground_truth,
                    cluster = predicted
  )
  
  cluster_label <- df %>% group_by(cluster, labels) %>% 
    summarise(total_count=n(),.groups = "drop_last") %>%
    mutate(freq_by_cluster = total_count / sum(total_count)) %>%
    filter(freq_by_cluster == max(freq_by_cluster)) 
  
  cluster_label <- cluster_label[, -which(names(cluster_label) 
                         %in% c("total_count","freq_by_cluster"))]
  cluster_label <- as.data.frame(cluster_label)
  return(cluster_label)
  
}

cluster_predicted_label <- function(model, predicted){
  for (c in model$cluster_label$cluster){
    clabel <- filter(model$cluster_label, cluster == c)
    predicted[predicted==clabel$cluster]<-as.character(clabel$labels)
    
  }
  return(predicted)
  
}

define_label_cluster2 <- function(predicted, gt){
  df <- data.frame (labels = gt,
                    cluster = predicted
  )
  
  cluster_label <- df %>% group_by(cluster, labels) %>% 
    summarise(total_count=n(),.groups = "drop_last") %>%
    mutate(freq_by_cluster = total_count / sum(total_count)) %>%
    filter(freq_by_cluster == max(freq_by_cluster)) 
  
  return(sapply(df$cluster, 
                function(s) cluster_label[cluster_label$cluster == s,]$labels))
  
}

accuracy_model <- function(model, gt){
  predict_clust <- dtwclust::predict(model, model$datalist)
  cluster_labeled <- cluster_predicted_label(model, predict_clust)
  hits <- as.integer(gt == cluster_labeled)
  acc <- sum(hits)/length(hits)
  return(acc)
}
predicted <-dtwclust::predict(best_clusters[[1]])
accuracy_model2 <- function(model, values){
  predict_clust <- dtwclust::predict(model, values)
  return(predict_clust)
}

clusters_by_bands_combination <- function(samples, bands, max_nf){
  
  combns_hcluster <- list()
  
  for(nf in 1:max_nf){
    combinations <- combn(bands,nf)
    
    for(c in 1:length(combinations[1,])){
      print(combinations[,c])
      comb_bands <- combinations[,c]
      values <- sits_values(samples, comb_bands, format = "cases_dates_bands")
      
      
      dendro <- dtwclust::tsclust(
        series = values,
        type = "hierarchical",
        k = max(nrow(values)-1, 2),
        distance = "dtw_basic",
        control = dtwclust::hierarchical_control(method = "ward.D2")
        
      )
      dendro$bands <- comb_bands
      ground_truth <- factor(samples$label)
      predicted <- dtwclust::predict(dendro, values)
      dendro$cluster_label <- define_label_cluster(predicted, ground_truth)
      
      combns_hcluster <- append(combns_hcluster,list(dendro))
      
    }
    
  }
  return(combns_hcluster)
  
}

cluster_by_band <- function(clusters, bands){
  count = 1
  for(c in clusters){
    tband <- paste(c$bands, collapse = "/")
    if(tband == bands){
      return(count)
    }
    count <- count + 1
  }
  return(NULL)
}


best_clusters <- function(score, clusters){
  bclusters <- list()
  voting <- data.frame(select(score,contains("best")))
  cols <- colnames(voting)
  for(col in cols){
    bbands<- score[which.max(voting[, col]),]$cbands
    
    cpos <- cluster_by_band(clusters, bbands)
    model <- clusters[[cpos]]
    bclusters <- append(bclusters, list(model))
  }
  return(bclusters)
}



accuracy_analysis <- function(samples, clusters, start_date, end_date, path){
  tiles <- unique(samples$tile)
  
  acc_df <- data.frame(band=character(0),
                       acc=numeric(0),
                       tile=character(0),
                       start_date=date,
                       end_date=date)
  
  for(model in clusters){
    for(tile in tiles){
      print(tile)
      tsamples <- samples[samples$tile == tile,]
      tsamples <-subset_by_date(tsamples, start_date,end_date)
      ts <- sits_values(tsamples, model$bands, format = "cases_dates_bands")
      gt <- factor(tsamples$label)
      predict_clust <- accuracy_model2(model, ts)
      cluster_labeled <- cluster_predicted_label(model, predict_clust)
      hits <- as.integer(gt == cluster_labeled)
      acc_cluster <- sum(hits)/length(hits)
      bands_cluster <- paste(model$bands, collapse = "/") 
      print(bands_cluster)
      acc_save <- data.frame(
        band=bands_cluster,
        acc = acc_cluster,
        tile = tile,
        start_date = start_date,
        end_date = end_date
      )
      acc_df <- rbind(acc_df, acc_save)
      
      
      
      tosave <- tsamples[, -which(names(samples) == "time_series")]
      tosave$cluster <- predict_clust
      tosave$cluster_label <- cluster_labeled
      nametosave <- paste(model$bands, collapse = "-")
      nametosave <- paste("cluster",nametosave,sep="_")
      nametosave <- paste(nametosave,tile, sep="_")
      nametosave <- paste(nametosave, start_date, end_date, sep="-")
      nametosave <- paste(path, nametosave,sep="")
      nametosave <- paste(nametosave,"csv",sep=".")
      write.csv(tosave, nametosave, row.names=TRUE)
      
      nameacc <- paste("acc", start_date, end_date, sep="_")
      nameacc <- paste(path,nameacc, sep="")
      nameacc <- paste(nameacc,"csv", sep=".")
      tosave <- as.data.frame(tosave)
      write.csv(acc_df, nameacc, row.names=TRUE)
      
    
    }
    
  }
  return(acc_df)
  
}
for(model in best_clusters_list){
  print(model$bands)
  
}
filter_NDVI <- function(sent_cube, samples){
  
  tiles <- sent_cube$tile
  
  input_all_filtered <- samples[0, ] 
  
  for (tile in tiles){
    row <- dplyr::filter(sent_cube, tile == tile)
    dates <-  (row$file_info[[1]]$date[row$file_info[[1]]$cloud_cover < 5.0 & row$file_info[[1]]$band == "NDVI"])
    dates <- dates[dates %in% input_data.tb$time_series[[1]]$Index]
    
    input_forest <- filter_vband(samples, dates[1], 0.60, 1,"Floresta", tile)
    input_deforestation <- filter_vband(samples, dates[1], -1, 0.865,"Desmatamento", tile)
    for (i in 2:length(dates)){
      input_forest <- filter_vband(input_forest, dates[i], 0.60, 1,"Floresta", tile)
      
      input_deforestation <- filter_vband(input_deforestation, dates[i], -1, 0.865,"Desmatamento", tile)
    } 
    input_all_filtered <- rbind(input_all_filtered, input_forest)
    input_all_filtered <- rbind(input_all_filtered, input_deforestation)
  }
  return(input_all_filtered)
}

bynary_classification_metrics <- function(predicted, actual){
  print("binary")
  cm <- table(predicted, actual)
  print(cm)
  print(head(predicted))
  print(head(actual))
  ACC <- sum(cm[1], cm[4]) / sum(cm[1:4])
  PREC <- cm[4] / sum(cm[4], cm[2])
  SENS <- cm[4] / sum(cm[4], cm[3])
  F1 <- (2 * (SENS * PREC))/(SENS + PREC)
  SPEC <- cm[1] / sum(cm[1], cm[2])
  
  ACC_F <-  cm["Forest", "Forest"] / sum(cm["Forest",]) 
  ACC_D <-  cm["Deforestation", "Deforestation"] / sum(cm["Deforestation",])
  
  metrics <- data.frame(ACC,PREC,SENS,F1,SPEC,ACC_F,ACC_D)
  
 return(metrics)
  
}
clusters <- readRDS("../data/rds/best_clusters_list.rds")
clustering_metrics <- function(model, ground_truth){
  vi_evaluators <- cvi_evaluators("valid", ground.truth = ground_truth)
  score_fun <- vi_evaluators$score
  metrics <- score_fun(list(model))
  metrics = data.frame(metrics)
  bands <- paste(model$bands, collapse = "/") 
  metrics <- cbind(bands, metrics)
  return(metrics)
  
}

classification_metrics <- function(model, ground_truth){
  predicted <- dtwclust::predict(model, model$datalist)
  predicted <- cluster_predicted_label(model, predicted)
  cmetrics <-bynary_classification_metrics(predicted, ground_truth)
  bands <- paste(model$bands, collapse = "/") 
  cmetrics <- cbind(bands,cmetrics)
  return(cmetrics)
}



models_metrics <- function(models, samples){
  ground_truth <- factor(samples$label)
  cluster_metrics <- lapply(models, clustering_metrics, ground_truth)
  cluster_metrics <- bind_rows(cluster_metrics)
  bclass_metrics  <- lapply(models, classification_metrics, ground_truth)
  bclass_metrics <- bind_rows(bclass_metrics)
  metrics <- bind_cols(cluster_metrics, bclass_metrics, .id = "bands")
  names(metrics)[names(metrics) == "bands...1"] <- "bands"
  metrics <- metrics[, -which(names(metrics) %in% c(".id", "bands...14"))]
  return(metrics)
  
}


model_metrics <- function(hclusters, samples){
  gt <- factor(samples$label)
  vi_evaluators <- cvi_evaluators("valid", ground.truth = gt)
  score_fun <- vi_evaluators$score
  metrics <- score_fun(hclusters)
  ACC <- sapply(hclusters,accuracy_model,gt=gt)
  metrics <- cbind(metrics, ACC)
  score <- as.data.frame(metrics)
  cbands <- c()
  n_bands <- c()
  inte <- 0
  for(nf in 1:n_comb){
    comb <-combn(bands,nf)
    for(col in 1:ncol(comb)){
      cbands <- append(cbands, paste(comb[,col],collapse='/'))
      n_bands <- append(n_bands, nf)
    }
  }
  score <- cbind(score, cbands)
  score <- cbind(score, n_bands)
  
  score <- best_model_voting(score)
  
  for(i in 1:n_comb){
    score <- best_model_voting(score,i)
  }

  return(score)
}

prediction_test <- function(samples,
                            model,
                            start_date,
                            end_date,
                            path,
                            tile_train = NULL){
  
  prediction_df <- data.frame(
    start_date = start_date,
    end_date = end_date,
    bands = paste(model$bands, collapse = "-")
  )
  
  tsamples <-subset_by_date(samples, start_date,end_date)
  ts <- sits_values(tsamples, model$bands, format = "cases_dates_bands")
  predicted <- dtwclust::predict(model, ts)
  ground_truth <- factor(tsamples$label)
  predicted_labeled <- cluster_predicted_label(model, predicted)
  
  
  samples_save <- tsamples[, -which(names(tsamples) == "time_series")]
  samples_save$cluster_prediction <- predicted
  samples_save$cluster_labeled <- predicted_labeled
  nametosave <- paste(model$bands, collapse = "-")
  nametosave <- paste("cluster",nametosave,sep="_")
  nametosave <- paste(nametosave, start_date, end_date, sep="-")
  nametosave <- paste(path, nametosave,sep="/")
  nametosave <- paste(nametosave,"csv",sep=".")
  write.csv(samples_save, nametosave, row.names=TRUE)
  
  test_samples <- samples_save
  
  if(!is.null(tile_train)){
    test_samples <- samples_save[samples_save$tile != tile_train,]
    train_samples <- samples_save[samples_save$tile == tile_train,]
    
    metrics <- bynary_classification_metrics(samples_save$cluster_labeled, samples_save$label)
    prediction_df$type <- "Train"
    train <- cbind(prediction_df, metrics)
  }
  
  metrics <- bynary_classification_metrics(test_samples$cluster_labeled, test_samples$label)
  prediction_df$type <- "Generalization(Test)"  
  test <- cbind(prediction_df, metrics)
  
  if(!is.null(tile_train)){
    return_df <- rbind(train, test)
  }
  else{
    return_df <- test
  }
  
  return(return_df)

}

prediction_metrics_by_tile <- function(samples_predicted, start_date, end_date, bands){
  tiles <- unique(samples_predicted$tile)
  prediction_df <- data.frame(
    start_date = start_date,
    end_date = end_date,
    bands = bands
  )
  test_acc <- data.frame(tile=character(0),
                       start_date=date,
                       end_date=date,
                       band=character(0),
                       ACC=numeric(0),
                       PREC=numeric(0),
                       SENS=numeric(0),
                       F1=numeric(0),
                       SPEC=numeric(0),
                       ACC_F=numeric(0),
                       ACC_D=numeric(0)
                       )
  
  for (tile in tiles){
    print(tile)
    samples_by_tile <- samples_predicted[samples_predicted$tile == tile,]
    print(head(samples_by_tile))
    
    print(head(samples_by_tile$cluster_labeled))
    print(head(samples_by_tile$label))
    metrics <- bynary_classification_metrics(
    samples_by_tile$cluster_labeled, samples_by_tile$label)
    tile_metric <- data.frame(tile=tile,
                           start_date=start_date,
                           end_date=end_date,
                           band=bands)
    tile_metric <-cbind(tile_metric, metrics)
    test_acc <- rbind(test_acc,tile_metric)
  }
  return(test_acc)
}

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



path <- "/home/leonardo.vieira/git/Time-Series-Analysis/data/csv/samples-2019-2021"

list.files(path)
count <- 1
for(file in list.files(path)){
  path_save <- "/home/leonardo.vieira/git/Time-Series-Analysis/data/rds"
  path_samples <- paste(path, file, sep="/")
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

