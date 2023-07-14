library(cluster)
library(sits)
library(sitsdata)
library(dplyr)
library(factoextra)
library(data.table)
library(sf)
library(dtwSat)
library(dtw)
library(dtwclust)

# Studies dtwclust
data("uciCT")

# Partional

plot(CharTraj$A.V1)

pc <- tsclust(CharTraj, type = "partitional", k = 20L, 
              distance = "dtw_basic", centroid = "pam", 
              seed = 3247L, trace = TRUE,
              args = tsclust_args(dist = list(window.size = 20L)))

plot(pc)



#dtwclust and sits

samples <- readRDS("../data/rds/samples_def_filtered.rds")
samples_078094 <- samples[samples$tile == "078094",]

values <- sits_values(samples_078094, bands = "NDVI", format = "cases_dates_bands")

pc <- tsclust(values, type = "partitional", k = 20L, 
              distance = "dtw_basic", centroid = "pam", 
              seed = 3247L, trace = TRUE,
              args = tsclust_args(dist = list(window.size = 20L)))




samples_NDVI <- sits_select(samples, bands = "NDVI")


samples_NDVI$time_series

values <- sits_values(samples, bands = "NDVI", format = "cases_dates_bands")














# Test one tile

samples_078094.tb <- samples.tb[samples.tb$tile == "078094",]

dates <- samples_078094.tb$time_series[[1]]$Index

samples_078094_1y.tb <- subset_by_nsteps(samples_078094.tb, 1, 23)

samples_patterns.tb <- sits_patterns(samples_078094_1y.tb)

samples_patterns.tb %>% plot()

samples_078094.tb

alignment<-dtw(query,template,keep=TRUE);

# Studies in wtd package

# Example 1 - Time series
data(aami3a);
data(aami3b);

plot( main="ECG (mV)",
      window(
        cbind(aami3a,aami3b) ,end=10)
)

class(aami3a)


# Example 2 - Count possible warp paths

ds<-dtw(c(1:2),c(1:3),keep=TRUE,step=asymmetric);
countPaths(ds)

#Example 3 - Using dtw() function

## A noisy sine wave as query
idx<-seq(0,6.28,len=100);
query<-sin(idx)+runif(100)/10;
## A cosine is for reference; sin and cos are offset by 25 samples
reference<-cos(idx)
plot(reference); lines(query,col="blue");
## Find the best match
alignment<-dtw(query,reference);
## Display the mapping, AKA warping function - may be multiple-valued
## Equivalent to: plot(alignment,type="alignment")
plot(alignment$index1,alignment$index2,main="Warping function");
## Confirm: 25 samples off-diagonal alignment
lines(1:100-25,col="red")


#########
##
## Partial alignments are allowed.
##
alignmentOBE <-
  dtw(query[44:88],reference,
      keep=TRUE,step=asymmetric,
      open.end=TRUE,open.begin=TRUE);
plot(alignmentOBE,type="two",off=1);



#########
##
## Subsetting allows warping and unwarping of
## timeseries according to the warping curve.
## See first example below.
##
## Most useful: plot the warped query along with reference
plot(reference)
lines(query[alignment$index1]~alignment$index2,col="blue")
## Plot the (unwarped) query and the inverse-warped reference
plot(query,type="l",col="blue")
points(reference[alignment$index2]~alignment$index1)


#########
##
## Contour plots of the cumulative cost matrix
## similar to: plot(alignment,type="density") or
## dtwPlotDensity(alignment)
## See more plots in ?plot.dtw
##
## keep = TRUE so we can look into the cost matrix
alignment<-dtw(query,reference,keep=TRUE);
contour(alignment$costMatrix,col=terrain.colors(100),x=1:100,y=1:100,
        xlab="Query (noisy sine)",ylab="Reference (cosine)");
lines(alignment$index1,alignment$index2,col="red",lwd=2);

#########
##
## An hand-checkable example
##
ldist<-matrix(1,nrow=6,ncol=6); # Matrix of ones
ldist[2,]<-0; ldist[,5]<-0; # Mark a clear path of zeroes
ldist[2,5]<-.01; # Forcely cut the corner
ds<-dtw(ldist); # DTW with user-supplied local
# cost matrix
da<-dtw(ldist,step=asymmetric); # Also compute the asymmetric
plot(ds$index1,ds$index2,pch=3); # Symmetric: alignment follows

points(da$index1,da$index2,col="red"); # Asymmetric: visiting
# 1 is required twice
ds$distance;
da$distance;

## Symmetric step pattern => symmetric dissimilarity matrix;
## no problem coercing it to a dist object:
m <- matrix(0,ncol=3,nrow=4)
m <- row(m)
dist(m,method="DTW");


## Find the optimal warping _and_ scale factor at the same time.
## (There may be a better, analytic way)
# Prepare a query and a reference
query<-sin(seq(0,4*pi,len=100))
reference<-cos(seq(0,4*pi,len=100))
# Make a set of several references, scaled from 0 to 3 in .1 increments.
# Put them in a matrix, in rows
scaleSet <- seq(0.1,3,by=.1)
referenceSet<-outer(1/scaleSet,reference)
# The query has to be made into a 1-row matrix.
# Perform all of the alignments at once, and normalize the result.
dist(t(query),referenceSet,meth="DTW")->distanceSet
# The optimal scale for the reference is 1.0
plot(scaleSet,scaleSet*distanceSet,
     xlab="Reference scale factor (denominator)",
     ylab="DTW distance",type="o",
     main="Sine vs scaled cosine alignment, 0 to 4 pi")
# Old-fashioned call style would be:
# dtwDist(m)
# as.dist(dtwDist(m))

## Asymmetric step pattern: we can either disregard part of the pairs
## (as.dist), or average with the transpose
mm <- matrix(runif(12),ncol=3)
dm <- dist(mm,mm,method="DTW",step=asymmetric); # a crossdist object
# Old-fashioned call style would be:
# dm <- dtwDist(mm,step=asymmetric)
# as.dist(dm)
## Symmetrize by averaging:
(dm+t(dm))/2
## check definition
stopifnot(dm[2,1]==dtw(mm[2,],mm[1,],step=asymmetric)$distance)

subset_by_nsteps <- function(input_data, first, last) {
  return(
    input_data %>%
      mutate(end_date = input_data$time_series[[first]][last,first], time_series
             = lapply(input_data$time_series, '[', first:last,)) %>%
      dplyr::select(latitude,longitude,start_date,end_date,label,cube,time_series)
  )
}

split_time_series <- function(ts_data, first_date, end_date){
  return(
    ts_data %>%
        mutate(start_date = as.Date(first_date), 
               end_date = as.Date(end_date),
               time_series = lapply(ts_data$time_series, 
                                    function(x) filter(
                                      x, Index >= 
                                        as.Date(first_date), 
                                      Index <= as.Date(end_date)))
        )
    )
}

dtw_ts <- function(data, patterns){
  data$time_series[[1]]$NDVI
  
}

samples.tb <- readRDS("../data/rds/samples_def_filtered.rds")

# Test one tile

samples_078094.tb <- samples.tb[samples.tb$tile == "078094",]

dates <- samples_078094.tb$time_series[[1]]$Index

samples_078094_1y.tb <- subset_by_nsteps(samples_078094.tb, 1, 23)

samples_patterns.tb <- sits_patterns(samples_078094_1y.tb)

samples_patterns.tb %>% plot()

samples_078094.tb

alignment<-dtw(query,template,keep=TRUE);


## A noisy sine wave as query
idx<-seq(0,6.28,len=100);
query<-sin(idx)+runif(100)/10;

## A cosine is for template; sin and cos are offset by 25 samples
template<-cos(idx)

## Find the best match with the canonical recursion formula
alignment<-dtw(query,template,keep=TRUE);

## Display the warping curve, i.e. the alignment curve
plot(alignment,type="threeway")

head(query)
head(template)
head(samples_078094_1y.tb $time_series[[2]]$NDVI)

alignment<-dtw(samples_078094_1y.tb $time_series[[1]]$NDVI,samples_078094_1y.tb $time_series[[2]]$NDVI,keep=TRUE);

plot(alignment,type="threeway")

plot(
  dtw(samples_078094_1y.tb $time_series[[1]]$NDVI,samples_078094_1y.tb $time_series[[2]]$NDVI,keep=TRUE,
      step=rabinerJuangStepPattern(6,"c")),
  type="twoway",offset=-2);

install.packages("dtwclust")
library(dtwclust)

data("uciCT")

head(CharTraj)
head(samples_078094_1y.tb$time_series[[1]]$NDVI)

pc <- tsclust(CharTraj, type = "partitional", k = 20L, 
              distance = "dtw_basic", centroid = "pam", 
              seed = 3247L, trace = TRUE,
              args = tsclust_args(dist = list(window.size = 20L)))

plot(pc)

mvc <- tsclust(CharTrajMV[1L:20L], k = 4L, distance = "gak", seed = 390L)
# Note how the variables of each series are appended one after the other in the plot
plot(mvc, labels = list(nudge_x = -10, nudge_y = 1))

hc_sbd <- tsclust(CharTraj, type = "h", k = 30L,
                  preproc = zscore, seed = 899,
                  distance = "sbd", centroid = shape_extraction,
                  control = hierarchical_control(method = "average"))

plot(hc_sbd)
plot(hc_sbd, type = "series", clus = 1L)
plot(hc_sbd, type = "centroids", clus = 1L)



head(samples_078094_1y.tb$time_series)

.ts_cols <- c("sample_id", "label")

# Add sample_id column
samples_078094_1y.tb[["sample_id"]] <- seq_along(samples_078094_1y.tb[["time_series"]])
# Extract time_series from column
ts <- tidyr::unnest(
  data = samples_078094_1y.tb[c(.ts_cols, "time_series")],
  cols = "time_series"
)

ts


pcNDVI <- tsclust(ts$NDVI, type = "partitional", k = 2L, 
              distance = "dtw_basic", centroid = "pam", 
              seed = 3247L, trace = TRUE,
              args = tsclust_args(dist = list(window.size = 20L)))

plot(pc)
head(ts)
head(CharTraj)

ts <- ts[unique(c(.ts_cols, "Index", "NDVI"))]
head(ts)



