from datetime import datetime
import geopandas as gpd
import json
import matplotlib
import numpy as np
import pandas as pd
import pyproj
import psycopg2
import rasterio
import stac
from typing import List, Dict
from matplotlib import pyplot as plt
from osgeo import gdal
from rasterio import features
from shapely import wkt
from matplotlib import pyplot
from rasterio.mask import mask


# Handle the Vectorial data

# Define the projection used in the BDC grids
crs_bdc = pyproj.CRS("+proj=aea +lat_0=-12 +lon_0=-54 +lat_1=-2 +lat_2=-22 +x_0=5000000 +y_0=10000000 +ellps=GRS80 +units=m +no_defs")

#Return a connection of geodb database
def conn_db(json_pass):
    with open(json_pass) as json_file:
    my_pass = json.load(json_file)

    conn = psycopg2.connect(
        user=my_pass['USER'],
        password=my_pass['PASS'],
        database=my_pass['DB'],
        host=my_pass['HOST'],
        port=int(my_pass['PORT'])
    )
    
    return conn

# Get a specific cell grid considering bdc projection (small, medium or large). This return a geopandas dataframe.
def get_cell_grid_bdc(long_min: float, lat_min: float, long_max: float, lat_max: float, crs: pyproj.CRS,
                      grid: str, conn: psycopg2.connect) -> gpd.GeoDataFrame:
    """ Create a GeoDataFrame with the same length of a bdc grid with intersects the coordinates
                    Args:
                        long_min (gpd.GeoDataFrame): Minimum longitude of a bound box with intersects BDC grid

                        lat_min (str): Minimum latitude of a bound box with intersects BDC grid

                        long_max (str): Maximum longitude of a bound box with intersects BDC grid

                        lat_max (pyproj.Proj): Maximum latitude of a bound box with intersects BDC grid

                        grid (str): BDC grid size

                        crs (pyproj.Proj): Projection assumed by GeoDataFrame

                        conn (psycopg2.connect): Connection psycopg into a PRODES database

                    Returns:
                        gpd.GeoDataFrame: All BDC grid polygons with intersects the coordinates
                    """
    sql_params = [grid, long_min, lat_min, long_max, lat_max]
    sql = "SELECT id, tile, geom FROM bdc.{} " \
          "WHERE ST_intersects(ST_transform(" \
          "ST_MakeEnvelope({}, {}, {}, {},4674),100001), geom);".format(*sql_params)
    return gpd.read_postgis(sql=sql, crs=crs, con=conn)


# Use a gpd roi to extract the classes of PRODES over the region of interest
def roi_prodes(roi: gpd.GeoDataFrame, crs: pyproj.CRS, year: int, labels: tuple, srid: int, 
               conn: psycopg2.connect) -> gpd.GeoDataFrame:
    """ Create a GeoDataFrame with specific crs from a ROI of PRODES given a database connection, year and srid
                Args:
                    roi (gpd.GeoDataFrame): GeoDataFrame with a ROI area

                    crs (pyproj.Proj): Projection assumed by GeoDataFrame

                    year (int): Year of current PRODES

                    labels (tuple): Labels of polygon PRODES

                    srid (str): Spatial reference identifier

                    conn (psycopg2.connect): Connection psycopg into a PRODES database
                Returns:
                    gpd.GeoDataFrame: All PRODES polygons associated with the ROI
                """
    roi_wkt_string = wkt.dumps(roi['geom'][0])
        
    class_table = {
        'Desmatamento'  : 'incremento_anual',
        'Floresta'      : 'floresta_anual',
        'Higrografia'   : 'hidrografia',
        'Nao_Floresta'  : 'nao_floresta',
        'Nuvem'         : 'nuvem',
        'Acumulado_2007': 'desmatamento_acumulado'
    }
    
    if type(labels) != tuple:
        labels = [labels]
    else:
        labels = list(labels)  
    
    gpd_list = []
    for label in labels:
        sql =  "SELECT prodes.gid, prodes.id, prodes.origin_id, prodes.state, prodes.path_row, \
                       prodes.main_class, prodes.class_name, prodes.def_cloud, prodes.julian_day, \
                       prodes.image_date, prodes.year, prodes.area_km, prodes.scene_id, prodes.source, \
                       prodes.satellite, prodes.sensor, \
                       ST_Transform( \
                         ST_Intersection( \
                           ST_Buffer( prodes.geom, 0), \
                             ST_Transform( \
                               ST_SetSRID( \
                                 ST_GeometryFromText(\'{}\'),{}),4674)), {}) \
                                   AS geom \
                FROM prodes.{} AS prodes \
                WHERE prodes.year = {} AND \
                      ST_Intersects(prodes.geom, \
                        ST_Transform( \
                          ST_SetSRID( \
                            ST_GeometryFromText(\'{}\'),{}),4674)) \
                ORDER BY prodes.image_date;".format(*[roi_wkt_string,
                                                      srid, 
                                                      srid, 
                                                      class_table[label],
                                                      year,
                                                      roi_wkt_string,
                                                      srid])
        gpd_list.append(gpd.read_postgis(sql=sql, con=conn, crs=crs))
    return pd.concat(gpd_list, ignore_index=True)

# Use a SQL QUERY to obtain a vectorial data from geodatabase 
def get_geodf(sql_query, connection, crs_proj):
    return gpd.read_postgis( sql=sql_query, con=connection, crs=crs_proj)


def getFeatures(gdf):
    """Function to parse features from GeoDataFrame in such a manner that rasterio wants them"""
    import json
    return [json.loads(gdf.to_json())['features'][0]['geometry']]


# Use the parameters url, token and collection_name to obtain the BDC collection data
def get_bdc_collection(url, token, collection_name):
    bdc_stac_service = stac.STAC(url, access_token=token)
    collection = bdc_stac_service.collection(collection_name)

    return collection

#Parse the items from collection to obtain the raster data
def get_items(collection, **kwargs):
    with open('../data/json/my_pass.json') as json_file:
        my_pass = json.load(json_file)
    
    access_token = my_pass["STAC_TOKEN"]
    
    items = collection.get_items(**kwargs)
    
    for i in range(0, len(items.features)):
        item = items.features[i]
        
        for j in item["assets"].keys():
            items.features[i]["assets"][j]["href"] += f"?access_token={access_token}"
    return items


# Use a gpd to obtain a raster data considering the classes of vectorial data
def rasterize_gdf(geom_gdf: gpd.GeoDataFrame, column_classes: str, path_raster_ref: str,
                  path_raster_output: str) -> np.array:
    """Create a raster data from GeoDataFrame based in column classes
            Args:
                geom_gdf (gpd.GeoDataFrame): GeoDataFrame with labeled polygon

                column_classes (str): GeoDataFrame column with value to create a raster

                path_raster_ref (str): Image path used as a reference

                path_raster_output (str): Image path used to save a raster based in geom_gdf
            Returns:
                burned (np.array): numpy array with data saved in raster
            """
    raster_ref = rasterio.open(path_raster_ref)
    meta = raster_ref.meta.copy()
    meta.update(compress='lzw')

    with rasterio.open(path_raster_output, 'w+', **meta) as out:
        out_arr = out.read(1)

        shapes = ((geom, value) for geom, value in zip(geom_gdf.geometry, geom_gdf[column_classes]))

        burned = features.rasterize(shapes=shapes, fill=0, out=out_arr, transform=out.transform)
        
        out.write_band(1, burned)
        
# This function define a rule considering a 3x3 grid to apply in the sampling approach. 
# Return TRUE when all pixels in 3x3 neighborhood have the same class.
# Return FALSE if at least one pixel class is different to others.
def rule_3x3(xs: List[int], ys: List[int], image_data: np.array):
    rows, cols = image_data.shape
    mask = []
    for x, y in zip(xs, ys):
        if (x not in [0, 1, rows - 1, rows - 2]) & (y not in [0, 1, cols - 1, cols - 2]):
            flag = np.all(image_data[x - 1: x + 2, y - 1: y + 2] == image_data[x, y])
        else:
            flag = False
        mask.append(flag)
    return mask

# This function define a rule considering a 5x5 grid to apply in the sampling approach. 
# Return TRUE when all pixels in 5x5 neighborhood have the same class.
# Return FALSE if at least one pixel class is different to others.
def rule_5x5(xs: List[int], ys: List[int], image_data: np.array):
    rows, cols = image_data.shape
    mask = []
    for x, y in zip(xs, ys):
        if (x not in [0, 1, rows - 1, rows - 2]) & (y not in [0, 1, cols - 1, cols - 2]):
            flag =  np.all(image_data[x-2:x+3, y-2:y+3] == image_data[x, y])
        else:
            flag = False
        mask.append(flag)
    return mask

# Use the raster map to sort coordinates of samples.
def coord_samples(raster_path: str, label: int, method="all", rule_test=False, n_samples=0):
    map_raster = rasterio.open(raster_path)
    map_array = map_raster.read(1)
    xy_samples = np.where(map_array == label)
    idx = None
    if method == "random":
        np.random.seed(42)
        idx = np.random.choice(xy_samples[0].size, n_samples, replace=False)
    elif method == "all":
        idx = np.arange(0, len(xy_samples[0]), 1)

    if rule_test:
        mask = rule_5x5(xy_samples[0][idx], xy_samples[1][idx], map_array)
        idx = idx[mask]

    rows = xy_samples[0][idx]
    cols = xy_samples[1][idx]

    xs, ys = rasterio.transform.xy(map_raster.transform, rows, cols, offset='center')#4326

    return xs, ys

# Use a list of coordinates to extract time series from data cube.
def ts_extract(stac_features: List[dict], bands: List[str], #joblib
               coords: np.ndarray, label: str, label_2: str) -> pd.DataFrame:
    """Extract Timeseries from STAC Features
    Args:
        stac_feature (List[Dict]): Features extracted from STAC-API

        band_name (str): Band to extract value

        coords (List[list]): List of positions (In native CRS) where data is from

        use_matrix_colrow (bool): Use matrix col and row position to get timseries
    Returns:
        pd.Series: Values extracted
        :param coords:
        :param label:
        :param cube:
        :param bands:
        :param stac_features:
    """
    time_index = []
    time_series_per_band = {}
    print(label)
    for band in bands:
        time_series_per_band[band] = []
        for stac_feature in reversed(stac_features):
            _ds = rasterio.open(stac_feature['assets'][band]['href'])

            time_series_per_band[band].append(np.concatenate(list(_ds.sample(coords)), axis=0))
            time_index.append((stac_feature['properties']['datetime']))
        # time_series_per_band[band] = np.stack(time_series_per_band[band], axis=1)
    dates = pd.DatetimeIndex(time_index).unique().strftime('%Y-%m-%d').tolist()
    print(dates)
    _df = pd.DataFrame()
    _df['Coord1'] = [item[0] for item in coords]
    _df['Coord2'] = [item[1] for item in coords]
    _df['start_date'] = dates[0]
    _df['end_date'] = dates[-1]
    _df['label'] = label
    _df['label_raster'] = label_2
    for band in bands:
        for i in range(len(time_series_per_band[band])):
            df_name = band + "_t" + str(i)
            _df[df_name] = time_series_per_band[band][i]
    return _df

# Use a raster reference to apply the sampling approach rule and obtain the samples
def sample_ts(raster_ref, items, labels, bands, n_samples, rule, output):

    raster_ref = '../data/raster/LC8_30_PRODES_2017_2018_porto_velho.tif'
    labels = {175:"Desmatamento", 176:"Floresta"}

    frames = []
    for id_label in labels.keys():
        xs, ys = coord_samples(raster_ref, id_label, "random", rule, n_samples)
        xy = np.stack((xs, ys), axis=1)
        samples = ts_extract(items["features"], bands, xy, labels[id_label], str(id_label))
        frames.append(samples)
    total_samples = pd.concat(frames, ignore_index=True)
    total_samples.to_csv(output, index=False)
    return total_samples

def systematic_sample_areas(roi, distance):
    pass