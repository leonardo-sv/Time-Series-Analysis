-- Selection of the attributes (region, uf_acronym, centroid latitude, centroid longitude and centroid geometry) using the state Rondonia as reference.
SELECT nm_uf AS uf_name,
    nm_regiao AS region,
    sigla_uf AS uf_acronym,
    ST_Y(ST_CENTROID(geom)) AS latitude ,
    ST_X(ST_CENTROID(geom)) AS longitude,
    ST_CENTROID(geom) AS geom
FROM ibge.uf_2020 
WHERE sigla_uf='RO';

-- Create the table where the ring buffers are stored and the spatial indexing.
DROP TABLE IF EXISTS analysis.ring_buffer;
CREATE TABLE analysis.ring_buffer(
    gid serial PRIMARY KEY, 
    latitude_center float,
    longitude_center float,
    outer_radius integer,
    inner_radius integer,
    geom geometry(POLYGON, 100002)
);
CREATE INDEX idx_geom_ring_buffer ON analysis.ring_buffer USING GIST(geom); 

-- Remove older version of the centroid_ring_buffers function
DROP FUNCTION IF EXISTS centroid_ring_buffers(minimum integer, maximum integer, step integer, wkt_geom text, srid_geom integer);

-- Create the "plpgsql" function that returns concentric ring buffers considering the centroid of a geometry passed as argument. 
-- The function received a wkt geometry and it use the geometry centroid point to create concentric ring buffers.
CREATE FUNCTION centroid_ring_buffers(
    minimum integer,
    maximum integer,
    step integer,
    wkt_geom text,
    srid_geom integer    
) RETURNS integer AS $$
DECLARE
  quantity integer := 0;
BEGIN
 RAISE NOTICE 'Generating concentric ring buffers from % km to % km in % km steps at the %.', minimum, maximum, step, wkt_geom;
  FOR d IN minimum..maximum BY step LOOP
    INSERT INTO analysis.ring_buffer (latitude_center, longitude_center, outer_radius, inner_radius, geom) 
    (
     SELECT ST_Y(ST_CENTROID(ST_GeomFromText(wkt_geom,srid_geom))), ST_X(ST_CENTROID(ST_GeomFromText(wkt_geom,srid_geom))), d, d-step, ST_Transform  
     (
       ST_Difference
       (
         ST_Buffer(ST_Transform(ST_CENTROID(ST_GeomFromText(wkt_geom,srid_geom)), 100002), (d * 1000), 'quad_segs=90'), 
         ST_Buffer(ST_Transform(ST_CENTROID(ST_GeomFromText(wkt_geom,srid_geom)), 100002), ((d * 1000) - (step * 1000)), 'quad_segs=90')
       ), 100002
     )::geometry(Polygon, 100002)  
   );
    quantity := quantity + 1;
 END LOOP;
  
  RAISE NOTICE 'Generated % concentric ring buffers.', quantity;
  RETURN quantity;
END;
$$ LANGUAGE plpgsql;


-- Example of use the function centroid_ring_buffers(minimum, maximum, step, srid_geom, srid_rings, wkt_geom)
SELECT centroid_ring_buffers(10, 450, 5, ST_AsText(ST_CENTROID(geom))::text, 4674) FROM ibge.uf_2020 WHERE sigla_uf='RO';