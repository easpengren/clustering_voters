/* STEP 1: Preparing the points table
   a. Duplicate the points table into a table that you can edit so that you maintain original data
   b. Create a geometry column for the voters (points) table
   c. Populate the geometry column in the native projection
   d. Update the geometry column to use the same projection as the Census Files
   e. Add a spatial index to the table

 */
-- 1a
DROP TABLE IF EXISTS collective_nc_export_clustering;

-- 1b
CREATE TABLE collective_nc_export_clustering
AS (SELECT vannc.*
    FROM revised_nc_for_clustering AS vannc
    WHERE UPPER(vannc.state) = 'NC');


ALTER TABLE
    collective_nc_export_clustering
    ADD COLUMN IF NOT EXISTS geom GEOMETRY(Point, 4326);

-- 1c
UPDATE
    collective_nc_export_clustering
SET geom=ST_SetSRID(st_makepoint(cast(longitude as double precision), cast(latitude as double precision)), 4326) where 0=0;

-- 1d

ALTER TABLE collective_nc_export_clustering
    ALTER COLUMN geom TYPE GEOMETRY(Point, 4269) USING ST_Transform(geom, 4269);

-- 1e

DROP INDEX IF EXISTS
    collective_nc_export_clustering_sdx;
CREATE INDEX
    collective_nc_export_clustering_sdx
    ON
        collective_nc_export_clustering
            USING GIST (geom);


/* STEP 2: Filter out the voters who are in the wrong state, but keep them. This can be done for other geographies.
    a. Alter the voters table to add a column for whether the labeled state matches the spatially matched state.
    b. Update the state match column with a spatial query
    c. Reproject the points table into a projection that uses meters for distance (https://epsg.io/3968)

 */

--2a

ALTER TABLE collective_nc_export_clustering
    DROP COLUMN IF EXISTS hd_label_spatial_matches,
    DROP COLUMN IF EXISTS sd_label_spatial_matches,
    DROP COLUMN IF EXISTS cd_label_spatial_matches,
    DROP COLUMN IF EXISTS county_label_spatial_matches,
    DROP COLUMN IF EXISTS state_label_spatial_matches,
    ADD COLUMN hd_label_spatial_matches     BOOLEAN DEFAULT TRUE,
    ADD COLUMN sd_label_spatial_matches     BOOLEAN DEFAULT TRUE,
    ADD COLUMN cd_label_spatial_matches     BOOLEAN DEFAULT TRUE,
    ADD COLUMN county_label_spatial_matches BOOLEAN DEFAULT TRUE,
    ADD COLUMN state_label_spatial_matches  BOOLEAN DEFAULT TRUE;

-- 2b

-- SLDL

-- UPDATE collective_nc_export_clustering AS c
-- SET hd_label_spatial_matches = ST_CONTAINS(sldl.geom, c.geom)
-- FROM tl_2019_37_sldl AS sldl
-- WHERE sldl.statefp = '37'
--   AND LPAD(c.hd::TEXT, 3, '0') = sldl.sldlst;
--
-- SELECT
--        c.hd,
--        COUNT(c.*) AS total_voters,
--        COUNT(c.*) FILTER (WHERE c.hd_label_spatial_matches = 't') AS sldl_matches,
--        COUNT(c.*) FILTER (WHERE c.hd_label_spatial_matches = 'f') AS sldl_does_not_match,
--        COUNT(c.*) FILTER (WHERE c.hd_label_spatial_matches NOT IN ('t','f')) AS sldl_wtf
-- FROM
--      collective_nc_export_clustering AS c
-- GROUP BY c.hd
-- ORDER BY c.hd ASC;

-- SLDU

-- UPDATE collective_nc_export_clustering AS c
-- SET sd_label_spatial_matches = ST_CONTAINS(sldu.geom, c.geom)
-- FROM tl_2019_37_sldu AS sldu
-- WHERE sldu.statefp = '37'
--   AND LPAD(c.sd::TEXT, 3, '0') = sldu.sldust;
--
-- SELECT
--        c.sd,
--        COUNT(c.*) AS total_voters,
--        COUNT(c.*) FILTER (WHERE c.sd_label_spatial_matches = 't') AS sd_matches,
--        COUNT(c.*) FILTER (WHERE c.sd_label_spatial_matches = 'f') AS sd_does_not_match,
--        COUNT(c.*) FILTER (WHERE c.sd_label_spatial_matches NOT IN ('t','f')) AS sd_wtf
-- FROM
--      collective_nc_export_clustering AS c
-- GROUP BY c.sd
-- ORDER BY c.sd ASC;



-- CD

-- UPDATE collective_nc_export_clustering AS c
-- SET cd_label_spatial_matches = ST_CONTAINS(cd.geom, c.geom)
-- FROM tl_2019_us_cd116 AS cd
-- WHERE cd.statefp = '37'
--   AND LPAD(c.cd::TEXT, 2, '0') = (CASE WHEN cd.cd116fp = '00' THEN '01' ELSE cd.cd116fp END);
--
-- SELECT
--        c.cd,
--        COUNT(c.*) AS total_voters,
--        COUNT(c.*) FILTER (WHERE c.cd_label_spatial_matches = 't') AS cd_matches,
--        COUNT(c.*) FILTER (WHERE c.cd_label_spatial_matches = 'f') AS cd_does_not_match,
--        COUNT(c.*) FILTER (WHERE c.cd_label_spatial_matches NOT IN ('t','f')) AS cd_wtf
-- FROM
--      collective_nc_export_clustering AS c
-- GROUP BY c.cd
-- ORDER BY c.cd ASC;


-- COUNTY

-- UPDATE collective_nc_export_clustering AS c
-- SET county_label_spatial_matches = ST_CONTAINS(county.geom, c.geom)
-- FROM tl_2019_us_county AS county
-- WHERE county.statefp = '37'
--     AND c.countyname = county.name;
--
-- SELECT
--        c.countyname,
--        COUNT(c.*) AS total_voters,
--        COUNT(c.*) FILTER (WHERE c.county_label_spatial_matches = 't') AS countyname_matches,
--        COUNT(c.*) FILTER (WHERE c.county_label_spatial_matches = 'f') AS countyname_does_not_match,
--        COUNT(c.*) FILTER (WHERE c.county_label_spatial_matches NOT IN ('t','f')) AS countyname_wtf
-- FROM
--      collective_nc_export_clustering AS c
-- GROUP BY c.countyname
-- ORDER BY c.countyname ASC;
--

-- STATE
UPDATE collective_nc_export_clustering AS c
SET state_label_spatial_matches = 't'
FROM tl_2019_us_state AS state
WHERE TRIM(c.state) = TRIM(state.stusps)
AND ST_CONTAINS(state.geom, c.geom);

SELECT
       c.state,
       COUNT(c.*) AS total_voters,
       COUNT(c.*) FILTER (WHERE c.county_label_spatial_matches = 't') AS state_matches,
       COUNT(c.*) FILTER (WHERE c.county_label_spatial_matches = 'f') AS state_does_not_match,
       COUNT(c.*) FILTER (WHERE c.county_label_spatial_matches NOT IN ('t','f')) AS state_wtf
FROM
     collective_nc_export_clustering AS c
GROUP BY c.state;

ALTER TABLE collective_nc_export_clustering
    ALTER COLUMN geom TYPE GEOMETRY(Point, 32119) USING ST_Transform(geom, 32119);

DROP INDEX IF EXISTS
    collective_nc_export_clustering_sdx;
CREATE INDEX
    collective_nc_export_clustering_sdx
    ON
        collective_nc_export_clustering
            USING GIST (geom);


/* STEP 3. Create a clustering using DBSCAN

    a. Delete rows with values outside the state
    b. Create a targets column based on vote history
    c. Add a column for cluster_id
    d. Execute DBSCAN using CLUSTERWITHIN
    e. Create a geometry index for the table - tablename_sdx
 */

-- 3a

DELETE
FROM collective_nc_export_clustering AS cnec
WHERE cnec.state_label_spatial_matches = FALSE;

ALTER TABLE collective_nc_export_clustering
    DROP COLUMN IF EXISTS cluster_id,
    ADD COLUMN IF NOT EXISTS cluster_id NUMERIC DEFAULT NULL;

ALTER TABLE collective_nc_export_clustering
    DROP COLUMN IF EXISTS targeted_voter,
    ADD COLUMN IF NOT EXISTS targeted_voter BOOLEAN DEFAULT FALSE;

UPDATE collective_nc_export_clustering AS C
SET targeted_voter = 't'
WHERE c.general16 = '' OR c.general16 = 'f';

/*
 WHERE NOT (a, b) IS NULL AND NOT (c, d) IS NOT NULL

 this can set targets with more concise syntax at the expense of computation time

 https://dbfiddle.uk/?rdbms=postgres_12&fiddle=d606f479fedbdff29408f92b55eb44fd
 
 */

-- 3d

-- This kmeans cluster picks an arbitrary 100 cluster number for the state

UPDATE collective_nc_export_clustering AS cnec
SET cluster_id =
        CASE
            WHEN sq.cluster_id > -1
                THEN sq.cluster_id
            ELSE
                -1
            END

FROM (SELECT cnec."voter_file_vanid",
             ST_ClusterKMeans(cnec.geom, 100)
             OVER (ORDER BY cnec."voter_file_vanid") AS cluster_id
      FROM collective_nc_export_clustering AS cnec
      WHERE cnec.targeted_voter = 't'
     ) AS sq
WHERE cnec."voter_file_vanid" = sq."voter_file_vanid";

-- But statewide clustering is not very useful - we wind up with so many clusters that
-- we have to start partitioning by smaller geographies to make this useful

/* Step 4: Add clustering within smaller geographies (CD, SD, HD)
    a. Add necessary columns
    b. Execute the queries
 */

-- 4a

SELECT cnec.cluster_id,
       COUNT(cnec.*)
FROM collective_nc_export_clustering cnec
GROUP BY cnec.cluster_id;

ALTER TABLE collective_nc_export_clustering
    DROP COLUMN IF EXISTS cluster_id_within_cd,
    ADD COLUMN IF NOT EXISTS cluster_id_within_cd TEXT DEFAULT '-1';

ALTER TABLE collective_nc_export_clustering
    DROP COLUMN IF EXISTS cluster_id_within_sd,
    ADD COLUMN IF NOT EXISTS cluster_id_within_sd TEXT DEFAULT '-1';

ALTER TABLE collective_nc_export_clustering
    DROP COLUMN IF EXISTS cluster_id_within_hd,
    ADD COLUMN IF NOT EXISTS cluster_id_within_hd TEXT DEFAULT '-1';

-- 4b

-- kmeans within CD
-- calculate area of CDs
DROP TABLE IF EXISTS state_CD_density_lookup;


CREATE TABLE state_CD_density_lookup
 AS   select t1.cd, t2.area
    FROM (
             SELECT distinct(cd116fp) as cd
             FROM tl_2019_us_cd116
                where statefp = '37'
         ) t1
             left join (
        select cd116fp as cd, (st_area(st_transform(geom, 32119))/1000000) as area
        from tl_2019_us_cd116
                where statefp = '37'
    ) t2

    on t1.cd = t2.cd;

select * from state_CD_density_lookup;
--calculate density of targeted voters in CDs
ALTER TABLE state_CD_density_lookup
    DROP COLUMN IF EXISTS cd_count,
    ADD COLUMN IF NOT EXISTS cd_count numeric;
WITH new_values as
(select cd, count(cd) as cd_count
        from collective_nc_export_clustering
    where targeted_voter = 't'
         group by cd)
UPDATE state_CD_density_lookup as scdl
    set cd_count = nv.cd_count
    from new_values as nv
WHERE right(nv.cd,2) = right(scdl.cd,2);

select * from state_CD_density_lookup;

ALTER TABLE state_CD_density_lookup
    DROP COLUMN IF EXISTS cd_density,
    ADD COLUMN IF NOT EXISTS cd_density numeric;

update state_CD_density_lookup
    set cd_density = cd_count/area where 0=0;

select * from state_CD_density_lookup;
select min(cd_density), avg(cd_density) from state_CD_density_lookup;

ALTER TABLE collective_nc_export_clustering
        DROP COLUMN IF EXISTS cd_density,
    ADD COLUMN IF NOT EXISTS cd_density numeric;

WITH new_values as
(select cd, cd_density
        from state_cd_density_lookup
)
UPDATE collective_nc_export_clustering as cnec
    set cd_density = nv.cd_density
    from new_values as nv
WHERE right(nv.cd,2) = right(cnec.cd,2);

select cd_density from collective_nc_export_clustering;

UPDATE collective_nc_export_clustering AS cnec
SET cluster_id_within_cd = CASE
                               WHEN sq.cluster_id_within_cd IS NOT NULL
                                   THEN
                                       cnec.cd || '-' || sq.cluster_id_within_cd::TEXT
                               ELSE '-1'
    END
FROM (SELECT cnec."voter_file_vanid",
             ST_ClusterKMeans(cnec.geom,cnec.cd_density::integer)
             OVER (PARTITION BY cnec.cd ORDER BY cnec."voter_file_vanid") AS cluster_id_within_cd
      FROM collective_nc_export_clustering AS cnec
      WHERE cnec.targeted_voter = 't'
     ) AS sq
WHERE cnec."voter_file_vanid" = sq."voter_file_vanid";

select cluster_id_within_cd, count(cluster_id_within_cd)
    from collective_nc_export_clustering
    group by cluster_id_within_cd;

-- kmeans within SD

DROP TABLE IF EXISTS state_SD_density_lookup;


CREATE TABLE state_SD_density_lookup
 AS   select t1.sd, t2.area
    FROM (
             SELECT distinct(sldust) as sd
             FROM tl_2019_37_sldu

         ) t1
             left join (
        select sldust as sd, (st_area(st_transform(geom, 32119))/1000000) as area
        from tl_2019_37_sldu

    ) t2

    on t1.sd = t2.sd;

select * from state_SD_density_lookup;
--calculate density of targeted voters in HDs
ALTER TABLE state_SD_density_lookup
    DROP COLUMN IF EXISTS sd_count,
    ADD COLUMN IF NOT EXISTS sd_count numeric;
WITH new_values as
(select sd, count(sd) as sd_count
        from collective_nc_export_clustering
    where targeted_voter = 't'
         group by sd)
UPDATE state_SD_density_lookup as ssdl
    set sd_count = nv.sd_count
    from new_values as nv
WHERE right(nv.sd,2) = right(ssdl.sd,2);

select * from state_sd_density_lookup;

ALTER TABLE state_sd_density_lookup
    DROP COLUMN IF EXISTS sd_density,
    ADD COLUMN IF NOT EXISTS sd_density numeric;

update state_sd_density_lookup
    set sd_density = sd_count/area where 0=0;

select * from state_sd_density_lookup;
select min(cd_density), avg(cd_density) from state_CD_density_lookup;

ALTER TABLE collective_nc_export_clustering
        DROP COLUMN IF EXISTS sd_density,
    ADD COLUMN IF NOT EXISTS sd_density numeric;

WITH new_values as
(select sd, sd_density
        from state_sd_density_lookup
)
UPDATE collective_nc_export_clustering as cnec
    set sd_density = nv.sd_density
    from new_values as nv
WHERE right(nv.sd,2) = right(cnec.sd,2);

select sd_density from collective_nc_export_clustering;

UPDATE collective_nc_export_clustering AS cnec
SET cluster_id_within_sd = CASE
                               WHEN sq.cluster_id_within_sd IS NOT NULL
                                   THEN
                                       cnec.cd || '-' || sq.cluster_id_within_sd::TEXT
                               ELSE '-1'
    END
FROM (SELECT cnec."voter_file_vanid",
             ST_ClusterKMeans(cnec.geom,cnec.sd_density::integer)
             OVER (PARTITION BY cnec.sd ORDER BY cnec."voter_file_vanid") AS cluster_id_within_sd
      FROM collective_nc_export_clustering AS cnec
      WHERE cnec.targeted_voter = 't'
     ) AS sq
WHERE cnec."voter_file_vanid" = sq."voter_file_vanid";

select cluster_id_within_sd, count(cluster_id_within_sd)
    from collective_nc_export_clustering
    group by cluster_id_within_sd;


-- kmeans within HD

DROP TABLE IF EXISTS state_hd_density_lookup;


CREATE TABLE state_hd_density_lookup
 AS   select t1.hd, t2.area
    FROM (
             SELECT distinct(sldust) as hd
             FROM tl_2019_37_sldu

         ) t1
             left join (
        select sldlst as hd, (st_area(st_transform(geom, 32119))/1000000) as area
        from tl_2019_37_sldl

    ) t2

    on t1.hd = t2.hd;

select * from state_hd_density_lookup;

ALTER TABLE state_hd_density_lookup
    DROP COLUMN IF EXISTS hd_count,
    ADD COLUMN IF NOT EXISTS hd_count numeric;
WITH new_values as
(select hd, count(hd) as hd_count
        from collective_nc_export_clustering
    where targeted_voter = 't'
         group by hd)
UPDATE state_hd_density_lookup as shdl
    set hd_count = nv.hd_count
    from new_values as nv
WHERE right(nv.hd,2) = right(shdl.hd,2);

select * from state_hd_density_lookup;

ALTER TABLE state_hd_density_lookup
    DROP COLUMN IF EXISTS hd_density,
    ADD COLUMN IF NOT EXISTS hd_density numeric;

update state_hd_density_lookup
    set hd_density = hd_count/area where 0=0;

select hd_density from state_hd_density_lookup;

select * from state_hd_density_lookup;
select min(hd_density), avg(hd_density) from state_hd_density_lookup;

ALTER TABLE collective_nc_export_clustering
        DROP COLUMN IF EXISTS hd_density,
    ADD COLUMN IF NOT EXISTS hd_density numeric;

WITH new_values as
(select hd, hd_density
        from state_hd_density_lookup
)
UPDATE collective_nc_export_clustering as cnec
    set hd_density = nv.hd_density
    from new_values as nv
WHERE nv.hd = cnec.hd;



UPDATE collective_nc_export_clustering AS cnec
SET cluster_id_within_hd = CASE
                               WHEN sq.cluster_id_within_hd IS NOT NULL
                                   THEN
                                       cnec.cd || '-' || sq.cluster_id_within_hd::TEXT
                               ELSE '-1'
    END
FROM (SELECT cnec."voter_file_vanid",
             ST_ClusterKMeans(cnec.geom,cnec.hd_density::integer)
             OVER (PARTITION BY cnec.hd ORDER BY cnec."voter_file_vanid") AS cluster_id_within_hd
      FROM collective_nc_export_clustering AS cnec
      WHERE cnec.targeted_voter = 't'
     ) AS sq
WHERE cnec."voter_file_vanid" = sq."voter_file_vanid";

select cluster_id_within_hd, count(cluster_id_within_hd)
    from collective_nc_export_clustering
    group by cluster_id_within_hd;

-- kmeans by precinct Within CD


/* Even those smaller geographies aren't very useful, but precincts within them could be.
   a. Add columns for cluster id's within smaller geogrpahies, precincts
   b. Do a DBScan for each
 */

-- 5a

ALTER TABLE collective_nc_export_clustering
    DROP COLUMN IF EXISTS cluster_id_within_cd_and_precinct,
    ADD COLUMN IF NOT EXISTS cluster_id_within_cd_and_precinct TEXT DEFAULT '-1';

ALTER TABLE collective_nc_export_clustering
    DROP COLUMN IF EXISTS cluster_id_within_sd_and_precinct,
    ADD COLUMN IF NOT EXISTS cluster_id_within_sd_and_precinct TEXT DEFAULT '-1';

ALTER TABLE collective_nc_export_clustering
    DROP COLUMN IF EXISTS cluster_id_within_hd_and_precinct,
    ADD COLUMN IF NOT EXISTS cluster_id_within_hd_and_precinct TEXT DEFAULT '-1';


-- 5b

-- kmeans within CD and Precinct
--calculating area for precincts
DROP TABLE IF EXISTS state_precinctname_density_lookup;


CREATE TABLE state_precinctname_density_lookup
 AS   select t1.precinctname, t2.area
    FROM (
             SELECT distinct(precinctname) as precinctname
             FROM nc_precinctname_tabblock_union

         ) t1
             left join (
        select precinctname as precinctname, (st_area(st_transform(geom, 32119))) as area
        from nc_precinctname_tabblock_union

    ) t2

    on t1.precinctname = t2.precinctname;


ALTER TABLE state_precinctname_density_lookup
    DROP COLUMN IF EXISTS precinctname_count,
    ADD COLUMN IF NOT EXISTS precinctname_count numeric;
WITH new_values as
(select precinctname, count(precinctname) as precinctname_count
        from collective_nc_export_clustering
    /*where targeted_voter = 't'*/
         group by precinctname)
UPDATE state_precinctname_density_lookup as precinctnamel
    set precinctname_count = nv.precinctname_count
    from new_values as nv
WHERE nv.precinctname = precinctnamel.precinctname;

select count(*) from state_precinctname_density_lookup;

ALTER TABLE state_precinctname_density_lookup
    DROP COLUMN IF EXISTS precinctname_density,
    ADD COLUMN IF NOT EXISTS precinctname_density numeric;

update state_precinctname_density_lookup
    set precinctname_density = precinctname_count/area where 0=0;

select precinctname_density from state_precinctname_density_lookup;

select * from state_precinctname_density_lookup;
select min(precinctname_density), avg(precinctname_density) from state_precinctname_density_lookup;

ALTER TABLE collective_nc_export_clustering
        DROP COLUMN IF EXISTS precinctname_density,
    ADD COLUMN IF NOT EXISTS precinctname_density numeric;

WITH new_values as
(select precinctname, precinctname_density
        from state_precinctname_density_lookup
)
UPDATE collective_nc_export_clustering as cnec
    set precinctname_density = nv.precinctname_density
    from new_values as nv
WHERE nv.precinctname = cnec.precinctname;

ALTER TABLE collective_nc_export_clustering
        DROP COLUMN IF EXISTS precinctname_ntile,
    ADD COLUMN IF NOT EXISTS precinctname_ntile numeric;

DROP TABLE IF EXISTS precinct_ntile_lookup;

/*create table precinct_ntile_lookup as
select precinctname,ntile(10) over ( order by col.precinctname_density asc) as ntile
    from collective_nc_export_clustering col;*/

select precinctname, precinctname_density from state_precinctname_density_lookup;

WITH new_values as
(select precinctname,ntile(100) over ( order by col.precinctname_density asc) as ntile
    from state_precinctname_density_lookup col
)
UPDATE collective_nc_export_clustering as cnec
    set precinctname_ntile = nv.ntile
    from new_values as nv
WHERE nv.precinctname = cnec.precinctname;


select count(precinctname_ntile) from collective_nc_export_clustering;

select min(precinctname_density) from state_precinctname_density_lookup;


UPDATE collective_nc_export_clustering AS cnec
SET cluster_id_within_sd_and_precinct = CASE
                                            WHEN sq.cluster_id_within_sd_and_precinct IS NOT NULL
                                                THEN
                                                    cnec.sd || '-' || cnec.precinctname || '-' ||
                                                    sq.cluster_id_within_sd_and_precinct::TEXT
                                            ELSE '-1'
    END
FROM (SELECT cnec."voter_file_vanid",
             ST_ClusterWithin(cnec.geom, precinctname_density)
             OVER (PARTITION BY cnec.sd, cnec.precinctname ORDER BY cnec."voter_file_vanid") AS cluster_id_within_sd_and_precinct
      FROM collective_nc_export_clustering AS cnec
      WHERE cnec.targeted_voter = 't'
     ) AS sq
WHERE cnec."voter_file_vanid" = sq."voter_file_vanid";

select precinctname_density, collective_nc_export_clustering.cluster_id_within_cd_and_precinct from collective_nc_export_clustering;

-- DBSCAN within SD and Precinct

UPDATE collective_nc_export_clustering AS cnec
SET cluster_id_within_sd_and_precinct = CASE
                                            WHEN sq.cluster_id_within_sd_and_precinct IS NOT NULL
                                                THEN
                                                    cnec.sd || '-' || cnec.precinctname || '-' ||
                                                    sq.cluster_id_within_sd_and_precinct::TEXT
                                            ELSE '-1'
    END
FROM (SELECT cnec."voter_file_vanid",
             ST_ClusterKMeans(cnec.geom, cnec.precinctname_area::integer)
             OVER (PARTITION BY cnec.sd, cnec.precinctname ORDER BY cnec."voter_file_vanid") AS cluster_id_within_sd_and_precinct
      FROM collective_nc_export_clustering AS cnec
      WHERE cnec.targeted_voter = 't'
     ) AS sq
WHERE cnec."voter_file_vanid" = sq."voter_file_vanid";

select count(cluster_id_within_sd_and_precinct) from collective_nc_export_clustering
group by cluster_id_within_sd_and_precinct;

-- DBSCAN WITHIN HD and Precinct

UPDATE collective_nc_export_clustering AS cnec
SET cluster_id_within_hd_and_precinct = CASE
                                            WHEN sq.cluster_id_within_hd_and_precinct IS NOT NULL
                                                THEN
                                                    cnec.hd || '-' || cnec.precinctname || '-' ||
                                                    sq.cluster_id_within_hd_and_precinct::TEXT
                                            ELSE '-1'
    END
FROM (SELECT cnec."voter_file_vanid",
             ST_ClusterKMeans(cnec.geom, cnec.precinctname_area::integer)
             OVER (PARTITION BY cnec.hd, cnec.precinctname ORDER BY cnec."voter_file_vanid") AS cluster_id_within_hd_and_precinct
      FROM collective_nc_export_clustering AS cnec
      WHERE cnec.targeted_voter = 't'
     ) AS sq
WHERE cnec."voter_file_vanid" = sq."voter_file_vanid";



select count(cluster_id_within_hd_and_precinct) from collective_nc_export_clustering
group by cluster_id_within_hd_and_precinct;

/*
 Step 6: Create distinct tables for each dataset of clusters. This is really bad boundary estimation because
 convex_hull is not parsimonious. You will get something that holds all the points, but it will contain a lot of
 extra space. Put another way, if you use convex_hull to get the boundaries of [A,B,C,D], A will contain parts of
 [B..D], etc.

 This is here for people who can't get boundary estimation from me.

    a. CD & Precinct
    b. SD & Precinct
    c. HD & Precinct
 */

-- -- 6a
--
-- DROP TABLE IF EXISTS jamaa_export_clustering_cd_and_precinct;
--
-- CREATE TABLE jamaa_export_clustering_cd_and_precinct AS (
--     SELECT ROW_NUMBER() OVER ()                as gid,
--            jvm.cluster_id_within_cd_and_precinct,
--            st_convexhull(st_collect(jvm.geom)) AS geom
--
--     FROM jamaa_va_export_clustering AS jvm
--     WHERE jvm.cluster_id_within_cd_and_precinct != '-1'
--     GROUP BY jvm.cluster_id_within_cd_and_precinct
-- );
--
-- -- 6b
--
-- DROP TABLE IF EXISTS jamaa_export_clustering_sd_and_precinct;
--
-- CREATE TABLE jamaa_export_clustering_sd_and_precinct AS (
--     SELECT ROW_NUMBER() OVER ()                as gid,
--            jvm.cluster_id_within_sd_and_precinct,
--            st_convexhull(st_collect(jvm.geom)) AS geom
--
--     FROM jamaa_va_export_clustering AS jvm
--     WHERE jvm.cluster_id_within_sd_and_precinct != '-1'
--     GROUP BY jvm.cluster_id_within_sd_and_precinct
-- );
--
--
-- -- 6c
--
-- DROP TABLE IF EXISTS jamaa_export_clustering_hd_and_precinct;
--
-- CREATE TABLE jamaa_export_clustering_hd_and_precinct AS (
--     SELECT ROW_NUMBER() OVER ()                as gid,
--            jvm.cluster_id_within_hd_and_precinct,
--            st_convexhull(st_collect(jvm.geom)) AS geom
--
--     FROM jamaa_va_export_clustering AS jvm
--     WHERE jvm.cluster_id_within_hd_and_precinct != '-1'
--     GROUP BY jvm.cluster_id_within_hd_and_precinct
-- );





