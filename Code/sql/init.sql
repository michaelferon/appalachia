DROP TABLE IF EXISTS methane_full;

CREATE TABLE methane_full (
    time_utc                DATETIME         NOT NULL,
    time                    DECIMAL(12, 3)   NOT NULL,
    latitude                DECIMAL(15, 13)  NOT NULL,
    longitude               DECIMAL(15, 13)  NOT NULL,
    mmr                     DECIMAL(15, 11)  NOT NULL,
    mmr_bc                  DECIMAL(15, 11)  NOT NULL,
    qa_value                DECIMAL(2, 1)    NOT NULL,
    east_wind               DECIMAL(17, 14)          ,
    north_wind              DECIMAL(17, 14)          ,
    solar_zenith            DECIMAL(15, 13)  NOT NULL,
    viewing_zenith          DECIMAL(15, 13)  NOT NULL,
    water_tot_column        DECIMAL(15, 11)  NOT NULL,
    surf_albedo_swir        DECIMAL(17, 16)  NOT NULL,
    surf_albedo_nir         DECIMAL(16, 15)  NOT NULL,
    aerosol_opt_thick_swir  DECIMAL(17, 16)  NOT NULL,
    surf_alt                DECIMAL(17, 14)  NOT NULL,
    surf_alt_prec           DECIMAL(16, 14)  NOT NULL,
    surf_class              INTEGER          NOT NULL,
    surf_pressure           DECIMAL(13, 7)   NOT NULL,
    app_scene_pressure      DECIMAL(13, 7)   NOT NULL,
    m_weak_twoband          DECIMAL(16, 15)          ,
    m_strong_twoband        DECIMAL(16, 15)          ,

    PRIMARY KEY (time, latitude, longitude)
);


LOAD DATA LOCAL INFILE
    '/Volumes/SSD/Appalachia/Code/sql/methane.csv'
    INTO TABLE methane_full
    FIELDS TERMINATED BY ','
    LINES TERMINATED BY '\n'
    IGNORE 1 ROWS;




