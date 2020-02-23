DROP TABLE IF EXISTS methane;

CREATE TABLE methane (
    time_utc    DATETIME         NOT NULL,
    time        DECIMAL(12, 3)   NOT NULL,
    latitude    DECIMAL(15, 13)  NOT NULL,
    longitude   DECIMAL(15, 13)  NOT NULL,
    mmr         DECIMAL(15, 11)  NOT NULL,
    mmr_bc      DECIMAL(15, 11)  NOT NULL,
    qa_value    DECIMAL(2, 1)    NOT NULL,
    east_wind   DECIMAL(17, 14)          ,
    north_wind  DECIMAL(17, 14)          ,

    PRIMARY KEY (time, latitude, longitude)
);

INSERT INTO methane
SELECT
    time_utc,
    time,
    latitude,
    longitude,
    mmr,
    mmr_bc,
    qa_value,
    east_wind,
    north_wind
from methane_full;

