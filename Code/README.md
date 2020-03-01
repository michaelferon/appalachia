This is a breakdown of what each file in the `CODE` directory does.

initialize.R
============
__DO NOT RUN__. Processes the `.nc` files, producing `varDictFull.Rdata`.

process.R
=========
Processes `varDictFull.Rdata`. It creates the `X` matrix and the `dateTimes` object, saving them to `dataFull.Rdata`. It is not necessary to run this, but is quick if you do run it.

functions.R
===========
Most of the heavy-lifting is composed here. This file is sourced by `analyze.R`, `heatmaps.R`, `maps.R`, `maskInit.R`, and `mask.R`.

analyze.R
=========
Loads `dataFull.Rdata`. This is where analysis is performed, with some plots and graphs being produced. In particular, `analyze.R` does the following:
1. Divides data up into quadrants, based on `RESO`.
2. Aggregates data by month, week, and day.
3. Computes monthly, weekly, and daily means for each quadrant.
4. Produces weekly and daily time-series plots by quadrant.
5. Produces 3-day plots using both `fields::quilt.plot` and `ggmap`.
6. This file used to compute an estimate for each quadrant of the proportion of days of above-average CH4 levels. This has been moved to `heatmaps.R`.

heatmaps.R
==========
This script loads `dataFull.Rdata`. It then produces an estimate, for each quadrant, of the proportion of days of above-average CH4 levels. The heatmap is `~/Figures/proportions/propmat.pdf`. It performs this same process month-by-month as well. These heatmaps are in `~/Figures/proportions/monthly-pdf`. Finally, the results of the algorithm for producing this estimated proportion are then saved to `propList.Rdata` for further analysis.

maps.R
======
Produces the Haynesville-Bossier map and the satellite map.

maskInit.R
==========
Loads the `netCDF` files with the MOD44W v006 MODIS/Terra Land Water Mask 250m SIN Grid data, downloaded from [__USGS/EarthData__](https://lpdaac.usgs.gov/products/mod44wv006/). These files were originally formatted in `HDF4`, a difficult format to work with in `R`. Prior to processing, make sure to convert from `.hdf` to `.nc`. This script then filters the data to include only those points that lie within our region of study. It then saves the data to `mask.Rdata`.

mask.R
======
Loads `mask.Rdata` for processing. It applies the land water mask to the `X` data matrix and then, like `heatmaps.R`, produces an estimate, for each quadrant, of the proportion of days of above-average CH4 levels. It does this monthly as well. Plots are saved in the `~/Figures/proportions/mask-adjusted` folder, and results are saved to `propListAdj.Rdata`.



