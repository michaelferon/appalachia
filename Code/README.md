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
This file loads `dataFull.Rdata`. This file produces an estimate, for each quadrant, of the proportion of days of above-average CH4 levels. The heatmap is `~/Figures/proportions/propmat.pdf`. It performs this same process month-by-month as well. These heatmaps are in `~/Figures/proportions/monthly-pdf`. Finally, the results of the algorithm for producing this estimated proportion are then saved to `propList.Rdata` for further analysis.

maps.R
======
Produces the Haynesville-Bossier map and the satellite map.

maskInit.R
==========
This file loads the `.hdf` files with the MOD44W v006 MODIS/Terra Land Water Mask 250m SIN Grid data, downloaded from [__USGS/EarthData__](https://lpdaac.usgs.gov/products/mod44wv006/). It converts these `HDF4` files, which are difficult to work with in `R`, into `netCDF` files. The `.nc` files do take substantially more disk-space, unfortunately; but, they are much easier to work with. This file then filters the data to include only those points that lie within our region of study. It then saves the data to `mask.Rdata`.

mask.R
======
This file loads `mask.Rdata` for processing. It applies the mask to the `X` data matrix and then, like `heatmaps.R`, produces an estimate, for each quadrant, of the proportion of days of above-average CH4 levels. It does this monthly as well. Images are saved in the `~/Figures/proportions/mask-adjusted` folder, and results are saved to `propListAdj.Rdata`.



