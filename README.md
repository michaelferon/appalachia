This is a breakdown of what each folder in the `code` directory contains and what each file does. Note that the symbol `~` refers to the `Dropbox/Haynesville` directory, or the "home" directory for this project.

Important note to reader: to function, the following scripts require data that is too large to have been stored in our Dropbox. These data are stored locally, and these scripts SHOULD NOT BE RUN.
1. `init/initalize.R`
2. `init/initNew.R`
3. `init/maskInit.R`

Moreover, the shell code in the `latex` directory is meant to be run locally as well, and should not be run.

The scripts described here require many different packages to run, which the user will need to install at their own discretion. All the code in this Dropbox has been confirmed to run on `macOS Catalina v.10.15.4` running `R v.3.6.2 (2019-12-12) "Dark and Stormy Night"`, but has not been verified on other operating systems or with other versions of `R`.

If you are to run the code contained in this Dropbox, do the following.
1. __DO NOT RUN__ the few scripts listed above.
2. Make sure `~/data/data-full/varDictFull.Rdata`, `~data/data-full/varDictNew.Rdata`, and `~/data/mask/mask.Rdata` exist in their prescribed locations.
3. __This is the most important thing__. Make sure your working directory in `R` is set to one of the following:
  * `~/code/init/`
  * `~/code/analysis/`
  * `~/code/maps/`


Once you've done this, run the code in this order:
1. `~/code/init/process.R`
2. `~/code/analysis/analyze.R`
3. `~/code/maps/heatmaps.R`
4. `~/code/maps/maps.R`
5. `~/code/maps/mask.R`
6. `~/code/maps/maskHeatmaps.R`
7. `~/code/analysis/arima.R`
8. `~/code/analysis/cows.R`
9. `~/code/analysis/wind.R`
10. `~/code/analysis/fourier.R`
11. `~/code/analysis/regression.R`


We now describe in detail the contents of this Dropbox.



folder: ~/code
=====

*functions.R*
============
Most of the heavy-lifting is composed here. This file is sourced by most of the other R scripts.





folder: ~/code/init
===================

*initialize.R*
=============
__DO NOT RUN__. Processes the `.nc` data files from 2018-05-01 through 2020-01-31, producing `varDictFull.Rdata`, which holds the data in immediate pre-processing form.

*initNew.R*
==========
__DO NOT RUN__. Processes the `.nc` data files from 2020-02-01 through 2020-03-31, producing `varDictNew.Rdata`.

*maskInit.R*
===========
__DO NOT RUN__. This file loads the `.hdf` files with the MOD44W v006 MODIS/Terra Land Water Mask 250m SIN Grid data, downloaded
from [__USGS/EarthData__](https://lpdaac.usgs.gov/products/mod44wv006/). It converts these `HDF4` files, which are difficult to work with in `R`, into `netCDF` files. The `.nc` files do take substantially more disk-space, unfortunately; but, they are much easier to work with. This file then filters the data to include only those points that lie within our region of study. It then saves the data to `mask.Rdata`.

*process.R*
==========
This file reads in `varDictFull.Rdata` and `varDictNew.Rdata` and processes the data, producing a dataframe object in a format ready for statistical analysis. This object is stored to `dataFull.Rdata`.

*convert.R*
==========
This file reads in `dataFull.Rdata` and produces `dataFull.csv`, a `.csv`-file version of the dataframe object stored in `dataFull.Rdata`.





folder: ~/code/analysis
=======================

*analyze.R*
==========
This file reads in `dataFull.Rdata`. Some preliminary analysis is performed, including quadrant-time-series analysis and some basis statistical inference. Figure 6 and the statistics in Table 1 are produced here.

*arima.R*
========
Loads `dataFullMask.Rdata`. Builds ARIMA models for predicting methane concentrations in specific subregions of the total region of analysis, such as the Haynesville Shale and the wetlands of Louisiana, and plots the results. In particular, Figures 27a, 27c, 27d, 28, and 29 are produced here.

*cows.R*
=======
Takes a shapefile on Texas counties, and takes cow population data in each country to create a heatmap of cattle population in Texas. This is Figure 26.

*fourier.R*
==========
Loads `dataFullMask.Rdata`. Performs spectral analysis on our region at large, and on three subregions of interest. Produces Figures 18 through 25.

*regression.R*
=============
Loads `dataFullMask.Rdata` and creates a regression model by doing the following:
1. Loads in shale gas production data in the Haynesville region.
2. Loads in all high qa methane mixing ratio measurements.
3. Separates all data as being inside/outside the Haynesville region.
4. Aggregates by day, inside and out, and then averages to collect one measurement per day inside and outside the region.
5. Subtracts inside methane level-outside methene level as a background correction.
6. Aggregate again by month, and average to get one background corrected measurement.
7. Fits a linear model predicting methane mixing ratio from shale gas production.

*wind.R*
=======
Loads `dataFullMask.Rdata` and implements a basic shift of methane data using each data point's wind vector.





folder: ~/code/maps
===================

*maps.R*
========
Produces several maps of our region: Figures 3, 4, and 7.

*heatmaps.R*
============
Conducts proportion analysis using `dataFull.Rdata` (unmasked data). Produces Figure 8 and the statistics in Table 2.

*maskHeatmaps.R*
================
Conducts proportion analysis using `dataFullMask.Rdata` with high-qa values. Produces Figure 12b. This file also performs some exploratory `qa` analysis, producing Figure 11.

*mask.R*
========
Processes `mask.Rdata`, the land-water-mask data, and produces Figure 9. This file also performs proportion analysis on the masked data, producing Figure 10b.





folder: ~/code/config
=====================
This folder contains one file, `config.yml`, which contains a Google API key necessary for the functionality of most of the aforementioned scripts. Please don't store this key anywhere or post it publicly.




folder: ~/code/latex
====================
__DO NOT RUN__. This folder contains a shell script, `tex.sh`, which is only meant to be run locally. It combines all the 3-day quilt plots into a latex document, compiles it, and outputs it to `~/code/latex/plots.pdf`. The other files in this folder are miscellaneous ones necessary for latex compilation.


