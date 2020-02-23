This is a breakdown of what each file in the CODE directory does.


initialize.R -- DO NOT RUN. Processes the .nc files, producing ~/Data/data-full/varDictFull.Rdata

process.R -- Processes ~/Data/data-full/varDictFull.Rdata. It creates the X matrix and the dateTimes object, saving them to ~/Data/data-full/dataFull.Rdata. It is not necessary to run this, but is quick if you do run it.

functions.R -- Most of the heavy-lifting is composed here. This file is sourced by analyze.R and heatmaps.R.

analyze.R -- Loads ~/Data/dataFull.Rdata. This is where analysis is performed, with some plots and graphs being produced. In particular, analyze.R does the following:
1) Divides data up into quadrants, based on RESO.
2) Aggregates data by month, week, and day.
3) Computes monthly, weekly, and daily means for each quadrant.
4) Produces weekly and daily time-series plots by quadrant.
5) Produces 3-day plots using both fields::quilt.plot and ggmap.
6) This file used to compute an estimate for each quadrant of the proportion of days of above-average CH4 levels. This has been moved to heatmaps.R.

heatmaps.R -- This file loads ~/Data/data-full/X100.Rdata, which is similar to dataFull.Rdata, but the X matrix has been divided into 10,000 quadrants (a time-consuming processs). This file produces an estimate, for each quadrant, of the proportion of days of above-average CH4 levels. The heatmap is ~/Figures/proportions/propmat.pdf. It performs this same process month-by-month as well. These heatmaps are in ~/Figures/proportions/monthly-pdf. Finally, the results of the algorithm for producing this estimated proportion are then saved to ~/Data/data-full/propList.Rdata for further analysis.

maps.R -- Produces the Haynesville-Bossier map and the satellite map.



