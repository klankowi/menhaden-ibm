# menhaden-ibm

This repo contains data files, R scripts, and some results for individual-based models (IBMs) of juvenile menhaden movement in shallow estuarine tributaries. 

There are two IBMs: the first is a bioclimate model (also known as a habitat suitability model) in which the individuals sense and respond to the local abiotic environment but NOT to each other. The second is a model based on common schooling rules (see Aoki 1981 and Huth & Wissel 1992) in which the individuals sense and respond to each other but NOT to the local abiotic environment.

The bioclimate model relies on data that are not publicly available. However, code to work with ncdf files and convert raw data into a habitat suitability index is provided.

Output from both IBMs is compared to empirical data collected in 2016-2017 in the Patuxent River and its associated creeks. The Patuxent is a western-shore tributary of Chesapeake Bay known to serve as a nursery ground for a large population of YOY and juvenile menhaden.
