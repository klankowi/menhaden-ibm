# menhaden-ibm

This repo contains data files, R scripts, and some results for individual-based models (IBMs) of juvenile menhaden movement in shallow estuarine tributaries. 

There are two IBMs: the first is a habitat suitability index model (also known as a bioclimate model) in which the individuals sense and respond to the local abiotic environment but NOT to each other. The second is a model based on common schooling rules (see Aoki 1981 and Huth & Wissel 1992) in which the individuals sense and respond to each other but NOT to the local abiotic environment.

The HSI model relies on output from a Semi-implicit Cross-scale Hydroscience Integrated System Model - Integrated Compartment Model (SCHISM-ICM) graciously provided by the Testa lab at CBL. The data are not publicly available. However, code to work with .ncdf files common to these kinds of hydroscience models and code to convert raw data into a habitat suitability index are demonstrated and the outputs shown.

Output from both IBMs is compared to empirical data collected in 2016-2017 in the Patuxent River and its associated creeks. The Patuxent is a western-shore tributary of Chesapeake Bay known to serve as a nursery ground for a large population of YOY and juvenile menhaden.
