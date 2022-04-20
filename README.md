# menhaden-ibm

This repo contains data files, R scripts, and some results for individual-based models (IBMs) of juvenile menhaden movement in shallow estuarine tributaries. 

There are two IBMs: the first is a habitat suitability index model (also known as a bioclimate model) in which the individuals sense and respond to the local abiotic environment but NOT to each other. The second is a model based on common schooling rules (see  [Aoki 1981](https://www.jstage.jst.go.jp/article/suisan1932/48/8/48_8_1081/_article), [Reynolds 1987](https://dl.acm.org/doi/10.1145/37402.37406), and [Huth & Wissel 1992](https://www.sciencedirect.com/science/article/abs/pii/S0022519305806812)) in which the individuals sense and respond to each other but NOT to the local abiotic environment.

The HSI model relies on output from a Semi-implicit Cross-scale Hydroscience Integrated System Model - Integrated Compartment Model (SCHISM-ICM) graciously provided by the Testa lab at CBL. The data are not publicly available. However, code to work with .ncdf files common to these kinds of hydroscience models and code to convert raw data into a habitat suitability index are demonstrated and the outputs shown.

Output from both IBMs is compared to empirical data collected in 2016-2017 in the Patuxent River and its associated creeks. The Patuxent is a western-shore tributary of Chesapeake Bay known to serve as a nursery ground for a large population of YOY and juvenile menhaden.

RMarkdown files should be viewed in this order to follow the development of the models:
1. work_with_ncdf.RMD
2. hsi_modeling.RMD
3. hsi_correlation_check.RMD
4. EnvSensing.RMD
5. SchoolMove.RMD
