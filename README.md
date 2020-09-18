# C_fire_invasives repository

This repository contains the code for: Nagy, R.C., Fusco, E., Bradley, B. Finn, J. Mahood, A. Allen, J. Balch, J.K., 2020. "A synthesis of the effects of cheatgrass invasion on U.S. Great Basin carbon storage". Journal of Applied Ecology.

The data can be found here: Nagy, R.C., Fusco, E., Bradley, B. Finn, J. Mahood, A. Allen, J. Balch, J.K., 2020. Data from "A 
synthesis of the effects of cheatgrass invasion on U.S. Great Basin carbon storage". Dryad Digital Repository, https://doi.org/10.5061/dryad.4mw6m9082. 

The following files are used to clean, compile, and analyze the data and prepare the tables and figures in the manuscript:

1_data_import_cleaning.R : compiling all individual data sources into one master dataset
1b_mean_BD_calcs.R : calculating mean soil bulk densities (to use for studies that were missing this info) to convert soil %C into content
2_data_prep_for_metaanalysis.R : adding unique study ids for different combininations of article, carbon pool, vegetation, and site
3_add_fire_data.R : adding fire data from MTBS and BAECV to the master dataset
3b_point_extract_from_raw_baecv.R : extracts the last year burned prior to field sampling for sites with more than one fire
4_data_summary.R : creates summary tables for carbon storage in pool-vegetation combinations; creates Table 1, Table S1, Figure 2, and Figure S1
5a_prep_for_meta_analysis.R : final preparation of the dataframe for meta-analysis
5b_meta_analysis_keep.R : performs the meta-analysis; creates Table 3 and Table S5
6_lmm_tables.Rmd : performs the linear mixed modeling; creates Figure 3 and Table 2
6b_ANOVAs : performs the ANOVAs and non-parametric Kruskal Wallis tests for pool-vegetation combinations

