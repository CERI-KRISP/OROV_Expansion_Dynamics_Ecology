# OROV_Expansion_Dynamics_Ecology

Dynamics and ecology of a multi-stage expansion of Oropouche virus in Brazil.
https://doi.org/10.1101/2024.10.29.24316328

Description of the data and file structure
This data and code repository is currently provided to facilitate the peer-review process for a submitted manuscript. Materials may change upon final publication.

Results related to Figure 1 and Figure 2 were generated from Bayesian phytogeographic reconstructions in BEAST. BEAST input XML, as well as result files (trees and logs) are available in the “BEASTfiles” folder. A sample of 100 trees are extracted per segment, and available in the folders started with “Tree_extractions”.
Environmental data is available in the folder called “Geospatial”, and in “Environmental_rasters” inside the “Phylogeography Analyses” folder. All results are in ”Phylogeography Analyses/All_seraphim_analyses”. Shapefiles are found in multiple places in the repo for ease of use in various scripts.
Analysis code and intermediate files related to the first part of the results and Figure 1, including phylogeography map, dispersal statistics and E-test results are available in the folder “Phylogeography Analyses”.
For the second part of the results and Figure 2, environmental values at branch locations are first extracted using the script “phylogeospreadvalues.R” and saved in the folder “SpreadValues” and in “Environmental_values” inside the “Phylogeography Analyses” folder.
Visualizations for Figure 2 are done with the script “Figure2.R”, and then superimposed with E stat results separately (all available in “OROV_all_E_tests_FINAL.xlsx”, which is an aggregate of what is available in “Phylogeography Analyses/All_seraphim_analyses”).
The ecological niche model is performed using the script “OROV_ENM.R”, by specifying the correct dataset as described in the text. All occurrence data for this part of the analysis can be found in “OROV_occurrence_data”. Resulting ENM estimate maps are found at “Geospatial/OROV_SuitabilityMaps.tiff” and “Geospatial/preExpansion_map_half2023.tiff”
Additional scripts are provided to plot some additional figures “Additional_Figures.R”. Epidemiological data used in this study is saved at “OROPOUCHE_cases.xlsx”
