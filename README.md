# ENIGMA-CHR-P-Normative-Modeling
This repository provides the code used to perform the individual-level analyses described in Haas et al. (2023): [here](https://jamanetwork.com/journals/jamapsychiatry/fullarticle/2810624)

## Extreme Deviations
In file: `computation_additional_metrics_extreme_deviations.m`

* Compute average deviation scores (lines [44](https://github.com/shalailahaas/ENIGMA-CHR-P-Normative-Modeling/blob/9602bd1fface13d56ae61dbd14a8f4131d5d2d75/computation_additional_metrics_extreme_deviations.m#L44)-[141](https://github.com/shalailahaas/ENIGMA-CHR-P-Normative-Modeling/blob/9602bd1fface13d56ae61dbd14a8f4131d5d2d75/computation_additional_metrics_extreme_deviations.m#L141))
* For each brain region, compute percentage of patients at + or - extremes based on defined SD threshold cutoff (lines [143](https://github.com/shalailahaas/ENIGMA-CHR-P-Normative-Modeling/blob/9602bd1fface13d56ae61dbd14a8f4131d5d2d75/computation_additional_metrics_extreme_deviations.m#L143)-[163](https://github.com/shalailahaas/ENIGMA-CHR-P-Normative-Modeling/blob/9602bd1fface13d56ae61dbd14a8f4131d5d2d75/computation_additional_metrics_extreme_deviations.m#L163))
* For each brain region, compute percentage of healthy individuals at + or - extremes based on defined SD threshold cutoff (lines [164](https://github.com/shalailahaas/ENIGMA-CHR-P-Normative-Modeling/blob/9602bd1fface13d56ae61dbd14a8f4131d5d2d75/computation_additional_metrics_extreme_deviations.m#L164)-[185](https://github.com/shalailahaas/ENIGMA-CHR-P-Normative-Modeling/blob/9602bd1fface13d56ae61dbd14a8f4131d5d2d75/computation_additional_metrics_extreme_deviations.m#L185))
* For each individual participant, compute the percentage of regions with extreme + or - deviations based on predefined SD threshold cutoff (lines [186](https://github.com/shalailahaas/ENIGMA-CHR-P-Normative-Modeling/blob/9602bd1fface13d56ae61dbd14a8f4131d5d2d75/computation_additional_metrics_extreme_deviations.m#L186)-[232](https://github.com/shalailahaas/ENIGMA-CHR-P-Normative-Modeling/blob/9602bd1fface13d56ae61dbd14a8f4131d5d2d75/computation_additional_metrics_extreme_deviations.m#L232))

 
## Two-proportion z-test
In file: `2_proportion_z-test.R`

__Credit:__ Portions of this script were written by Dr. Maria Jalbrzikowski. 

Script to compute 2-proportion z-test to examine whether proportions of extreme deviations significantly differ between 2 groups. 

* Subcortical volume (lines [21](https://github.com/shalailahaas/ENIGMA-CHR-P-Normative-Modeling/blob/9716768e6666ec356163e2119622364fb4264af8/2_proportion_z-test.R#L21)-[151](https://github.com/shalailahaas/ENIGMA-CHR-P-Normative-Modeling/blob/9716768e6666ec356163e2119622364fb4264af8/2_proportion_z-test.R#L151))
* Cortical thickness (lines [152](https://github.com/shalailahaas/ENIGMA-CHR-P-Normative-Modeling/blob/9716768e6666ec356163e2119622364fb4264af8/2_proportion_z-test.R#L152)-[450](https://github.com/shalailahaas/ENIGMA-CHR-P-Normative-Modeling/blob/9716768e6666ec356163e2119622364fb4264af8/2_proportion_z-test.R#L450))
* Cortical surface area (lines [451](https://github.com/shalailahaas/ENIGMA-CHR-P-Normative-Modeling/blob/9716768e6666ec356163e2119622364fb4264af8/2_proportion_z-test.R#L451)-[748](https://github.com/shalailahaas/ENIGMA-CHR-P-Normative-Modeling/blob/9716768e6666ec356163e2119622364fb4264af8/2_proportion_z-test.R#L748))
* Compute 2-proportion z-test across all regions using FDR correction based on all 150 measures (lines [749](https://github.com/shalailahaas/ENIGMA-CHR-P-Normative-Modeling/blob/9716768e6666ec356163e2119622364fb4264af8/2_proportion_z-test.R#L749)-[789](https://github.com/shalailahaas/ENIGMA-CHR-P-Normative-Modeling/blob/9716768e6666ec356163e2119622364fb4264af8/2_proportion_z-test.R#L789)) 


## Distribution Plots

In file: `Freesurfer_region_specific_distributions.R`

__Credit:__ Portions of this script were written by Dr. Nicole Sanford.

Script to generate distribution plots for cortical thickness, surface area and subcortical volume measures  for two groups based on z-score outputs from CentileBrain. 

Accompanying tutorial slides to use the script are available [here](https://docs.google.com/presentation/d/19dMnLIi21FdUXMm6betug6VuHMxS5YID/edit?usp=sharing&ouid=105633577450428342048&rtpof=true&sd=true).


