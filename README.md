# ENIGMA-CHR-P-Normative-Modeling
This repository provides the code used to perform the individual-level analyses described in Haas et al. (2023): [here](https://jamanetwork.com/journals/jamapsychiatry/fullarticle/2810624)

##Extreme Deviations
computation\_additional\_metrics\_extreme\_deviations.m

* Compute average deviation scores (lines 44-141)
* For each brain region, compute percentage of patients at + or - extremes based on defined SD threshold cutoff (lines 143-163)
* For each brain region, compute percentage of healthy individuals at + or - extremes based on defined SD threshold cutoff (lines 164-185)
* For each individual participant, compute the percentage of regions with extreme + or - deviations based on predefined SD threshold cutoff (186-232)

 
##Two-proportion z-test
2\_proportion\_z-test.R

__Credit:__ Portions of this script were developed and written by Dr. Maria Jalbrzikowski. 

Script to compute 2-proportion z-test to examine whether proportions of extreme deviations significantly differ between 2 groups. 

* 



##Distribution Plots

Freesurfer\_region\_specific\_distributions.R

Script to generate distribution plots for cortical thickness, surface area and subcortical volume measures  for two groups based on z-score outputs from CentileBrain. 

Accompanying tutorial slides to use the script are available: [here]
(https://docs.google.com/presentation/d/19dMnLIi21FdUXMm6betug6VuHMxS5YID/edit?usp=sharing&ouid=105633577450428342048&rtpof=true&sd=true)


