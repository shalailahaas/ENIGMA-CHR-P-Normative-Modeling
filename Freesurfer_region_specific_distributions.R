install.packages("ggjoy")
install.packages("ggplot2")
install.packages("ggridges")
install.packages("tidyr")
install.packages("tidyverse")
install.packages("data.table")
install.packages("ggh4x")
install.packages("sos")
install.packages("magrittr")
install.packages("dplyr")
install.packages("DescTools")


library(ggjoy)
library(ggplot2)
library(ggridges)
library(tidyr)
library(tidyverse)
library(data.table)
library(ggh4x)
library(sos)
library(magrittr)
library(dplyr)
library(DescTools)

# set working directory (INPUT OWN DIRECTORY TO WHERE YOUR CSV FILES ARE HERE)
setwd(r"(D:\Dropbox\PostDoc_MtSinai\My_Stuff\Teaching\Translational-neuro\Scripts\Test)")

### INDIVIDUAL REGIONS ------------------------------------------------------

#REPLACE PATH HERE TO THE PATH OF YOUR CSV FILE EITHER WITH RAW DATA OR Z-SCORE DATA. USE SAME COLUMN NAMES FOR FREESURFER AS IN EXCEL SHEET TEMPLATE
data<-read.csv(r"(D:\Dropbox\PostDoc_MtSinai\My_Stuff\Teaching\Translational-neuro\Scripts\Test\template_Freesurfer_distributions.csv)")

#CHANGE THE NAME OF YOUR GROUPS HERE, "Healthy" and "CHR" - SHOULD MATCH "Group" Variable in template sheet. 
data$Group <- factor(data$Group, labels = c("Healthy","CHR")) # originally, 0 = healthy, 1 = CHR (or other patient group)

#This section will generate distribution plots for your two groups based on the column Group in the template. It will generate one figure for Cortical Thickness and Surface Area measures, and another figure for subcortical volume. 
#No alterations need to be made here, with the exception of xlim depending on the range of your data and whether you are using z-scores or raw values. 
############################################################################
# create a few variables to help with data organization and aesthetics
group <- levels(data$Group)
colour <- c("#FDC171", "#D071B8") # use "#D071B8" for CHR, "#FDC171" for healthy
region <- colnames(select(data,(starts_with("L_") & ends_with("thickavg"))))
region <- gsub("L_|_thickavg","",region)
region <- c(region,"accumb", "amyg", "caud", "hippo", "pal", "put", "thal")

# create a new dataframe with only the variables you need for plotting
df <- data %>% select(Group,contains(region))

# pivot data so that regions are vertically concatenated
df <- pivot_longer(df,
                   cols = -c(Group),
                   names_to = "Region")
# create some new columns to indicate hemisphere and phenotype
df$pheno_RL <- "Right"
df$pheno_RL[df$Region %like% "L%"] <- "Left"
df$pheno_tav <- "SV"
df$pheno_tav[df$Region %like% "%thickavg"] <- "CT"
df$pheno_tav[df$Region %like% "%surfavg"] <- "SA"
# drop "L/R" and "thickavg/surfavg" from the region names (these are now specified in separate columns)
df$Region <- gsub("L|R|_|thickavg|surfavg","",df$Region)

# create some variables that will be used in ggplot labels
tav_labels <- c(
  "CT" = "Cortical Thickness",
  "SA" = "Surface Area",
  "SV" = "Subcortical Volume"
)
hem_labels <- c(
  "Left" = "Left",
  "Right" = "Right"
)
region_labels <- c(
  "bankssts" = "Banks Superior Temporal Sulcus",
  "caudalanteriorcingulate" = "Caudal Anterior Cingulate Cortex",
  "caudalmiddlefrontal" = "Caudal Middle Frontal Gyrus",
  "cuneus" = "Cuneus Cortex",
  "entorhinal" = "Entorhinal Cortex",
  "fusiform" = "Fusiform Gyrus",
  "inferiorparietal" = "Inferior Parietal Cortex",
  "inferiortemporal" = "Inferior Temporal Gyrus",
  "isthmuscingulate" = "Isthmus-Cingulate Cortex",
  "lateraloccipital" = "Lateral Occipital Cortex",
  "lateralorbitofrontal" = "Lateral Orbital Frontal Cortex",
  "lingual" = "Lingual Gyrus",
  "medialorbitofrontal" = "Medial Orbital Frontal Cortex",
  "middletemporal" = "Middle Temporal Gyrus",
  "parahippocampal" = "Parahippocampal Gyrus",
  "paracentral" = "Paracentral Lobule",
  "parsopercularis" = "Pars Opercularis",
  "parsorbitalis" = "Pars Orbitalis",
  "parstriangularis" = "Pars Triangularis",
  "pericalcarine" = "Pericalcarine Cortex",
  "postcentral" = "Postcentral Gyrus",
  "posteriorcingulate"  = "Posterior Cingulate Cortex",
  "precentral" = "Precentral Gyrus",
  "precuneus" = "Precuneus Cortex",
  "rostralanteriorcingulate" = "Rostral Anterior Cingulate Cortex",
  "rostralmiddlefrontal" = "Rostral Middle Frontal Gyrus",
  "superiorfrontal" = "Superior Frontal Gyrus",
  "superiorparietal" = "Superior Parietal Cortex",
  "superiortemporal" = "Superior Temporal Gyrus",
  "supramarginal" = "Supramarginal Gyrus",
  "frontalpole" = "Frontal Pole",
  "temporalpole" = "Temporal Pole",
  "transversetemporal" = "Transverse Temporal Cortex",
  "insula" = "Insula",
  "accumb" = "Accumbens Area",
  "amyg" = "Amygdala",
  "caud" = "Caudate",
  "hippo" = "Hippocampus",
  "pal" = "Pallidum",
  "put" = "Putamen",
  "thal" = "Thalamus"
)

df<-df %>% mutate(Region = region_labels[Region])

############################################################

#THIS ACTUALLY PLOTS THE DATA. FIRST FOR CT/SA. 
#YOU MAY NEED TO CHANGE LINE 147 "xlim" range based on the range of your data. 
#I set it to -4 to 4 for the zscore data, but for the raw data, you may need to change this 
#ALSO CHANGE THE OUTPUT DIRECTORY PATH in line 149 to LOCATION WHERE YOUR CSV FILES ARE. 
#ALSO RENAME OUTPUT FILES BASED ON WHETHER YOU USE ZSCORE OR RAW DATA
# Plot CT/SA, L/R, groups overlapping
ggplot(subset(df,pheno_tav!="SV"), aes(x = value, y = Region, fill = Group))+
  geom_joy(scale = 2, alpha = 0.7)+
  theme_joy(grid = FALSE)+
  scale_fill_manual(values = colour)+
  scale_y_discrete(limits=rev)+
  facet_nested(~ pheno_tav + pheno_RL,
               nest_line = element_line(),
               labeller = labeller(pheno_tav = tav_labels,
                                   pheno_RL = hem_labels))+
  labs(title = "Region-Specific Distributions of Z-Scores",
       fill='Group')+
  theme(axis.text = element_text(size = 7),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.line.x = element_line(colour = "black", linewidth = 0.5),
        strip.text = element_text(margin = margin(1,0,2,0)),
        strip.background = element_blank(),
        legend.margin = margin(0,0,0,0),
        plot.margin = margin(0,0,0,0),
        plot.background = element_rect(fill = "white"))+
  xlim(-4, 4)
ggsave(paste(c("Freesurfer_Distributions/cortregions_deviations_LR_group.png"),collapse = ""), device = "png", units="in", width=6.5, height=8, dpi=300)
#ggsave(paste(c("option7b/site_avdevs_LR_all.tiff"),collapse = ""), units="in", width=6.5, height=8, dpi=300, compression = 'lzw')

#THIS ACTUALLY PLOTS THE DATA. NOW FOR SV. 
#YOU MAY NEED TO CHANGE LINE 177 "xlim" range based on the range of your data (keep same as above for CT/SA) 
#I set it to -4 to 4 for the zscore data, but for the raw data, you may need to change this 
#ALSO CHANGE THE OUTPUT DIRECTORY PATH in line 178 to LOCATION WHERE YOUR CSV FILES ARE.
#ALSO RENAME OUTPUT FILES BASED ON WHETHER YOU USE ZSCORE OR RAW DATA
# Plot SV, L/R, groups overlapping
ggplot(subset(df,pheno_tav=="SV"), aes(x = value, y = Region, fill = Group))+
  geom_joy(scale = 2, alpha = 0.7)+
  theme_joy(grid = FALSE)+
  scale_fill_manual(values = colour)+
  scale_y_discrete(limits=rev)+
  facet_nested(~ pheno_tav + pheno_RL,
               nest_line = element_line(),
               labeller = labeller(pheno_tav = tav_labels,
                                   pheno_RL = hem_labels))+
  labs(title = "Region-Specific Distributions of Z-Scores",
       fill='Group')+
  theme(axis.text = element_text(size = 7),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.line.x = element_line(colour = "black", linewidth = 0.5),
        strip.text = element_text(margin = margin(1,0,2,0)),
        strip.background = element_blank(),
        legend.margin = margin(0,0,0,0),
        plot.margin = margin(0,0,0,0),
        plot.background = element_rect(fill = "white"))+
  xlim(-4, 4)
ggsave(paste(c("Freesurfer_Distributions/subcortregions_deviations_LR_group.png"),collapse = ""), device = "png", units="in", width=6.5, height=8, dpi=300)
#ggsave(paste(c("option7b/site_avdevs_LR_all.tiff"),collapse = ""), units="in", width=6.5, height=8, dpi=300, compression = 'lzw')

