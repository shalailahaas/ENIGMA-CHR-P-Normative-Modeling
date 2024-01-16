install.packages("dbplyr")
install.packages("FactoMineR")
install.packages("factoextra")
install.packages("corrplot")
install.packages("magrittr")
install.packages("dplyr")

library(dbplyr)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%

setwd("D:/Dropbox/PostDoc_MtSinai/My_Stuff/Teaching/Translational-neuro/Scripts/") #Set your working directory here. 
setwd("Add/patient/To/Where/You/Want/Files/Saved") #Set your working directory here. 

#Your template csv file should be saved in this folder. 

df<-read.csv(file="template_z-scores.csv")
colnames(df)
sub_reg<-c("Laccumb","Lamyg","Lcaud","Lhippo","Lpal" ,"Lput","Lthal",
           "Raccumb","Ramyg","Rcaud","Rhippo","Rpal","Rput","Rthal")       

df<-df %>% mutate(Laccumb_cat=cut(Laccumb, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(Raccumb_cat=cut(Raccumb, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(Lamyg_cat=cut(Lamyg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(Ramyg_cat=cut(Ramyg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(Lcaud_cat=cut(Lcaud, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(Rcaud_cat=cut(Rcaud, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(Lhippo_cat=cut(Lhippo, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(Rhippo_cat=cut(Rhippo, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(Lpal_cat=cut(Lpal, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(Rpal_cat=cut(Rpal, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(Lput_cat=cut(Lput, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(Rput_cat=cut(Rput, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(Lthal_cat=cut(Lthal, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(Rthal_cat=cut(Rthal, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))

df$supranormal_subcortical<-ifelse(df$Laccumb_cat == "supranormal" | df$Raccumb_cat =="supranormal"| df$Lamyg_cat =="supranormal"| 
                                     df$Ramyg_cat =="supranormal"| df$Lcaud_cat =="supranormal"| df$Rcaud_cat =="supranormal"|
                                     df$Lhippo_cat =="supranormal"| df$Rhippo_cat =="supranormal"|
                                     df$Lpal_cat =="supranormal"| df$Rpal_cat =="supranormal"|
                                     df$Lpal_cat =="supranormal"| df$Rpal_cat =="supranormal"|
                                     df$Lput_cat =="supranormal"| df$Rput_cat =="supranormal"|
                                     df$Lthal_cat =="supranormal"| df$Rthal_cat =="supranormal",
                                   "yes", "no")

df$infranormal_subcortical<-ifelse(df$Laccumb_cat == "infranormal" | df$Raccumb_cat =="infranormal"| df$Lamyg_cat =="infranormal"| 
                                     df$Ramyg_cat =="infranormal"| df$Lcaud_cat =="infranormal"| df$Rcaud_cat =="infranormal"|
                                     df$Lhippo_cat =="infranormal"| df$Rhippo_cat =="infranormal"|
                                     df$Lpal_cat =="infranormal"| df$Rpal_cat =="infranormal"|
                                     df$Lpal_cat =="infranormal"| df$Rpal_cat =="infranormal"|
                                     df$Lput_cat =="infranormal"| df$Rput_cat =="infranormal"|
                                     df$Lthal_cat =="infranormal"| df$Rthal_cat =="infranormal",
                                   "yes", "no")

#proportion participants with supranormal deviation in any region
#healthy
healthy<-df[df$Group==0,]
table1<-table(healthy$supranormal_subcortical)
prop.table(table1)
#patient
patient<-df[df$Group==1,]
table2<-table(patient$supranormal_subcortical)
prop.table(table2)

#proportion participants with infranormal deviation in any region
#healthy
table1<-table(healthy$infranormal_subcortical)
prop.table(table1)
#patient
table2<-table(patient$infranormal_subcortical)
prop.table(table2)

#Is there a significant differences between number of healthy vs. patient with at least one SV region with extreme supranormal deviation
chisq.test(df$supranormal_subcortical,df$Group)
#Is there a significant differences between number of healthy vs. patient with at least one SV region with extreme infranormal deviation
chisq.test(df$infranormal_subcortical,df$Group)

#For each region, % of healthy and patient with infranormal or supranormal deviations
colnames(healthy)
nvar = c("Laccumb_cat","Raccumb_cat","Lamyg_cat","Ramyg_cat","Lcaud_cat","Rcaud_cat",
         "Lhippo_cat","Rhippo_cat","Lpal_cat","Rpal_cat","Lput_cat","Rput_cat","Lthal_cat","Rthal_cat")

resmat_prop_SV<-matrix(0,length(nvar),4)

iter=0
for ( i in nvar) {
  iter=iter+1
  tmp_c<-((sum(healthy[,i]=="infranormal"))/sum(df$Group==0))*100
  resmat_prop_SV[iter,1]<-tmp_c
  tmp_c<-((sum(patient[,i]=="infranormal"))/sum(df$Group==1))*100
  resmat_prop_SV[iter,2]<-tmp_c
  tmp_c<-((sum(healthy[,i]=="supranormal"))/sum(df$Group==0))*100
  resmat_prop_SV[iter,3]<-tmp_c
  tmp_c<-((sum(patient[,i]=="supranormal"))/sum(df$Group==1))*100
  resmat_prop_SV[iter,4]<-tmp_c
}

colnames(resmat_prop_SV)<-c("Infra_healthy","Infra_patient", "Supr_healthy", "Supra_patient")
rownames(resmat_prop_SV)<-colnames(df[,nvar])
write.table(resmat_prop_SV, file = "proportions_SV.csv", sep = ",")

########################################################################
#Two proportion z-test
nvar = c("Laccumb_cat","Raccumb_cat","Lamyg_cat","Ramyg_cat","Lcaud_cat","Rcaud_cat",
         "Lhippo_cat","Rhippo_cat","Lpal_cat","Rpal_cat","Lput_cat","Rput_cat","Lthal_cat","Rthal_cat")

#infranormal
resmat_sv_infranormal<-matrix(0,length(nvar),4)
iter=0
for ( i in nvar) {
  iter = iter+1
  res<-prop.test(x = c(sum(df[df$Group==0,i] == 'infranormal'),sum(df[df$Group==1,i] == 'infranormal')), n = c(sum(df$Group==0),sum(df$Group==1)))
  resmat_sv_infranormal[iter,1:2]<-res[["estimate"]]
  resmat_sv_infranormal[iter,3]<-res[["statistic"]]
  resmat_sv_infranormal[iter,4]<-res[["p.value"]]
}

resmat_p_infranormal<-p.adjust(resmat_sv_infranormal[,4],"fdr")
resmat_sv_infranormal<-cbind(resmat_sv_infranormal,resmat_p_infranormal)
colnames(resmat_sv_infranormal)<-c("prop-healthy","prop-patient","X-squared","p-value","p-FDR")
rownames(resmat_sv_infranormal) = colnames(df[,nvar])
write.table(resmat_sv_infranormal, file = "two-prop_z-test_infranormal_SV.csv", sep = ",")

#supranormal
resmat_sv_supranormal<-matrix(0,length(nvar),4)
iter=0
for ( i in nvar) {
  iter = iter+1
  res<-prop.test(x = c(sum(df[df$Group==0,i] == 'supranormal'),sum(df[df$Group==1,i] == 'supranormal')), n = c(sum(df$Group==0),sum(df$Group==1)))
  resmat_sv_supranormal[iter,1:2]<-res[["estimate"]]
  resmat_sv_supranormal[iter,3]<-res[["statistic"]]
  resmat_sv_supranormal[iter,4]<-res[["p.value"]]
}

resmat_p_supranormal<-p.adjust(resmat_sv_supranormal[,4],"fdr")
resmat_sv_supranormal<-cbind(resmat_sv_supranormal,resmat_p_supranormal)
colnames(resmat_sv_supranormal)<-c("prop-healthy","prop-patient","X-squared","p-value","p-FDR")
rownames(resmat_sv_supranormal) = colnames(df[,nvar])
write.table(resmat_sv_supranormal, file = "two-prop_z-test_supranormal_SV.csv", sep = ",")

#2-proportion z-test for differences between groups with any subcortical region infra or supranormal
res_infranormal_sv<-prop.test(x = c(sum(df[df$Group==0,"infranormal_subcortical"] == 'yes'),sum(df[df$Group==1,"infranormal_subcortical"] == 'yes')), n = c(sum(df$Group==0),sum(df$Group==1)))
res_infranormal_sv
res_supranormal_sv<-prop.test(x = c(sum(df[df$Group==0,"supranormal_subcortical"] == 'yes'),sum(df[df$Group==1,"supranormal_subcortical"] == 'yes')), n = c(sum(df$Group==0),sum(df$Group==1)))
res_supranormal_sv

#############################################
#CT

colnames(df)

df<-df %>% mutate(L_bankssts_thickavg_cat=cut(L_bankssts_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_bankssts_thickavg_cat=cut(R_bankssts_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_caudalanteriorcingulate_thickavg_cat=cut(L_caudalanteriorcingulate_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_caudalanteriorcingulate_thickavg_cat=cut(R_caudalanteriorcingulate_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_caudalmiddlefrontal_thickavg_cat=cut(L_caudalmiddlefrontal_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_caudalmiddlefrontal_thickavg_cat=cut(R_caudalmiddlefrontal_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_cuneus_thickavg_cat=cut(L_cuneus_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_cuneus_thickavg_cat=cut(R_cuneus_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_entorhinal_thickavg_cat=cut(L_entorhinal_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_entorhinal_thickavg_cat=cut(R_entorhinal_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_fusiform_thickavg_cat=cut(L_fusiform_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_fusiform_thickavg_cat=cut(R_fusiform_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_inferiorparietal_thickavg_cat=cut(L_inferiorparietal_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_inferiorparietal_thickavg_cat=cut(R_inferiorparietal_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_inferiortemporal_thickavg_cat=cut(L_inferiortemporal_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_inferiortemporal_thickavg_cat=cut(R_inferiortemporal_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_isthmuscingulate_thickavg_cat=cut(L_isthmuscingulate_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_isthmuscingulate_thickavg_cat=cut(R_isthmuscingulate_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_lateraloccipital_thickavg_cat=cut(L_lateraloccipital_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_lateraloccipital_thickavg_cat=cut(R_lateraloccipital_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_lateralorbitofrontal_thickavg_cat=cut(L_lateralorbitofrontal_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_lateralorbitofrontal_thickavg_cat=cut(R_lateralorbitofrontal_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_lingual_thickavg_cat=cut(L_lingual_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_lingual_thickavg_cat=cut(R_lingual_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_medialorbitofrontal_thickavg_cat=cut(L_medialorbitofrontal_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_medialorbitofrontal_thickavg_cat=cut(R_medialorbitofrontal_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_middletemporal_thickavg_cat=cut(L_middletemporal_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_middletemporal_thickavg_cat=cut(R_middletemporal_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_parahippocampal_thickavg_cat=cut(L_parahippocampal_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_parahippocampal_thickavg_cat=cut(R_parahippocampal_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_paracentral_thickavg_cat=cut(L_paracentral_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_paracentral_thickavg_cat=cut(R_paracentral_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_parsopercularis_thickavg_cat=cut(L_parsopercularis_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_parsopercularis_thickavg_cat=cut(R_parsopercularis_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_parsorbitalis_thickavg_cat=cut(L_parsorbitalis_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_parsorbitalis_thickavg_cat=cut(R_parsorbitalis_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_parstriangularis_thickavg_cat=cut(L_parstriangularis_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_parstriangularis_thickavg_cat=cut(R_parstriangularis_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_pericalcarine_thickavg_cat=cut(L_pericalcarine_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_pericalcarine_thickavg_cat=cut(R_pericalcarine_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_postcentral_thickavg_cat=cut(L_postcentral_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_postcentral_thickavg_cat=cut(R_postcentral_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_posteriorcingulate_thickavg_cat=cut(L_posteriorcingulate_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_posteriorcingulate_thickavg_cat=cut(R_posteriorcingulate_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_precentral_thickavg_cat=cut(L_precentral_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_precentral_thickavg_cat=cut(R_precentral_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_precuneus_thickavg_cat=cut(L_precuneus_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_precuneus_thickavg_cat=cut(R_precuneus_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_rostralanteriorcingulate_thickavg_cat=cut(L_rostralanteriorcingulate_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_rostralanteriorcingulate_thickavg_cat=cut(R_rostralanteriorcingulate_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_rostralmiddlefrontal_thickavg_cat=cut(L_rostralmiddlefrontal_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_rostralmiddlefrontal_thickavg_cat=cut(R_rostralmiddlefrontal_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_superiorfrontal_thickavg_cat=cut(L_superiorfrontal_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_superiorfrontal_thickavg_cat=cut(R_superiorfrontal_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_superiorparietal_thickavg_cat=cut(L_superiorparietal_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_superiorparietal_thickavg_cat=cut(R_superiorparietal_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_superiortemporal_thickavg_cat=cut(L_superiortemporal_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_superiortemporal_thickavg_cat=cut(R_superiortemporal_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_supramarginal_thickavg_cat=cut(L_supramarginal_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_supramarginal_thickavg_cat=cut(R_supramarginal_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_frontalpole_thickavg_cat=cut(L_frontalpole_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_frontalpole_thickavg_cat=cut(R_frontalpole_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_temporalpole_thickavg_cat=cut(L_temporalpole_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_temporalpole_thickavg_cat=cut(R_temporalpole_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_transversetemporal_thickavg_cat=cut(L_transversetemporal_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_transversetemporal_thickavg_cat=cut(R_transversetemporal_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_insula_thickavg_cat=cut(L_insula_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_insula_thickavg_cat=cut(R_insula_thickavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))



df$supranormal_CT<-ifelse(df$L_bankssts_thickavg_cat == "supranormal" | 
                            df$R_bankssts_thickavg_cat == "supranormal" | 
                            df$L_caudalanteriorcingulate_thickavg_cat == "supranormal" | 
                            df$R_caudalanteriorcingulate_thickavg_cat == "supranormal" | 
                            df$L_caudalmiddlefrontal_thickavg_cat == "supranormal" | 
                            df$R_caudalmiddlefrontal_thickavg_cat == "supranormal" | 
                            df$L_cuneus_thickavg_cat == "supranormal" | 
                            df$R_cuneus_thickavg_cat == "supranormal" | 
                            df$L_entorhinal_thickavg_cat == "supranormal" | 
                            df$R_entorhinal_thickavg_cat == "supranormal" | 
                            df$L_fusiform_thickavg_cat == "supranormal" | 
                            df$R_fusiform_thickavg_cat == "supranormal" | 
                            df$L_inferiorparietal_thickavg_cat == "supranormal" | 
                            df$R_inferiorparietal_thickavg_cat == "supranormal" | 
                            df$L_inferiortemporal_thickavg_cat == "supranormal" | 
                            df$R_inferiortemporal_thickavg_cat == "supranormal" | 
                            df$L_isthmuscingulate_thickavg_cat == "supranormal" | 
                            df$R_isthmuscingulate_thickavg_cat== "supranormal" | 
                            df$L_lateraloccipital_thickavg_cat== "supranormal" | 
                            df$R_lateraloccipital_thickavg_cat== "supranormal" | 
                            df$L_lateralorbitofrontal_thickavg_cat== "supranormal" | 
                            df$R_lateralorbitofrontal_thickavg_cat== "supranormal" | 
                            df$L_lingual_thickavg_cat== "supranormal" | 
                            df$R_lingual_thickavg_cat== "supranormal" | 
                            df$L_medialorbitofrontal_thickavg_cat== "supranormal" | 
                            df$R_medialorbitofrontal_thickavg_cat== "supranormal" | 
                            df$L_middletemporal_thickavg_cat == "supranormal" | 
                            df$R_middletemporal_thickavg_cat == "supranormal" | 
                            df$L_parahippocampal_thickavg_cat== "supranormal" | 
                            df$R_parahippocampal_thickavg_cat== "supranormal" | 
                            df$L_paracentral_thickavg_cat== "supranormal" | 
                            df$R_paracentral_thickavg_cat== "supranormal" | 
                            df$L_parsopercularis_thickavg_cat== "supranormal" | 
                            df$R_parsopercularis_thickavg_cat== "supranormal" | 
                            df$L_parsorbitalis_thickavg_cat== "supranormal" | 
                            df$R_parsorbitalis_thickavg_cat== "supranormal" | 
                            df$L_parstriangularis_thickavg_cat== "supranormal" | 
                            df$R_parstriangularis_thickavg_cat== "supranormal" | 
                            df$L_pericalcarine_thickavg_cat== "supranormal" | 
                            df$R_pericalcarine_thickavg_cat== "supranormal" | 
                            df$L_postcentral_thickavg_cat== "supranormal" | 
                            df$R_postcentral_thickavg_cat== "supranormal" | 
                            df$L_posteriorcingulate_thickavg_cat == "supranormal" | 
                            df$R_posteriorcingulate_thickavg_cat == "supranormal" | 
                            df$L_precentral_thickavg_cat == "supranormal" | 
                            df$R_precentral_thickavg_cat == "supranormal" | 
                            df$L_precuneus_thickavg_cat== "supranormal" | 
                            df$R_precuneus_thickavg_cat== "supranormal" | 
                            df$L_rostralanteriorcingulate_thickavg_cat== "supranormal" | 
                            df$R_rostralanteriorcingulate_thickavg_cat== "supranormal" | 
                            df$L_rostralmiddlefrontal_thickavg_cat== "supranormal" | 
                            df$R_rostralmiddlefrontal_thickavg_cat== "supranormal" | 
                            df$L_superiorfrontal_thickavg_cat== "supranormal" | 
                            df$R_superiorfrontal_thickavg_cat== "supranormal" | 
                            df$L_superiorparietal_thickavg_cat== "supranormal" | 
                            df$R_superiorparietal_thickavg_cat== "supranormal" | 
                            df$L_superiortemporal_thickavg_cat== "supranormal" | 
                            df$R_superiortemporal_thickavg_cat== "supranormal" | 
                            df$L_supramarginal_thickavg_cat== "supranormal" | 
                            df$R_supramarginal_thickavg_cat== "supranormal" | 
                            df$L_frontalpole_thickavg_cat== "supranormal" | 
                            df$R_frontalpole_thickavg_cat== "supranormal" | 
                            df$L_temporalpole_thickavg_cat== "supranormal" | 
                            df$R_temporalpole_thickavg_cat== "supranormal" | 
                            df$L_transversetemporal_thickavg_cat == "supranormal" | 
                            df$R_transversetemporal_thickavg_cat == "supranormal" | 
                            df$L_insula_thickavg_cat == "supranormal" | 
                            df$R_insula_thickavg_cat == "supranormal",
                          "yes", "no")





df$infranormal_CT<-ifelse(df$L_bankssts_thickavg_cat == "infranormal" | 
                            df$R_bankssts_thickavg_cat == "infranormal" | 
                            df$L_caudalanteriorcingulate_thickavg_cat == "infranormal" | 
                            df$R_caudalanteriorcingulate_thickavg_cat == "infranormal" | 
                            df$L_caudalmiddlefrontal_thickavg_cat == "infranormal" | 
                            df$R_caudalmiddlefrontal_thickavg_cat == "infranormal" | 
                            df$L_cuneus_thickavg_cat == "infranormal" | 
                            df$R_cuneus_thickavg_cat == "infranormal" | 
                            df$L_entorhinal_thickavg_cat == "infranormal" | 
                            df$R_entorhinal_thickavg_cat == "infranormal" | 
                            df$L_fusiform_thickavg_cat == "infranormal" | 
                            df$R_fusiform_thickavg_cat == "infranormal" | 
                            df$L_inferiorparietal_thickavg_cat == "infranormal" | 
                            df$R_inferiorparietal_thickavg_cat == "infranormal" | 
                            df$L_inferiortemporal_thickavg_cat == "infranormal" | 
                            df$R_inferiortemporal_thickavg_cat == "infranormal" | 
                            df$L_isthmuscingulate_thickavg_cat == "infranormal" | 
                            df$R_isthmuscingulate_thickavg_cat== "infranormal" | 
                            df$L_lateraloccipital_thickavg_cat== "infranormal" | 
                            df$R_lateraloccipital_thickavg_cat== "infranormal" | 
                            df$L_lateralorbitofrontal_thickavg_cat== "infranormal" | 
                            df$R_lateralorbitofrontal_thickavg_cat== "infranormal" | 
                            df$L_lingual_thickavg_cat== "infranormal" | 
                            df$R_lingual_thickavg_cat== "infranormal" | 
                            df$L_medialorbitofrontal_thickavg_cat== "infranormal" | 
                            df$R_medialorbitofrontal_thickavg_cat== "infranormal" | 
                            df$L_middletemporal_thickavg_cat == "infranormal" | 
                            df$R_middletemporal_thickavg_cat == "infranormal" | 
                            df$L_parahippocampal_thickavg_cat== "infranormal" | 
                            df$R_parahippocampal_thickavg_cat== "infranormal" | 
                            df$L_paracentral_thickavg_cat== "infranormal" | 
                            df$R_paracentral_thickavg_cat== "infranormal" | 
                            df$L_parsopercularis_thickavg_cat== "infranormal" | 
                            df$R_parsopercularis_thickavg_cat== "infranormal" | 
                            df$L_parsorbitalis_thickavg_cat== "infranormal" | 
                            df$R_parsorbitalis_thickavg_cat== "infranormal" | 
                            df$L_parstriangularis_thickavg_cat== "infranormal" | 
                            df$R_parstriangularis_thickavg_cat== "infranormal" | 
                            df$L_pericalcarine_thickavg_cat== "infranormal" | 
                            df$R_pericalcarine_thickavg_cat== "infranormal" | 
                            df$L_postcentral_thickavg_cat== "infranormal" | 
                            df$R_postcentral_thickavg_cat== "infranormal" | 
                            df$L_posteriorcingulate_thickavg_cat == "infranormal" | 
                            df$R_posteriorcingulate_thickavg_cat == "infranormal" | 
                            df$L_precentral_thickavg_cat == "infranormal" | 
                            df$R_precentral_thickavg_cat == "infranormal" | 
                            df$L_precuneus_thickavg_cat== "infranormal" | 
                            df$R_precuneus_thickavg_cat== "infranormal" | 
                            df$L_rostralanteriorcingulate_thickavg_cat== "infranormal" | 
                            df$R_rostralanteriorcingulate_thickavg_cat== "infranormal" | 
                            df$L_rostralmiddlefrontal_thickavg_cat== "infranormal" | 
                            df$R_rostralmiddlefrontal_thickavg_cat== "infranormal" | 
                            df$L_superiorfrontal_thickavg_cat== "infranormal" | 
                            df$R_superiorfrontal_thickavg_cat== "infranormal" | 
                            df$L_superiorparietal_thickavg_cat== "infranormal" | 
                            df$R_superiorparietal_thickavg_cat== "infranormal" | 
                            df$L_superiortemporal_thickavg_cat== "infranormal" | 
                            df$R_superiortemporal_thickavg_cat== "infranormal" | 
                            df$L_supramarginal_thickavg_cat== "infranormal" | 
                            df$R_supramarginal_thickavg_cat== "infranormal" | 
                            df$L_frontalpole_thickavg_cat== "infranormal" | 
                            df$R_frontalpole_thickavg_cat== "infranormal" | 
                            df$L_temporalpole_thickavg_cat== "infranormal" | 
                            df$R_temporalpole_thickavg_cat== "infranormal" | 
                            df$L_transversetemporal_thickavg_cat == "infranormal" | 
                            df$R_transversetemporal_thickavg_cat == "infranormal" | 
                            df$L_insula_thickavg_cat == "infranormal" | 
                            df$R_insula_thickavg_cat == "infranormal",
                          "yes", "no")

#proportion participants with supranormal deviation in any region
healthy<-df[df$Group==0,]
table1<-table(healthy$supranormal_CT)
prop.table(table1)

patient<-df[df$Group==1,]
table2<-table(patient$supranormal_CT)
prop.table(table2)

#proportion participants with infranormal deviation in any region
table1<-table(healthy$infranormal_CT)
prop.table(table1)

table2<-table(patient$infranormal_CT)
prop.table(table2)

#Is there a significant differences between number of healthy vs. patient with at least one CT region with extreme supranormal deviation
chisq.test(df$supranormal_CT,df$Group)
#Is there a significant differences between number of healthy vs. patient with at least one CT region with extreme infranormal deviation
chisq.test(df$infranormal_CT,df$Group)

#For each region, % of healthy and patient with infranormal or supranormal deviations
colnames(healthy)
nvar = as.integer(c(179:246)) #adjust columns in case have additional variables (should start at L_bankssts_thickavg_cat and end at R_insula_thickavg_cat)

resmat_prop_CT<-matrix(0,length(nvar),4)
iter=0
for ( i in nvar) {
  iter=iter+1
  tmp_c<-((sum(healthy[,i]=="infranormal"))/sum(df$Group==0))*100
  resmat_prop_CT[iter,1]<-tmp_c
  tmp_c<-((sum(patient[,i]=="infranormal"))/sum(df$Group==1))*100
  resmat_prop_CT[iter,2]<-tmp_c
  tmp_c<-((sum(healthy[,i]=="supranormal"))/sum(df$Group==0))*100
  resmat_prop_CT[iter,3]<-tmp_c
  tmp_c<-((sum(patient[,i]=="supranormal"))/sum(df$Group==1))*100
  resmat_prop_CT[iter,4]<-tmp_c
}

colnames(resmat_prop_CT)<-c("Infra_healthy","Infra_patient", "Supr_healthy", "Supra_patient")
rownames(resmat_prop_CT)<-colnames(df[,nvar])
write.table(resmat_prop_CT, file = "proportions_CT.csv", sep = ",")

#Two proportion z-test
nvar=as.integer(c(179:246))
colnames(df)

resmat_ct_infranormal<-matrix(0,length(nvar),4)
iter=0
for ( i in nvar) {
  iter = iter+1
  res<-prop.test(x = c(sum(df[df$Group==0,i] == 'infranormal'),sum(df[df$Group==1,i] == 'infranormal')), n = c(sum(df$Group==0),sum(df$Group==1)))
  resmat_ct_infranormal[iter,1:2]<-res[["estimate"]]
  resmat_ct_infranormal[iter,3]<-res[["statistic"]]
  resmat_ct_infranormal[iter,4]<-res[["p.value"]]
  
}

resmat_p_infranormal<-p.adjust(resmat_ct_infranormal[,4],"fdr")
resmat_ct_infranormal<-cbind(resmat_ct_infranormal,resmat_p_infranormal)
colnames(resmat_ct_infranormal)<-c("prop-healthy","prop-patient","X-squared","p-value","p-FDR")
rownames(resmat_ct_infranormal) = colnames(df[,nvar])
write.table(resmat_ct_infranormal, file = "two-prop_z-test_infranormal_CT.csv", sep = ",")

resmat_ct_supranormal<-matrix(0,length(nvar),4)
iter=0
for ( i in nvar) {
  iter = iter+1
  res<-prop.test(x = c(sum(df[df$Group==0,i] == 'supranormal'),sum(df[df$Group==1,i] == 'supranormal')), n = c(sum(df$Group==0),sum(df$Group==1)))
  resmat_ct_supranormal[iter,1:2]<-res[["estimate"]]
  resmat_ct_supranormal[iter,3]<-res[["statistic"]]
  resmat_ct_supranormal[iter,4]<-res[["p.value"]]
}

resmat_p_supranormal<-p.adjust(resmat_ct_supranormal[,4],"fdr")
resmat_ct_supranormal<-cbind(resmat_ct_supranormal,resmat_p_supranormal)
colnames(resmat_ct_supranormal)<-c("prop-healthy","prop-patient","X-squared","p-value","p-FDR")
rownames(resmat_ct_supranormal) = colnames(df[,nvar])
write.table(resmat_ct_supranormal, file = "two-prop_z-test_supranormal_CT.csv", sep = ",")

#2-proportion z-test for differences between groups with any cortical thickness region infra or supranormal
res_infranormal_ct<-prop.test(x = c(sum(df[df$Group==0,"infranormal_CT"] == 'yes'),sum(df[df$Group==1,"infranormal_CT"] == 'yes')), n = c(sum(df$Group==0),sum(df$Group==1)))
res_infranormal_ct
res_supranormal_ct<-prop.test(x = c(sum(df[df$Group==0,"supranormal_CT"] == 'yes'),sum(df[df$Group==1,"supranormal_CT"] == 'yes')), n = c(sum(df$Group==0),sum(df$Group==1)))
res_supranormal_ct

#############################################
#Surface Area (SA)

colnames(df)

df<-df %>% mutate(L_bankssts_surfavg_cat=cut(L_bankssts_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_bankssts_surfavg_cat=cut(R_bankssts_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_caudalanteriorcingulate_surfavg_cat=cut(L_caudalanteriorcingulate_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_caudalanteriorcingulate_surfavg_cat=cut(R_caudalanteriorcingulate_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_caudalmiddlefrontal_surfavg_cat=cut(L_caudalmiddlefrontal_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_caudalmiddlefrontal_surfavg_cat=cut(R_caudalmiddlefrontal_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_cuneus_surfavg_cat=cut(L_cuneus_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_cuneus_surfavg_cat=cut(R_cuneus_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_entorhinal_surfavg_cat=cut(L_entorhinal_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_entorhinal_surfavg_cat=cut(R_entorhinal_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_fusiform_surfavg_cat=cut(L_fusiform_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_fusiform_surfavg_cat=cut(R_fusiform_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_inferiorparietal_surfavg_cat=cut(L_inferiorparietal_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_inferiorparietal_surfavg_cat=cut(R_inferiorparietal_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_inferiortemporal_surfavg_cat=cut(L_inferiortemporal_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_inferiortemporal_surfavg_cat=cut(R_inferiortemporal_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_isthmuscingulate_surfavg_cat=cut(L_isthmuscingulate_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_isthmuscingulate_surfavg_cat=cut(R_isthmuscingulate_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_lateraloccipital_surfavg_cat=cut(L_lateraloccipital_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_lateraloccipital_surfavg_cat=cut(R_lateraloccipital_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_lateralorbitofrontal_surfavg_cat=cut(L_lateralorbitofrontal_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_lateralorbitofrontal_surfavg_cat=cut(R_lateralorbitofrontal_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_lingual_surfavg_cat=cut(L_lingual_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_lingual_surfavg_cat=cut(R_lingual_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_medialorbitofrontal_surfavg_cat=cut(L_medialorbitofrontal_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_medialorbitofrontal_surfavg_cat=cut(R_medialorbitofrontal_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_middletemporal_surfavg_cat=cut(L_middletemporal_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_middletemporal_surfavg_cat=cut(R_middletemporal_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_parahippocampal_surfavg_cat=cut(L_parahippocampal_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_parahippocampal_surfavg_cat=cut(R_parahippocampal_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_paracentral_surfavg_cat=cut(L_paracentral_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_paracentral_surfavg_cat=cut(R_paracentral_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_parsopercularis_surfavg_cat=cut(L_parsopercularis_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_parsopercularis_surfavg_cat=cut(R_parsopercularis_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_parsorbitalis_surfavg_cat=cut(L_parsorbitalis_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_parsorbitalis_surfavg_cat=cut(R_parsorbitalis_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_parstriangularis_surfavg_cat=cut(L_parstriangularis_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_parstriangularis_surfavg_cat=cut(R_parstriangularis_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_pericalcarine_surfavg_cat=cut(L_pericalcarine_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_pericalcarine_surfavg_cat=cut(R_pericalcarine_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_postcentral_surfavg_cat=cut(L_postcentral_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_postcentral_surfavg_cat=cut(R_postcentral_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_posteriorcingulate_surfavg_cat=cut(L_posteriorcingulate_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_posteriorcingulate_surfavg_cat=cut(R_posteriorcingulate_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_precentral_surfavg_cat=cut(L_precentral_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_precentral_surfavg_cat=cut(R_precentral_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_precuneus_surfavg_cat=cut(L_precuneus_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_precuneus_surfavg_cat=cut(R_precuneus_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_rostralanteriorcingulate_surfavg_cat=cut(L_rostralanteriorcingulate_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_rostralanteriorcingulate_surfavg_cat=cut(R_rostralanteriorcingulate_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_rostralmiddlefrontal_surfavg_cat=cut(L_rostralmiddlefrontal_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_rostralmiddlefrontal_surfavg_cat=cut(R_rostralmiddlefrontal_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_superiorfrontal_surfavg_cat=cut(L_superiorfrontal_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_superiorfrontal_surfavg_cat=cut(R_superiorfrontal_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_superiorparietal_surfavg_cat=cut(L_superiorparietal_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_superiorparietal_surfavg_cat=cut(R_superiorparietal_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_superiortemporal_surfavg_cat=cut(L_superiortemporal_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_superiortemporal_surfavg_cat=cut(R_superiortemporal_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_supramarginal_surfavg_cat=cut(L_supramarginal_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_supramarginal_surfavg_cat=cut(R_supramarginal_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_frontalpole_surfavg_cat=cut(L_frontalpole_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_frontalpole_surfavg_cat=cut(R_frontalpole_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_temporalpole_surfavg_cat=cut(L_temporalpole_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_temporalpole_surfavg_cat=cut(R_temporalpole_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_transversetemporal_surfavg_cat=cut(L_transversetemporal_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_transversetemporal_surfavg_cat=cut(R_transversetemporal_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(L_insula_surfavg_cat=cut(L_insula_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))
df<-df %>% mutate(R_insula_surfavg_cat=cut(R_insula_surfavg, breaks=c(-Inf, -1.96, 1.96,Inf), labels=c("infranormal","normal","supranormal")))



df$supranormal_SA<-ifelse(df$L_bankssts_surfavg_cat == "supranormal" | 
                            df$R_bankssts_surfavg_cat == "supranormal" | 
                            df$L_caudalanteriorcingulate_surfavg_cat == "supranormal" | 
                            df$R_caudalanteriorcingulate_surfavg_cat == "supranormal" | 
                            df$L_caudalmiddlefrontal_surfavg_cat == "supranormal" | 
                            df$R_caudalmiddlefrontal_surfavg_cat == "supranormal" | 
                            df$L_cuneus_surfavg_cat == "supranormal" | 
                            df$R_cuneus_surfavg_cat == "supranormal" | 
                            df$L_entorhinal_surfavg_cat == "supranormal" | 
                            df$R_entorhinal_surfavg_cat == "supranormal" | 
                            df$L_fusiform_surfavg_cat == "supranormal" | 
                            df$R_fusiform_surfavg_cat == "supranormal" | 
                            df$L_inferiorparietal_surfavg_cat == "supranormal" | 
                            df$R_inferiorparietal_surfavg_cat == "supranormal" | 
                            df$L_inferiortemporal_surfavg_cat == "supranormal" | 
                            df$R_inferiortemporal_surfavg_cat == "supranormal" | 
                            df$L_isthmuscingulate_surfavg_cat == "supranormal" | 
                            df$R_isthmuscingulate_surfavg_cat== "supranormal" | 
                            df$L_lateraloccipital_surfavg_cat== "supranormal" | 
                            df$R_lateraloccipital_surfavg_cat== "supranormal" | 
                            df$L_lateralorbitofrontal_surfavg_cat== "supranormal" | 
                            df$R_lateralorbitofrontal_surfavg_cat== "supranormal" | 
                            df$L_lingual_surfavg_cat== "supranormal" | 
                            df$R_lingual_surfavg_cat== "supranormal" | 
                            df$L_medialorbitofrontal_surfavg_cat== "supranormal" | 
                            df$R_medialorbitofrontal_surfavg_cat== "supranormal" | 
                            df$L_middletemporal_surfavg_cat == "supranormal" | 
                            df$R_middletemporal_surfavg_cat == "supranormal" | 
                            df$L_parahippocampal_surfavg_cat== "supranormal" | 
                            df$R_parahippocampal_surfavg_cat== "supranormal" | 
                            df$L_paracentral_surfavg_cat== "supranormal" | 
                            df$R_paracentral_surfavg_cat== "supranormal" | 
                            df$L_parsopercularis_surfavg_cat== "supranormal" | 
                            df$R_parsopercularis_surfavg_cat== "supranormal" | 
                            df$L_parsorbitalis_surfavg_cat== "supranormal" | 
                            df$R_parsorbitalis_surfavg_cat== "supranormal" | 
                            df$L_parstriangularis_surfavg_cat== "supranormal" | 
                            df$R_parstriangularis_surfavg_cat== "supranormal" | 
                            df$L_pericalcarine_surfavg_cat== "supranormal" | 
                            df$R_pericalcarine_surfavg_cat== "supranormal" | 
                            df$L_postcentral_surfavg_cat== "supranormal" | 
                            df$R_postcentral_surfavg_cat== "supranormal" | 
                            df$L_posteriorcingulate_surfavg_cat == "supranormal" | 
                            df$R_posteriorcingulate_surfavg_cat == "supranormal" | 
                            df$L_precentral_surfavg_cat == "supranormal" | 
                            df$R_precentral_surfavg_cat == "supranormal" | 
                            df$L_precuneus_surfavg_cat== "supranormal" | 
                            df$R_precuneus_surfavg_cat== "supranormal" | 
                            df$L_rostralanteriorcingulate_surfavg_cat== "supranormal" | 
                            df$R_rostralanteriorcingulate_surfavg_cat== "supranormal" | 
                            df$L_rostralmiddlefrontal_surfavg_cat== "supranormal" | 
                            df$R_rostralmiddlefrontal_surfavg_cat== "supranormal" | 
                            df$L_superiorfrontal_surfavg_cat== "supranormal" | 
                            df$R_superiorfrontal_surfavg_cat== "supranormal" | 
                            df$L_superiorparietal_surfavg_cat== "supranormal" | 
                            df$R_superiorparietal_surfavg_cat== "supranormal" | 
                            df$L_superiortemporal_surfavg_cat== "supranormal" | 
                            df$R_superiortemporal_surfavg_cat== "supranormal" | 
                            df$L_supramarginal_surfavg_cat== "supranormal" | 
                            df$R_supramarginal_surfavg_cat== "supranormal" | 
                            df$L_frontalpole_surfavg_cat== "supranormal" | 
                            df$R_frontalpole_surfavg_cat== "supranormal" | 
                            df$L_temporalpole_surfavg_cat== "supranormal" | 
                            df$R_temporalpole_surfavg_cat== "supranormal" | 
                            df$L_transversetemporal_surfavg_cat == "supranormal" | 
                            df$R_transversetemporal_surfavg_cat == "supranormal" | 
                            df$L_insula_surfavg_cat == "supranormal" | 
                            df$R_insula_surfavg_cat == "supranormal",
                          "yes", "no")





df$infranormal_SA<-ifelse(df$L_bankssts_surfavg_cat == "infranormal" | 
                            df$R_bankssts_surfavg_cat == "infranormal" | 
                            df$L_caudalanteriorcingulate_surfavg_cat == "infranormal" | 
                            df$R_caudalanteriorcingulate_surfavg_cat == "infranormal" | 
                            df$L_caudalmiddlefrontal_surfavg_cat == "infranormal" | 
                            df$R_caudalmiddlefrontal_surfavg_cat == "infranormal" | 
                            df$L_cuneus_surfavg_cat == "infranormal" | 
                            df$R_cuneus_surfavg_cat == "infranormal" | 
                            df$L_entorhinal_surfavg_cat == "infranormal" | 
                            df$R_entorhinal_surfavg_cat == "infranormal" | 
                            df$L_fusiform_surfavg_cat == "infranormal" | 
                            df$R_fusiform_surfavg_cat == "infranormal" | 
                            df$L_inferiorparietal_surfavg_cat == "infranormal" | 
                            df$R_inferiorparietal_surfavg_cat == "infranormal" | 
                            df$L_inferiortemporal_surfavg_cat == "infranormal" | 
                            df$R_inferiortemporal_surfavg_cat == "infranormal" | 
                            df$L_isthmuscingulate_surfavg_cat == "infranormal" | 
                            df$R_isthmuscingulate_surfavg_cat== "infranormal" | 
                            df$L_lateraloccipital_surfavg_cat== "infranormal" | 
                            df$R_lateraloccipital_surfavg_cat== "infranormal" | 
                            df$L_lateralorbitofrontal_surfavg_cat== "infranormal" | 
                            df$R_lateralorbitofrontal_surfavg_cat== "infranormal" | 
                            df$L_lingual_surfavg_cat== "infranormal" | 
                            df$R_lingual_surfavg_cat== "infranormal" | 
                            df$L_medialorbitofrontal_surfavg_cat== "infranormal" | 
                            df$R_medialorbitofrontal_surfavg_cat== "infranormal" | 
                            df$L_middletemporal_surfavg_cat == "infranormal" | 
                            df$R_middletemporal_surfavg_cat == "infranormal" | 
                            df$L_parahippocampal_surfavg_cat== "infranormal" | 
                            df$R_parahippocampal_surfavg_cat== "infranormal" | 
                            df$L_paracentral_surfavg_cat== "infranormal" | 
                            df$R_paracentral_surfavg_cat== "infranormal" | 
                            df$L_parsopercularis_surfavg_cat== "infranormal" | 
                            df$R_parsopercularis_surfavg_cat== "infranormal" | 
                            df$L_parsorbitalis_surfavg_cat== "infranormal" | 
                            df$R_parsorbitalis_surfavg_cat== "infranormal" | 
                            df$L_parstriangularis_surfavg_cat== "infranormal" | 
                            df$R_parstriangularis_surfavg_cat== "infranormal" | 
                            df$L_pericalcarine_surfavg_cat== "infranormal" | 
                            df$R_pericalcarine_surfavg_cat== "infranormal" | 
                            df$L_postcentral_surfavg_cat== "infranormal" | 
                            df$R_postcentral_surfavg_cat== "infranormal" | 
                            df$L_posteriorcingulate_surfavg_cat == "infranormal" | 
                            df$R_posteriorcingulate_surfavg_cat == "infranormal" | 
                            df$L_precentral_surfavg_cat == "infranormal" | 
                            df$R_precentral_surfavg_cat == "infranormal" | 
                            df$L_precuneus_surfavg_cat== "infranormal" | 
                            df$R_precuneus_surfavg_cat== "infranormal" | 
                            df$L_rostralanteriorcingulate_surfavg_cat== "infranormal" | 
                            df$R_rostralanteriorcingulate_surfavg_cat== "infranormal" | 
                            df$L_rostralmiddlefrontal_surfavg_cat== "infranormal" | 
                            df$R_rostralmiddlefrontal_surfavg_cat== "infranormal" | 
                            df$L_superiorfrontal_surfavg_cat== "infranormal" | 
                            df$R_superiorfrontal_surfavg_cat== "infranormal" | 
                            df$L_superiorparietal_surfavg_cat== "infranormal" | 
                            df$R_superiorparietal_surfavg_cat== "infranormal" | 
                            df$L_superiortemporal_surfavg_cat== "infranormal" | 
                            df$R_superiortemporal_surfavg_cat== "infranormal" | 
                            df$L_supramarginal_surfavg_cat== "infranormal" | 
                            df$R_supramarginal_surfavg_cat== "infranormal" | 
                            df$L_frontalpole_surfavg_cat== "infranormal" | 
                            df$R_frontalpole_surfavg_cat== "infranormal" | 
                            df$L_temporalpole_surfavg_cat== "infranormal" | 
                            df$R_temporalpole_surfavg_cat== "infranormal" | 
                            df$L_transversetemporal_surfavg_cat == "infranormal" | 
                            df$R_transversetemporal_surfavg_cat == "infranormal" | 
                            df$L_insula_surfavg_cat == "infranormal" | 
                            df$R_insula_surfavg_cat == "infranormal",
                          "yes", "no")
#proportion participants with supranormal deviation in any region
healthy<-df[df$Group==0,]
table1<-table(healthy$supranormal_SA)
prop.table(table1)

patient<-df[df$Group==1,]
table2<-table(patient$supranormal_SA)
prop.table(table2)

#proportion participants with infranormal deviation in any region
table1<-table(healthy$infranormal_SA)
prop.table(table1)

table2<-table(patient$infranormal_SA)
prop.table(table2)

#Is there a significant differences between number of healthy vs. patient with at least one SA region with extreme supranormal deviation
chisq.test(df$supranormal_SA,df$Group)
#Is there a significant differences between number of healthy vs. patient with at least one SA region with extreme infranormal deviation
chisq.test(df$infranormal_SA,df$Group)

#For each region, % of healthy and patient with infranormal or supranormal deviations
colnames(df)
nvar = as.integer(c(249:316))#adjust depending on number of variables have - should start at L_bankssts_surfavg_cat and end at R_insula_surfavg_cat

resmat_prop_SA<-matrix(0,length(nvar),4)
iter=0
for ( i in nvar) {
  iter=iter+1
  tmp_c<-((sum(healthy[,i]=="infranormal"))/sum(df$Group==0))*100
  resmat_prop_SA[iter,1]<-tmp_c
  tmp_c<-((sum(patient[,i]=="infranormal"))/sum(df$Group==1))*100
  resmat_prop_SA[iter,2]<-tmp_c
  tmp_c<-((sum(healthy[,i]=="supranormal"))/sum(df$Group==0))*100
  resmat_prop_SA[iter,3]<-tmp_c
  tmp_c<-((sum(patient[,i]=="supranormal"))/sum(df$Group==1))*100
  resmat_prop_SA[iter,4]<-tmp_c
}

colnames(resmat_prop_SA)<-c("Infra_healthy","Infra_patient", "Supr_healthy", "Supra_patient")
rownames(resmat_prop_SA)<-colnames(healthy[,nvar])
write.table(resmat_prop_SA, file = "proportions_SA.csv", sep = ",")

#Two proportion z-test
nvar=as.integer(c(249:316))
colnames(df)

resmat_sa_infranormal<-matrix(0,length(nvar),4)
iter=0
for ( i in nvar) {
  iter = iter+1
  res<-prop.test(x = c(sum(df[df$Group==0,i] == 'infranormal'),sum(df[df$Group==1,i] == 'infranormal')), n = c(sum(df$Group==0),sum(df$Group==1)))
  resmat_sa_infranormal[iter,1:2]<-res[["estimate"]]
  resmat_sa_infranormal[iter,3]<-res[["statistic"]]
  resmat_sa_infranormal[iter,4]<-res[["p.value"]]
  
}

resmat_p_infranormal<-p.adjust(resmat_sa_infranormal[,4],"fdr")
resmat_sa_infranormal<-cbind(resmat_sa_infranormal,resmat_p_infranormal)
colnames(resmat_sa_infranormal)<-c("prop-healthy","prop-patient","X-squared","p-value","p-FDR")
rownames(resmat_sa_infranormal) = colnames(df[,nvar])
write.table(resmat_sa_infranormal, file = "two-prop_z-test_infranormal_SA.csv", sep = ",")

resmat_sa_supranormal<-matrix(0,length(nvar),4)
iter=0
for ( i in nvar) {
  iter = iter+1
  res<-prop.test(x = c(sum(df[df$Group==0,i] == 'supranormal'),sum(df[df$Group==1,i] == 'supranormal')), n = c(sum(df$Group==0),sum(df$Group==1)))
  resmat_sa_supranormal[iter,1:2]<-res[["estimate"]]
  resmat_sa_supranormal[iter,3]<-res[["statistic"]]
  resmat_sa_supranormal[iter,4]<-res[["p.value"]]
}

resmat_p_supranormal<-p.adjust(resmat_sa_supranormal[,4],"fdr")

resmat_sa_supranormal<-cbind(resmat_sa_supranormal,resmat_p_supranormal)
colnames(resmat_sa_supranormal)<-c("prop-healthy","prop-patient","X-squared","p-value","p-FDR")
rownames(resmat_sa_supranormal) = colnames(df[,nvar])
write.table(resmat_sa_supranormal, file = "two-prop_z-test_supranormal_SA.csv", sep = ",")

#2-proportion z-test for differences between groups with any surface area region infra or supranormal
res_infranormal_sa<-prop.test(x = c(sum(df[df$Group==0,"infranormal_SA"] == 'yes'),sum(df[df$Group==1,"infranormal_SA"] == 'yes')), n = c(sum(df$Group==0),sum(df$Group==1)))
res_infranormal_sa
res_supranormal_sa<-prop.test(x = c(sum(df[df$Group==0,"supranormal_SA"] == 'yes'),sum(df[df$Group==1,"supranormal_SA"] == 'yes')), n = c(sum(df$Group==0),sum(df$Group==1)))
res_supranormal_sa

##############################################################################
#Test significance of two proportion z-test across all regions (SV, CT, and SA)
##############################################################################

#Two proportion z-test
nvar=as.integer(c(163:176,179:246,249:316))
colnames(df)

resmat_all_infranormal<-matrix(0,length(nvar),4)
iter=0
for ( i in nvar) {
  iter = iter+1
  res<-prop.test(x = c(sum(df[df$Group==0,i] == 'infranormal'),sum(df[df$Group==1,i] == 'infranormal')), n = c(sum(df$Group==0),sum(df$Group==1)))
  resmat_all_infranormal[iter,1:2]<-res[["estimate"]]
  resmat_all_infranormal[iter,3]<-res[["statistic"]]
  resmat_all_infranormal[iter,4]<-res[["p.value"]]
  
}

resmat_p_infranormal<-p.adjust(resmat_all_infranormal[,4],"fdr")
resmat_all_infranormal<-cbind(resmat_all_infranormal,resmat_p_infranormal)
colnames(resmat_all_infranormal)<-c("prop-healthy","prop-patient","X-squared","p-value","p-FDR")
rownames(resmat_all_infranormal) = colnames(df[,nvar])
write.table(resmat_all_infranormal, file = "two-prop_z-test_infranormal_ALL.csv", sep = ",")

resmat_all_supranormal<-matrix(0,length(nvar),4)
iter=0
for ( i in nvar) {
  iter = iter+1
  res<-prop.test(x = c(sum(df[df$Group==0,i] == 'supranormal'),sum(df[df$Group==1,i] == 'supranormal')), n = c(sum(df$Group==0),sum(df$Group==1)))
  resmat_all_supranormal[iter,1:2]<-res[["estimate"]]
  resmat_all_supranormal[iter,3]<-res[["statistic"]]
  resmat_all_supranormal[iter,4]<-res[["p.value"]]
}

resmat_p_supranormal<-p.adjust(resmat_all_supranormal[,4],"fdr")

resmat_all_supranormal<-cbind(resmat_all_supranormal,resmat_p_supranormal)
colnames(resmat_all_supranormal)<-c("prop-healthy","prop-patient","X-squared","p-value","p-FDR")
rownames(resmat_all_supranormal) = colnames(df[,nvar])
write.table(resmat_all_supranormal, file = "two-prop_z-test_supranormal_ALL.csv", sep = ",")