%% Script: computation_additional_metrics_extreme_deviations.m 
% Description: Script for generating average deviation scores based on z-score outputs
% from CentileBrain, and computing extreme deviations based on predefined
% threshold (here 1.96 SD)
% Written by: Shalaila S. Haas, PhD
% Institute: Icahn School of Medicine at Mount Sinai, New York
% Date: October 29, 2022
% Updated: January 15, 2024
% Variables:
    % filename = input your output data from CentileBrain into the template_z-scores.csv file
    % replace filepath with location where you saved the
    % template_z-scores.csv file. The outputs will also be saved here. 
    % sample = replace CHR with your project name or study sample of
    % interest
    % Within template_z-scores file, make sure: 
        % Sex: 1=male; 0=female 
        % Group: 1 = patient; 0 = healthy
clear all

%% Load data
cd('D:\Path\To\Folder\With\Template\')%enter path to location where you 
%want all the outputs to be saved (i.e., project folder).
filename = 'template_z-scores.csv'
Data = readtable(filename);
%Import Data from CSV (template_z-scores.csv) - make sure ID and Site are text
%(i.e., cell array of string), and all other variables are numbers (i.e., double). 

sample = 'CHR'; %Enter your project name here. 
%There should not be any spaces here because it will be included
%as a descriptor in output filenames 

%% Get unilateral regions
Freesurfer_Measures = [6:155]; %columns of your freesurfer measures
List_varnames = Data.Properties.VariableNames(Freesurfer_Measures)'
z_transformed_uni = Data{:,Freesurfer_Measures};
imaging_unilateral_varnames = Data.Properties.VariableNames(Freesurfer_Measures);

ind_HC = Data.Group ==0;
sum(ind_HC)
ind_pat = Data.Group ==1;
sum(ind_pat)
ID = Data.ID;

%% Indices for various parcellation averages
%All
ind_all = logical(zeros(1,150))
ind_all(1:150)=1
%check
imaging_unilateral_varnames(ind_all)'

%Cortical thickness
ind_CT   = logical(zeros(1,150));
ind_CT(15:82) = 1;
%check
imaging_unilateral_varnames(ind_CT)'

%Surface area
ind_SA   = logical(zeros(1,150));
ind_SA(83:150) = 1;
%check
imaging_unilateral_varnames(ind_SA)'

%Subcortical volume
ind_SV   = logical(zeros(1,150));
ind_SV(1:14) = 1;
%check
imaging_unilateral_varnames(ind_SV)'

%% Get average deviation per subject
global_deviations = {ind_all;ind_SV;ind_CT;ind_SA};
%Unilateral

%%%%%%%%
%Average Deviation Score across all regardless of sign
%%%%%%%%
ind = global_deviations;
for k = 1:numel(ind)
    for i = 1:numel(ID)
        mean_deviation_all(i,k) = mean(z_transformed_uni(i,ind{k}));
    end
end

mean_deviation_tab_uni = array2table([mean_deviation_all]);
mean_deviation_tab_uni.Properties.VariableNames = {'ADS-G','ADS-SV','ADS-CT','ADS-SA'};
mean_deviation_tab_uni = [Data(:,1:5),mean_deviation_tab_uni,Data(:,156:end)];
writetable(mean_deviation_tab_uni,['mean_deviation_global_unilateral_',sample,'.csv'],'Delimiter',',','WriteVariableNames',1);

clear mean_deviation_tab_uni

%%%%%%%%
%absolute
%%%%%%%%
ind = global_deviations;
for k = 1:numel(ind)
for i = 1:numel(ID)
mean_deviation_abs(i,k) = mean(abs(z_transformed_uni(i,ind{k})));
end
end

mean_deviation_tab_uni = array2table([mean_deviation_abs]);
mean_deviation_tab_uni.Properties.VariableNames = {'ADS-G','ADS-SV','ADS-CT','ADS-SA'};
mean_deviation_tab_uni = [Data(:,1:5),mean_deviation_tab_uni,Data(:,156:end)];
writetable(mean_deviation_tab_uni,['mean_deviation_global_abs_unilateral_',sample,'.csv'],'Delimiter',',','WriteVariableNames',1);

clear mean_deviation_tab_uni

%%%%%%%%
%positive
%%%%%%%%
ind = global_deviations;
for k = 1:numel(ind)
for i = 1:numel(ID)
ind_pos=z_transformed_uni(i,1:150)>0 & ind{k}==1;
mean_deviation_pos(i,k) = mean(z_transformed_uni(i,ind_pos));
end
end

mean_deviation_tab_uni = array2table([mean_deviation_pos]);
mean_deviation_tab_uni.Properties.VariableNames = {'ADS-G','ADS-SV','ADS-CT','ADS-SA'};
mean_deviation_tab_uni = [Data(:,1:5),mean_deviation_tab_uni,Data(:,156:end)];
writetable(mean_deviation_tab_uni,['mean_deviation_global_pos_unilateral_',sample,'.csv'],'Delimiter',',','WriteVariableNames',1);

clear mean_deviation_tab_uni

%%%%%%%%
%negative
%%%%%%%%
ind = global_deviations;
for k = 1:numel(ind)
for i = 1:numel(ID)
ind_neg=z_transformed_uni(i,1:150)<0 & ind{k}==1;
mean_deviation_neg(i,k) = mean(z_transformed_uni(i,ind_neg));
end
end

mean_deviation_tab_uni = array2table([mean_deviation_neg]);
mean_deviation_tab_uni.Properties.VariableNames = {'ADS-G','ADS-SV','ADS-CT','ADS-SA'};
mean_deviation_tab_uni = [Data(:,1:5),mean_deviation_tab_uni,Data(:,156:end)];
writetable(mean_deviation_tab_uni,['mean_deviation_global_neg_unilateral_',sample,'.csv'],'Delimiter',',','WriteVariableNames',1);

clear mean_deviation_tab_uni

%% For each brain region, get percentage of Patients at + or - extremes (set SD)
extremes_pos_pat = [];
extremes_neg_pat = [];
extremes_pos_pat_N = [];
extremes_neg_pat_N = [];

SD = 1.96;
for i = 1:numel(imaging_unilateral_varnames)
ind_pos = z_transformed_uni(Data.Group==1,i)>SD;
ind_neg = z_transformed_uni(Data.Group==1,i)<-SD;
extremes_pos_pat(i,1) = (sum(ind_pos)/(numel(ID(Data.Group==1))))*100;
extremes_pos_pat_N(i,1) = sum(ind_pos); %N patients with extreme + deviation
extremes_neg_pat(i,1) = (sum(ind_neg)/(numel(ID(Data.Group==1))))*100;
extremes_neg_pat_N(i,1) = sum(ind_neg); %N patients with extreme - deviation
end

extremes_pat = [List_varnames,cellstr(num2str(-(extremes_neg_pat))),cellstr(num2str(extremes_pos_pat))]
extremes_pat_table = array2table(extremes_pat)
extremes_pat_table.Properties.VariableNames = {'Freesurfer_Region','Extreme_Negative_Deviation','Exreme_Positive_Deviation'}
writetable(extremes_pat_table,['extreme_deviations_per_region_patients_',sample,'.csv'],'Delimiter',',','WriteVariableNames',1);

%% For each brain region, get percentage of Healthy at + or - extremes (set SD)

extremes_pos_HC = [];
extremes_neg_HC = [];
extremes_pos_HC_N = [];
extremes_neg_HC_N = [];
SD = 1.96;
for i = 1:numel(imaging_unilateral_varnames)
ind_pos = z_transformed_uni(Data.Group==0,i)>SD;
ind_neg = z_transformed_uni(Data.Group==0,i)<-SD;
extremes_pos_HC(i,1) = (sum(ind_pos)/(numel(ID(Data.Group==0))))*100;
extremes_pos_HC_N(i,1) = sum(ind_pos); %N healthy with extreme + deviation
extremes_neg_HC(i,1) = (sum(ind_neg)/(numel(ID(Data.Group==0))))*100;
extremes_neg_HC_N(i,1) = sum(ind_neg); %N healthy with extreme - deviation

end

extremes_HC = [List_varnames,cellstr(num2str(-(extremes_neg_HC))),cellstr(num2str(extremes_pos_HC))]
extremes_HC_table = array2table(extremes_HC)
extremes_HC_table.Properties.VariableNames = {'Freesurfer_Region','Extreme_Negative_Deviation','Exreme_Positive_Deviation'}
writetable(extremes_HC_table,['extreme_deviations_per_region_healthy_',sample,'.csv'],'Delimiter',',','WriteVariableNames',1);

%% Get percentage of regions at + or - extremes for each subject
SD = 1.96; %Define threshold cutoff for extremes 

%Across all Freesurfer measures (global)
for i = 1:numel(ID)
ind_pos = z_transformed_uni(i,:)>SD;
ind_neg = z_transformed_uni(i,:)<-SD;
extremes_pos_regions(i,1) = sum(ind_pos);
extremes_neg_regions(i,1) = sum(ind_neg);
end
clear ind_pos
clear ind_neg

%Across all subcortical volume measures (SV)
for i = 1:numel(ID)
ind_pos = z_transformed_uni(i,1:14)>SD;
ind_neg = z_transformed_uni(i,1:14)<-SD;
extremes_pos_regions_SV(i,1) = sum(ind_pos);
extremes_neg_regions_SV(i,1) = sum(ind_neg);
end
clear ind_pos
clear ind_neg

%Across all cortical thickness measures (CT)
for i = 1:numel(ID)
ind_pos = z_transformed_uni(i,15:82)>SD;
ind_neg = z_transformed_uni(i,15:82)<-SD;
extremes_pos_regions_CT(i,1) = sum(ind_pos);
extremes_neg_regions_CT(i,1) = sum(ind_neg);
end
clear ind_pos
clear ind_neg

%Across all cortical surface area measures (SA)
for i = 1:numel(ID)
ind_pos = z_transformed_uni(i,83:150)>SD;
ind_neg = z_transformed_uni(i,83:150)<-SD;
extremes_pos_regions_SA(i,1) = sum(ind_pos);
extremes_neg_regions_SA(i,1) = sum(ind_neg);
end
clear ind_pos
clear ind_neg

extremes_subjects = [ID,cellstr(num2str(Data.Group)),cellstr(num2str(extremes_pos_regions)),cellstr(num2str(extremes_neg_regions)),cellstr(num2str(extremes_pos_regions_SV)),cellstr(num2str(extremes_neg_regions_SV)),cellstr(num2str(extremes_pos_regions_CT)),cellstr(num2str(extremes_neg_regions_CT)),cellstr(num2str(extremes_pos_regions_SA)),cellstr(num2str(extremes_neg_regions_SA))]
extremes_subjects_table = array2table(extremes_subjects)
extremes_subjects_table.Properties.VariableNames = {'ID','Group','Extreme_Pos_Deviations', 'Extreme_Neg_Deviations','Extreme_Pos_SV_Deviatons', 'Extreme_Neg_SV_Deviations','Extreme_Pos_CT_Deviations', 'Extreme_Neg_CT_Deviations', 'Extreme_Pos_SA_Deviations','Extreme_Neg_SA_Deviations'}
writetable(extremes_subjects_table,['extreme_deviations_per_individual_',sample,'.csv'],'Delimiter',',','WriteVariableNames',1);
