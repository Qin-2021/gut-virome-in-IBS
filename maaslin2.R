rm(list=ls())
#BiocManager::install("Maaslin2")
library(Maaslin2)
library(dplyr)
library(ggplot2)



spe1<-read.delim("df_for_maaslin.csv",sep=",", row.names = 1) 

spe4<-as.data.frame(t(spe1))

 

meta_all<-read.delim("IBS_data/group.csv",sep=",", row.names = 1)  


fit_data = Maaslin2(
  input_data = spe4, 
  input_metadata =meta_all, 
  output = "massline_output", 
  fixed_effects = "Class",    # A D a&D
  reference=c("Class,HC"),
  correction = "BH")   












