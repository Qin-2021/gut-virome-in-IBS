library(reshape2)
library(ggalluvial)
library(tidyverse)
library (ggpubr)
library(tidyr)
library(reshape2)
library(dplyr)
library(stringr)
library(ggplot2)
library(stringr)
library(forcats)
library(viridis)
library(vegan)
library(ggridges)



####                                           load the data ################################################################################################################################################
divdata<-read.delim("data_profile.csv",sep=",",row.names = 1)

divdata_t<-as.data.frame(t(divdata))

shannon_diversity=diversity(divdata_t,index="shannon") 
simpson_diversity=diversity(divdata_t,index="simpson")
diversity<-cbind(shannon_diversity,simpson_diversity)
print(diversity)

# Richness 
divdata<-read.delim("data_profile.csv",sep=",", row.names = 1)
rep<-rep(1000000, rep = 401) 
mode(divdata)
dicdata_integ<-divdata*rep
dicdata_integ_round<-round(dicdata_integ)

divdata_integ_richness<-as.data.frame(dicdata_integ_round)

divdata_integ_richness_t<-t(divdata_integ_richness) 
##Richness calculation
richness <- estimateR(divdata_integ_richness_t)[1, ]    # rowname is sample

print(richness)
diversity_richness <-cbind(shannon_diversity,simpson_diversity,richness)
diversity_richness



###             plot diversity  ##############################################################################################################

#rm(list=ls())
divdata=read.delim("processed/diversity.richness.csv", sep=",", header=TRUE,row.names = NULL)
colnames(divdata)[1]<-"sample"
group=read.table("data/group.csv", sep=",", header=TRUE)#, row.names = 1)
divdata<-inner_join(divdata, group, by="sample")


mycol=c('#1597A5','#FFC24B','#5e4fa2', '#9970ab','#e7d4e8')

shannon<-ggviolin(divdata, x="Class", y="shannon_diversity", fill = "Class", 
                  palette = c('#762a83','#9970ab','#c2a5cf','#e7d4e8','#f7f7f7','#d9f0d3','#a6dba0','#5aae61','#1b7837'),
                  add = "boxplot", add.params = list(fill="white"))+
  
  scale_y_continuous(limits=c(0,6))+labs(x="", y = "Shannon Index")+
  theme(legend.title=element_blank(),legend.text=element_text(size=15),axis.text = element_text(size=12), axis.title = element_text(size=12))

mycol=c('#1597A5','#FFC24B','#5e4fa2', '#9970ab','#e7d4e8')

richness<-ggviolin(divdata, x="Class", y="richness", fill = "Class", 
                   add = "boxplot", add.params = list(fill="white"))+
  
  scale_y_continuous(limits=c(0,250))+labs(x="", y = "Richness")+ scale_y_continuous(limits=c(100,300))+
  theme(legend.title=element_blank(),legend.text=element_text(size=15),axis.text = element_text(size=12),axis.title = element_text(size=12))
richness




diversity<-ggarrange(shannon,richness,common.legend=F, labels = c("D","E"),font.label = list(size = 16, color = "black"))
diversity
