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





plt1<-ggplot(merged_data, aes(x = Genus, y = Phage_per_host_genus, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +coord_flip()+
  theme_bw()+theme(axis.text = element_text(size=16,color = "black"),axis.title.x =element_text(size=16,color = "black") ,legend.text = element_text(size=16,color = "black"),legend.title = element_blank()) +
  theme(axis.title.y =element_blank())+
  labs(x = "Genus", y = "Number", title = "vOTU per host genus",size=16)


plt2<-ggplot(merged_data, aes(x = Genus, y = Unique_Host_genome, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +coord_flip()+
  theme_bw()+theme(axis.text.x = element_text(size=16,color = "black"),axis.title.x =element_text(size=16,color = "black"),axis.text.y = element_blank() ) +
  theme(axis.title.y=element_blank(),axis.ticks.y=element_blank(),legend.text = element_text(size=16,color = "black"),legend.title = element_blank())+
  labs(x = "Genus", y = "Number", title = "vOTU Unique Host genome",size=16)

library(ggpubr)

ggarrange(plt1, plt2, common.legend = TRUE,widths = c(1,0.7))

ggsave('comparison of phage host_vOTU.pdf',width = 8,height = 8)

