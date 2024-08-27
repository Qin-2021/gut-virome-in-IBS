library ("phyloseq")
library("ggplot2")
library(vegan) 
library(tidyverse) 
library("ggpubr")
library("dplyr")
library(RColorBrewer)


################ ---  AMG pathway --- #######################
df<-read.delim("AMG_pathway_merged_data.tsv",sep="\t")
aggregated_df <- aggregate(Total.AMGs ~ Metabolism, data = df, sum)
sorted_df <- aggregated_df[order(aggregated_df$Total.AMGs, decreasing = TRUE), ]



###--plot--pathway########## 
colors_set3 <- brewer.pal(12, "Set3")
colors_paired <- brewer.pal(12, "Paired")
colors_puor<-brewer.pal(3, "PuOr")
colors_dark2<-brewer.pal(8, "Dark2")
colors <- c(colors_paired,colors_dark2)


ggplot(sorted_df, aes(x = reorder(Metabolism, Total.AMGs), y = Total.AMGs, fill=Metabolism)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "AMG annotations", x = "", y = "Total AMGs") +
  theme_bw()+
  theme(axis.text = element_text( color="black",size=12))+
  scale_fill_manual(values = colors_set3)+theme(legend.position = "none")+
  ylim(0,10200)+
  theme(axis.text = element_text(size=15,color="black"),axis.title = element_text(size=15,color="black"),title = element_text(size=15,color="black"))


ggsave('vibrant_pathways_all_samples_3June2024.pdf',width = 8,height = 5,unit="in")

