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
library(ggsankey)
library(dplyr)
library(ggplot2)



colors <- c("#3B9AB2","#78B7C5","#EBCC2A","#E1AF00","#F21A00",
            "#ECCBAE","#046C9A","#D69C4E","#ABDDDE","#000000")


IBS<-ggplot(df_long, aes(x = x, next_x = next_x, 
                    node = node, next_node = next_node, 
                    fill = factor(x),
                    label = node,)) +
  geom_sankey(color = "grey", # node.color=, flow.color=
              #fill = "brown", # node.fill=, flow.fill=
              flow.alpha = 0.6,
              # space = 1,
              width = 0.1) +
  geom_sankey_text(size = 4, vjust = 0.2, hjust = -0.5,
                   color = "black", fontface = "bold") +
  scale_fill_manual(values = colors) +   # ggsci::scale_fill_aaas() 
  theme_sankey(base_size = 18) +
  labs(x = NULL) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = .25)) +
  ggtitle("Virus–host linkages in IBS")


IBS


HC<-ggplot(df_long_HC, aes(x = x, next_x = next_x, 
                         node = node, next_node = next_node, 
                         fill = factor(x),
                         label = node,)) +
  geom_sankey(color = "grey", # node.color=, flow.color=
              #fill = "brown", # node.fill=, flow.fill=
              flow.alpha = 0.6,
              # space = 1,
              width = 0.1) +
  geom_sankey_text(size = 4, vjust = 0.2, hjust = -0.5,
                   color = "black", fontface = "bold") +
  scale_fill_manual(values = colors)+#ggsci::scale_fill_npg() + #  +   # 
  theme_sankey(base_size = 18) +
  labs(x = NULL) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = .25)) +
  ggtitle("Virus–host linkages in HC")


HC

ggarrange(IBS,HC, nrow=2, heights = c(1.5,1))

ggsave('viral-host_link_sankey.pdf',width = 14,height = 12)










