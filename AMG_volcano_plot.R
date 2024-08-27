
library ("phyloseq")
library("ggplot2")
library(vegan) 
library(tidyverse) 
library("ggpubr")
library("dplyr")
library(RColorBrewer)
library(viridis)


################################################################# Deseq 2 and  volcano ##########################


df<-read.delim("AMG_table.csv",sep=",")

gp<-read.delim("group.csv",sep=",")



library(DESeq2)
gp$group<- as.factor(gp$group)

dds <- DESeqDataSetFromMatrix(countData = df+1,           
                              colData = gp_select_new,
                              design = ~ group)

dds <- DESeq(dds) 
#resultsNames(dds) # lists the coefficients
res <- results(dds, name=resultsNames(dds)[2])
DESeq.res=subset(res)
DESeq.res=as.data.frame(DESeq.res)

DESeq.res<-as.data.frame(cbind(AMG=df$AMG,DESeq.res))



df<-DESeq.res

df <- df %>%
  mutate(change = ifelse(log2FoldChange > 0, "Up", ifelse(log2FoldChange < 0, "Down", NA)))


library(tidyverse)
library(ggrepel)
library(ggfun)
library(grid)


ggplot(data = df) + 
  geom_point(aes(x = log2FoldChange, y = -log10(padj), 
                 color = log2FoldChange,
                 size = -log10(padj))) + 
  geom_point(data =  df %>%
               tidyr::drop_na() %>%
               dplyr::filter(change != "Normal") %>%
               dplyr::arrange(desc(-log10(padj))) %>%
               dplyr::slice(1:20),
             aes(x = log2FoldChange, y = -log10(padj),
                 # fill = log2FoldChange,
                 size = -log10(padj)),
             shape = 21, show.legend = F, color = "#000000") +
geom_text_repel(data =  df %>% 
                  tidyr::drop_na() %>% 
                  dplyr::filter(change != "Normal") %>%
                  dplyr::arrange(desc(-log10(padj))) %>%
                  dplyr::slice(1:15) %>%
                  dplyr::filter(change == "Up"),
                aes(x = log2FoldChange, y = -log10(padj), label = AMG),
                box.padding = 0.5,
                nudge_x = 0.5,
                nudge_y = 0.2,
                segment.curvature = -0.2,
                segment.ncp = 3,
                # segment.angle = 10,
                direction = "y", 
                hjust = "left",size=5
) + 
  geom_text_repel(data =  df %>% 
                    tidyr::drop_na() %>% 
                    dplyr::filter(change != "Normal") %>%
                    dplyr::arrange(desc(-log10(padj))) %>%
                    dplyr::slice(1:15) %>%
                    dplyr::filter(change == "Down"),
                  aes(x = log2FoldChange, y = -log10(padj), label = AMG),
                  box.padding = 0.5,
                  nudge_x = -0.2,
                  nudge_y = 0.2,
                  segment.curvature = -0.1,
                  segment.ncp = 3,
                  segment.angle = 20,
                  direction = "y", 
                  hjust = "right",size=5
  ) + 
  scale_color_gradientn(colours = c("#3288bd", "#66c2a5","#ffffbf", "#f46d43", "#9e0142"),
                        values = seq(0, 1, 0.2)) +
  scale_fill_gradientn(colours = c("#3288bd", "#66c2a5","#ffffbf", "#f46d43", "#9e0142"),
                       values = seq(0, 1, 0.2)) +
geom_vline(xintercept = c(-log2(1.5), log2(1.5)), linetype = 2) +
  geom_hline(yintercept = -log10(0.05), linetype = 4) + 
  scale_size(range = c(1,7)) + 
  ggtitle(label = "aIBS vs HC",
          subtitle = " ") +  
    # xlim(c(-3, 3)) ylim(c(-1, 90)) 
  theme_bw() + 
  theme(panel.grid = element_blank(),
        legend.background = element_roundrect(color = "#808080", linetype = 1),
        axis.text = element_text(size = 13, color = "#000000"),
        axis.title = element_text(size = 15),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)
  ) + 
  annotate(geom = "text", x = 5.5, y = 0.23, label = "p = 0.05", size = 5) + 
  coord_cartesian(clip = "off") + 
  annotation_custom(
    grob = grid::segmentsGrob(
      y0 = unit(-10, "pt"),
      y1 = unit(-10, "pt"),
      arrow = arrow(angle = 45, length = unit(.2, "cm"), ends = "first"),
      gp = grid::gpar(lwd = 3, col = "#74add1")
    ), 
    xmin = -5, 
    xmax = -1,
    ymin = 35,
    ymax = 35
  ) +
  annotation_custom(
    grob = grid::textGrob(
      label = "Down",
      gp = grid::gpar(col = "#74add1")
    ),
    xmin = -5, 
    xmax = -1,
    ymin = 35,
    ymax = 35
  ) +
  annotation_custom(
    grob = grid::segmentsGrob(
      y0 = unit(-10, "pt"),
      y1 = unit(-10, "pt"),
      arrow = arrow(angle = 45, length = unit(.2, "cm"), ends = "last"),
      gp = grid::gpar(lwd = 3, col = "#d73027")
    ), 
    xmin = 5, 
    xmax = 1,
    ymin = 35,
    ymax = 35            
  ) +
  annotation_custom(
    grob = grid::textGrob(
      label = "Up",
      gp = grid::gpar(col = "#d73027")
    ),
    xmin = 5, 
    xmax = 1,
    ymin = 35,
    ymax = 35
  ) 


####----save result----####

ggsave(filename = "volcano_plot.pdf", height = 8.5, width = 9)





