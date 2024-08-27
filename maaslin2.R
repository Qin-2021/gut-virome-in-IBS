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
  output = "massline_output_IBS_depression_anxiety_21June2024_votu", 
  fixed_effects = "Class",    # A D a&D
  reference=c("Class,HC"),
  correction = "BH")   # 在input_metadata的class一列，有4个分类变量，分别为Normal, A&D,A,D , 此处指定HC为reference






###########################################################################################################################################

coef_result<-fit_data$results
coef1<-as.data.frame(coef_result%>% filter(name=='ClassA&D' & abs(coef)> 0.8 & pval<0.05) %>% arrange(coef))  #
coef1$feature<-factor(coef1$feature,levels = coef1$feature)


ggplot(coef1,aes(x=feature,y=coef,fill=coef))+
  geom_bar(stat='identity') +  scale_fill_gradient2(high="#AB8DD9",low= '#3381b8')+
  coord_flip()+theme_classic()+
  ggtitle(label = 'Viral taxa associated with Anxiety and Depression ')+                                    ###  revise
  labs(y='Linear model coefficient',x=NULL)+
  geom_hline(yintercept = 0) +
  theme(axis.line.y = element_blank(),
        axis.ticks.y=element_blank())+
  theme(axis.text = element_text(size=12,color="black"), axis.title=element_text(size=16,face = "bold"),
        legend.text=element_text(size=12), legend.title=element_text(size=16))+
  theme(plot.title = element_text(color = "Black", size = 12, face = "bold"))



ggsave('maaslin2.pdf',width = 16,height = 10)



######################################################################################################################################
library(pheatmap)
library(reshape2)
library(tibble)
head(coef_result)
library(RColorBrewer)
myColor<-colorRampPalette(rev(brewer.pal(n = 7, name ="RdYlBu")))(60)


pheatmap_plot<-pheatmap(mat=coef_result_for_heat,
        
         color=myColor,
         width = 10,height = 6,
         silent=FALSE,
         cluster_cols =F,
         cluster_rows =T,
         show_rownames = F)



save_pheatmap <- function(x, filename, width=7, height=7){
  stopifnot(!missing(x))
  stopifnot(!missing(filename))
  if(grepl(".png",filename)){
    png(filename, width=width, height=height, units = "in", res=300)
    grid::grid.newpage()
    grid::grid.draw(x$gtable)
    dev.off()
  }
  else if(grepl(".pdf",filename)){
    pdf(filename, width=width, height=height)
    grid::grid.newpage()
    grid::grid.draw(x$gtable)
    dev.off()
  }
  else{
    print("Filename did not contain '.png' or '.pdf'")
  }
}


save_pheatmap(pheatmap_plot, "maaslin2_heatmap.png")






