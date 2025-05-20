
library(vegan)
library(ggpubr)
library(reshape2)
library(ggsci)
library(dplyr)
library(ggside)
options(stringsAsFactors=F)

df<-read.delim("PCoA.csv",sep=",", row.names = 1)
group=read.table("IBS_C2_HC_labels_0429.csv", sep=",", header=TRUE)#, row.names = 1)

colnames(group)[2]<-"group"
sd<-group
sample_list<-intersect(sd$sample,colnames((df)))   

dataT<-as.data.frame(t(df))
dist <- vegdist(dataT, method="bray")
dist <- as.matrix(dist)
adist<-as.dist(dist)




rownames(sd) <- as.character(sd[,1])
pc_num <-c(1,2)
pc_x <- pc_num[1]
pc_y <- pc_num[2]
pcoa <- cmdscale(dist, k=3, eig=TRUE)  
pc12 <- pcoa$points[,pc_num]      

pc <- round(pcoa$eig/sum(pcoa$eig)*100,digits = 2) 
pc
pc12 <- as.data.frame(pc12)
colnames(pc12) <- c("pc_x","pc_y")
pc12['sample'] <- rownames(pc12)
colnames(sd)[1:2] <- c("sample","group")
sd$group<-factor(sd$group,levels=sd$group[!duplicated(sd$group)])
pc12 <- merge(pc12,sd,by="sample")



pc12$group<-factor(pc12$group,levels=levels(sd$group))

pc_aver<-as.data.frame(pc12 %>% dplyr::select(-sample) %>% group_by(group)%>%summarise_all(funs(mean)))


mycols<-c('#1597A5','#5e4fa2','#FFC24B', '#9970ab','#e7d4e8','#3288bd',"#66c2a5") 


ADONIS<-adonis2(dist~sd$group)   
TEST<-ADONIS$`Pr(>F)`[1]
R<-round(ADONIS$R2[1],3)

R2adonis<-round(ADONIS$R2[1],digits = 3)






########################################################### PCoA  plot##########################################################

pc12$group<-factor(pc12$group,levels=c("C1","C2"),labels=c("C1 IBS","C2 IBS"),order=TRUE)

pc_aver$group<-factor(pc_aver$group,levels=c("C1","C2"),labels=c("C1 IBS","C2 IBS"),order=TRUE)

p<-ggscatter(pc12, x = "pc_x", y = "pc_y",color = 'group',
             fill = "group", shape = "group", palette = mycols, size=3,
             ellipse = F,# conf.int.level = 0.95,
             alpha=0.5,
             mean.point = F,
             star.plot = TRUE,star.plot.lty = 1,star.plot.lwd = 0.2)+
  geom_point(data=pc_aver,aes(x=pc_x,y=pc_y,fill=group),size=6,shape=21)+
  theme(legend.position = "right",
        legend.title = element_blank(),
        panel.border = element_rect(color = "black",size = 1.0,fill = NA),
        text = element_text(size=25))+ 
  annotate('text',x=0.4,y=-0.24,label=paste0('p=',TEST),size=7)+
  annotate('text',x=0.4,y=-0.28,label=paste0("R=",R),size=7)+
  ggside::geom_xsidedensity( aes(fill = group), alpha = 0.5, show.legend = FALSE)+
  ggside::geom_ysidedensity( aes(fill = group), alpha = 0.5, show.legend = FALSE) + ggside::theme_ggside_void() 

p

data_pcoa <- cmdscale(dist,k=(nrow(dataT)-1),eig=TRUE,add=TRUE) 
data_pcoa_eig <- pcoa$eig                     
data_pcoa_exp <- data_pcoa_eig/sum(data_pcoa_eig)  
pcoa1 <- paste(round(100*data_pcoa_exp[1],2),'%') 
pcoa2 <- paste(round(100*data_pcoa_exp[2],2),'%') 
pcoa_exp<-as.data.frame(data_pcoa_exp)


cluster_PCoA<-p+ylab(paste0("PCoA",pc_y,"(","12.06","%",")"))+
  xlab(paste0("PCoA",pc_x,"(","14.91","%",")"))+ scale_shape_manual(values = rep(23,length(levels(pc12$group)))) 

cluster_PCoA
ggsave('IBS_cluster1vsCluster2_PCoA_9May2025.pdf',height = 6, width=8)  

