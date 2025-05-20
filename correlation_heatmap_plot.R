library(ggplot2)
library(dplyr)
library(psych)
library(corrplot)

rm(list=ls())


## loading data1
df1 <- read.csv("rIBS/bac_rIBS_wide.csv",row.names = 1)
df1_list <- colnames(df1)

## loading data2
df2 <- read.csv("rIBS/phage_rIBS_wide.csv",row.names = 1)
df2_list <- colnames(df2)

filtered_data <- df1 %>%
  select(one_of(df2_list))
filtered_data <- as.data.frame(t(filtered_data))



correlation <- corr.test(filtered_data,filtered_SBM,method="spearman")


cor_matrix <- correlation$r
cor_matrix <- na.omit(cor_matrix)
p_matrix <- correlation$p
p_matrix <- na.omit(p_matrix)

# creat blank p_matrix_sig and cor_matrix_sig
p_matrix_sig <- data.frame()
cor_matrix_sig <- data.frame()

for (i in 1:nrow(p_matrix)) {

  if (any(p_matrix[i, ] < 0.05)) {
    
    p_matrix_sig <- rbind(p_matrix_sig, data.frame(p_matrix[i, , drop = FALSE]))
    cor_matrix_sig <- rbind(cor_matrix_sig, data.frame(cor_matrix[i, , drop = FALSE]))
  }
}

write.csv(cor_matrix,"rIBS/cor_matrix.csv") 
write.csv(p_matrix,"rIBS/p_matrix.csv") 
write.csv(cor_matrix_sig,"rIBS/cor_matrix_sig.csv") 
write.csv(p_matrix_sig,"rIBS/p_matrix_sig.csv") 




######reload####

p_matrix_sig <- as.matrix(p_matrix_sig)


pdf("rIBS_bac-phage_interaction.pdf", width = 8, height = 8) 
corrplot(cor_matrix_sig,is.corr = T,method = "color", type = "lower",
         tl.cex = 0.6,tl.srt = 90,tl.col='black',pch.cex = 0.6,
         p.mat = p_matrix_sig, insig = 'label_sig', sig.level = c(0.05), 
         col = colorRampPalette(c( "#2878B5","white","#C82323"))(100),
         col.lim = c(-1,1),addgrid.col = 'black',
         cl.pos = "b",cl.ratio=0.05,mar = c(0, 2, 0, 2))

dev.off()
 