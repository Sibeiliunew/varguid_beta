source("./leash2.0.7.R")
source("./VarGuid20240626.R")
#source("./organized code:data/functions.R")
library(glmnet)
library(tidyverse)
library(MASS)
library(xtable)
library(scales)
library(faux)
library(olsrr)
library(caTools)
library(caret)
library(pheatmap)
library(dendsort)
library(egg)
library(gridExtra)
sort_hclust <- function(...) as.hclust(dendsort(as.dendrogram(...)))
breaks=seq(0,20,by=0.25)

#genes=load("~/Documents/Dissertation/varguid/PAM50.RData")

#### The heatmap
p50=as.data.frame(cbind(topgene,outcome)) %>% drop_na(outcome) %>% arrange(desc(outcome))
p20133=as.data.frame(cbind(genes,outcome)) %>% drop_na(outcome)

######

c1=sort_hclust(hclust(dist(t(p50[,-51])))) ## col order
r1=sort_hclust(hclust(dist(p50[,-51]))) ## row order

pheatmap(as.matrix(p50[,-51]),
         fontsize_col = 10,
         fontsize_row = 10,
         display_numbers = F,
         number_color = "black", 
         cluster_cols = c1,
         breaks = breaks,
         cluster_rows = F,
         color=colorRampPalette(c("green", "black", "red"))(length(breaks)),
         fontsize_number = 6,#
         border_color = "black",
         legend=T,
         show_colnames = T, show_rownames = F,
         main=c("50 Gene expression based on outcome decresing row order"))



# p50=p50 %>% dplyr::select(col1)
# 
# d1_1=list(p50[1:12,-51],p50[13:24,-51],
#         p50[25:36,-51],p50[37:49,-51])
# t1_1=c("Gene expression based on outcome decresing row order")
# 
# res1_1=p_heatmap(data=d1_1 ,title=t1_1,col_order=c1)
# grid.arrange(arrangeGrob(grobs= res1_1,ncol=1))
# 

########### in p20133 data,use varguid lasso to select genes
set.seed(2024)
o_p20133 <- lmv(X =as.matrix(p20133[,1:20133]) , Y = unlist(p20133$outcome), lasso = TRUE)
m=as.data.frame(as.matrix(o_p20133$beta)) %>% filter(s0>0)
select_gene=rownames(m) # 34

p34=p20133 %>% dplyr::select(select_gene,outcome)%>% arrange(desc(outcome))
c2=sort_hclust(hclust(dist(t(p34[,-35]))))
breaks2=seq(0,15,by=0.25)
pheatmap(p34[,-35],
         fontsize_col = 10,
         fontsize_row = 10,
         display_numbers = F,
         number_color = "black", 
         cluster_cols = c2,
         breaks = breaks2,
         cluster_rows = F,
         color=colorRampPalette(c("green", "black", "red"))(length(breaks2)),
         fontsize_number = 6,#
         border_color = "black",
         legend=T,
         show_colnames = T, show_rownames = F,
         main=c("Var-guild selected 34 Gene expression based on outcome decresing row order"))



  ########### in p20133 data,use traditional lasso to select genes
  set.seed(2024)
  o_p20133_2 <- cv.glmnet(x =as.matrix(p20133[,1:20133]) , y = p20133$outcome,alpha = 1,lambda =exp(seq(-1,1,length=100) ))
  plot(o_p20133_2)
  m2=as.data.frame(as.matrix(coef(o_p20133_2, s = "lambda.min"))) %>% filter(s1>0)
  select_gene2=rownames(m2) # 16

  intersect(select_gene,select_gene2)
  
  p16=p20133 %>% dplyr::select(select_gene2,outcome) %>%arrange(desc(outcome))
  c3=sort_hclust(hclust(dist(t(p16[,-17]))))
  

 ############################

  # Reorder selected_gene2 to match the order in selected_gene
  select_gene2_ordered <- select_gene2[match(c0, select_gene2, nomatch = 0)]
  
  # Remove NA values resulting from genes in selected_gene that are not in selected_gene2
  select_gene2_ordered <- select_gene2_ordered[select_gene2_ordered != ""]
  
  #pheatmap(p16[,c(select_gene2_ordered,"SPIRE2")],
  pheatmap(p16[,c("GALNT9", "SPANXB1","NBPF4","NBPF6","HUS1B","SYNGR4","ABHD16B","SPIRE2","NUDT8","LOC100506870","AIFM3",
                  "CDK2AP2","HDAC10" ,"CHST12","IER5L","ELF3" )],
           fontsize_col = 10,
           fontsize_row = 10,
           display_numbers = F,
           number_color = "black", 
           cluster_cols = F,
           breaks = breaks2,
           cluster_rows = F,
           color=colorRampPalette(c("green", "black", "red"))(length(breaks2)),
           fontsize_number = 6,#
           border_color = "black",
           legend=T,
           show_colnames = T, show_rownames = F,
           main=c("Lasso selected 16 Gene expression based on outcome decresing row order"))
  

  