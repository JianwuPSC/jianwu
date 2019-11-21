#安装devtools，并使用devtools安装pheatmap，已安装的可以不用执行本步骤
if(!requireNamespace('devtools',quietly = TRUE))
  install.packages('devtools')
if(!requireNamespace('pheatmap',quietly = TRUE))
  devtools::install_github("raivokolde/pheatmap")
if(!requireNamespace('RColorBrewer',quietly = TRUE))
  install.packages('RColorBrewer')


library(RColorBrewer)
library(pheatmap)

rm(list=ls())
setwd('D:/cluster')# 设置工作目录

# 设置配色
cc = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))
#cc = colorRampPalette(c('red','yellow','green','#EE7621'))

#============ 读入文件 =======================
matrix=read.delim(file='data/mfuzz.exp.txt',header=T,row.names = 1,sep="\t",check.names =F)
#matrix=matrix[1:10,]

#============标准化，缩小显示范围 =========== 
#方法1. 取log： log2(fpkm+1)
matrix=log2(matrix+1)
#方法2. 取z-score，即每个表达值减均值除以标准差
matrix=t(scale(t(matrix)))

# 绘制热图
h=pheatmap(matrix,main = "expression heatmap", #设置主标题
         na_col = "grey",color=cc(100),        #设置颜色
         cluster_rows =T,cluster_cols = F,     #是否对行/列进行聚类
         show_rownames=F,show_colnames =T,     #是否显示行/列名
#         cellwidth = 20, cellheight = 20,     #固定每个cell的宽与高
         display_numbers=F,                    #是否显示每个单元格的值
         legend=T,                              #是否显示legend
         )

png('heatmap.png',width=2000,height=2000,res=300,units="px")
h
dev.off()
