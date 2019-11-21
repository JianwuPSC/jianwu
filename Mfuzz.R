#Mfuz为模糊聚类算法的一种
#模糊聚类又叫做软聚类，相比“硬聚类”，其最大的特点是允许同一数据属于多个不同的类
#用0-1的数字表示与所有分类的隶属度

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager", update=F)
if (!requireNamespace("Mfuzz", quietly = TRUE))
  BiocManager::install("Mfuzz", update=F)

library(Mfuzz)
library(RColorBrewer)

rm(list=ls())
setwd('D:/cluster')

color=colorRampPalette(rev(brewer.pal(n=11,'Spectral')))(100)

#加载数据
data <- read.delim(header=T,file='data/mfuzz.exp.txt',row.names = 1,sep="\t",na.strings = "-",check.names=F)

n_clust=9  #指定分类数

#将矩阵转换为ExpressionSet类型，用于Mfuzz分类
eset <- new("ExpressionSet",exprs = as.matrix(data))

eset <- filter.std(eset,min.std=0.3,visu = F) # 根据标准差去除样本间差异太小的基因
eset <- standardise(eset) #标准化，使每个基因的均值为0，标准差为1
cl <- mfuzz(eset, c = n_clust, m = mestimate(eset) )  #m为模糊指数

#输出
write.table(cl$membership,file='mfuzz.membership.txt',col.names = NA,quote=F,sep="\t")
write.table(cl$cluster,file='mfuzz.cluster.txt',col.names = F,quote=F,sep="\t")


# 画图，每个cluster单独画
pdf('mfuzz.sep.pdf',h=8,w=10)
mfuzz.plot2(
  eset,
  cl, 
  xlab='',          #x轴名称
  ylab='Expression', #y轴名称
  centre=T,    #绘制中心线
  time.labels=colnames(data),
  colo='fancy', 
#  colo=color,   #也可以自定义颜色，提示警告可以忽略
  x11=F) 
dev.off()


# 画图，所有cluster画到一张图上
pdf('mfuzz.onepage.pdf',h=8,w=10)
mfuzz.plot2(
  eset,
  cl,
  xlab='',
  ylab='Expression',
  mfrow=c(3,3),
  centre=T,
  time.labels=colnames(data),
  colo='fancy', 
#  colo=color,   #也可以自定义颜色，注意，使用自定义时提示警告，可以忽略
  x11=F) 
dev.off()
