setwd("C:/Users/pcbar/Desktop/computer_learning")
options(stringsAsFactors = FALSE)
control1<-read.table("SRR404315.count",sep = "\t",col.names = c("gene_id","control1"))
control2<-read.table("SRR404309.count",sep = "\t",col.names = c("gene_id","control2"))
treat1<-read.table("SRR404311.count",sep = "\t",col.names = c("gene_id","treat1"))

treat2<-read.table("SRR404313.count",sep = "\t",col.names = c("gene_id","treat2"))
raw_count <- merge(merge(control1, control2, by="gene_id"), merge(treat1, treat2, by="gene_id"))
head(raw_count)
raw_count_filt = raw_count[-1:-5,]
ENSEMBL = gsub("\\.\\d*","",raw_count_filt$gene_id)
row.names(raw_count_filt) <- ENSEMBL
head(raw_count_filt)
condition <- factor(c(rep("control",2),rep("treat",2)), levels = c("control","treat"))
condition
mycounts=raw_count_filt[,2:5]
mycounts
colData <- data.frame(row.names=colnames(mycounts), condition)
colData
library(DESeq2)
dds = DESeqDataSetFromMatrix(countData=mycounts, 
                             colData=colData, 
                             design= ~ condition)
dds
dds = DESeq(dds)
res = results(dds, contrast=c("condition", "control", "treat"))
res = res[order(res$pvalue),]
head(res)
summary(res)
write.csv(res,file="All_results.csv")
table(res$padj<0.01)
diff_gene_deseq2 <-subset(res, padj < 0.01 & abs(log2FoldChange) > 1)
dim(diff_gene_deseq2)
head(diff_gene_deseq2)
write.csv(diff_gene_deseq2,file= "HCC_DEG_0.05_2.csv")
dim(diff_gene_deseq2)
head(diff_gene_deseq2)
#MA plot
plotMA(res,ylim=c(-2,2))
topGene <- rownames(res)[which.min(res$padj)]
with(res[topGene, ], {
  points(baseMean, log2FoldChange, col="dodgerblue", cex=6, lwd=2)
  text(baseMean, log2FoldChange, topGene, pos=2, col="dodgerblue")
})
#shirnked
res_order<-res[order(row.names(res)),]
res = res_order
res.shrink <- lfcShrink(dds, contrast = c("condition","treat","control"), res=res)
plotMA(res.shrink, ylim = c(-5,5))
topGene <- rownames(res)[which.min(res$padj)]
with(res[topGene, ], {
  points(baseMean, log2FoldChange, col="dodgerblue", cex=2, lwd=2)
  text(baseMean, log2FoldChange, topGene, pos=2, col="dodgerblue")
})
#identify

idx <- identify(res$baseMean, res$log2FoldChange)
rownames(res)[idx]
#plotcounts

plotCounts(dds, gene=which.min(res$padj), intgroup="condition", returnData=TRUE)
plotCounts(dds, gene="ENSG00000130649", intgroup="condition", returnData=FALSE)
#boxplot

plotCounts(dds, gene="ENSG00000130649", intgroup="condition", returnData=TRUE) %>% 
ggplot(aes(condition, count)) + geom_boxplot(aes(fill=condition)) + scale_y_log10() + ggtitle("CYP2E1")

#pointplot

d <- plotCounts(dds, gene="ENSG00000130649", intgroup="condition", returnData=TRUE)
ggplot(d, aes(x=condition, y=count)) + 
geom_point(aes(color= condition),size= 4, position=position_jitter(w=0.5,h=0)) + 
scale_y_log10(breaks=c(25,100,400))+ ggtitle("CYP2E1")

##3最小padj

d <- plotCounts(dds, gene=which.min(res$padj), intgroup="condition", 
                returnData=TRUE)
ggplot(d, aes(x=condition, y=count)) + 
  geom_point(position=position_jitter(w=0.1,h=0)) + 
  scale_y_log10(breaks=c(25,100,400))




summary(results(dds,alpha = 0.05))
