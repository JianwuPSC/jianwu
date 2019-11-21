sri = read.csv("SraRunInfo.csv",        
               stringsAsFactors=FALSE) 
keep = grep("CG8144|Untreated-",    
            sri$ LibraryName) 
 sri = sri[keep,] 
 fs = basename(sri$download_path)
 for(i in 1:nrow(sri))  
   download.file(sri$download_path[i], fs[i])
sri = read.csv("SraRunInfo.csv",        
                stringsAsFactors=FALSE)
stopifnot( all(file.exists(fs)) ) # assure FTP download was successful
for(f in fs) { cmd = paste("fastq-dump --split-3", f) 
   cat(cmd,"\n") 
   system(cmd)
   }
library("ShortRead")
fqQC  =  qa(dirPath = "SRR404309.fastq", type = "fastq")
report(fqQC, type = "html", dest = "fastqQAreport") 
sri$LibraryName  =  gsub("S2_DRSC_","",sri$LibraryName) # trim label
samples  =  unique(sri[,c("LibraryName","LibraryLayout")])
for(i in seq_len(nrow(samples))) { 
  rw  =  (sri$LibraryName == samples$LibraryName[i]) 
  if(samples$LibraryLayout[i] == "PAIRED") {  
    samples$fastq1[i]  =  paste0(sri$Run[rw],"_1.fastq",collapse = ",") 
    samples$fastq2[i]  =  paste0(sri$Run[rw],"_2.fastq",collapse = ",")  
  } else{ 
    samples$fastq1[i]  =  paste0(sri$Run[rw],".fastq",collapse = ",")  
    samples$fastq2[i]  =  ""  }
}

samples$condition = "CTL"
samples$condition[grep("RNAi",samples$LibraryName)]  =  "KD"
samples$shortname  =  paste(substr(samples$condition,1,2),  
                            substr(samples$LibraryLayout,1,2),                 
                          seq_len(nrow(samples)), sep = ".")
samples = read.csv("samples.csv",        
               stringsAsFactors=FALSE)  
gf  =  "Drosophila_melanogaster.BDGP5.70.gtf"
bowind  =  "Dme1_BDGP5_70"
cmd = with(samples, paste("tophat2 -G", gf, "-p 5 -o",                                       LibraryName, bowind, fastq1, fastq2))
cmd
for(i in seq_len(nrow(samples))) { 
  lib = samples$LibraryName[i] 
  ob =file.path(lib, "accepted_hits.bam")
# sort by name, convert to SAM for htseq-count 
  cat(paste0("samtools sort -n ",ob," ",lib,"_sn"),"\n") 
  cat(paste0("samtools view -o ",lib,"_sn.sam ",lib,"_sn.bam"),"\n")
# sort by position and index for IGV 
  cat(paste0("samtools sort ",ob," ",lib,"_s"),"\n") 
  cat(paste0("samtools index ",lib,"_s.bam"),"\n\n") 
  }
samples$countf  =  paste(samples$LibraryName, "count", sep = ".")
gf  =  "Drosophila_melanogaster.BDGP5.70.gtf"
cmd  =  paste0("htseq-count -s no -a 10 ", samples$LibraryName, 
               "_sn.sam ", gf," > ", samples$countf)
cmd

library("edgeR")
counts = readDGE(samples$countf)$counts 
noint  =  rownames(counts) %in%   
       c("no_feature","ambiguous","too_low_aQual",    
         "not_aligned","alignment_not_unique")
cpms  =  cpm(counts)
keep  =  rowSums(cpms> 1)  >= 3 & !noint
counts  =  counts[keep,] 
colnames(counts)  =  samples$shortname
head( counts[,order(samples$condition)], 5 )

#edger—simple design
counts =read.csv("counts.csv",        
                        stringsAsFactors=FALSE)

counts =  data.matrix(counts)
counts
str( data.matrix(counts))
d = DGEList(counts = counts, group = samples$condition,genes = NULL)
d  =  calcNormFactors(d)
plotMDS(d, labels = samples$shortname,
        col = c("darkgreen","blue")[factor(samples$condition)]) 
d  =  estimateCommonDisp(d)
d  =  estimateTagwiseDisp(d)
plotMeanVar(d, show.tagwise.vars = TRUE, NBline = TRUE)
plotBCV(d) 
de  =  exactTest(d, pair = c("CTL","KD"))


#edger—complex design
design  =  model.matrix( ~ Library.Layout  +  condition, samples)
design 
d2  =  estimateGLMTrendedDisp(d, design)
d2  =  estimateGLMTagwiseDisp(d2, design) 
f  =  glmFit(d2, design)
de  =  glmLRT(f, coef = 3)
tt  =  topTags(de, n = nrow(d))
head(tt$table)
nc  =  cpm(d, normalized.lib.sizes = TRUE)
rn  =  rownames(tt$table)
head(nc[rn,order(samples$condition)],5)
deg  =  rn[tt$table$FDR  <  .05]
plotSmear(d, de.tags = deg) 
write.csv(tt$table, file = "toptags_edgeR.csv") 

#Deseq—simple design

BiocManager::install("DESeq")
samplesDESeq  =  with(samples,         
                  data.frame(shortname  =  I(shortname), 
                  countf  =  I(countf), 
                  condition   =  condition, 
                  LibraryLayout  =  Library.Layout)) 
samplesDESeq
library("DESeq")
cds  =  newCountDataSetFromHTSeqCount(samplesDESeq) 
cds  =  estimateSizeFactors(cds) 
sizeFactors(cds)
cdsB  =  estimateDispersions(cds, method = "blind")
vsd  =  varianceStabilizingTransformation(cdsB)
p  =  plotPCA(vsd, intgroup = c("condition","LibraryLayout")) 
cds  =  estimateDispersions(cds) 
plotDispEsts(cds) (viii) Perform the test for differential expression by using nbinomTest, as follows:
res  =  nbinomTest(cds,"CTL","KD") (ix)  #Given the table of differential expression results, use plotMA to display differential expression (log-fold changes)  versus expression strength (log-average read count), as follows (Fig. 6b):
plotMA(res) (x) 
#Inspect the result tables of significantly upregulated and downregulated genes, at a 10% false discovery rate (FDR)  as follows:
resSig  =  res[which(res$padj  <  0.1),]
head( resSig[ order(resSig$log2FoldChange, decreasing = TRUE), ] )
head( resSig[ order(resSig$log2FoldChange, decreasing = FALSE), ] )
table( res$padj  <  0.1 )
write.csv(res, file = "res_DESeq.csv")
hist(res$pval, breaks = 100) 

#Deseq—complex design 
cds  =  estimateDispersions(cds, method  =  "pooled-CR",            
        modelFormula  =  count ~ LibraryLayout  +  condition) 
fit1  =  fitNbinomGLMs(cds, count ~ LibraryLayout  +  condition)
fit0  =  fitNbinomGLMs(cds, count ~ LibraryLayout)
pval  =  nbinomGLMTest(fit1, fit0)
padj  =  p.adjust(pval, method = "BH")
res  =  cbind(fit1, pval = pval, padj = padj)
head( resSig[ order(resSig$conditionKD, decreasing = TRUE), ] )
head( resSig[ order(resSig$conditionKD, decreasing = FALSE), ] )









