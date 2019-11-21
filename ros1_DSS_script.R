CpG_wt11 = read.csv("CpG_wt_1.cov",header = T,sep = "\t",quote = "",check.names = F) 
#CpG_wt11 <- read_tsv( ".cov", col_names = F ) 
head( CpG_wt11 )
a = CpG_wt11[ ,1]
b = CpG_wt11[ ,2]
c = CpG_wt11[ ,5]
d = CpG_wt11[ ,6]
CpG_wt11 = data.frame(a,b,c+d,c)
names(CpG_wt11) = c("chr","pos","N" ,"X")
head( CpG_wt11 )
CpG_wt21 = read.csv("CpG_wt_2.cov",header = T,sep = "\t",quote = "",check.names = F) 
#CpG_wt21 <- read_tsv( "../wu_jian/CpG_wt21_CHH.bismark.cov.gz", col_names = F ) 
a = CpG_wt21[ ,1]
b = CpG_wt21[ ,2]
c = CpG_wt21[ ,5]
d = CpG_wt21[ ,6]
CpG_wt21 = data.frame(a,b,c+d,c)
names(CpG_wt21) = c("chr","pos","N" ,"X")
head( CpG_wt21 )
CpG_treat11 = read.csv("CpG_ros1-14_1.cov",header = T,sep = "\t",quote = "",check.names = F) 
#CpG_treat11 <- read_tsv( "../wu_jian/Mu1_CHH.bismark.cov.gz", col_names = F ) 
a = CpG_treat11[ ,1]
b = CpG_treat11[ ,2]
c = CpG_treat11[ ,5]
d = CpG_treat11[ ,6]
CpG_treat11 = data.frame(a,b,c+d,c)
names(CpG_treat11) = c("chr","pos","N" ,"X")
head( CpG_treat11 )
CpG_treat21 = read.csv("CpG_ros1-14_2.cov",header = T,sep = "\t",quote = "",check.names = F) 
#CpG_treat21 <- read_tsv( "../wu_jian/Mu2_CHH.bismark.cov.gz", col_names = F ) 
a = CpG_treat21[ ,1]
b = CpG_treat21[ ,2]
c = CpG_treat21[ ,5]
d = CpG_treat21[ ,6]
CpG_treat21 = data.frame(a,b,c+d,c)
names(CpG_treat21) = c("chr","pos","N" ,"X")
head( CpG_treat21 )
library( bsseq )
library( DSS )
BSobj1 <- makeBSseqData( list(CpG_wt11, CpG_wt21, CpG_treat11, CpG_treat21),
                        c("C1","C2", "N1", "N2") ) # start 14:25
BSobj1 

# Timing the call
start_time1 <- Sys.time()
start_time1 
dmlTest1.sm <- DMLtest(BSobj1, group1=c("C1", "C2"), group2=c("N1", "N2"), smoothing=TRUE)
Sys.time() - start_time1
head(dmlTest1.sm)

dml1 <- callDML(dmlTest1.sm, p.threshold=0.05)
head(dml1)
dmrs2 <-  callDMR(dmlTest1.sm, delta=0, p.threshold=0.0001,
        minlen=200, minCG=2, dis.merge=100, pct.sig=0.5)
head(dmrs2)
showOneDMR(dmrs2[1,], BSobj1)
write.csv(dml1,file="ros1-14_CpG-DML.csv",quote = F)
write.csv(dmrs2,file="ros1-14_CpG-DMR.csv",quote = F)


CHG_wt11 = read.csv("CHG_wt_1.cov",header = T,sep = "\t",quote = "",check.names = F) 
#CHG_wt11 <- read_tsv( ".cov", col_names = F ) 
head( CHG_wt11 )
a = CHG_wt11[ ,1]
b = CHG_wt11[ ,2]
c = CHG_wt11[ ,5]
d = CHG_wt11[ ,6]
CHG_wt11 = data.frame(a,b,c+d,c)
names(CHG_wt11) = c("chr","pos","N" ,"X")
head( CHG_wt11 )
CHG_wt21 = read.csv("CHG_wt_2.cov",header = T,sep = "\t",quote = "",check.names = F) 
#CHG_wt21 <- read_tsv( "../wu_jian/CHG_wt21_CHH.bismark.cov.gz", col_names = F ) 
a = CHG_wt21[ ,1]
b = CHG_wt21[ ,2]
c = CHG_wt21[ ,5]
d = CHG_wt21[ ,6]
CHG_wt21 = data.frame(a,b,c+d,c)
names(CHG_wt21) = c("chr","pos","N" ,"X")
head( CHG_wt21 )
CHG_treat11 = read.csv("CHG_ros1-14_1.cov",header = T,sep = "\t",quote = "",check.names = F) 
#CHG_treat11 <- read_tsv( "../wu_jian/Mu1_CHH.bismark.cov.gz", col_names = F ) 
a = CHG_treat11[ ,1]
b = CHG_treat11[ ,2]
c = CHG_treat11[ ,5]
d = CHG_treat11[ ,6]
CHG_treat11 = data.frame(a,b,c+d,c)
names(CHG_treat11) = c("chr","pos","N" ,"X")
head( CHG_treat11 )
CHG_treat21 = read.csv("CHG_ros1-14_2.cov",header = T,sep = "\t",quote = "",check.names = F) 
#CHG_treat21 <- read_tsv( "../wu_jian/Mu2_CHH.bismark.cov.gz", col_names = F ) 
a = CHG_treat21[ ,1]
b = CHG_treat21[ ,2]
c = CHG_treat21[ ,5]
d = CHG_treat21[ ,6]
CHG_treat21 = data.frame(a,b,c+d,c)
names(CHG_treat21) = c("chr","pos","N" ,"X")
head( CHG_treat21 )
library( bsseq )
library( DSS )
BSobj2 <- makeBSseqData( list(CHG_wt11, CHG_wt21, CHG_treat11, CHG_treat21),
                        c("C1","C2", "N1", "N2") ) # start 14:25
BSobj2 

# Timing the call
start_time2 <- Sys.time()
start_time2 
dmlTest2.sm <- DMLtest(BSobj2, group1=c("C1", "C2"), group2=c("N1", "N2"), smoothing=TRUE)
Sys.time() - start_time2
head(dmlTest2.sm)

dml3 <- callDML(dmlTest2.sm, p.threshold=0.05)
head(dml3)
dmrs4 <-  callDMR(dmlTest2.sm, delta=0, p.threshold=0.0001,
        minlen=200, minCG=2, dis.merge=100, pct.sig=0.5)
head(dmrs4)
showOneDMR(dmrs2[1,], BSobj2)
write.csv(dml3,file="ros1-14_CHG-DML.csv",quote = F)
write.csv(dmrs4,file="ros1-14_CHG-DMR.csv",quote = F)


CHH_wt11 = read.csv("CHH_wt_1.cov",header = T,sep = "\t",quote = "",check.names = F) 
#CHH_wt11 <- read_tsv( ".cov", col_names = F ) 
head( CHH_wt11 )
a = CHH_wt11[ ,1]
b = CHH_wt11[ ,2]
c = CHH_wt11[ ,5]
d = CHH_wt11[ ,6]
CHH_wt11 = data.frame(a,b,c+d,c)
names(CHH_wt11) = c("chr","pos","N" ,"X")
head( CHH_wt11 )
CHH_wt21 = read.csv("CHH_wt_2.cov",header = T,sep = "\t",quote = "",check.names = F) 
#CHH_wt21 <- read_tsv( "../wu_jian/CHH_wt21_CHH.bismark.cov.gz", col_names = F ) 
a = CHH_wt21[ ,1]
b = CHH_wt21[ ,2]
c = CHH_wt21[ ,5]
d = CHH_wt21[ ,6]
CHH_wt21 = data.frame(a,b,c+d,c)
names(CHH_wt21) = c("chr","pos","N" ,"X")
head( CHH_wt21 )
CHH_treat11 = read.csv("CHH_ros1-14_1.cov",header = T,sep = "\t",quote = "",check.names = F) 
#CHH_treat11 <- read_tsv( "../wu_jian/Mu1_CHH.bismark.cov.gz", col_names = F ) 
a = CHH_treat11[ ,1]
b = CHH_treat11[ ,2]
c = CHH_treat11[ ,5]
d = CHH_treat11[ ,6]
CHH_treat11 = data.frame(a,b,c+d,c)
names(CHH_treat11) = c("chr","pos","N" ,"X")
head( CHH_treat11 )
CHH_treat21 = read.csv("CHH_ros1-14_2.cov",header = T,sep = "\t",quote = "",check.names = F) 
#CHH_treat21 <- read_tsv( "../wu_jian/Mu2_CHH.bismark.cov.gz", col_names = F ) 
a = CHH_treat21[ ,1]
b = CHH_treat21[ ,2]
c = CHH_treat21[ ,5]
d = CHH_treat21[ ,6]
CHH_treat21 = data.frame(a,b,c+d,c)
names(CHH_treat21) = c("chr","pos","N" ,"X")
head( CHH_treat21 )
library( bsseq )
library( DSS )
BSobj3 <- makeBSseqData( list(CHH_wt11, CHH_wt21, CHH_treat11, CHH_treat21),
                        c("C1","C2", "N1", "N2") ) # start 14:25
BSobj3 

# Timing the call


start_time3 <- Sys.time()
start_time3 
dmlTest3.sm <- DMLtest(BSobj3, group1=c("C1", "C2"), group2=c("N1", "N2"), smoothing=TRUE)
Sys.time() - start_time3
head(dmlTest3.sm)

dml5 <- callDML(dmlTest3.sm, p.threshold=0.05)
head(dml5)
dmrs6 <-  callDMR(dmlTest3.sm, delta=0, p.threshold=0.0001,
        minlen=200, minCG=2, dis.merge=100, pct.sig=0.5)
head(dmrs6)
showOneDMR(dmrs2[1,], BSobj3)
write.csv(dml5,file="ros1-14_CHH-DML.csv",quote = F)
write.csv(dmrs6,file="ros1-14_CHH-DMR.csv",quote = F)

