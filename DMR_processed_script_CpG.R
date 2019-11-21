library(dplyr)
idm = read.table("idm1-9_CpG-DMR.csv",header = T,sep = ",",quote = "",check.names = F)
head(idm)
ros = read.table("ros1-14_CpG-DMR.csv",header = T,sep = ",",quote = "",check.names = F)
head(ros)
duf6 = read.table("duf6_CpG-DMR.csv",header = T,sep = ",",quote = "",check.names = F)
head(duf6)


idm = idm [,-1]
idm = filter(idm, diff.Methy < -0)
head(idm)
ros = ros [,-1]
ros = filter(ros, diff.Methy < -0)
head(ros)
duf6 = duf6 [,-1]
head(duf6)
duf6 = filter(duf6, diff.Methy < -0)
head(duf6)
a_duf60 = length(duf6[,2])
b_ros0 =length(ros[,2])
c_idm0 = length(idm[,2])


idm1 = filter(idm, chr == "Chr1")
head(idm1)
duf61 = filter(duf6, chr == "Chr1")
head(duf61)
ros1 = filter(ros, chr == "Chr1")
head(ros1)



idm2 = filter(idm, chr == "Chr2")
head(idm2)
duf62 = filter(duf6, chr == "Chr2")
head(duf62)
ros2 = filter(ros, chr == "Chr2")
head(ros2)

idm3 = filter(idm, chr == "Chr3")
head(idm3)
duf63 = filter(duf6, chr == "Chr3")
head(duf63)
ros3 = filter(ros, chr == "Chr3")
head(ros3)

idm4 = filter(idm, chr == "Chr4")
head(idm4)
duf64 = filter(duf6, chr == "Chr4")
head(duf64)
ros4 = filter(ros, chr == "Chr4")
head(ros4)

idm5 = filter(idm, chr == "Chr5")
head(idm5)
duf65 = filter(duf6, chr == "Chr5")
head(duf65)
ros5 = filter(ros, chr == "Chr5")
head(ros5)

idmC = filter(idm, chr == "ChrC")
head(idmC)
duf6C = filter(duf6, chr == "ChrC")
head(duf6C)
rosC = filter(ros, chr == "ChrC")
head(rosC)

idmM = filter(idm, chr == "ChrM")
head(idmM)
duf6M = filter(duf6, chr == "ChrM")
head(duf6M)
rosM = filter(ros, chr == "ChrM")
head(rosM)


#chr1
a_duf6 = length(duf61[,2])
b_ros = length(ros1[,2])
b_ros
c_idm = length(idm1[,2])
c_idm

i_11=0
for (a in 1:b_ros) {
  num_1 = unlist(ros1 [a,2])
  num_2 = unlist(ros1 [a,3])
  for (b in 1:c_idm) {
    num_3 =unlist(idm1 [b,2])
    num_4 =unlist(idm1 [b,3])
    
    if (num_3>=num_1 & num_2>=num_4 & num_4-num_3>=25){
      i_11=i_11+1
    } else if (num_1>=num_3 & num_4>=num_2 & num_2-num_1>=25){
       i_11=i_11+1
    }else if (num_3>=num_1 & num_4>=num_2 & num_2-num_3>=25){
       i_11=i_11+1
    }else if (num_1>=num_3 & num_2>=num_4 & num_4-num_1>=25 ){
      i_11 =i_11+1
    }
  }
}
print(i_11)

i_12=0
for (a in 1:b_ros) {
  num_1 = unlist(ros1 [a,2])
  num_2 = unlist(ros1 [a,3])
  for (b in 1:a_duf6) {
    num_3 =unlist(duf61 [b,2])
    num_4 =unlist(duf61 [b,3])
    
    if (num_3>=num_1 & num_2>=num_4 & num_4-num_3>=25){
      i_12=i_12+1
    } else if (num_1>=num_3 & num_4>=num_2 & num_2-num_1>=25){
       i_12=i_12+1
    }else if (num_3>=num_1 & num_4>=num_2 & num_2-num_3>=25){
       i_12=i_12+1
    }else if (num_1>=num_3 & num_2>=num_4 & num_4-num_1>=25 ){
      i_12 =i_12+1
    }
  }
}
print(i_12)


i_13=0
for (a in 1:c_idm) {
  num_1 = unlist(idm1 [a,2])
  num_2 = unlist(idm1 [a,3])
  for (b in 1:a_duf6) {
    num_3 =unlist(duf61 [b,2])
    num_4 =unlist(duf61 [b,3])
    
    if (num_3>=num_1 & num_2>=num_4 & num_4-num_3>=25){
      i_13=i_13+1
    } else if (num_1>=num_3 & num_4>=num_2 & num_2-num_1>=25){
       i_13=i_13+1
    }else if (num_3>=num_1 & num_4>=num_2 & num_2-num_3>=25){
       i_13=i_13+1
    }else if (num_1>=num_3 & num_2>=num_4 & num_4-num_1>=25 ){
      i_13 =i_13+1
    }
  }
}
print(i_13)


i_14=0
for (a in 1:c_idm) {
  num_1 = unlist(idm1 [a,2])
  num_2 = unlist(idm1 [a,3])
  for (b in 1:a_duf6) {
    num_3 =unlist(duf61 [b,2])
    num_4 =unlist(duf61 [b,3])
    for (c in 1:b_ros){
	  num_5=unlist(ros1 [c,2])
	  num_6=unlist(ros1 [c,3])
    if (num_3>=num_1 & num_2>=num_4 & num_4-num_3>=25){
      if (num_3>=num_5 & num_6>=num_4 & num_4-num_3>=25 | 
	  num_5>=num_3 & num_4>=num_6 & num_6-num_5>=25 |
	  num_3>=num_5 & num_4>=num_6 & num_6-num_3>=25 |
	  num_5>=num_3 & num_6>=num_4 & num_4-num_5>=25
	  ){i_14=i_14+1
	  }
    } else if (num_1>=num_3 & num_4>=num_2 & num_2-num_1>=25){
       if (num_5>=num_1 & num_2>=num_6 & num_6-num_5>=25|
	   num_1>=num_5 & num_6>=num_2 & num_2-num_1>=25|
	   num_5>=num_1 & num_6>=num_2 & num_2-num_5>=25|
	   num_1>=num_5 & num_2>=num_6 & num_6-num_1>=25){i_14=i_14+1
	   }
    } else if (num_3>=num_1 & num_4>=num_2 & num_2-num_3>=25){
	num_1 = num_3 
	   if (num_5>=num_1 & num_2>=num_6 & num_6-num_5>=25|
	   num_1>=num_5 & num_6>=num_2 & num_2-num_1>=25|
	   num_5>=num_1 & num_6>=num_2 & num_2-num_5>=25|
	   num_1>=num_5 & num_2>=num_6 & num_6-num_1>=25){i_14=i_14+1
	   } 
    }else if (num_1>=num_3 & num_2>=num_4 & num_4-num_1>=25 ){
	num_2 = num_4
	   if (num_5>=num_1 & num_2>=num_6 & num_6-num_5>=25|
	   num_1>=num_5 & num_6>=num_2 & num_2-num_1>=25|
	   num_5>=num_1 & num_6>=num_2 & num_2-num_5>=25|
	   num_1>=num_5 & num_2>=num_6 & num_6-num_1>=25){i_14 =i_14+1
	   }
    }
   }
  }
}
print(i_14)

#chr2

a_duf6 = length(duf62[,2])
b_ros = length(ros2[,2])
b_ros
c_idm = length(idm2[,2])
c_idm

i_21=0
for (a in 1:b_ros) {
  num_1 = unlist(ros2 [a,2])
  num_2 = unlist(ros2 [a,3])
  for (b in 1:c_idm) {
    num_3 =unlist(idm2 [b,2])
    num_4 =unlist(idm2 [b,3])
    
    if (num_3>=num_1 & num_2>=num_4 & num_4-num_3>=25){
      i_21=i_21+1
    } else if (num_1>=num_3 & num_4>=num_2 & num_2-num_1>=25){
       i_21=i_21+1
    }else if (num_3>=num_1 & num_4>=num_2 & num_2-num_3>=25){
       i_21=i_21+1
    }else if (num_1>=num_3 & num_2>=num_4 & num_4-num_1>=25 ){
      i_21 =i_21+1
    }
   }
}
print(i_21)

i_22=0
for (a in 1:b_ros) {
  num_1 = unlist(ros2 [a,2])
  num_2 = unlist(ros2 [a,3])
  for (b in 1:a_duf6) {
    num_3 =unlist(duf62 [b,2])
    num_4 =unlist(duf62 [b,3])
    
    if (num_3>=num_1 & num_2>=num_4 & num_4-num_3>=25){
      i_22=i_22+1
    } else if (num_1>=num_3 & num_4>=num_2 & num_2-num_1>=25){
       i_22=i_22+1
    }else if (num_3>=num_1 & num_4>=num_2 & num_2-num_3>=25){
       i_22=i_22+1
    }else if (num_1>=num_3 & num_2>=num_4 & num_4-num_1>=25 ){
      i_22 =i_22+1
    }
  }
}
print(i_22)


i_23=0
for (a in 1:c_idm) {
  num_1 = unlist(idm2 [a,2])
  num_2 = unlist(idm2 [a,3])
  for (b in 1:a_duf6) {
    num_3 =unlist(duf62 [b,2])
    num_4 =unlist(duf62 [b,3])
    
    if (num_3>=num_1 & num_2>=num_4 & num_4-num_3>=25){
      i_23=i_23+1
    } else if (num_1>=num_3 & num_4>=num_2 & num_2-num_1>=25){
       i_23=i_23+1
    }else if (num_3>=num_1 & num_4>=num_2 & num_2-num_3>=25){
       i_23=i_23+1
    }else if (num_1>=num_3 & num_2>=num_4 & num_4-num_1>=25 ){
      i_23 =i_23+1
    }
  }
}
print(i_23)


i_24=0
for (a in 1:c_idm) {
  num_1 = unlist(idm2 [a,2])
  num_2 = unlist(idm2 [a,3])
  for (b in 1:a_duf6) {
    num_3 =unlist(duf62 [b,2])
    num_4 =unlist(duf62 [b,3])
    for (c in 1:b_ros){
	  num_5=unlist(ros2 [c,2])
	  num_6=unlist(ros2 [c,3])
    if (num_3>=num_1 & num_2>=num_4 & num_4-num_3>=25){
      if (num_3>=num_5 & num_6>=num_4 & num_4-num_3>=25 | 
	  num_5>=num_3 & num_4>=num_6 & num_6-num_5>=25 |
	  num_3>=num_5 & num_4>=num_6 & num_6-num_3>=25 |
	  num_5>=num_3 & num_6>=num_4 & num_4-num_5>=25
	  ){i_24=i_24+1
	  }
    } else if (num_1>=num_3 & num_4>=num_2 & num_2-num_1>=25){
       if (num_5>=num_1 & num_2>=num_6 & num_6-num_5>=25|
	   num_1>=num_5 & num_6>=num_2 & num_2-num_1>=25|
	   num_5>=num_1 & num_6>=num_2 & num_2-num_5>=25|
	   num_1>=num_5 & num_2>=num_6 & num_6-num_1>=25){i_24=i_24+1
	   }
    } else if (num_3>=num_1 & num_4>=num_2 & num_2-num_3>=25){
	num_1 = num_3 
	   if (num_5>=num_1 & num_2>=num_6 & num_6-num_5>=25|
	   num_1>=num_5 & num_6>=num_2 & num_2-num_1>=25|
	   num_5>=num_1 & num_6>=num_2 & num_2-num_5>=25|
	   num_1>=num_5 & num_2>=num_6 & num_6-num_1>=25){i_24=i_24+1
	   } 
    }else if (num_1>=num_3 & num_2>=num_4 & num_4-num_1>=25 ){
	num_2 = num_4
	   if (num_5>=num_1 & num_2>=num_6 & num_6-num_5>=25|
	   num_1>=num_5 & num_6>=num_2 & num_2-num_1>=25|
	   num_5>=num_1 & num_6>=num_2 & num_2-num_5>=25|
	   num_1>=num_5 & num_2>=num_6 & num_6-num_1>=25){i_24 =i_24+1
	   }
    }
   }
  }
}
print(i_24)
#chr3

a_duf6 = length(duf63[,2])
b_ros = length(ros3[,2])
b_ros
c_idm = length(idm3[,2])
c_idm

i_31=0
for (a in 1:b_ros) {
  num_1 = unlist(ros3 [a,2])
  num_2 = unlist(ros3 [a,3])
  for (b in 1:c_idm) {
    num_3 =unlist(idm3 [b,2])
    num_4 =unlist(idm3 [b,3])
    
    if (num_3>=num_1 & num_2>=num_4 & num_4-num_3>=25){
      i_31=i_31+1
    } else if (num_1>=num_3 & num_4>=num_2 & num_2-num_1>=25){
       i_31=i_31+1
    }else if (num_3>=num_1 & num_4>=num_2 & num_2-num_3>=25){
       i_31=i_31+1
    }else if (num_1>=num_3 & num_2>=num_4 & num_4-num_1>=25 ){
      i_31 =i_31+1
    }
  }
}
print(i_31)

i_32=0
for (a in 1:b_ros) {
  num_1 = unlist(ros3 [a,2])
  num_2 = unlist(ros3 [a,3])
  for (b in 1:a_duf6) {
    num_3 =unlist(duf63 [b,2])
    num_4 =unlist(duf63 [b,3])
    
    if (num_3>=num_1 & num_2>=num_4 & num_4-num_3>=25){
      i_32=i_32+1
    } else if (num_1>=num_3 & num_4>=num_2 & num_2-num_1>=25){
       i_32=i_32+1
    }else if (num_3>=num_1 & num_4>=num_2 & num_2-num_3>=25){
       i_32=i_32+1
    }else if (num_1>=num_3 & num_2>=num_4 & num_4-num_1>=25 ){
      i_32 =i_32+1
    }
  }
}
print(i_32)


i_33=0
for (a in 1:c_idm) {
  num_1 = unlist(idm3 [a,2])
  num_2 = unlist(idm3 [a,3])
  for (b in 1:a_duf6) {
    num_3 =unlist(duf63 [b,2])
    num_4 =unlist(duf63 [b,3])
    
    if (num_3>=num_1 & num_2>=num_4 & num_4-num_3>=25){
      i_33=i_33+1
    } else if (num_1>=num_3 & num_4>=num_2 & num_2-num_1>=25){
       i_33=i_33+1
    }else if (num_3>=num_1 & num_4>=num_2 & num_2-num_3>=25){
       i_33=i_33+1
    }else if (num_1>=num_3 & num_2>=num_4 & num_4-num_1>=25 ){
      i_33 =i_33+1
    }
  }
}
print(i_33)


i_34=0
for (a in 1:c_idm) {
  num_1 = unlist(ros [a,2])
  num_2 = unlist(ros [a,3])
  for (b in 1:a_duf6) {
    num_3 =unlist(idm [b,2])
    num_4 =unlist(idm [b,3])
    for (c in 1:b_ros){
	  num_5=unlist(ros3 [c,2])
	  num_6=unlist(ros3 [c,3])
    if (num_3>=num_1 & num_2>=num_4 & num_4-num_3>=25){
      if (num_3>=num_5 & num_6>=num_4 & num_4-num_3>=25 | 
	  num_5>=num_3 & num_4>=num_6 & num_6-num_5>=25 |
	  num_3>=num_5 & num_4>=num_6 & num_6-num_3>=25 |
	  num_5>=num_3 & num_6>=num_4 & num_4-num_5>=25
	  ){i_34=i_34+1
	  }
    } else if (num_1>=num_3 & num_4>=num_2 & num_2-num_1>=25){
       if (num_5>=num_1 & num_2>=num_6 & num_6-num_5>=25|
	   num_1>=num_5 & num_6>=num_2 & num_2-num_1>=25|
	   num_5>=num_1 & num_6>=num_2 & num_2-num_5>=25|
	   num_1>=num_5 & num_2>=num_6 & num_6-num_1>=25){i_34=i_34+1
	   }
    } else if (num_3>=num_1 & num_4>=num_2 & num_2-num_3>=25){
	num_1 = num_3 
	   if (num_5>=num_1 & num_2>=num_6 & num_6-num_5>=25|
	   num_1>=num_5 & num_6>=num_2 & num_2-num_1>=25|
	   num_5>=num_1 & num_6>=num_2 & num_2-num_5>=25|
	   num_1>=num_5 & num_2>=num_6 & num_6-num_1>=25){i_34=i_34+1
	   } 
    }else if (num_1>=num_3 & num_2>=num_4 & num_4-num_1>=25 ){
	num_2 = num_4
	   if (num_5>=num_1 & num_2>=num_6 & num_6-num_5>=25|
	   num_1>=num_5 & num_6>=num_2 & num_2-num_1>=25|
	   num_5>=num_1 & num_6>=num_2 & num_2-num_5>=25|
	   num_1>=num_5 & num_2>=num_6 & num_6-num_1>=25){i_34 =i_34+1
	   }
    }
   }
  }
}
print(i_34)
#chr4
a_duf6 = length(duf64[,2])
b_ros = length(ros4[,2])
b_ros
c_idm = length(idm4[,2])
c_idm

i_41=0
for (a in 1:b_ros) {
  num_1 = unlist(ros4 [a,2])
  num_2 = unlist(ros4 [a,3])
  for (b in 1:c_idm) {
    num_3 =unlist(idm4 [b,2])
    num_4 =unlist(idm4 [b,3])
    
    if (num_3>=num_1 & num_2>=num_4 & num_4-num_3>=25){
      i_41=i_41+1
    } else if (num_1>=num_3 & num_4>=num_2 & num_2-num_1>=25){
       i_41=i_41+1
    }else if (num_3>=num_1 & num_4>=num_2 & num_2-num_3>=25){
       i_41=i_41+1
    }else if (num_1>=num_3 & num_2>=num_4 & num_4-num_1>=25 ){
      i_41 =i_41+1
    }
  }
}
print(i_41)

i_42=0
for (a in 1:b_ros) {
  num_1 = unlist(ros4 [a,2])
  num_2 = unlist(ros4 [a,3])
  for (b in 1:a_duf6) {
    num_3 =unlist(duf64 [b,2])
    num_4 =unlist(duf64 [b,3])
    
    if (num_3>=num_1 & num_2>=num_4 & num_4-num_3>=25){
      i_42=i_42+1
    } else if (num_1>=num_3 & num_4>=num_2 & num_2-num_1>=25){
       i_42=i_42+1
    }else if (num_3>=num_1 & num_4>=num_2 & num_2-num_3>=25){
       i_42=i_42+1
    }else if (num_1>=num_3 & num_2>=num_4 & num_4-num_1>=25 ){
      i_42 =i_42+1
    }
  }
}
print(i_42)


i_43=0
for (a in 1:c_idm) {
  num_1 = unlist(idm4 [a,2])
  num_2 = unlist(idm4 [a,3])
  for (b in 1:a_duf6) {
    num_3 =unlist(duf64 [b,2])
    num_4 =unlist(duf64 [b,3])
    
    if (num_3>=num_1 & num_2>=num_4 & num_4-num_3>=25){
      i_43=i_43+1
    } else if (num_1>=num_3 & num_4>=num_2 & num_2-num_1>=25){
       i_43=i_43+1
    }else if (num_3>=num_1 & num_4>=num_2 & num_2-num_3>=25){
       i_43=i_43+1
    }else if (num_1>=num_3 & num_2>=num_4 & num_4-num_1>=25 ){
      i_43 =i_43+1
    }
  }
}
print(i_43)


i_44=0
for (a in 1:c_idm) {
  num_1 = unlist(idm4 [a,2])
  num_2 = unlist(idm4 [a,3])
  for (b in 1:a_duf6) {
    num_3 =unlist(duf64 [b,2])
    num_4 =unlist(duf64 [b,3])
    for (c in 1:b_ros){
	  num_5=unlist(ros4 [c,2])
	  num_6=unlist(ros4 [c,3])
    if (num_3>=num_1 & num_2>=num_4 & num_4-num_3>=25){
      if (num_3>=num_5 & num_6>=num_4 & num_4-num_3>=25 | 
	  num_5>=num_3 & num_4>=num_6 & num_6-num_5>=25 |
	  num_3>=num_5 & num_4>=num_6 & num_6-num_3>=25 |
	  num_5>=num_3 & num_6>=num_4 & num_4-num_5>=25
	  ){i_44=i_44+1
	  }
    } else if (num_1>=num_3 & num_4>=num_2 & num_2-num_1>=25){
       if (num_5>=num_1 & num_2>=num_6 & num_6-num_5>=25|
	   num_1>=num_5 & num_6>=num_2 & num_2-num_1>=25|
	   num_5>=num_1 & num_6>=num_2 & num_2-num_5>=25|
	   num_1>=num_5 & num_2>=num_6 & num_6-num_1>=25){i_44=i_44+1
	   }
    } else if (num_3>=num_1 & num_4>=num_2 & num_2-num_3>=25){
	num_1 = num_3 
	   if (num_5>=num_1 & num_2>=num_6 & num_6-num_5>=25|
	   num_1>=num_5 & num_6>=num_2 & num_2-num_1>=25|
	   num_5>=num_1 & num_6>=num_2 & num_2-num_5>=25|
	   num_1>=num_5 & num_2>=num_6 & num_6-num_1>=25){i_44=i_44+1
	   } 
    }else if (num_1>=num_3 & num_2>=num_4 & num_4-num_1>=25 ){
	num_2 = num_4
	   if (num_5>=num_1 & num_2>=num_6 & num_6-num_5>=25|
	   num_1>=num_5 & num_6>=num_2 & num_2-num_1>=25|
	   num_5>=num_1 & num_6>=num_2 & num_2-num_5>=25|
	   num_1>=num_5 & num_2>=num_6 & num_6-num_1>=25){i_44 =i_44+1
	   }
    }
   }
  }
}
print(i_44)
#chr5
a_duf6 = length(duf65[,2])
b_ros = length(ros5[,2])
b_ros
c_idm = length(idm5[,2])
c_idm

i_51=0
for (a in 1:b_ros) {
  num_1 = unlist(ros5 [a,2])
  num_2 = unlist(ros5 [a,3])
  for (b in 1:c_idm) {
    num_3 =unlist(idm5 [b,2])
    num_4 =unlist(idm5 [b,3])
    #print(num_1)
    #print(num_2)
    #print(num_4)
    #print (num_3)
    if (num_3>=num_1 & num_2>=num_4 & num_4-num_3>=25){
      i_51=i_51+1
    } else if (num_1>=num_3 & num_4>=num_2 & num_2-num_1>=25){
       i_51=i_51+1
    }else if (num_3>=num_1 & num_4>=num_2 & num_2-num_3>=25){
       i_51=i_51+1
    }else if (num_1>=num_3 & num_2>=num_4 & num_4-num_1>=25 ){
      i_51 =i_51+1
    }
  }
}
print(i_51)

i_52=0
for (a in 1:b_ros) {
  num_1 = unlist(ros5 [a,2])
  num_2 = unlist(ros5 [a,3])
  for (b in 1:a_duf6) {
    num_3 =unlist(duf65 [b,2])
    num_4 =unlist(duf65 [b,3])
    #print(num_1)
    #print(num_2)
    #print(num_4)
    #print (num_3)
    if (num_3>=num_1 & num_2>=num_4 & num_4-num_3>=25){
      i_52=i_52+1
    } else if (num_1>=num_3 & num_4>=num_2 & num_2-num_1>=25){
       i_52=i_52+1
    }else if (num_3>=num_1 & num_4>=num_2 & num_2-num_3>=25){
       i_52=i_52+1
    }else if (num_1>=num_3 & num_2>=num_4 & num_4-num_1>=25 ){
      i_52 =i_52+1
    }
  }
}
print(i_52)


i_53=0
for (a in 1:c_idm) {
  num_1 = unlist(idm5 [a,2])
  num_2 = unlist(idm5 [a,3])
  for (b in 1:a_duf6) {
    num_3 =unlist(duf65 [b,2])
    num_4 =unlist(duf65 [b,3])
    #print(num_1)
    #print(num_2)
    #print(num_4)
    #print (num_3)
    if (num_3>=num_1 & num_2>=num_4 & num_4-num_3>=25){
      i_53=i_53+1
    } else if (num_1>=num_3 & num_4>=num_2 & num_2-num_1>=25){
       i_53=i_53+1
    }else if (num_3>=num_1 & num_4>=num_2 & num_2-num_3>=25){
       i_53=i_53+1
    }else if (num_1>=num_3 & num_2>=num_4 & num_4-num_1>=25 ){
      i_53 =i_53+1
    }
  }
}
print(i_53)


i_54=0
for (a in 1:c_idm) {
  num_1 = unlist(idm5 [a,2])
  num_2 = unlist(idm5 [a,3])
  for (b in 1:a_duf6) {
    num_3 =unlist(duf65 [b,2])
    num_4 =unlist(duf65 [b,3])
    for (c in 1:b_ros){
	  num_5=unlist(ros5 [c,2])
	  num_6=unlist(ros5 [c,3])
    if (num_3>=num_1 & num_2>=num_4 & num_4-num_3>=25){
      if (num_3>=num_5 & num_6>=num_4 & num_4-num_3>=25 | 
	  num_5>=num_3 & num_4>=num_6 & num_6-num_5>=25 |
	  num_3>=num_5 & num_4>=num_6 & num_6-num_3>=25 |
	  num_5>=num_3 & num_6>=num_4 & num_4-num_5>=25
	  ){i_54=i_54+1
	  }
    } else if (num_1>=num_3 & num_4>=num_2 & num_2-num_1>=25){
       if (num_5>=num_1 & num_2>=num_6 & num_6-num_5>=25|
	   num_1>=num_5 & num_6>=num_2 & num_2-num_1>=25|
	   num_5>=num_1 & num_6>=num_2 & num_2-num_5>=25|
	   num_1>=num_5 & num_2>=num_6 & num_6-num_1>=25){i_54=i_54+1
	   }
    } else if (num_3>=num_1 & num_4>=num_2 & num_2-num_3>=25){
	num_1 = num_3 
	   if (num_5>=num_1 & num_2>=num_6 & num_6-num_5>=25|
	   num_1>=num_5 & num_6>=num_2 & num_2-num_1>=25|
	   num_5>=num_1 & num_6>=num_2 & num_2-num_5>=25|
	   num_1>=num_5 & num_2>=num_6 & num_6-num_1>=25){i_54=i_54+1
	   } 
    }else if (num_1>=num_3 & num_2>=num_4 & num_4-num_1>=25 ){
	num_2 = num_4
	   if (num_5>=num_1 & num_2>=num_6 & num_6-num_5>=25|
	   num_1>=num_5 & num_6>=num_2 & num_2-num_1>=25|
	   num_5>=num_1 & num_6>=num_2 & num_2-num_5>=25|
	   num_1>=num_5 & num_2>=num_6 & num_6-num_1>=25){i_54 =i_54+1
	   }
    }
   }
  }
}
print(i_54)


i_1 = i_11+i_21+i_31+i_41+i_51
i_2 = i_12+i_22+i_32+i_42+i_52
i_3 = i_13+i_23+i_33+i_43+i_53
i = i_14+i_24+i_34+i_44+i_54




library(grid)
library(futile.logger)
library(VennDiagram)
venn.plot1 <- draw.triple.venn(area1 = a_duf60,area2 = b_ros0,area3 = c_idm0,
                              n12 = i_2,n23 = i_1,n13 = i_3,n123 = i,
                              category = c("duf6", "ros1-14", "idm1-9"),
                              fill = c("blue", "red", "green"),lty = "blank",
                              cex = 2,cat.cex = 2,cat.col = c("black"))
grid.draw(venn.plot1)

