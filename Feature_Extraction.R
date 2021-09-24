#RASA
rm(list = ls())
filenames_1=list.files(pattern="*.csv")
s_1=length(filenames_1)
result_3=0
for (i in 1:s_1) {
  zan_a1=read.csv(filenames_1[i],colClasses = c("character","character","numeric","numeric"))
  
  zan_c1=0
  
  zan_c2=0
  for (i_1 in 1:nrow(zan_a1)) {
    if(zan_a1[i_1,4]>=0 & zan_a1[i_1,4]<=1 ){
      zan_c1[i_1]=0
      zan_c2[i_1]=1
    } else {  zan_c2[i_1]=0
    zan_c1[i_1]=1}
  }
  zan_b1=nrow(zan_a1)
  zan_a2=as.data.frame(matrix(0,5,4))
  colnames(zan_a2)=colnames(zan_a1)
  zan_a3=rbind(zan_a2,zan_a1,zan_a2)
  b_1=zan_a3[1:zan_b1,4]
  b_2=zan_a3[2:(zan_b1+1),4]
  b_3=zan_a3[3:(zan_b1+2),4]
  b_4=zan_a3[4:(zan_b1+3),4]
  b_5=zan_a3[5:(zan_b1+4),4]
  b_6=zan_a3[6:(zan_b1+5),4]
  b_7=zan_a3[7:(zan_b1+6),4]
  b_8=zan_a3[8:(zan_b1+7),4]
  b_9=zan_a3[9:(zan_b1+8),4]
  b_10=zan_a3[10:(zan_b1+9),4]
  b_11=zan_a3[11:(zan_b1+10),4]
  zan_c3=cbind(b_1,b_2,b_3,b_4,b_5,
               b_6,b_7,b_8,
               b_9,b_10,b_11)
  zan_c4_1=0
  zan_c4_2=0
  zan_c4_3=0
  zan_c4_4=0
  zan_c4_5=0
  for (i_2 in 1:5) {
    zan_c4_1[i_2]=
      mean(zan_a1[1:(i_2+5),4])
    zan_c4_2[i_2]=
      sd(zan_a1[1:(i_2+5),4])
    zan_c4_3[i_2]=
      max(zan_a1[1:(i_2+5),4])
    zan_c4_4[i_2]=
      min(zan_a1[1:(i_2+5),4])
    zan_c4_5[i_2]=
      zan_c4_2[i_2]/zan_c4_1[i_2]
  }
  for (i_3 in 6:(nrow(zan_a1)-5)) {
    zan_c4_1[i_3]=
      mean(zan_a1[(i_3-5):(i_3+5),4])
    zan_c4_2[i_3]=
      sd(zan_a1[(i_3-5):(i_3+5),4])
    zan_c4_3[i_3]=
      max(zan_a1[(i_3-5):(i_3+5),4])
    zan_c4_4[i_3]=
      min(zan_a1[(i_3-5):(i_3+5),4])
    zan_c4_5[i_3]=
      zan_c4_2[i_3]/zan_c4_1[i_3]
  }
  for (i_4 in (nrow(zan_a1)-5):nrow(zan_a1)) {
    zan_c4_1[i_4]=
      mean(zan_a1[(i_4-5):nrow(zan_a1),4])
    zan_c4_2[i_4]=
      sd(zan_a1[(i_4-5):nrow(zan_a1),4])
    zan_c4_3[i_4]=
      max(zan_a1[(i_4-5):nrow(zan_a1),4])
    zan_c4_4[i_4]=
      min(zan_a1[(i_4-5):nrow(zan_a1),4])
    zan_c4_5[i_4]=
      zan_c4_2[i_4]/zan_c4_1[i_4]
  }
  zan_c5_1=0
  zan_c5_2=0
  zan_c5_3=0
  zan_c5_4=0
  for (i_5 in 1:5) {
    zan_a4_1=zan_a1[1:(i_5+5),4]
    zan_c5_1[i_5]=length(which(zan_a4_1 >= (-1) & zan_a4_1< (-0.5)))
    zan_c5_2[i_5]=length(which(zan_a4_1 >= (-0.5) & zan_a4_1< 0))
    zan_c5_3[i_5]=length(which(zan_a4_1 >= 0 & zan_a4_1< 0.5))
    zan_c5_4[i_5]=length(which(zan_a4_1 >= 0.5 & zan_a4_1<= 1))
  }
  for (i_6 in 6:(nrow(zan_a1)-5)) {
    zan_a4_2=zan_a1[(i_6-5):(i_6+5),4]
    zan_c5_1[i_6]=length(which(zan_a4_2 >= (-1) & zan_a4_2 < (-0.5)))
    zan_c5_2[i_6]=length(which(zan_a4_2 >= (-0.5) & zan_a4_2 < 0))
    zan_c5_3[i_6]=length(which(zan_a4_2 >= 0 & zan_a4_2 < 0.5))
    zan_c5_4[i_6]=length(which(zan_a4_2 >= 0.5 & zan_a4_2 <= 1))
  }
  for (i_7 in (nrow(zan_a1)-5):nrow(zan_a1)) {
    zan_a4_3=zan_a1[(i_7-5):nrow(zan_a1),4]
    zan_c5_1[i_7]=length(which(zan_a4_3 >= (-1) & zan_a4_3 < (-0.5)))
    zan_c5_2[i_7]=length(which(zan_a4_3 >= (-0.5) & zan_a4_3 < 0))
    zan_c5_3[i_7]=length(which(zan_a4_3 >= 0 & zan_a4_3 < 0.5))
    zan_c5_4[i_7]=length(which(zan_a4_3 >= 0.5 & zan_a4_3 <= 1))
  }
  result_1=cbind(
    as.data.frame(zan_c1),as.data.frame(zan_c2),as.data.frame(zan_c3),
    as.data.frame(zan_c4_1),as.data.frame(zan_c4_2),as.data.frame(zan_c4_3),
    as.data.frame(zan_c4_4),as.data.frame(zan_c4_5),as.data.frame(zan_c5_1),
    as.data.frame(zan_c5_2),as.data.frame(zan_c5_3),as.data.frame(zan_c5_4)
  )
  colnames(result_1)=paste("X",c(326:347),sep = "")
  result_2=cbind(zan_a1[,1:2],result_1)
  result_3=rbind(result_3,result_2)
}
result_4=result_3[-1,]
any(is.na(result_4))
write.csv(result_4,file = "feature_5.csv",row.names = F)


#HMM profile
DATA_Blosum62=read.csv("BLOSUM62_1.csv")
filenames_1=list.files(pattern="*.csv")
filenames_2=substr(filenames_1,1,nchar(filenames_1)-4)
s_1=length(filenames_1)
result_1=0
for (i in 1:s_1) {
  zan_a1=read.csv(filenames_1[i])
  s_2=nrow(zan_a1)
  zan_a2=zan_a1[,3:22]
  zan_b1=zan_a1[,1]
  zan_b2=rep(filenames_2[i],s_2)
  for (i_1 in 1:20) {
    zan_a2[,i_1]=as.numeric(zan_a2[,i_1])
  }
  zan_a2[is.na(zan_a2)]=0
  zan_a3=matrix(NA,s_2,20)
  for (i_4 in 1:20) {
    zan_a3[,i_4]=
      (zan_a2[,i_4]-min(zan_a2[,i_4]))/(max(zan_a2[,i_4])-min(zan_a2[,i_4]))
  }
  zan_a3=as.data.frame(zan_a3)
  colnames(zan_a2)=c(
    "A","C","D","E","F","G","H","I","K","L",
    "M","N","P","Q","R","S","T","V","W","Y")
  colnames(zan_a3)=c(
    "A","C","D","E","F","G","H","I","K","L",
    "M","N","P","Q","R","S","T","V","W","Y")
  zan_ECO=0
  for (i_2 in 1:s_2) {
    zan_b3=zan_b1[i_2]
    zan_b4=DATA_Blosum62[which(DATA_Blosum62[,1]==zan_b3),]
    zan_b5_1=0
    zan_b5_2=0
    for (i_3 in 1:20) {
      if(zan_b4[1,i_3+1]!=0){zan_b5_1[i_3]=(zan_a2[i_2,i_3]^2)/zan_b4[1,i_3+1]
      zan_b5_2[i_3]=zan_a2[i_2,i_3]/zan_b4[1,i_3+1]} else  {zan_b5_1[i_3]=0
      zan_b5_2[i_3]=0}
      
    }
    if(sum(zan_b5_2)==0){zan_ECO[i_2]=0
    } else {
      zan_ECO[i_2]=log(sum(zan_b5_1))/log(sum(zan_b5_2))}
  }
  zan_ECO=as.data.frame(zan_ECO)
  zan_a4=cbind(zan_a3,zan_ECO)
  zan_a5=as.data.frame(matrix(0,5,21))
  colnames(zan_a5)=colnames(zan_a4)
  zan_a6=rbind(zan_a5,zan_a4,zan_a5)
  b_1=zan_a6[1:s_2,]
  b_2=zan_a6[2:(s_2+1),]
  b_3=zan_a6[3:(s_2+2),]
  b_4=zan_a6[4:(s_2+3),]
  b_5=zan_a6[5:(s_2+4),]
  b_6=zan_a6[6:(s_2+5),]
  b_7=zan_a6[7:(s_2+6),]
  b_8=zan_a6[8:(s_2+7),]
  b_9=zan_a6[9:(s_2+8),]
  b_10=zan_a6[10:(s_2+9),]
  b_11=zan_a6[11:(s_2+10),]
  zan_result1=cbind(b_1,b_2,b_3,
                    b_4,b_5,b_6,
                    b_7,b_8,b_9,
                    b_10,b_11)
  zan_result3=0
  
  for (i_5 in 1:21) {
    windows_1=c(3,7,11)
    zan_result2=0
    for (i_6 in 1:3) {
      zan_c1_1=0
      zan_c1_2=0
      zan_c1_3=0
      zan_c1_4=0
      zan_c1_5=0
      zan_b6=windows_1[i_6]
      zan_b6_2=(zan_b6-1)/2
      for (i_7 in 1:zan_b6_2) {
        zan_c1_1[i_7]=
          mean(zan_a4[1:(i_7+zan_b6_2),i_5])
        zan_c1_2[i_7]=
          sd(zan_a4[1:(i_7+zan_b6_2),i_5])
        zan_c1_3[i_7]=
          max(zan_a4[1:(i_7+zan_b6_2),i_5])
        zan_c1_4[i_7]=
          min(zan_a4[1:(i_7+zan_b6_2),i_5])
        if(zan_c1_1[i_7]==0){zan_c1_5[i_7]=0
        } else  {  zan_c1_5[i_7]=zan_c1_2[i_7]/zan_c1_1[i_7]
        }
      }
      for (i_8 in (zan_b6_2+1):(s_2-zan_b6_2)) {
        zan_c1_1[i_8]=
          mean(zan_a4[(i_8-zan_b6_2):(i_8+zan_b6_2),i_5])
        zan_c1_2[i_8]=
          sd(zan_a4[(i_8-zan_b6_2):(i_8+zan_b6_2),i_5])
        zan_c1_3[i_8]=
          max(zan_a4[(i_8-zan_b6_2):(i_8+zan_b6_2),i_5])
        zan_c1_4[i_8]=
          min(zan_a4[(i_8-zan_b6_2):(i_8+zan_b6_2),i_5])
        if(zan_c1_1[i_8]==0){zan_c1_5[i_8]=0
        } else  {  zan_c1_5[i_8]=zan_c1_2[i_8]/zan_c1_1[i_8]
        }
        
      }
      for (i_9 in (s_2-zan_b6_2+1):s_2) {
        zan_c1_1[i_9]=
          mean(zan_a4[(i_9-zan_b6_2):s_2,i_5])
        zan_c1_2[i_9]=
          sd(zan_a4[(i_9-zan_b6_2):s_2,i_5])
        zan_c1_3[i_9]=
          max(zan_a4[(i_9-zan_b6_2):s_2,i_5])
        zan_c1_4[i_9]=
          min(zan_a4[(i_9-zan_b6_2):s_2,i_5])
        if(zan_c1_1[i_9]==0){zan_c1_5[i_9]=0
        } else  {  zan_c1_5[i_9]=zan_c1_2[i_9]/zan_c1_1[i_9]
        }
        
      }
      zan_result2=cbind(zan_result2,zan_c1_1,zan_c1_2,zan_c1_3,zan_c1_4,zan_c1_5)
    }
    zan_result3=cbind(zan_result3,zan_result2[,-1])
  }
  zan_result4=zan_result3[,-1]
  zan_result5=cbind(zan_b2,zan_b1,zan_result1,zan_result4)
  colnames(zan_result5)=c("Protein_ID","Site_ID",paste("X",886:1431,sep = ""))
  
  result_1=rbind(result_1,zan_result5)
}
result_2=result_1[-1,]
any(is.na(result_2))
write.csv(result_2,"feature_9.csv")

#PSSM
data_name=list.files(pattern="*.csv")
s_1=length(data_name)
zan_result=0
for (i in 1:s_1) {
  zan_data=read.csv(file = data_name[i])
  a_1=as.data.frame(zan_data[,1])
  a_2=as.data.frame(rep(c(1,0,1),c(5,nrow(zan_data)-10,5)))
  a_3=as.data.frame(rep(substring(data_name[i],1,(nchar(data_name[i])-4)),
                        nrow(zan_data)) )
  a_4=cbind(a_3,a_1,a_2)
  zan_result=rbind(zan_result,a_4)
}
zan_result=zan_result[-1,]
colnames(zan_result)=c("Protein_ID","Site_ID","X1")
write.csv(zan_result,"feature_1.csv")
zan_result_2=0
for (i in 1:s_1) {
  zan_data=read.csv(file = data_name[i])
  B_1=nrow(zan_data)
  p_1=as.data.frame(matrix(0,5,21))
  colnames(p_1)=colnames(zan_data)
  zan_data_2=rbind(p_1,zan_data,p_1)
  b_1=as.data.frame(zan_data[,1])
  b_2=as.data.frame(rep(substring(data_name[i],1,(nchar(data_name[i])-4)),
                        B_1) )
  zan_B_1=0
  for (i_2 in 2:21) {
    b_3=zan_data_2[1:B_1,i_2]
    b_4=zan_data_2[2:(B_1+1),i_2]
    b_5=zan_data_2[3:(B_1+2),i_2]
    b_6=zan_data_2[4:(B_1+3),i_2]
    b_7=zan_data_2[5:(B_1+4),i_2]
    b_8=zan_data_2[6:(B_1+5),i_2]
    b_9=zan_data_2[7:(B_1+6),i_2]
    b_10=zan_data_2[8:(B_1+7),i_2]
    b_11=zan_data_2[9:(B_1+8),i_2]
    b_12=zan_data_2[10:(B_1+9),i_2]
    b_13=zan_data_2[11:(B_1+10),i_2]
    zan_B_1=cbind(zan_B_1,b_3,b_4,b_5,
                  b_6,b_7,b_8,
                  b_9,b_10,b_11,
                  b_12,b_13)
  }
  zan_B_2=zan_B_1[,-1]
  zan_B_2=cbind(b_2,b_1,zan_B_2)
  zan_result_2=rbind(zan_result_2,zan_B_2)
}
bb_1=paste("X",c(2:221),sep = "")
bb_2=c("Protein_ID","Site_ID",bb_1)
zan_result_3=zan_result_2[-1,]
colnames(zan_result_3)=bb_2
write.csv(zan_result_3,"feature_2.csv")
zan_result_5=0
for (i in 1:s_1) {
  zan_result_4=0
  C_4=paste("X",c(222:321),sep = "")
  zan_data_3=read.csv(file = data_name[i])
  C_1=nrow(zan_data_3)
  C_2=as.data.frame(zan_data_3[,1])
  C_3=as.data.frame(rep(substring(data_name[i],1,(nchar(data_name[i])-4)),
                        C_1) )
  zan_CC_1=0
  for (i_1 in 1:20) {
    Cc_1=0
    Cc_2=0
    Cc_3=0
    Cc_4=0
    Cc_5=0
    for (i_3 in 1:5) {
      Cc_1[i_3]=
        mean(zan_data_3[1:(i_3+5),i_1+1])
      Cc_2[i_3]=
        sd(zan_data_3[1:(i_3+5),i_1+1])
      Cc_3[i_3]=
        max(zan_data_3[1:(i_3+5),i_1+1])
      Cc_4[i_3]=
        min(zan_data_3[1:(i_3+5),i_1+1])
      Cc_5[i_3]=
        Cc_2[i_3]/Cc_1[i_3]
    }
    for (i_2 in 6:(C_1-5)) {
      Cc_1[i_2]=
        mean(zan_data_3[(i_2-5):(i_2+5),i_1+1])
      Cc_2[i_2]=
        sd(zan_data_3[(i_2-5):(i_2+5),i_1+1])
      Cc_3[i_2]=
        max(zan_data_3[(i_2-5):(i_2+5),i_1+1])
      Cc_4[i_2]=
        min(zan_data_3[(i_2-5):(i_2+5),i_1+1])
      Cc_5[i_2]=Cc_2[i_2]/Cc_1[i_2]
    }
    for (i_4 in (C_1-5):C_1) {
      Cc_1[i_4]=
        mean(zan_data_3[(i_4-5):C_1,i_1+1])
      Cc_2[i_4]=
        sd(zan_data_3[(i_4-5):C_1,i_1+1])
      Cc_3[i_4]=
        max(zan_data_3[(i_4-5):C_1,i_1+1])
      Cc_4[i_4]=
        min(zan_data_3[(i_4-5):C_1,i_1+1])
      Cc_5[i_4]=
        Cc_2[i_4]/Cc_1[i_4]
    }
    zan_CC_1=cbind(zan_CC_1,Cc_1,Cc_2,Cc_3,Cc_4,Cc_5)
  }
  zan_result_4=cbind(C_3,C_2,zan_CC_1[,-1])
  colnames(zan_result_4)=c('Protein_ID','site_ID',C_4)
  zan_result_5=rbind(zan_result_5,zan_result_4)
}
result_3=zan_result_5[-1,]
write.csv(result_3,"feature_3.csv")


#Physicochemical properties
aaindex=read.csv("AA_17.csv")
zan_b1=colnames(aaindex)[-1]
filenames_1=list.files(pattern="*.csv")
filenames_2=substr(filenames_1,1,nchar(filenames_1)-4)
s_1=length(filenames_1)
result_3=0
s_3=nrow(aaindex)
for (ii in 1:s_3) {
  zan_b2=aaindex[ii,-1]
  result_2=0
  Site_ID=0
  Protein_ID=0
  for (i in 1:s_1) {
    result_1=0
    zan_a1=read.csv(filenames_1[i])
    s_2=nrow(zan_a1)
    zan_a1[,4]=zan_a1[,2]
    for (i_1 in 1:20) {
      b_1_1=zan_b1[i_1]
      b_1_2=zan_b2[1,i_1]
      zan_a1[,4]=gsub(b_1_1,b_1_2,zan_a1[,4])
    }
    zan_a1[,4]=as.numeric(zan_a1[,4])
    zan_a2=as.data.frame(matrix(0,5,4))
    colnames(zan_a2)=colnames(zan_a1)
    zan_a3=rbind(zan_a2,zan_a1,zan_a2)
    b_1=zan_a3[1:s_2,4]
    b_2=zan_a3[2:(s_2+1),4]
    b_3=zan_a3[3:(s_2+2),4]
    b_4=zan_a3[4:(s_2+3),4]
    b_5=zan_a3[5:(s_2+4),4]
    b_6=zan_a3[6:(s_2+5),4]
    b_7=zan_a3[7:(s_2+6),4]
    b_8=zan_a3[8:(s_2+7),4]
    b_9=zan_a3[9:(s_2+8),4]
    b_10=zan_a3[10:(s_2+9),4]
    b_11=zan_a3[11:(s_2+10),4]
    zan_result1=cbind(b_1,b_2,b_3,
                      b_4,b_5,b_6,
                      b_7,b_8,b_9,
                      b_10,b_11)
    zan_c1_1=0
    zan_c1_2=0
    zan_c1_3=0
    zan_c1_4=0
    zan_c1_5=0
    windows_1=c(3,7,11)
    zan_result2=0
    for (i_2 in 1:3) {
      zan_b3=windows_1[i_2]
      zan_b3_2=(zan_b3-1)/2
      for (i_3 in 1:zan_b3_2) {
        zan_c1_1[i_3]=
          mean(zan_a1[1:(i_3+zan_b3_2),4])
        zan_c1_2[i_3]=
          sd(zan_a1[1:(i_3+zan_b3_2),4])
        zan_c1_3[i_3]=
          max(zan_a1[1:(i_3+zan_b3_2),4])
        zan_c1_4[i_3]=
          min(zan_a1[1:(i_3+zan_b3_2),4])
        if(zan_c1_1[i_3]==0){zan_c1_5[i_3]=0
        } else  {  zan_c1_5[i_3]=zan_c1_2[i_3]/zan_c1_1[i_3]
        }
        
      }
      for (i_4 in (zan_b3_2+1):(s_2-zan_b3_2)) {
        zan_c1_1[i_4]=
          mean(zan_a1[(i_4-zan_b3_2):(i_4+zan_b3_2),4])
        zan_c1_2[i_4]=
          sd(zan_a1[(i_4-zan_b3_2):(i_4+zan_b3_2),4])
        zan_c1_3[i_4]=
          max(zan_a1[(i_4-zan_b3_2):(i_4+zan_b3_2),4])
        zan_c1_4[i_4]=
          min(zan_a1[(i_4-zan_b3_2):(i_4+zan_b3_2),4])
        if(zan_c1_1[i_4]==0){zan_c1_5[i_4]=0
        } else  {  zan_c1_5[i_4]=zan_c1_2[i_4]/zan_c1_1[i_4]
        }
      }
      for (i_5 in (s_2-zan_b3_2+1):s_2) {
        zan_c1_1[i_5]=
          mean(zan_a1[(i_5-zan_b3_2):s_2,4])
        zan_c1_2[i_5]=
          sd(zan_a1[(i_5-zan_b3_2):s_2,4])
        zan_c1_3[i_5]=
          max(zan_a1[(i_5-zan_b3_2):s_2,4])
        zan_c1_4[i_5]=
          min(zan_a1[(i_5-zan_b3_2):s_2,4])
        if(zan_c1_1[i_5]==0){zan_c1_5[i_5]=0
        } else  {  zan_c1_5[i_5]=zan_c1_2[i_5]/zan_c1_1[i_5]
        }
      }
      zan_result2=cbind(zan_result2,zan_c1_1,zan_c1_2,zan_c1_3,zan_c1_4,zan_c1_5)
    }
    zan_result3=as.data.frame(zan_result2[,-1])
    Site_ID_1=as.data.frame(zan_a1[,2])
    Protein_ID_1=as.data.frame(rep(filenames_2[i],s_2))
    Site_ID=rbind(Site_ID,Site_ID_1)
    Protein_ID=rbind(Protein_ID,Protein_ID_1)
    result_1=cbind(zan_result1,zan_result3) 
    result_2=as.data.frame(rbind(result_2,result_1))
  }
  result_3=cbind(result_3,result_2[-1,])
}
result_4=result_3[,-1]
result_5=cbind(Protein_ID[-1,],Site_ID[-1,],result_4)
colnames(result_5)=c("Protein_ID","Site_ID",paste("X",c(444:885),sep = ""))
write.csv(result_5,file =  "feature_8.csv", row.names = F)



#Protein disorder
rm(list = ls())
filenames_1=list.files(pattern="*.csv")
library(stringr)
filenames_2 <-substring(filenames_1,1,nchar(filenames_1)-4)
s_1=length(filenames_1)
result_3=0
for (i in 1:s_1) {
  zan_a1=read.csv(filenames_1[i])
  B_1=nrow(zan_a1)
  zan_b1=as.data.frame(rep(filenames_2[i],B_1))
  zan_b2=as.data.frame(zan_a1[,3])
  zan_b3=cbind(zan_b1,zan_b2)
  colnames(zan_b3)=c("Protein_ID","Site_ID")
  zan_a2=as.data.frame(matrix(0,5,5))
  colnames(zan_a2)=colnames(zan_a1)
  zan_a3=rbind(zan_a2,zan_a1,zan_a2)
  
  result_1=0
  zan_result1=0
  zan_result2=0
  for (i_1 in 1:2) {
    b_1=zan_a3[1:B_1,(i_1+3)]
    b_2=zan_a3[2:(B_1+1),(i_1+3)]
    b_3=zan_a3[3:(B_1+2),(i_1+3)]
    b_4=zan_a3[4:(B_1+3),(i_1+3)]
    b_5=zan_a3[5:(B_1+4),(i_1+3)]
    b_6=zan_a3[6:(B_1+5),(i_1+3)]
    b_7=zan_a3[7:(B_1+6),(i_1+3)]
    b_8=zan_a3[8:(B_1+7),(i_1+3)]
    b_9=zan_a3[9:(B_1+8),(i_1+3)]
    b_10=zan_a3[10:(B_1+9),(i_1+3)]
    b_11=zan_a3[11:(B_1+10),(i_1+3)]
    zan_result1=cbind(zan_result1,b_1,b_2,b_3,
                      b_4,b_5,b_6,
                      b_7,b_8,b_9,
                      b_10,b_11)
    zan_c1_1=0
    zan_c1_2=0
    zan_c1_3=0
    zan_c1_4=0
    zan_c1_5=0
    for (i_2 in 1:5) {
      zan_c1_1[i_2]=
        mean(zan_a1[1:(i_2+5),(i_1+3)])
      zan_c1_2[i_2]=
        sd(zan_a1[1:(i_2+5),(i_1+3)])
      zan_c1_3[i_2]=
        max(zan_a1[1:(i_2+5),(i_1+3)])
      zan_c1_4[i_2]=
        min(zan_a1[1:(i_2+5),(i_1+3)])
      if(zan_c1_1[i_2]!=0){
        zan_c1_5[i_2]=zan_c1_2[i_2]/zan_c1_1[i_2]
      } else {zan_c1_5[i_2]=0}
    }
    for (i_3 in 6:(B_1-5)) {
      zan_c1_1[i_3]=
        mean(zan_a1[(i_3-5):(i_3+5),(i_1+3)])
      zan_c1_2[i_3]=
        sd(zan_a1[(i_3-5):(i_3+5),(i_1+3)])
      zan_c1_3[i_3]=
        max(zan_a1[(i_3-5):(i_3+5),(i_1+3)])
      zan_c1_4[i_3]=
        min(zan_a1[(i_3-5):(i_3+5),(i_1+3)])
      if(zan_c1_1[i_3]!=0){
        zan_c1_5[i_3]=zan_c1_2[i_3]/zan_c1_1[i_3]
      } else {zan_c1_5[i_3]=0}
    }
    for (i_4 in (B_1-5):B_1) {
      zan_c1_1[i_4]=
        mean(zan_a1[(i_4-5):B_1,(i_1+3)])
      zan_c1_2[i_4]=
        sd(zan_a1[(i_4-5):B_1,(i_1+3)])
      zan_c1_3[i_4]=
        max(zan_a1[(i_4-5):B_1,(i_1+3)])
      zan_c1_4[i_4]=
        min(zan_a1[(i_4-5):B_1,(i_1+3)])
      if(zan_c1_1[i_4]!=0){
        zan_c1_5[i_4]=zan_c1_2[i_4]/zan_c1_1[i_4]
      } else {zan_c1_5[i_4]=0}
    }
    zan_result2=cbind(zan_result2,zan_c1_1,
                      zan_c1_2,zan_c1_3,zan_c1_4,
                      zan_c1_5)
  }
  zan_a4=zan_a1
  zan_a4[,6]=zan_a4[,4]-zan_a4[,5]
  zan_c2_1=0
  zan_c2_2=0
  zan_c2_3=0
  zan_c2_4=0
  for (i_5 in 1:B_1) {
    if(zan_a4[i_5,6]>0){
      zan_c2_1[i_5]=0
      zan_c2_2[i_5]=1} else if(zan_a4[i_5,6]<0){
        zan_c2_1[i_5]=1
        zan_c2_2[i_5]=0} else {
          zan_c2_1[i_5]=0
          zan_c2_2[i_5]=0}
  }
  for (i_6 in 1:5) {
    zan_c2_3[i_6]=sum(zan_c2_1[1:(i_6+5)])
    zan_c2_4[i_6]=sum(zan_c2_2[1:(i_6+5)])
  }
  for (i_7 in 6:(B_1-5)) {
    zan_c2_3[i_7]=sum(zan_c2_1[(i_7-5):(i_7+5)])
    zan_c2_4[i_7]=sum(zan_c2_2[(i_7-5):(i_7+5)])
  }
  for (i_8 in (B_1-5):B_1) {
    zan_c2_3[i_8]=sum(zan_c2_1[(i_8-5):B_1])
    zan_c2_4[i_8]=sum(zan_c2_2[(i_8-5):B_1])
  }
  zan_result5=cbind(zan_c2_1,zan_c2_2,zan_c2_3,
                    zan_c2_4)
  zan_result3=as.data.frame(zan_result1[,-1])
  zan_result4=as.data.frame(zan_result2[,-1])
  result_1=cbind(zan_result3,zan_result4,zan_result5)
  colnames(result_1)=paste("X",c(348:383),sep = "")
  result_2=cbind(zan_b3,result_1)
  result_3=rbind(result_3,result_2)
}
result_4=result_3[-1,]
any(is.na(result_4))
write.csv(result_4,"feature_6.csv")


#SS
filenames_1=list.files(pattern="*.csv")
filenames_2=substr(filenames_1,1,nchar(filenames_1)-4)
s_1=length(filenames_1)
result_3=0
for (i in 1:s_1) {
  zan_a1=read.csv(filenames_1[i])
  B_1=nrow(zan_a1)
  zan_b1=as.data.frame(rep(filenames_2[i],B_1))
  zan_b2=as.data.frame(zan_a1[,3])
  zan_b3=cbind(zan_b1,zan_b2)
  colnames(zan_b3)=c("Protein_ID","Site_ID")
  zan_a2=as.data.frame(matrix(0,5,6))
  colnames(zan_a2)=colnames(zan_a1)
  zan_a3=rbind(zan_a2,zan_a1,zan_a2)
  result_1=0
  zan_result1=0
  zan_result2=0
  for (i_1 in 1:3) {
    b_1=zan_a3[1:B_1,(i_1+3)]
    b_2=zan_a3[2:(B_1+1),(i_1+3)]
    b_3=zan_a3[3:(B_1+2),(i_1+3)]
    b_4=zan_a3[4:(B_1+3),(i_1+3)]
    b_5=zan_a3[5:(B_1+4),(i_1+3)]
    b_6=zan_a3[6:(B_1+5),(i_1+3)]
    b_7=zan_a3[7:(B_1+6),(i_1+3)]
    b_8=zan_a3[8:(B_1+7),(i_1+3)]
    b_9=zan_a3[9:(B_1+8),(i_1+3)]
    b_10=zan_a3[10:(B_1+9),(i_1+3)]
    b_11=zan_a3[11:(B_1+10),(i_1+3)]
    zan_result1=cbind(zan_result1,b_1,b_2,b_3,
                      b_4,b_5,b_6,
                      b_7,b_8,b_9,
                      b_10,b_11)
    zan_c1_1=0
    zan_c1_2=0
    zan_c1_3=0
    zan_c1_4=0
    zan_c1_5=0
    for (i_2 in 1:5) {
      zan_c1_1[i_2]=
        mean(zan_a1[1:(i_2+5),(i_1+3)])
      zan_c1_2[i_2]=
        sd(zan_a1[1:(i_2+5),(i_1+3)])
      zan_c1_3[i_2]=
        max(zan_a1[1:(i_2+5),(i_1+3)])
      zan_c1_4[i_2]=
        min(zan_a1[1:(i_2+5),(i_1+3)])
      if(zan_c1_1[i_2]!=0){
        zan_c1_5[i_2]=zan_c1_2[i_2]/zan_c1_1[i_2]
      } else {zan_c1_5[i_2]=0}
      
    }
    for (i_3 in 6:(B_1-5)) {
      zan_c1_1[i_3]=
        mean(zan_a1[(i_3-5):(i_3+5),(i_1+3)])
      zan_c1_2[i_3]=
        sd(zan_a1[(i_3-5):(i_3+5),(i_1+3)])
      zan_c1_3[i_3]=
        max(zan_a1[(i_3-5):(i_3+5),(i_1+3)])
      zan_c1_4[i_3]=
        min(zan_a1[(i_3-5):(i_3+5),(i_1+3)])
      if(zan_c1_1[i_3]!=0){
        zan_c1_5[i_3]=zan_c1_2[i_3]/zan_c1_1[i_3]
      } else {zan_c1_5[i_3]=0}
    }
    for (i_4 in (B_1-4):B_1) {
      zan_c1_1[i_4]=
        mean(zan_a1[(i_4-5):B_1,(i_1+3)])
      zan_c1_2[i_4]=
        sd(zan_a1[(i_4-5):B_1,(i_1+3)])
      zan_c1_3[i_4]=
        max(zan_a1[(i_4-5):B_1,(i_1+3)])
      zan_c1_4[i_4]=
        min(zan_a1[(i_4-5):B_1,(i_1+3)])
      if(zan_c1_1[i_4]!=0){
        zan_c1_5[i_4]=zan_c1_2[i_4]/zan_c1_1[i_4]
      } else {zan_c1_5[i_4]=0}
    }
    zan_result2=cbind(zan_result2,zan_c1_1,
                      zan_c1_2,zan_c1_3,zan_c1_4,
                      zan_c1_5)
  }
  zan_windows=c(5,7,9,11)
  zan_c2_C=0
  zan_c2_H=0
  zan_c2_E=0
  zan_result3=0
  for (i_5 in 1:4) {
    zan_c1=zan_windows[i_5]
    for (i_6 in 1:((zan_c1-1)/2)) {
      zan_c1_1=zan_a1[1:(i_6+((zan_c1-1)/2)),3]
      zan_c2_C[i_6]=length(which(zan_c1_1=="C"))
      zan_c2_H[i_6]=length(which(zan_c1_1=="H"))
      zan_c2_E[i_6]=length(which(zan_c1_1=="E"))
    }
    for (i_7 in ((zan_c1-1)/2+1):(B_1-(zan_c1-1)/2)) {
      zan_c1_2=zan_a1[(i_7-(zan_c1-1)/2):(i_7+(zan_c1-1)/2),3]
      zan_c2_C[i_7]=length(which(zan_c1_2=="C"))
      zan_c2_H[i_7]=length(which(zan_c1_2=="H"))
      zan_c2_E[i_7]=length(which(zan_c1_2=="E"))
    }
    for (i_8 in (B_1-(zan_c1-1)/2+1):B_1) {
      zan_c1_3=zan_a1[(i_8-(zan_c1-1)/2):B_1,3]
      zan_c2_C[i_8]=length(which(zan_c1_3=="C"))
      zan_c2_H[i_8]=length(which(zan_c1_3=="H"))
      zan_c2_E[i_8]=length(which(zan_c1_3=="E"))
    }
    zan_result3=cbind(zan_result3,zan_c2_C,zan_c2_E,zan_c2_H)
  }
  zan_result4=as.data.frame(zan_result1[,-1])
  zan_result5=as.data.frame(zan_result2[,-1])
  zan_result6=as.data.frame(zan_result3[,-1])
  result_1=cbind(zan_result4,zan_result5,zan_result6)
  colnames(result_1)=paste("X",c(384:443),sep = "")
  result_2=cbind(zan_b3,result_1)
  result_3=rbind(result_3,result_2)
}
result_4=result_3[-1,]
any(is.na(result_4))
write.csv(result_4,file ="feature_7.csv")


#Charge and polarity
data_name=list.files(pattern="*.csv")
s_1=length(data_name)
zan_result=0
for (i in 1:s_1) {
  zan_data=read.csv(file = data_name[i])
  a_1=as.data.frame(zan_data[,1])
  a_2=as.data.frame(rep(substring(data_name[i],1,(nchar(data_name[i])-4)),
                        nrow(zan_data)) )
  a_3=cbind(a_2,a_1)
  zan_result=rbind(zan_result,a_3)
}
zan_result_2=zan_result[-1,]
P_Uc=c("G","S","T","C","Q","N","Y")
P_P=c("K","R","H")
P_N=c("D","E")
NP=c("A","V","L","I","P","F","W","M")
A_1=zan_result_2[,2]
for (i in 1:7) {
  A_1=gsub(P_Uc[i],"1",A_1)
}
A_2=zan_result_2[,2]
for (i in 1:3) {
  A_2[i]=gsub(P_P[i],"1",A_2)
}
A_3=zan_result_2[,2]
for (i in 1:2) {
  A_3=gsub(P_N[i],"1",A_3)
}
A_4=zan_result_2[,2]
for (i in 1:8) {
  A_4=gsub(NP[i],"1",A_4)
}
for (i in 1:nrow(zan_result_2)) {
  if(A_1[i]!="1"){A_1[i]="0"}
  if(A_2[i]!="1"){A_2[i]="0"}
  if(A_3[i]!="1"){A_3[i]="0"}
  if(A_4[i]!="1"){A_4[i]="0"}
}
AA_1=as.numeric(A_1)
AA_2=as.numeric(A_2)
AA_3=as.numeric(A_3)
AA_4=as.numeric(A_4)
result_1=cbind(zan_result_2,AA_1,AA_2,AA_3,AA_4)
colnames(result_1)=c("Protein_ID","Site_ID","X322","X323","X324","X325")
write.csv(result_1,"Feature_4")


#All feature
s_1=length(protein_sequence)
result_1=0
for (i in 1:s_1) {
  zan_a1=read.csv(protein_sequence[i],colClasses = c("character","character","numeric"))
  zan_b1=substring(protein_sequence[i],1,nchar(protein_sequence[i])-4)
  zan_c1_1=as.data.frame(feature_1[which(feature_1$Protein_ID==zan_b1),-c(1,2)])
  colnames(zan_c1_1)=colnames(feature_1)[-c(1,2)]
  zan_c1_2=as.data.frame(feature_2[which(feature_2$Protein_ID==zan_b1),-c(1,2)])
  zan_c1_3=as.data.frame(feature_3[which(feature_3$Protein_ID==zan_b1),-c(1,2)])
  zan_c1_4=as.data.frame(feature_4[which(feature_4$Protein_ID==zan_b1),-c(1,2)])
  zan_c1_5=as.data.frame(feature_5[which(feature_5$Protein_ID==zan_b1),-c(1,2)])
  zan_c1_6=as.data.frame(feature_6[which(feature_6$Protein_ID==zan_b1),-c(1,2)])
  zan_c1_7=as.data.frame(feature_7[which(feature_7$Protein_ID==zan_b1),-c(1,2)])
  zan_c1_8=as.data.frame(feature_8[which(feature_8$Protein_ID==zan_b1),-c(1,2)])
  zan_c1_9=as.data.frame(feature_9[which(feature_9$Protein_ID==zan_b1),-c(1,2)])
  zan_result1=cbind(zan_a1,zan_c1_1,zan_c1_2,zan_c1_3,zan_c1_4,
                    zan_c1_5,zan_c1_6,zan_c1_7,zan_c1_8,zan_c1_9)
  result_1=rbind(result_1,zan_result1)
  
}
result_2=result_1[-1,]
any(is.na(result_2))
write.csv(result_2, "All_feature.csv")
rm(list=ls())
