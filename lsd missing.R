LATIN SQUARE DESIGN WITH ONE MISSING VALUE

row=rep(1:5,each=5)   # row
coln=rep(1:5,5)        # columnn     'A'.'B'.'

treatment=c('A','D','E','B','C',
           'C','B','A','E','D',
           'B','C','D','A','E',
           'E','A','C','D','B',
           'B','E','D','C','A')
treatment
values=c(9.4,10.9,7.8,14,16.3,
         13.8,8.5,NA,9.6,9.9,
         12.1,19.9,11,10.5,9.1,
         10.8,12.1,17.7,13.1,9.4,
         13.8,10.8,16.3,20.4,12.1)    
values



LSDMIS=data.frame(row,coln,treatment,values); LSDMIS

LSDMIS$row=as.factor(LSDMIS$row)  # converting rs,cln,trt as factors

LSDMIS$coln=as.factor(LSDMIS$coln)

LSDMIS$treatment=as.factor(LSDMIS$treatment)
LSDMIS$values

# MISSING VALUE

m=match(NA,LSDMIS$values);m
missrow= LSDMIS$row[m];missrow
misscol= LSDMIS$coln[m];misscol
misstre= LSDMIS$treatment[m];misstre


LSDMIS$values[m]=0; LSDMIS$values

LSDMIS

strvec = sort(unique(LSDMIS$treatment))

Rowsum=c()
for(i in 1:5){
  Rowsum[i]=sum(LSDMIS$values[LSDMIS$row==i])
}
Rowsum

colnsum=c()
for(i in 1:5){
  colnsum[i]=sum(LSDMIS$values[LSDMIS$coln==i])
}
colnsum

trtsum=c()
for(i in 1:5){
  trtsum[i]=sum(LSDMIS$values[LSDMIS$treatment==strvec[i]])
}
trtsum

# SUM BEFORE MISSING VALUES

missingrowsum= sum(LSDMIS$values[LSDMIS$row==2])
missingcolsum= sum(LSDMIS$values[LSDMIS$coln==3])
missingtrtsum= sum(LSDMIS$values[LSDMIS$treatment==strvec[1]])

r=sum(LSDMIS$values[LSDMIS$row==2]);r
c=sum(LSDMIS$values[LSDMIS$coln==3]);c
t=sum(LSDMIS$values[LSDMIS$treatment==strvec[1]]);t


G=sum(colnsum);G

l = length(unique(LSDMIS$treatment));l

  x = (l*(r+c+t) - 2*G)/((l - 1)*(l - 2))  ;x
  
#  LSDMIS$treatment[m]=x

  LSDMIS$values[m]=x; LSDMIS$values
  
# now we have substituted the missing value in lsd
  # we find aadjusted factor
  
  adj_f=(((l-1)*t+c+r-G)^2)/(((l-1)*(l-2))^2);adj_f
  
  # normal 3 way anova

  strvec = sort(unique(LSDMIS$treatment))
  
  ROWSUM=c()
  for(i in 1:5){
    ROWSUM[i]=sum(LSDMIS$values[LSDMIS$row==i])
  }
  ROWSUM
  
  COLSUM=c()
  for(i in 1:5){
    COLSUM[i]=sum(LSDMIS$values[LSDMIS$coln==i])
  }
  COLSUM
  
  TRTSUM=c()
  for(i in 1:5){
    TRTSUM[i]=sum(LSDMIS$values[LSDMIS$treatment==strvec[i]])
  }
  TRTSUM
  
  GNEW=sum(COLSUM);GNEW # cf=G^2/N
 cf=GNEW^2/(l*l);cf
 TSS= sum((LSDMIS$values)^2)-cf ;TSS #TSS= sum(data2^2)-cf
 ROWSS= (sum(ROWSUM^2))/l-cf  ;ROWSS #  BSS=(sum(B^2)/c-cf)
 COLSS= (sum(COLSUM^2)/l)-cf  ; COLSS # CSS=(sum(C^2)/b-cf)
 TRTSS= (sum(TRTSUM^2)/l)-cf  ; TRTSS
  ESS=TSS-ROWSS-COLSS-TRTSS ; ESS
  adjTSS= TRTSS-adj_f;adjTSS
  
 dfT=l-1;dfT #  dfT=N-1
  dfR=l-1; dfR
  dfC=l-1; dfC
  dfE= ((l-1)*(l-2))-1 ;dfE
  dfTOT=(l)*(l)-1
  
  msT=adjTSS/dfT
  msR=ROWSS/dfR
  msC=COLSS/dfC
  msE=ESS/dfE
  
  Ft=msT/msE
  Fr=msR/msE
  Fc=msC/msE
  
  Ft_tab=qf(0.95,dfT,dfE)
  Fr_tab=qf(0.95,dfR,dfE)
  Fc_tab=qf(0.95,dfC,dfE)
  
  sov=c('ROW','COL','TREATMENT','ERROR', 'Total')
  df=c(dfR,dfC,dfT,dfE,dfTOT)
  ss=c(ROWSS,COLSS,adjTSS,ESS,TSS)
  ms=c(msR,msC,msT,msE,'')
  f=c(Fr,Fc,Ft,'','')
  f_tab=c(Fr_tab,Fc_tab,Ft_tab,'','')
  
  ano=data.frame(sov,df,ss,ms,f,f_tab)
  ano
  
  decisionR=ifelse(Fr_tab>Fr,'Fr_tab > F_r , Accept H0 row',
                  'F_tab < F_cal , Reject H0 row')
  
  decisionC=ifelse(Fc_tab>Fc,'Fc_tab > F_r , Accept H0 coln',
                   'F_tab < F_cal , Reject H0 coln')
  
  decisionT=ifelse(Ft_tab>Ft,'Fr_tab > F_r , Accept H0 trt',
                   'F_tab < F_cal , Reject H0 trt')
  
  print(decisionR)
  print(decisionC)
  print(decisionT)
   
  
  
  
  
                                     
