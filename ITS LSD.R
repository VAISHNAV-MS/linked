LATIN SQUARE DESIGN

row=rep(1:5,each=5)   # column
col=rep(1:5,5)        # rows

treatment=c("B","D","E","C","A","C","A","B","D","E","D","C","A","E","B","E","B","C","A","D",
            "A","E","D","B","C")

A=c(4.5,6,3.6,10,9)
B=c(8.6,8.6,6.5,7,5)
C=c(7.5,9,7.8,4.2,6)
D=c(5,7.2,9.5,7.9,8.5)
E=c(8.8,5.9,6.9,7.5,6.9)

values=c(A,B,C,D,E);values


data=data.frame(row,col,treatment,values);data

strvec = sort(unique(data$treatment))

l = length(unique(data$treatment));l

ROWSUM=c()
for(i in 1:5){
  ROWSUM[i]=sum(data$values[data$row==i])
}
ROWSUM

COLSUM=c()
for(i in 1:5){
  COLSUM[i]=sum(data$values[data$col==i])
}
COLSUM

TRTSUM=c()
for(i in 1:5){
  TRTSUM[i]=sum(data$values[data$treatment==strvec[i]])
}
TRTSUM

G=sum(COLSUM);G # cf=G^2/N
cf=G^2/(l*l);cf
TSS= sum((data$values)^2)-cf ;TSS #TSS= sum(data2^2)-cf
ROWSS= (sum(ROWSUM^2))/l-cf  ;ROWSS #  BSS=(sum(B^2)/c-cf)
COLSS= (sum(COLSUM^2)/l)-cf  ; COLSS # CSS=(sum(C^2)/b-cf)
TRTSS= (sum(TRTSUM^2)/l)-cf  ; TRTSS
ESS=TSS-ROWSS-COLSS-TRTSS         ; ESS


dfT=l-1;dfT #  dfT=N-1
dfR=l-1; dfR
dfC=l-1; dfC
dfE= ((l-1)*(l-2)) ;dfE
dfTOT=(l)*(l)-1; dfTOT

msT=TRTSS/dfT;msT
msR=ROWSS/dfR;msR
msC=COLSS/dfC;msC
msE=ESS/dfE;msE

Ft=msT/msE;Ft
Fr=msR/msE;Fr
Fc=msC/msE;Fc

Ft_tab=qf(0.95,dfT,dfE);Ft_tab
Fr_tab=qf(0.95,dfR,dfE);Fr_tab
Fc_tab=qf(0.95,dfC,dfE);Fc_tab

sov=c('ROW','COL','TREATMENT','ERROR', 'Total')
df=c(dfR,dfC,dfT,dfE,dfTOT)
ss=c(ROWSS,COLSS,TRTSS,ESS,TSS)
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
