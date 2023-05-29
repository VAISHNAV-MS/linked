COMPLETELY RANDOMISED DESIGN

# CRD ( LAB PRACTICAL 1)

# A person wanted to purchase a lot of electric drills,
# He got the quotations from five manufacturers. For selection ,
# he wanted to conduct an experiment to estimate the time taken by each drill
# in making a hole in a metallic sheet.As the sheet might not be uniform all 
# over with respect to thickness and hardness, he marked 20 places on the sheet 
# and applies 5 random drillls for each concerned in 4 random places to make
# holes. The time for making each hole is recorded and is given below.



# R1    R2    R3    R4
# 1 D1_19 D5_29 D1_26 D5_28
# 2 D3_22 D2_24 D4_25 D3_25
# 3 D4_20 D5_30 D1_16 D5_31
# 4 D1_20 D3_24 D2_22 D4_28


d1=c(19,20,16,16)   #SEPERATING INDIVIDUAL TREATMENTS 
d2=c(24,26,22,27)
d3=c(22,24,25,20)
d4=c(20,25,28,27)
d5=c(29,30,28,31)

doe=data.frame(d1,d2,d3,d4,d5)   # creating a df for treatments
names(doe)=c("D1","D2","D3","D4","D5")
doe 
#read.table, readLines,load
rowSums(doe)
colSums(doe)
c=ncol(doe);c
r=nrow(doe);r
n=(c*r);n
#total
g=sum(rowSums(doe));g

cf=(g^2/(r*c));cf #correction factor
m=as.matrix(doe)   # converting the d.f as a matrix to implement operations
t=sum(m*m)  ;t       # total square sum
ti=sum(colSums(doe)^2);ti   # column square sum
tss=t-cf;tss            #total ss
trss=(ti/nrow(doe))-cf;trss # treatment ss
ess=tss-trss;ess # error ss
mst=trss/r;mst #ms treatment
mse=ess/(((n-1)-(c-1)));mse # ms error
f=mst/mse;f # f ratio
dftr=c-1;dftr
dfe=n-c;dfe

Ft_tab=qf(0.95,dftr,dfe);Ft_tab



sov=c('TREATMENT','ERROR', 'Total')
df=c(dftr,dfe,dftot)
ss=c(trss,ess,tss)
ms=c(mst,mse,'')
f=c(Ft,'','')
f_tab=c(Ft_tab,'','')

anov=data.frame(sov,df,ss,ms,f,f_tab)
anov




decisionT=ifelse(Ft_tab>Ft,'Ft_tab > F_t , Accept H0 trt',
                 'F_tab < F_cal , Reject H0 trt')

print(decisionT)
