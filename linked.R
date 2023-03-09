install.packages("rmarkdown
                ")

# CRD ( LAB PRACTICAL 1)

# A person wanted to purchase a lot of electric drills,
# He got the quotations from five manufacturers. For selection ,
# he wanted to conduct an experiment to estimate the time taken by each drill
# in making a hole in a metallic sheet.As the sheet might not be uniform all 
# over with respect to thickness and hardness, he marked 20 places on the sheet 
# and applies 5 random drillls for each concerned in 4 random places to make
# holes. The time for making each hole is recorded and is given below.

R1=c("D1_19","D3_22","D4_20","D1_20")
R2=c("D5_29","D2_24","D5_30","D3_24")
R3=c("D1_26","D4_25","D1_16","D2_22")
R4=c("D5_28","D3_25","D5_31","D4_28")
R5=c("D4_27","D1_16","D2_27","D3_20")


data.frame(R1,R2,R3,R4)

# R1    R2    R3    R4
# 1 D1_19 D5_29 D1_26 D5_28
# 2 D3_22 D2_24 D4_25 D3_25
# 3 D4_20 D5_30 D1_16 D5_31
# 4 D1_20 D3_24 D2_22 D4_28


d1=c(19,20,16,16)   #SEPERATING INDIVIDUAL TREATMENTS 
d2=c(24,26,22,27)
d3=c(22,24,25,20)
d4=c(20,25,28,27)
d5=c(30,28,31,20)

doe=data.frame(d1,d2,d3,d4,d5)   # creating a df for treatments

doe
rowSums(doe)
colSums(doe)
#total
g=sum(rowSums(doe))
g
cf=(g^2/20)#correction factor
m=as.matrix(doe)   # converting the d.f as a matrix to implement operations
t=sum(m*m)         # total square sum
ti=sum(colSums(doe)^2)   # column square sum
tss=t-cf #total ss
trss=(ti/nrow(doe))-cf # treatment ss
ess=tss-trss # error ss
mstr=trss/4 #ms treatment
mse=ess/15 # ms error
f=mstr/mse # f ratio
f
mstr
mse
ess
trss
tss
?df
qf=qf(0.95,4,15)   # to find the f critical/table  value wrt alpha,df1,df2
cf
qf>f# henc we accept the null hypothesis

pf=pf(f,4,15,lower.tail=F)# so when (pf<alpha)=true then accept h.
# so we check the p value with alpha value, so we dont need to find
# for different df and alpha value what is the f tabulated value
# makes this easy.
pfsummary(doe)
pf
pf>qf
# p value => use pf(f-cal,df1,df2)

pf=1-pf(f,4,15)
pf<0.95
