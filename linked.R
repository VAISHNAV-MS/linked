install.packages("rmarkdown
                ")

# CRD ( LAB PRACTICAL 1)

d1=c(1,2,3,4,5)
d2=c(23,45,33,67,89)
d3=c(3,66,34,22,5)
d4=c(11,47,72,43,99)
d5=c(33,45,64,21,43)

doe=data.frame(d1,d2,d3,d4,d5)

doe
rowSums(doe)
colSums(doe)
#total
g=sum(rowSums(doe))
g
cf=(g^2/25)#correction factor
m=as.matrix(doe)
t=sum(m*m)
ti=sum(colSums(doe)^2) 
tss=ti-cf #total ss
trss=(ti/nrow(doe))-cf # treatment ss
ess=tss-trss # error ss
mstr=trss/4 #ms treatment
mse=ess/20 # ms error
f=mstr/mse # f ratio
f
mstr
mse
ess
trss
tss
?df
df(4,20)
