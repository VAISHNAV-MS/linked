# 2^2 factorial experiment

#from the given data, we segregate the replicates based on the factors and levels.

a0b0=c(28,25,27)
a1b0=c(36,32,32)
a0b1=c(18,19,23)
a1b1=c(31,30,29)

data=data.frame(a0b0,a1b0,a0b1,a1b1)

# replicates sum of square
r=nrow(data) # is the number of replcates
g=sum(data);g
cf=g^2/(2^2*(r))

sum(rowSums(data)^2)/3
rss=sum(colSums(data)^2)/3-cf;rss

#now we create yates table where 

I=a0b0
  a=a1b0
  b=a0b1
  ab=a1b1

  #col1-total
#I=sum(data[1,])
#a=sum(data[2,])
#b=sum(data[3,])
#ab=sum(data[4,])

  I=sum(data[1])
  a=sum(data[2])
  b=sum(data[3])
  ab=sum(data[4])
  
  # col2
#sum(I+a);sum(b+ab);sum(a-I);sum(ab-b)
u1=I+a
u2=b+ab
u3=a-I
u4=ab-b

c((I+a),(b+ab),(a-I),sum(ab-b))

#col3

v1=(u1+u2)
v2=(u3+u4)
v3=u2-u1
v4=u4-u3

# yates table
col1=c('a0b0','a1b0','a0b1','a1b1')
col2=c(I,a,b,ab)
col3=c(u1,u2,u3,u4)
col4=c(v1,v2,v3,v4)
yates=data.frame(col1,col2,col3,col4)
# square sum

G=v1;G
tss=sum(data^2)-cf
ssa=v2^2/(4*r)-cf
ssb=v3^2/(4*r)-cf
ssab=v4^2/(4*4)-cf
sse= tss-ssa-ssb-ssab-rss;sse

#df
dfr=r-1
dfa=1
dfb=1
dfab=1
dfe=6
dft=11
# ms

msr=rss/dfr
msa=sa/dfa
msb=sb/dfb
msab=sab/dfab
mse=sse/dfe

sa=v2^2/12
sb=v3^2/12
sab=v4^2/12

sse=tss-rss-sa-sb-sab;sse

msa=sa/dfa
msb=sb/dfb
msab=sab/dfab

#f
fr=msr/mse
#fa=msa/mse
#fb=msb/mse
#fab=msab/mse

fa=msa/mse
fb=msb/mse
fab=msab/mse

f1=qf(0.95,dfa,dfe)
f2=qf(0.95,dfr,dfe)

decisionr=ifelse(fr<f2,'ACCEPT H0 REPLICATES','REJECT H0 REPLICATES')

decisiona=ifelse(fa<f1,'ACCEPT H0 me A','REJECT H0 ME A')

decisionb=ifelse(fb<f1,'ACCEPT H0 ME B','REJECT H0 ME B')

decisionab=ifelse(fab<f1,'ACCEPT H0 IE  A','REJECT H0 IE AB')

print(decisionab)
print(decisiona)
print(decisionb)
print(decisionr)

# anova table

sov=c('REPLICATES','A','B','AB','ERROR', 'Total')
#df=c(dfr,dfa,dfb,dfab,dfe,dft)
df=c(dfr,dfa,dfb,dfab,dfe,dft)
#ss=c(rss,ssa,ssb,ssab,sse,tss)
ss=c(rss,sa,sb,sab,sse,tss)
ms=c(msr,msa,msb,msab,mse,'')
f=c(fr,fa,fb,fab,'','')
f_tab=c(f2,f1,'','','','')

anova=data.frame(sov,df,ss,ms,f,f_tab);anova

# using aov
repl=as.factor(rep(1:3,4))
a=as.factor(rep(0:1,each=3,times=2))
b=as.factor(rep(0:1,each=6))
values=c(28,25,27,
         36,32,32,
         18,19,23,
         31,30,29)
data=data.frame(repl,a,b,values)
anv=aov(values~repl+a+b+a*b,data);summary(anv)

