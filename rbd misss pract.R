d1=c(73,68,74,71,61)
d2=c(73,67,75,72,70)
d3=c(75,68,78,NA,68)
d4=c(73,71,75,75,69)

rbd=data.frame(d1,d2,d3,d4);rbd  # coln= treatments

t=ncol(rbd);t
b=nrow(rbd);b
n=b*t;n

# rbd missing ku ena pananum!

match(NA,rbd)
rs=rowSums(rbd)
cs=colSums(rbd
          )
rm=match(NA,rs);rm
cm=match(NA,cs)
rbd[rm,cm]
rbd[rm,cm]=0

rsm=rowSums(rbd)
csm=colSums(rbd)# treatments
B=rsm[rm];T=csm[cm];G=sum(rbd)

t=ncol(rbd)
b=nrow(rbd)
x=((t*T)+(B*b)-G)/((t-1)*(b-1));x

rbd[rm,cm]=x


adjx=((T*t)+B-G)^2/((t-1)*t*(b-1)*(b-1));adjx







cs=colSums(rbd);cs  # treatments
rs=rowSums(rbd);rs

g=sum(rbd);g
cf=g^2/n;cf

tss=sum(rbd^2)-cf;tss
trss=sum(cs^2)/b-cf;trss
bss=sum(rs^2)/t-cf;bss
ess=tss-trss-bss;ess

dftr=t-1;dftr
dfb=b-1;dfb
dfe=(t-1)*(b-1)-1;dfe
dftot=(t*b)-1-1;dftot

mstr=trss/dftr;mstr
msb=bss/dfb;msb
mse=ess/dfe;mse

ft=mstr/mse;ft
fb=msb/mse;fb

ft_cri=qf(0.95,dftr,dfe);ft_cri
fb_cri=qf(0.95,dfb,dfe);fb_cri

decisiont=ifelse(ft<ft_cri,'ACCEPT H0 OF TREATMENT','REJECT H0 OF TREATMENT')
print(decisiont)
decisionb=ifelse(fb<fb_cri,'ACCEPT H0 OF block','REJECT H0 OF block')
print(decisionb)
