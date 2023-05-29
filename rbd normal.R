d1=c(73,68,74,71,61)
d2=c(73,67,75,72,70)
d3=c(75,68,78,73,68)
d4=c(73,71,75,75,69)

RBDM=data.frame(d1,d2,d3,d4);RBDM


t=nrow(RBDM);b=ncol(RBDM);t;b


CS=colSums(RBDM);CS
RS=rowSums(RBDM);RS

g=sum(CS);g #grand total

# cf=(g^2)/(nrow(RBDM)*ncol(RBDM));cf  # correction factor

cf=(g^2)/(t*b);cf

#trss=(sum(CS^2)/nrow(RBDM))-cf;trss  #treatment ss
bss=(sum(CS^2)/t)-cf;bss

#blss=(sum(RS^2)/ncol(RBDM))-cf;blss   # block ss
trss=(sum(RS^2)/b)-cf;trss



#TSS=sum(mat*mat)-cf;TSS
tss=sum(RBDM^2)-cf;tss


ess=tss-bss-trss;ess


dftr=t-1;dftr #  dfT=N-1
dfb=b-1; dfb
dfe= ((t-1)*(b-1)) ;dfe
dftot=(t)*(b)-1; dftot

mstr=trss/dftr;mstr
msb=bss/dfb;msb
mse=ess/dfe;mse

Ft=mstr/mse;Ft
Fb=msb/mse;Fb


Ft_tab=qf(0.95,dftr,dfe);Ft_tab
Fb_tab=qf(0.95,dfb,dfe);Fb_tab


sov=c('BLOCK','TREATMENT','ERROR', 'Total')
df=c(dfb,dftr,dfe,dftot)
ss=c(bss,trss,ess,tss)
ms=c(msb,mstr,mse,'')
f=c(Fb,Ft,'','')
f_tab=c(Fb_tab,Ft_tab,'','')

anova=data.frame(sov,df,ss,ms,f,f_tab)
anova

decisionB=ifelse(Fb_tab>Fb,'Fr_tab > F_r , Accept H0 BLOCK',
                 'F_tab < F_cal , Reject H0 BLOCK')


decisionT=ifelse(Ft_tab>Ft,'Fr_tab > F_r , Accept H0 trt',
                 'F_tab < F_cal , Reject H0 trt')

print(decisionB)

print(decisionT)     

