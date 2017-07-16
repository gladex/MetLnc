
rm(list=ls())
library(limma)
design <- model.matrix(~0 +factor(c(rep(1,12),rep(2,12))))
design
# 1: Carcinoma Tissues
colnames(design)=c("group1","group2")  
# 2: Para-Carcinoma Tissues
design

fit<-lmFit(C[,1:24],design)
contrast.matrix <- makeContrasts(group1-group2, levels=design)
fit2 <- contrasts.fit(fit, contrast.matrix)
fit2 <- eBayes(fit2)
fit2  # 485577 x 1
Tab=topTable(fit2, coef=1, number=485577, adjust="BH", sort.by="none") #number=20, 
dim(Tab) # 485577 x 6
Tab2=cbind(Tab, C[,25:41])
head(Tab2,2)
# Checking:
# tt=mean(as.numeric(C[1,1:12]))-mean(as.numeric(C[1,13:24]))

library(ggplot2)
adPV=1.3e-4; FC=0.2; PS=0.5;
g1=ggplot(data=Tab,aes(x=logFC,y=-log10(adj.P.Val)))+
  geom_point(colour='black',size=PS)+expand_limits(y=0)+theme_bw()
g1+geom_point(data=subset(Tab,adj.P.Val<=adPV & abs(logFC)>=FC),aes(x=logFC,y=-log10(adj.P.Val)),colour='red',size=PS)+
  geom_point(data=subset(Tab,abs(logFC)<FC),aes(x=logFC,y=-log10(adj.P.Val)),colour='gray',size=PS)+
  xlab("Differential methylation value")+ ylab("-log10(adjusted P.Value)")+
  geom_vline(xintercept=-FC,colour='gray')+
  geom_vline(xintercept=FC,colour='gray')+
  geom_hline(yintercept=-log10(adPV),colour='gray')+
  #theme(legend.key=element_rect(colour="black", fill="yellow"))+
  geom_text(size=3,aes(x=0,y=15,label=''))
