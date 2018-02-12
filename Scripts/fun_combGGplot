fun_combGGplot <- function(Sp,Sr) {
  
  NZ=array(0,dim=c(2,ncol(Sp)))
  for(i in 1:ncol(Sp)){
    if(Sp[1,i]>0){
      NZ[1,i]=Sp[2,i]/Sp[1,i]
      } else {
        NZ[1,i]=0 }
    if(Sr[1,i]>0){
      NZ[2,i]=Sr[2,i]/Sr[1,i]
      } else {
        NZ[2,i]=0 }
    }
  
  ## None-Zero Entry
  p=Sp[1,]>0; r=Sr[1,]>0; int=(p & r)
  #cor(NZ[1,int],NZ[2,int])
  ## OK
  X1=NZ[1,int]; X2=NZ[2,int]; dat=data.frame(X1,X2)
  
  par(mar=c(3,3,3,3))
  # tiff(file="a4_Pairwise_MCF7_nz.tiff",width=4,height=4,units='in',res=300)
  ggplot(dat,aes(x=X1,y=X2))+
    theme_bw() +
    xlab("450K methylation (score)")+ylab("RRBS methylation (100%)")+
    geom_point(shape=20,alpha=0.05,colour="navyblue")+
      # dodgerblue #1177FF")+ #,color="red")+
    scale_colour_brewer()+#palette="Set2")+
    geom_density2d(color="red",width=3)+
    geom_text(color="black",size=4,  #hjust=1.0, 
              aes(x=0.1,y=0.9,label=paste('R =',signif(cor(X1,X2),4))))+
    theme(axis.title=element_text(face="bold",colour="black",size=12))
  #print(pp)
# dev.off()
}

