fun_combPlot <- function(Sp,Sr) {

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
  X1=NZ[1,int]; X2=NZ[2,int]; #dat=data.frame(X1,X2)
  coefs=cor(NZ[1,int],NZ[2,int])
  z <- kde2d(X1, X2, n=50)
  #L=lm(X2 ~ X1)
  k <- 11
  #my.cols <- rev(brewer.pal(k, "RdYlBu"))
  par(mar=c(3,3,3,3)) 
  plot(NZ[1,int],NZ[2,int],pch='.',col='blue',
       #xlab="450K methylation (score)",
       #ylab="RRBS methylation (100%)",
       #main="Genome-wide methylation pairwise correlation",
       xlim=c(0,1),ylim=c(0,1))
  contour(z, drawlabels=F, nlevels=k, col='red', lwd=2, add=T)
      # col='red'my.cols
  #OK:abline(h=mean(X1), v=mean(X2), lwd=2)
  #abline(a=0,b=coefs,lwd=2,col='royalblue')
  #abline(coef=coef(L),col='royalblue')
  mtext("450K methylation (score)", side=1, line=1.75, cex=1, font=2)
  mtext("RRBS methylation (100%)", side=2, line=1.75, cex=1, font=2)
  legend("topleft",paste('R =',signif(coefs,5)),cex=1.5,bty="n")
  
  grid()
  return(signif(coefs,5))
}
