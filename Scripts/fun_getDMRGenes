
fun_getDMRGenes <- function(myDmr, subGeneReg) {
  DMG=NULL
  
  for(i in 1:length(myDmr)){
    genes=get.dmr.genes(myDMR=myDmr[i],
                        subject=subGeneReg,
                        id.type="gene.symbol")
    #genes
    if(length(genes) != 0L) {
      met=myDmr[i]$mean.meth.diff
      genes=data.frame(genes,c(rep(met,length(genes))))
      DMG=rbind(DMG,genes)
      #append(DMG, genes)
    }
    rm(genes)
  }
  #class(DMG)  # data.frame
  colnames(DMG)=c("SYMBOL","Methy")
  DMGsimp <- aggregate(.~ SYMBOL, data=DMG, mean)
  #head(DMGsimp)
  return(DMGsimp)
}
