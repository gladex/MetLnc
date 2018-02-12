
ensem16=useMart("ENSEMBL_MART_ENSEMBL",
                host="jul2016.archive.ensembl.org/biomart/martservice/",
                #host="mar2016.archive.ensembl.org/biomart/martservice/",
                #host="may2009.archive.ensembl.org/biomart/martservice/",
                dataset="hsapiens_gene_ensembl")
chr.region=lncREG
chr.region
#chr.region = c("18:19092052:30289323",
#               "16:77462979:77500915",
#               "4:32776556:33393589",
#               "2:187947442:188506618")
entrez.ids=vector()
entrez.count=vector()
all.results=data.frame()
for(i in 1:length(chr.region)) {
  filterlist=list(chr.region[i],"protein_coding")
  results=getBM(attributes=c("hgnc_symbol",
                             "entrezgene",
                             "chromosome_name",
                             "start_position",
                             "end_position"), 
                filters=c("chromosomal_region","biotype"), 
                values=filterlist, 
                mart=ensem16)
  
  if(is.na(results[1,1])) {
    next  # Jump to Next Loop if No Gene Found
  }
  
  results$region=chr.region[i]
  all.results=rbind(all.results,results)
  
  ids=unique(results$entrezgene)
  ids=ids[!is.na(ids)]
  
  entrez.ids[i]=paste(ids, sep=",", collapse=",")
  entrez.count[i]=unique(length(ids))
}


