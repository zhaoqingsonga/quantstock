library(openxlsx)

  write.table(read.xlsx("HS300.xlsx"),"data/HS300.txt",row.names=FALSE)
  write.table(read.xlsx("SS50.xlsx"),"data/SS50.txt",row.names=FALSE)
