library(openxlsx)

  write.table(read.xlsx("HS300.xlsx"),"data/HS300.txt",row.names=FALSE)
  write.table(read.xlsx("SS50.xlsx"),"data/SS50.txt",row.names=FALSE)
#update_stock_data
  # stock_data_ss50<-getListStocks(SS50$code)
  # stock_data_hs300<-getListStocks(HS300$code)
  # save(stock_data_hs50,file="E:/FangCloudSync/R_WD360/Project/quantstock/data/stock_data_ss50.rda")
  # save(stock_data_hs300,file="E:/FangCloudSync/R_WD360/Project/quantstock/data/stock_data_hs300.rda")
