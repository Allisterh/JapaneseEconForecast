#### Data preprocessing for data from Thomson Reuter Data Stream

### Final output is called `ds.rds`.
### Run `readRDS("data/rds/ds.rds")` to get the preprocessed dataset  

foo <- read_excel("dat/ds/Book1.xlsx")
ds1 <- xts(sapply(foo[,-c(1,7:11,16:18)], as.numeric),
           order.by = as.yearmon(foo$X__1, "%Y-%m-%d")) %>% 
  set_colnames(c("StockPrices_TOPIX", "EmploymentHours_UnempRate",
                 "PriceIndicesWages_ProducerPrice","PriceIndicesWages_TOT",
                 "RetailManufacturingTradeSales_CorpTaxRevenue", 
                 "InterestRates_interestBeringGBond10y","OutputIncome_ProdCapacityManufacturing",
                 "Consumption_NewCar","InterestRates_LongPrime"))

foo <- read_excel("dat/ds/Book2.xlsx")
ds2<- xts(sapply(foo[,-c(1,6:8,10,13:16)], as.numeric),
          order.by = as.yearmon(foo$Name, "%Y-%m-%d")) %>% 
  set_colnames(c("RetailManufacturingTradeSales_ImportVolumeIndex",
                 "RetailManufacturingTradeSales_ExportVolumeIndex",
                 "MoneyCreditQuantityAggregate_AmoutClearingValue",
                 "MoneyCreditQuantityAggregate_AmoutClearingNum",
                 "InterestRates_STPrimeLending",
                 "StockPrices_N17", "StockPrices_N42"))

foo <- read_excel("dat/ds/Book3.xlsx")
ds3 <- xts(sapply(foo[,-c(1,5,16,19:22)], as.numeric),
          order.by = as.yearmon(foo$Name, "%Y-%m-%d")) %>% 
  set_colnames(c("EmploymentHours_NumUnemp","OutputIncome_HHdisposableIncome",
                 "StockPrices_N225","InventoriesOrders_machineryOrders",
                 "InventoriesOrders_machineryOrdersLessShip",  # 5
                 "InventoriesOrders_machineryOrdersLessVolatile",
                 "InterestRates_10y", "InterestRates_3m",
                 "InterestRates_spread","InventoriesOrders_BizExpendituresAll", # 10
                 "InterestRates_contractedNewLT", "InterestRates_contractedNewST",
                 "InterestRates_contractedNewGeneral","InterestRates_contractedST",
                 "InterestRates_contractedGeneral")) # 15

foo <- read_excel("dat/ds/Book4.xlsx")
ds4 <- xts(sapply(foo[,c(2,3,5,6,7,13)], as.numeric),
           order.by = as.yearmon(foo$Name, "%Y-%m-%d")) %>% 
  set_colnames(c("InterestRates_contractedLT", 
                 "MoneyCreditQuantityAggregate_MoneyStockM1",
                 "MoneyCreditQuantityAggregate_MoneyStockM2_SA",
                 "MoneyCreditQuantityAggregate_MoneyStockM3",
                 "MoneyCreditQuantityAggregate_MoneyStockL",
                 "MoneyCreditQuantityAggregate_MonetaryBaseReserve"))

foo <- read_excel("dat/ds/Book5.xlsx")
ds5 <- xts(sapply(foo[,2:5], as.numeric),
           order.by = as.yearmon(foo$...., "%Y-%m-%d")) %>% 
  set_colnames(c("MoneyCreditQuantityAggregate_MoneyStockM2",
                 "Consumption_durable", "Consumption_total",
                 "Consumption_nondurable"))

ds <- merge.xts(ds1,ds2,ds3,ds4,ds5)["197001/201812"]

saveRDS(ds, "rds/ds.rds")
