#### Data preprocessing for data from Bank of Japan
### Final output is called `boj.rds`.
### Run `readRDS("data/rds/boj.rds")` to get the preprocessed dataset  


foo <- read.csv("dat/boj/boj.csv", fileEncoding = "cp932", stringsAsFactors = F)

bar <- xts(sapply(foo[13:nrow(foo),-1],as.numeric), 
           order.by = as.yearmon(foo[13:nrow(foo),1], "%Y/%m")) %>% 
  set_colnames(foo[2,-1]) 

bojSeries <- bar["197001/201812",1:8] %>% 
  set_colnames(c("PriceIndicesWages_ExportPriceInd","PriceIndicesWages_ImportPriceInd",
                 "InterestRates_basicDiscountRate",
                 "MoneyCreditQuantityAggregate_MonetaryBaseBanknote",
                 "MoneyCreditQuantityAggregate_MonetaryBaseCion", "ExchangeRates_USDJPY",
                 "InterestRates_overnightEndMonth", "InterestRates_overnightAvgMonth"))

  

# Effective interest rate 
foo <- read.csv("dat/boj/bojExcRate.csv", fileEncoding = "cp932", stringsAsFactors = F)

bojExcRate <- xts(sapply(foo[13:nrow(foo),-1], as.numeric),
                  order.by = as.yearmon(foo[13:nrow(foo),1], "%Y/%m")) %>% 
  set_colnames(c("ExchangeRates_EffExcRateNominal","ExchangeRates_EffExcRateReal"))



boj <- merge.xts(bojSeries, bojExcRate)
saveRDS(boj, "rds/boj.rds")

