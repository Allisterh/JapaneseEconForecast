#### Data preprocessing for data from Nikkei NEEDS Financial Quest database
# rm(list=ls())
### Final object is called `needs.rds`.
### Run `readRDS("data/rds/needs.rds")` to get preprocessed dataset


# foo <- read_excel("needs/FqReport1.xlsx", sheet=2) %>% 
#   magrittr::extract(18:602, 2:16) 
# fq1 <- xts(sapply(foo[,-1], as.numeric), 
#            order.by=as.yearmon(foo$X__2, format="%Y/%m"))

foo <- read_excel("dat/needs/FqReport3.xlsx", sheet=2) %>% 
  magrittr::extract(18:602, 2:4)
fq3 <- xts(sapply(foo[,-c(1,2)], as.numeric), as.yearmon(foo$X__2, "%Y/%m")) %>% 
  set_colnames(c("RetailManufacturingTradeSales_CustomClearance"))

# foo <- read_excel("needs/FqReport4.xlsx", sheet=2) %>% 
#   magrittr::extract(3:425, 2:4)
# fq4 <- xts(sapply(foo[,-1], as.numeric), as.yearmon(foo$X__2, "%Y/%m"))

foo <- read_excel("dat/needs/FqReport5.xlsx", sheet=2) %>% 
  magrittr::extract(3:587, 2:13)
fq5 <- xts(sapply(foo[,8], as.numeric), as.yearmon(foo$X__2, "%Y/%m")) %>% 
  set_colnames(c("InventoriesOrders_circumstantialIndManufacturing"))



fq <- merge.xts(fq3, fq5)

saveRDS(fq, "rds/needs.rds")
