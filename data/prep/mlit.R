#### Data preprocessing for data from Ministry of Land, Infrastracture, Transport and Tourism

### Final output is called `mlit.rds`.
### Run `readRDS("data/rds/mlit.rds")` to get the preprocessed dataset  


foo <- read_excel("dat/mlit/jyuu4.xls") %>% 
  magrittr::extract(c(2,3,209:821), c(1,2,4,6,10,12,14,16,20))

newHousing <- xts(sapply(foo[3:nrow(foo), 2:ncol(foo)], as.numeric), 
                  order.by = as.yearmon(1968 + seq(0, (nrow(foo)-3))/12)) %>% 
  set_colnames(c("HousingStartsSales_NumTotal","HousingStartsSales_NumOwned",
                 "HousingStartsSales_NumRented", "HousingStartsSales_NumSales",
                 "HousingStartsSales_NewFloorTotal","HousingStartsSales_NewFloorOwned",
                 "HousingStartsSales_NewFloorRented","HousingStartsSales_NewFloorSales"))

mlit <- newHousing["197001/201812"]

saveRDS(mlit, "rds/mlit.rds")
