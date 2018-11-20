#### Data preprocessing for data from Ministry of Economy, Trade and Industry

### Final output is called `meti.rds`.
### Run `readRDS("data/rds/meti.rds")` to get the preprocessed dataset  

# 1. industrial Production, by industry (200801/201808)
codes <- c("2000000000","2A00000000","2AA0000000","2AB0000000","2AC0000000",
           "2AH0000000","2AI0000000","2AJ0000000","2AK0000000","2AM0000000",
           "2AN0000000","2AO0000000","2B00000000")

foo <- read_excel("dat/meti/iipIndustry.xls", skip=2, col_names=F) %>% 
  filter(X__1 %in% c("Item_Number", codes)) %>%
  t %>% magrittr::extract(c(2, 4:131),) %>%
  return

iipInd <- xts(foo[2:nrow(foo), 2:ncol(foo)], 
              order.by= as.yearmon(foo[-1,1], "%Y%m")) %>% 
  set_colnames(paste("Index of Industrial Production, ", foo[1,-1]))

# 2. Industrial Production, by industry, Connected Indices (197801/201212)

foo <- read_excel("dat/meti/iipIndustryCon.xls", skip=2) %>% 
  select("ITEM NO.",codes) %>%
  return

iipIndCon <- xts(sapply(foo[3:nrow(foo), 2:ncol(foo)], as.numeric), 
              order.by= as.yearmon(sapply(foo[3:nrow(foo),1],as.character), "%Y%m")) %>% 
  set_colnames(paste ("Index of Industrial Production, ",foo[1,-1]))

# 3. Industrial Production, by use of goods (200801/201808)
codes <- c("5000000201","5000000202","5000000203","5000000212","5000000223",
           "5000000224","5000000232","5000000243","5000000244","5000000262")

foo <- read_excel("dat/meti/iipGoods.xls", skip=2) %>% 
  filter(Item_Number %in% codes) %>% 
  return

iipGoods <- xts(t(foo[,4:(ncol(foo)-1)]),
                order.by = as.yearmon(names(foo)[4:(ncol(foo)-1)], "%Y%m")) %>% 
  set_colnames(paste("Index of Industrial Production, ", foo$Item_Name))

# 4. Industrial Production, by use of goods. Connected Indices (197801/201212)
codes <- c("4000000301","4000000302","4000000303","4000000313","4000000316",
  "4000000317","4000000323","4000000328","4000000329","4000000330")

foo <- read_excel("dat/meti/iipGoodsCon.xls", skip=2) %>% 
  select("ITEM NO.",codes) %>%
  return

iipGoodsCon <- xts(sapply(foo[3:nrow(foo), 2:ncol(foo)], as.numeric), 
                 order.by= as.yearmon(sapply(foo[3:nrow(foo),1],as.character), "%Y%m")) %>% 
  set_colnames(paste("Index of Industrial Production, ", foo[1,-1]))

# 5. Producer's Shipment, by use of goods (200801/201808)
codes <- c("5000000200","5000000201","5000000202","5000000203","5000000212","5000000223",
           "5000000224","5000000232","5000000243","5000000244","5000000262")

foo <- read_excel("dat/meti/iipGoods.xls", skip=2, sheet=2) %>% 
  filter(Item_Number %in% codes) %>% 
  return

shipGoods <- xts(t(foo[,4:(ncol(foo)-1)]),
                order.by = as.yearmon(names(foo)[4:(ncol(foo)-1)], "%Y%m")) %>% 
  set_colnames(paste("Index of Producer's Shipment, ", foo$Item_Name))

# 6. Industrial Production, by use of goods. Connected Indices (197801/201212)
codes <- c("4000000300","4000000301","4000000302","4000000303","4000000313","4000000316",
           "4000000317","4000000323","4000000328","4000000329","4000000330")

foo <- read_excel("dat/meti/iipGoodsCon.xls", skip=2, sheet=2) %>% 
  select("ITEM NO.",codes) %>%
  return

shipGoodsCon <- xts(sapply(foo[3:nrow(foo), 2:ncol(foo)], as.numeric), 
                   order.by= as.yearmon(sapply(foo[3:nrow(foo),1],as.character), "%Y%m")) %>% 
  set_colnames(paste("Index of Producer's Shipment, ", foo[1,-1]))

# 7. Producer's inventory, by use of goods (200801/201808)
codes <- c("5000000200","5000000201","5000000202","5000000203","5000000212","5000000223",
           "5000000224","5000000232","5000000243","5000000244","5000000262")

foo <- read_excel("dat/meti/iipGoods.xls", skip=2, sheet=3) %>% 
  filter(Item_Number %in% codes) %>% 
  return

inventoryGoods <- xts(t(foo[,4:(ncol(foo)-1)]),
                 order.by = as.yearmon(names(foo)[4:(ncol(foo)-1)], "%Y%m")) %>% 
  set_colnames(paste("Index of Producer's Inventory, ", foo$Item_Name))

# 8. Producer's Inventory, by use of goods. Connected Indices (197801/201212)
codes <- c("4000000300","4000000301","4000000302","4000000303","4000000313","4000000316",
           "4000000317","4000000323","4000000328","4000000329","4000000330")

foo <- read_excel("dat/meti/iipGoodsCon.xls", skip=2, sheet=3) %>% 
  select("ITEM NO.",codes) %>%
  return

inventoryGoodsCon <- xts(sapply(foo[3:nrow(foo), 2:ncol(foo)], as.numeric), 
                    order.by= as.yearmon(sapply(foo[3:nrow(foo),1],as.character), "%Y%m")) %>% 
  set_colnames(paste("Index of Producer's Inventory, ", foo[1,-1]))

# 9. Producer's inventory Ratio, by use of goods (200801/201808)
codes <- c("5000000200","5000000201","5000000202","5000000203","5000000212","5000000223",
           "5000000224","5000000232","5000000243","5000000244","5000000262")

foo <- read_excel("dat/meti/iipGoods.xls", skip=2, sheet=4) %>% 
  filter(Item_Number %in% codes) %>% 
  return

invRatioGoods <- xts(t(foo[,4:(ncol(foo)-1)]),
                      order.by = as.yearmon(names(foo)[4:(ncol(foo)-1)], "%Y%m")) %>% 
  set_colnames(paste("Index of Producer's Inventory Ratio, ", foo$Item_Name))

# 10. Producer's Inventory Ratio, by use of goods. Connected Indices (197801/201212)
codes <- c("4000000300","4000000301","4000000302","4000000303","4000000313","4000000316",
           "4000000317","4000000323","4000000328","4000000329","4000000330")

foo <- read_excel("dat/meti/iipGoodsCon.xls", skip=2, sheet=4) %>% 
  select("ITEM NO.",codes) %>%
  return

invRatioGoodsCon <- xts(sapply(foo[3:nrow(foo), 2:ncol(foo)], as.numeric), 
                         order.by= as.yearmon(sapply(foo[3:nrow(foo),1],as.character), "%Y%m")) %>% 
  set_colnames(paste("Index of Producer's Inventory Ratio, ", foo[1,-1]))

# 11. Producer's inventory Ratio, by use of goods (200801/201808)  
foo <- read_excel("dat/meti/iipIndustry.xls", sheet = 4, skip=2) %>% 
  filter(Item_Number == "2A00000000") %>% 
  return

invRatioInd <- xts(t(foo[,4:(ncol(foo)-1)]),
                     order.by = as.yearmon(names(foo)[4:(ncol(foo)-1)], "%Y%m")) %>% 
  set_colnames(paste("Index of Producer's Inventory Ratio, ", foo$Item_Name))

# 12. Producer's Inventory Ratio, by industry. Connected Indices (197801/201212)
foo <- read_excel("dat/meti/iipIndustryCon.xls", skip=2, sheet=4) %>% 
  select("ITEM NO.","2A00000000") %>%
  return

invRatioIndCon <- xts(sapply(foo[3:nrow(foo), 2:ncol(foo)], as.numeric), 
                        order.by= as.yearmon(sapply(foo[3:nrow(foo),1],as.character), "%Y%m")) %>% 
  set_colnames(paste("Index of Producer's Inventory Ratio, ", foo[1,-1]))

# 13. Index of Capacity Utilisation Ratio (200801/201808)  
codes <- c("2A00000000","2AA0000000","2AB0000000","2AH0000000","2AI0000000",
           "2AJ0000000","2AK0000000","2AM0000000","2AN0000000")

foo <- read_excel("dat/meti/capUtilInd.xls", skip=2) %>% 
  filter(Item_Number %in% codes) %>% 
  return

capUtilInd <- xts(t(foo[,4:(ncol(foo)-1)]),
                     order.by = as.yearmon(names(foo)[4:(ncol(foo)-1)], "%Y%m")) %>% 
  set_colnames(paste("Index of Capacity Utilization Ratio, ", foo$Item_Name))

# 14. Index of Capacity Utilisation Ratio by industry. Connected Indices (197801/201212)
foo <- read_excel("dat/meti/capUtilIndCon.xls", skip=2) %>% 
  select("ITEM NO.", codes)

capUtilIndCon <- xts(sapply(foo[3:nrow(foo), 2:ncol(foo)], as.numeric), 
                      order.by= as.yearmon(sapply(foo[3:nrow(foo),1],as.character), "%Y%m")) %>% 
  set_colnames(paste("Index of Capital Utilization Ratio, ", foo[1,-1]))

# Sales at Department Stores
foo <- read_excel("dat/meti/departSales.xls", sheet=12, skip=12, col_names = F) %>% 
  magrittr::extract(, c(15,2)) %>% 
  return

departSales <- xts(foo$X__2, order.by = as.yearmon(foo$X__15)) %>% 
  set_colnames("Sales at Department Store (Total)")

foo <- read_excel("dat/meti/commercialSales.xls", sheet=7, skip=7) %>% 
  magrittr::extract(, c(1,3,20))

commercialSales <- xts(foo[2:465, 2:3], 
                       order.by = as.yearmon(1980 + seq(0, 463)/12)) %>% 
  set_colnames(c("Wholesale Sales Value", "Retail Sales Value"))


bar <- merge.xts(rbind(iipInd, iipIndCon["197801/200712"]),
                  rbind(iipGoods, iipGoodsCon["197801/200712"]),
                  rbind(shipGoods, shipGoodsCon["197801/200712"]),
                  rbind(inventoryGoods, inventoryGoodsCon["197801/200712"]),
                  rbind(invRatioGoods, invRatioGoodsCon["197801/200712"]),
                  rbind(invRatioInd, invRatioIndCon["197801/200712"]),
                  rbind(capUtilInd, capUtilIndCon["197801/200712"]),
                  departSales, commercialSales)

meti <- bar[,c(1,14:23, 35:45, 46:56,58, 67:69)] %>% 
  set_colnames(scan("txt/metiVarnames.txt", character()))


saveRDS(meti, "rds/meti.rds")
