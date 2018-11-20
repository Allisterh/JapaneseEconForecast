#### Data preprocessing for data from Ministry of Internal Affairs and Communications (i.e. CPI)

### Final output is called `mic.rds`.
### Run `readRDS("data/rds/mic.rds")` to get the preprocessed dataset  

codes <- c("1","737","739","742","2","281","309","320","387","471","504","557","577","677")

foo <- read.csv("dat/mic/CPI.csv",fileEncoding = "cp932", stringsAsFactors = F) 

bar <- foo %>% 
  set_colnames(c("date",foo[3,-1])) %>% # temporal colname to use `select`
  select(c("date", codes)) %>% 
  return

cpi <- xts(sapply(bar[6:nrow(bar),-1],as.numeric),
           order.by = as.yearmon(bar[6:nrow(bar),1], "%Y%m")) %>% 
  set_colnames(scan("txt/micVarnames.txt", character()))


mic <- cpi
saveRDS(mic, "rds/mic.rds")

