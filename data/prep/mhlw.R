#### Data preprocessing for data from Ministry of Health, Labour and Walfare (MHLW)

### Final object is called `mhlw.rds`.
### Run `readRDS("data/rds/mhlw.rds")` to get preprocessed dataset


# run this code if you encounter error in deailng with date class 
Sys.setlocale("LC_TIME","C") # http://cse.naro.affrc.go.jp/takezawa/r-tips/r/16.html


# Ministry of Health, Labour and Welfare ----------------------------------

# 1. Index of Non-Scheduled Worked Hours (All industry, 30 or More Persons)
foo <- read_excel("dat/mhlw/overtime.xls", sheet=1, skip=8) %>% 
  magrittr::extract(1:49,c(1,6:17)) %>% 
  gather(month, bar, -X__1) %>%
  unite(date, -bar, sep="-")
overtimeTL <- xts(as.numeric(foo$bar), 
                  order.by = as.yearmon(foo$date, format="%Y-%b")) %>% 
  set_colnames("EmploymentHours_overtimeAll")

# 2.Index of Non-Scheduled Worked Hours (Manufacturing, 30 or More Persons)
foo <- read_excel("dat/mhlw/overtime.xls", sheet=2, skip=8) %>% 
  magrittr::extract(16:64,c(1,6:17)) %>%
  gather(month, bar, -X__1) %>%
  unite(date, -bar, sep="-") 
overtimeM <- xts(as.numeric(foo$bar), 
                  order.by = as.yearmon(foo$date, format="%Y-%b")) %>% 
  set_colnames("EmploymentHours_overtimeManufacturing")

# 3.Employment Index of Regular Workers (All Industries, 30 or More Persons) 
foo <- read_excel("dat/mhlw/employ.xls", sheet=1, skip=8) %>% 
  magrittr::extract(1:49,c(1,6:17)) %>%
  gather(month, bar, -X__1) %>%
  unite(date, -bar, sep="-") 
employTL <- xts(as.numeric(foo$bar), 
                 order.by = as.yearmon(foo$date, format="%Y-%b")) %>% 
  set_colnames("EmploymentHours_EmploymentRegWorkerAll")

# 4.Employment Index of Regular Workers (Manufacturing, 30 or More Persons)
foo <- read_excel("dat/mhlw/employ.xls", sheet=2, skip=8) %>% 
  magrittr::extract(16:64,c(1,6:17)) %>%
  gather(month, bar, -X__1) %>%
  unite(date, -bar, sep="-") 
employM <- xts(as.numeric(foo$bar), 
                order.by = as.yearmon(foo$date, format="%Y-%b")) %>% 
  set_colnames("EmploymentHours_EmploymentRegWorkerManufacturing")

# 5.Real Wage Index (Contractual Cash Earnings in All Industries, 30 or More Persons)
foo <- read_excel("dat/mhlw/rWage.xls", sheet=1, skip=8) %>% 
  magrittr::extract(1:49,c(1,6:17)) %>%
  gather(month, bar, -X__1) %>%
  unite(date, -bar, sep="-")
rWageTL <- xts(as.numeric(foo$bar), 
               order.by = as.yearmon(foo$date, format="%Y-%b")) %>% 
  set_colnames("PriceIndicesWages_RealWageIndAll")

# 6.Real Wage Index (Contractual Cash Earnings in Manufacturing, 30 or More Persons) 
foo <- read_excel("dat/mhlw/rWage.xls", sheet=2, skip=8) %>% 
  magrittr::extract(1:49,c(1,6:17)) %>%
  gather(month, bar, -X__1) %>%
  unite(date, -bar, sep="-") 
rWageM <- xts(as.numeric(foo$bar), 
               order.by = as.yearmon(foo$date, format="%Y-%b")) %>% 
  set_colnames("PriceIndicesWages_RealWageIndManufacturing")

# 7.Wage Index (Contractual Cash Earnings in All Industries, 30 or More Persons)
foo <- read_excel("dat/mhlw/wage.xls", sheet=1, skip=8) %>% 
  magrittr::extract(1:49,c(1,6:17)) %>%
  gather(month, bar, -X__1) %>%
  unite(date, -bar, sep="-")
wageTL <- xts(as.numeric(foo$bar), 
               order.by = as.yearmon(foo$date, format="%Y-%b")) %>% 
  set_colnames("PriceIndicesWages_WageIndAll")

# 8.Wage Index (Contractual Cash Earnings in Manufacturing, 30 or More Persons) 
foo <- read_excel("dat/mhlw/wage.xls", sheet=2, skip=8) %>% 
  magrittr::extract(16:64,c(1,6:17)) %>%
  gather(month, bar, -X__1) %>%
  unite(date, -bar, sep="-") %>%
  return
wageM <- xts(as.numeric(foo$bar), 
              order.by = as.yearmon(foo$date, format="%Y-%b")) %>% 
  set_colnames("PriceIndicesWages_WageIndManufacturing")

# 9. Effective Job Offer Rate (Excluding New School Graduates)
## Here I convert the original excel file into separate csv files, because the file encoding...
## is not compatible to `readxl::read_excel` ()
foo <- read.csv("dat/mhlw/eJobRate/eJobRate1.csv", skip=3, fileEncoding="cp932") %>% 
  magrittr::extract(8:56,c(23,25:36)) %>%
  set_colnames(c("year", paste(1:12))) %>% 
  gather(month, bar, -year) %>%
  unite(date, -bar, sep="-")  # Dont worry about the error "invalid input in scan(.), this is probably because of multibyte input
eJobRate <- xts(foo$bar, order.by = as.yearmon(str_remove(foo$date, "年"), format="%y-%m")) %>% 
  set_colnames("EmploymentHours_EffJobOfferRate")

# 10. Effective Job Offer Rate (Part-Time)
foo <- read.csv("dat/mhlw/eJobRate/eJobRate3.csv", skip=3, fileEncoding="cp932") %>% 
  magrittr::extract(10:56,c(23,25:36)) %>%
  set_colnames(c("year", paste(1:12))) %>%
  gather(month, bar, -year) %>%
  unite(date, -bar, sep="-")
eJobRatePT <- xts(as.numeric(foo$bar),
                  order.by = as.yearmon(str_remove(foo$date, "年"), format="%y-%m")) %>% 
  set_colnames("EmploymentHours_EffJobOfferRatePartTime")

# 11. Effective Job Offers (Excluding New School Graduates)  
foo <- read.csv("dat/mhlw/eJobOffer/eJobOffer1.csv", skip=3, fileEncoding="cp932") %>% 
  magrittr::extract(8:56,c(23,25:36)) %>%
  set_colnames(c("year", paste(1:12))) %>%
  gather(month, bar, -year) %>%
  unite(date, -bar, sep="-") 
eJobOffer <- xts(as.numeric(str_remove_all(foo$bar, ",")), 
                 order.by = as.yearmon(str_remove(foo$date, "年"), format="%y-%m")) %>% 
  set_colnames("EmploymentHours_EffJobOffer")

# 12. Effective Job Offers (Part-Time)
foo <- read.csv("dat/mhlw/eJobOffer/eJobOffer3.csv", skip=3, fileEncoding="cp932") %>% 
  magrittr::extract(10:56,c(23,25:36)) %>%
  set_colnames(c("year", paste(1:12))) %>%
  gather(month, bar, -year) %>%
  unite(date, -bar, sep="-") 
eJobOfferPT <- xts(as.numeric(str_remove_all(foo$bar, ",")), 
                   order.by = as.yearmon(str_remove(foo$date, "年"), format="%y-%m")) %>% 
  set_colnames("EmploymentHours_EffJobOfferPartTime")

# 13. New Job Offers (Excluding New School Graduates)  
foo <- read.csv("dat/mhlw/nJobOffer/nJobOffer1.csv", skip=3, fileEncoding="cp932") %>% 
  magrittr::extract(8:56,c(23,25:36)) %>%
  set_colnames(c("year", paste(1:12))) %>%
  gather(month, bar, -year) %>%
  unite(date, -bar, sep="-")
nJobOffer <- xts(as.numeric(str_remove_all(foo$bar, ",")), 
                 order.by = as.yearmon(str_remove(foo$date, "年"), format="%y-%m")) %>% 
  set_colnames("EmploymentHours_NewJobOffer")

# 14. New Job Offers (Part-Time)
foo <- read.csv("dat/mhlw/nJobOffer/nJobOffer3.csv", skip=3, fileEncoding="cp932") %>% 
  magrittr::extract(10:56,c(23,25:36)) %>%
  set_colnames(c("year", paste(1:12))) %>%
  gather(month, bar, -year) %>%
  unite(date, -bar, sep="-") 
nJobOfferPT <- xts(as.numeric(str_remove_all(foo$bar, ",")),
                   order.by = as.yearmon(str_remove(foo$date, "年"), format="%y-%m")) %>% 
  set_colnames("EmploymentHours_NewJobOfferPartTime")

# 15. New Job Offer Rate (Excluding New School Graduates)

foo <- read.csv("dat/mhlw/nJobRate/nJobRate1.csv", skip=3, fileEncoding="cp932") %>% 
  magrittr::extract(8:56,c(23,25:36)) %>%
  set_colnames(c("year", paste(1:12))) %>% 
  gather(month, bar, -year) %>%
  unite(date, -bar, sep="-") 
nJobRate <- xts(foo$bar, order.by = as.yearmon(str_remove(foo$date, "年"), format="%y-%m")) %>% 
  set_colnames("EmploymentHours_NewJobOfferRate")


# 16. New Job Offer Rate (Part-Time)
foo <- read.csv("dat/mhlw/nJobRate/nJobRate3.csv", skip=3, fileEncoding="cp932") %>% 
  magrittr::extract(10:56,c(23,25:36)) %>%
  set_colnames(c("year", paste(1:12))) %>%
  gather(month, bar, -year) %>%
  unite(date, -bar, sep="-") 
nJobRatePT <- xts(as.numeric(foo$bar), 
                  order.by = as.yearmon(str_remove(foo$date, "年"), format="%y-%m")) %>% 
  set_colnames("EmploymentHours_NewJobOfferRatePartTime")

# 17. Index of Total Worked Hours (All Industries，30 or More Persons)
foo <- read_excel("dat/mhlw/totalHours.xls", sheet=1, skip=8) %>% 
  magrittr::extract(1:49,c(1,6:17)) %>%
  gather(month, bar, -X__1) %>%
  unite(date, -bar, sep="-")
totalHoursTL <- xts(as.numeric(foo$bar), 
                  order.by = as.yearmon(foo$date, format="%Y-%b")) %>% 
  set_colnames("EmploymentHours_totalHourAll")

# 18. Index of Total Worked Hours (Manufacturing，30 or More Persons)
foo <- read_excel("dat/mhlw/totalHours.xls", sheet=2, skip=8) %>% 
  magrittr::extract(16:64,c(1,6:17)) %>%
  gather(month, bar, -X__1) %>%
  unite(date, -bar, sep="-") %>%
  return

totalHoursM <- xts(as.numeric(foo$bar), 
                    order.by = as.yearmon(foo$date, format="%Y-%b")) %>% 
  set_colnames("EmploymentHours_totalHourManufacturing")


mhlw <- merge.xts(eJobOffer, eJobOfferPT, eJobRate, eJobRatePT,employM,
                  employTL, nJobOffer, nJobOfferPT, nJobRate, nJobRatePT,
                  overtimeM, overtimeTL, rWageM, rWageTL, totalHoursM, 
                  totalHoursTL, wageM, wageTL)

saveRDS(mhlw, "rds/mhlw.rds")

