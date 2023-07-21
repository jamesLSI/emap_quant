library(eurostat)
library(tidyverse)
library(magrittr)
# tables <- eurostat::get_eurostat_toc()


## eu current account ####
### get and transform usd exchange rate to giv common currency
eur_usd <- read_csv("data_raw/EUR_USD Historical Data.csv") %>% 
  mutate(time = dmy(Date)) %>% 
  select(time,
         usd_eur_rate = Price) %>% 
  mutate(month = month(time),
         year = year(time)) %>% 
  group_by(year,
           month) %>% 
  arrange(time) %>% 
  distinct(year,
           month,
           .keep_all = T) %>% 
  ungroup() %>% 
  mutate(time = dmy(paste0("01-",month,"-",year))) %>% 
  arrange(desc(time)) %>% 
  select(time,
         usd_eur_rate)

### get blanace of payment data
bop_eu_labelled <- get_eurostat("ei_bpm6ca_q") %>% 
  label_eurostat(.)%>% 
  mutate(time = lubridate::ymd(time))

### transform current account data add usd rate and make common figure
curr_acc_eu <- bop_eu_labelled %>% 
  filter(bop_item == "Current account",
         stk_flow == "Balance",
         partner == "Rest of the world") %>% 
  filter(!geo == "United Kingdom") %>%
  mutate(geo = if_else(geo == "Germany (until 1990 former territory of the FRG)",
                       "Germany",
                       geo)) %>% 
  mutate(geo = if_else(geo == "European Union (EU6-1958, EU9-1973, EU10-1981, EU12-1986, EU15-1995, EU25-2004, EU27-2007, EU28-2013, EU27-2020)",
                       "EU",
                       geo)) %>% 
  left_join(eur_usd) %>% 
  mutate(usd_curr_acc_values = if_else(unit == "Million euro",
                              values * usd_eur_rate,
                              0))

## uk current account ####
### get and transform usd exchange rate to giv common currency
gbp_usd <- read_csv("data_raw/GBP_USD Historical Data.csv") %>% 
  mutate(time = dmy(Date)) %>% 
  select(time,
         usd_gbp_rate = Price) %>% 
  mutate(month = month(time),
         year = year(time)) %>% 
  group_by(year,
           month) %>% 
  arrange(time) %>% 
  distinct(year,
           month,
           .keep_all = T) %>% 
  ungroup() %>% 
  mutate(time = dmy(paste0("01-",month,"-",year))) %>% 
  arrange(desc(time)) %>% 
  select(time,
         usd_gbp_rate)

### get balance of payments data remove info rows and rename columns
uk_bop_titles <- read_csv("data_raw/BOP_UK.csv")

uk_bop_raw <- suppressMessages(read_csv("data_raw/BOP_UK.csv",
                   skip = 6,
                   show_col_types = FALSE))
names(uk_bop_raw) <- names(uk_bop_titles)

### transform current account data add usd rate and make common figure
curr_acc_uk <- uk_bop_raw %>% 
  select(quarter = Title,
         values = "BoP Current Account Balance NSA £m") %>% 
  filter(str_detect(quarter,
                    "Q")) %>% 
  mutate(time = if_else(str_detect(quarter,
                                   "Q1") == T,
                        paste0("01-01-",
                               str_sub(quarter,
                                       1,
                                       4)),
                        if_else(str_detect(quarter,
                                           "Q2") == T,
                                paste0("01-04-",
                                       str_sub(quarter,
                                               1,
                                               4)),
                                if_else(str_detect(quarter,
                                                   "Q3") == T,
                                        paste0("01-07-",
                                               str_sub(quarter,
                                                       1,
                                                       4)),
                                        if_else(str_detect(quarter,
                                                           "Q4") == T,
                                                paste0("01-10-",
                                                       str_sub(quarter,
                                                               1,
                                                               4)),
                                                "error"))))) %>% 
  mutate(time = dmy(time)) %>% 
  mutate(unit = "Million sterling",
         s_adj = "Unadjusted data (i.e. neither seasonally adjusted nor calendar adjusted data)",
         sector10 = "Total Economy",
         sectpart = "Total Economy",
         partner = "rest of the world",
         bop_item = "Current account",
         stk_flow = "Balance",
         geo = "United Kingdom") %>% 
  left_join(gbp_usd) %>% 
  mutate(usd_curr_acc_values = if_else(unit == "Million sterling",
                              values * usd_gbp_rate,
                              0))

## canadian current account ####
### get and transform usd exchange rate to giv common currency
cad_usd <- read_csv("data_raw/CAD_USD Historical Data.csv") %>% 
  mutate(time = dmy(Date)) %>% 
  select(time,
         usd_cad_rate = Price) %>% 
  mutate(month = month(time),
         year = year(time)) %>% 
  group_by(year,
           month) %>% 
  arrange(time) %>% 
  distinct(year,
           month,
           .keep_all = T) %>% 
  ungroup() %>% 
  mutate(time = dmy(paste0("01-",month,"-",year))) %>% 
  arrange(desc(time)) %>% 
  select(time,
         usd_cad_rate)

### get balance of payments data remove info rows and rename columns
canada_bop_titles <- read_csv("data_raw/BOP_CANADA.csv",
                           skip = 10)

canada_bop_raw <- suppressMessages(read_csv("data_raw/BOP_CANADA.csv",
                                            skip = 11))

names(canada_bop_raw) <- names(canada_bop_titles)

### transform current account data add usd rate and make common figure
curr_acc_canada <- canada_bop_raw %>% 
  filter(`Receipts, payments and balances` == "Balances",
         `Current account and capital account` == "Total current account") %>% 
  mutate(across(3:ncol(.),
                .fns = as.numeric)) %>% 
  pivot_longer(3:ncol(.),
               names_to = "quarter",
               values_to = "values") %>%
  select(-c(`Receipts, payments and balances`,
            `Current account and capital account`)) %>% 
  mutate(time = if_else(str_detect(quarter,
                                   "Q1") == T,
                        paste0("01-01-",
                               str_sub(quarter,
                                       4,
                                       7)),
                        if_else(str_detect(quarter,
                                           "Q2") == T,
                                paste0("01-04-",
                                       str_sub(quarter,
                                               4,
                                               7)),
                                if_else(str_detect(quarter,
                                                   "Q3") == T,
                                        paste0("01-07-",
                                               str_sub(quarter,
                                                       4,
                                                       7)),
                                        if_else(str_detect(quarter,
                                                           "Q4") == T,
                                                paste0("01-10-",
                                                       str_sub(quarter,
                                                               4,
                                                               7)),
                                                "error"))))) %>% 
  mutate(time = dmy(time)) %>% 
  mutate(unit = "Million dollar",
         s_adj = "Unadjusted data (i.e. neither seasonally adjusted nor calendar adjusted data)",
         sector10 = "Total Economy",
         sectpart = "Total Economy",
         partner = "rest of the world",
         stk_flow = "Balance",
         bop_item = "Current account",
         geo = "Canada") %>% 
  left_join(cad_usd) %>% 
  mutate(usd_curr_acc_values = if_else(unit == "Million dollar",
                              values * usd_cad_rate,
                              0))


## combine files ####

curr_acc_combined_pre <- curr_acc_eu %>% 
  bind_rows(curr_acc_uk) %>% 
  bind_rows(curr_acc_canada) %>% 
  rowwise() %>% 
  mutate(usd_rate = max(usd_eur_rate,
                        usd_gbp_rate,
                        usd_cad_rate,
                        na.rm = T)) %>% 
  ungroup()

usd_2015 <- curr_acc_combined_pre %>% 
  filter(str_detect(unit,
                    "Million")) %>% 
  group_by(geo) %>% 
  filter(time == dmy("01-01-2015")) %>% 
  select(geo,
         usd_rate_2015 = usd_rate)

curr_acc_combined <- curr_acc_combined_pre %>% 
  left_join(usd_2015) %>% 
  group_by(geo) %>% 
  mutate(usd_rate_base_2015 = (usd_rate / usd_rate_2015)*100)

## refs ####

## https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=3610001401 Table 36-10-0014-01  Balance of international payments, current account and capital account, annual (x 1,000,000)
## https://www.ons.gov.uk/economy/nationalaccounts/balanceofpayments/datasets/balanceofpayments
## https://ec.europa.eu/eurostat/databrowser/view/ei_bpm6ca_q/default/table?lang=en

## remove objects to clean environment#### 
rm(bop_eu_labelled,
   uk_bop_titles,
   uk_bop_raw,
   canada_bop_titles,
   canada_bop_raw,
   eur_usd,
   cad_usd,
   gbp_usd,
   usd_2015,
   curr_acc_eu,
   curr_acc_uk,
   curr_acc_canada,
   curr_acc_combined_pre)




