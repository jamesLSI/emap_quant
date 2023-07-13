
# tables <- eurostat::get_eurostat_toc()


## eu current account ####
bop_eu <- get_eurostat("ei_bpm6ca_q")
bop_eu_labelled <- label_eurostat(bop_eu)%>% 
  mutate(time = lubridate::ymd(time))

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
                       geo))

## uk current account ####

bop_titles <- bop_uk <- read_csv("data_raw/BOP_UK.csv")

bop_uk <- suppressMessages(read_csv("data_raw/BOP_UK.csv",
                   skip = 6,
                   show_col_types = FALSE))
names(bop_uk) <- names(bop_titles)

curr_acc_uk <- bop_uk %>% 
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
         stk_flow = "Balance",
         geo = "United Kingdom")

## canadian current account ####

canada_bop_titles <- read_csv("data_raw/BOP_CANADA.csv",
                           skip = 10)

canada_bop_raw <- suppressMessages(read_csv("data_raw/BOP_CANADA.csv",
                                            skip = 11))

names(canada_bop_raw) <- names(canada_bop_titles)

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
         geo = "Canada")


## combined ####

curr_acc_combined <- curr_acc_eu %>% 
  bind_rows(curr_acc_uk) %>% 
  bind_rows(curr_acc_canada)

## refs ####

## Table 36-10-0014-01  Balance of international payments, current account and capital account, annual (x 1,000,000)
## https://www.ons.gov.uk/economy/nationalaccounts/balanceofpayments/datasets/balanceofpayments
## https://ec.europa.eu/eurostat/databrowser/view/ei_bpm6ca_q/default/table?lang=en


rm(bop_eu_labelled,
   bop_eu,
   bop_titles,
   bop_uk,
   canada_bop_titles,
   canada_bop_raw,
   curr_acc_eu,
   curr_acc_uk,
   curr_acc_canada)


