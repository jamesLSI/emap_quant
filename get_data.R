source("eurostat_data.R")

### UK CPI data read and transform to match eurostat ####
cpi_all_uk <- read_csv("data_raw/CPI_ALL_ITEMS_UK_2015_100.csv",
                        skip = 183) %>% 
  rename(time = 1,
         values = 2) %>% 
  mutate(time = paste0(str_sub(time, 
                               start = 1,
                               end = 4),
                       "-",
                       str_sub(time, 
                               start = 6,
                               end = 9),
                       "-",
                       "01"),
         time = as.Date(ymd(time))) %>% 
  mutate(unit = "Harmonized consumer price index, 2015=100",
         indic = "HICP - All items (HICP=Harmonized Index of Consumer Prices)",
         s_adj = "Unadjusted data (i.e. neither seasonally adjusted nor calendar adjusted data)",
         geo = "United Kingdom")


cpi_ex_energy_uk <- read_csv("data_raw/CPI_EX_ENERGY_UK_2015_100.csv",
                       skip = 183) %>% 
  rename(time = 1,
         values = 2) %>% 
  mutate(time = paste0(str_sub(time, 
                               start = 1,
                               end = 4),
                       "-",
                       str_sub(time, 
                               start = 6,
                               end = 9),
                       "-",
                       "01"),
         time = as.Date(ymd(time))) %>% 
  mutate(unit = "Harmonized consumer price index, 2015=100",
         indic = "HICP - All items excluding energy",
         s_adj = "Unadjusted data (i.e. neither seasonally adjusted nor calendar adjusted data)",
         geo = "United Kingdom")



### Canada CPI data read and transform to match eurostat including rebase from 2002 to 2015####

canada_raw <- read_csv("data_raw/CPI_ALL_ITEMS_CANADA_2002_100.csv",
                       skip = 9)

canada_all <- canada_raw %>% 
  filter(`Products and product groups 3 4` == "All-items") %>% 
  mutate(across(2:ncol(.),
                .fns = as.numeric)) %>% 
  pivot_longer(2:ncol(.)) %>% 
  rename(values = value,
         time = name) %>% 
  mutate(unit = "Harmonized consumer price index, 2002=100",
         indic = "HICP - All items (HICP=Harmonized Index of Consumer Prices)",
         geo = "Canada") %>% 
  mutate(time = paste0("01-",time),
         time = str_replace_all(time,
                                " ",
                                "-"),
         time = dmy(time)) %>% 
  mutate(unit = "Harmonized consumer price index, 2015=100",
         values = (values/122.9)*100)

canada_all_ex_energy <- canada_raw %>% 
  filter(`Products and product groups 3 4` == "All-items excluding energy 7") %>% 
  mutate(across(2:ncol(.),
                .fns = as.numeric)) %>% 
  pivot_longer(2:ncol(.)) %>% 
  rename(values = value,
         time = name) %>% 
  mutate(unit = "Harmonized consumer price index, 2002=100",
         indic = "HICP - All items excluding energy",
         geo = "Canada") %>% 
  mutate(time = paste0("01-",time),
         time = str_replace_all(time,
                                " ",
                                "-"),
         time = dmy(time)) %>% 
  mutate(unit = "Harmonized consumer price index, 2015=100",
         values = (values/122.9)*100)


canada_rebase_combined <- canada_all %>% 
  bind_rows(canada_all_ex_energy) %>% 
  mutate(s_adj = "Unadjusted data (i.e. neither seasonally adjusted nor calendar adjusted data)") %>% 
  select(-c(`Products and product groups 3 4`)) %>% 
  select(unit,
         s_adj,
         indic,
         geo,
         time,
         values)

## join UK, Canada, Eurostat and remove source files ####

hicp_cpi_post_brexit <- prc_hicp_labelled %>% 
  filter(!geo == "United Kingdom") %>% 
  bind_rows(cpi_all_uk) %>% 
  bind_rows(cpi_ex_energy_uk) %>% 
  bind_rows(canada_rebase_combined) %>% 
  filter(unit == "Harmonized consumer price index, 2015=100") %>% 
  filter(indic == "HICP - All items excluding energy" | indic == "HICP - All items (HICP=Harmonized Index of Consumer Prices)") %>% 
  mutate(geo = if_else(geo == "Germany (until 1990 former territory of the FRG)",
                       "Germany",
                       geo)) %>% 
  mutate(geo = if_else(geo == "European Union (EU6-1958, EU9-1973, EU10-1981, EU12-1986, EU15-1995, EU25-2004, EU27-2007, EU28-2013, EU27-2020)",
                       "EU",
                       geo)) %>% 
  arrange(indic,
          time,
          geo)

## ref ####

## Statistics Canada. Table 18-10-0004-01  Consumer Price Index, monthly, not seasonally adjusted
## https://www.ons.gov.uk/economy/inflationandpriceindices/timeseries/d7bt/mm23
## https://ec.europa.eu/eurostat/databrowser/view/EI_CPHI_M/default/table?lang=en

rm(cpi_all_uk,
   cpi_ex_energy_uk,
   prc_hicp,
   prc_hicp_labelled,
   canada_raw,
   canada_all,
   canada_all_ex_energy,
   canada_rebase_combined)

