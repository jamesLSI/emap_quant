source("eurostat_data.R")

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
         geo = "United Kingdom")

hicp_cpi_post_brexit <- prc_hicp_labelled %>% 
  filter(!geo == "United Kingdom") %>% 
  bind_rows(cpi_all_uk) %>% 
  bind_rows(cpi_ex_energy_uk) %>% 
  filter(unit == "Harmonized consumer price index, 2015=100") %>% 
  filter(indic == "HICP - All items excluding energy" | indic == "HICP - All items (HICP=Harmonized Index of Consumer Prices)") %>% 
  mutate(geo = if_else(geo == "Germany (until 1990 former territory of the FRG)",
                       "Germany",
                       geo)) %>% 
  arrange(indic,
          time,
          geo)

rm(cpi_all_uk,
   cpi_ex_energy_uk,
   prc_hicp,
   prc_hicp_labelled)

