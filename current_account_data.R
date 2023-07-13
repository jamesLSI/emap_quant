
tables <- eurostat::get_eurostat_toc()

bop_eu <- get_eurostat("ei_bpm6ca_q")
bop_eu_labelled <- label_eurostat(bop_eu)%>% 
  mutate(time = lubridate::ymd(time))

curr_acc_eu <- bop_eu_labelled %>% 
  filter(bop_item == "Current account",
         stk_flow == "Balance",
         partner == "Rest of the world")

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

curr_acc_combined <- curr_acc_eu %>% 
  bind_rows(curr_acc_uk)

rm(bop_eu_labelled,
   bop_eu,
   bop_titles,
   bop_uk,
   curr_acc_eu,
   curr_acc_uk)


