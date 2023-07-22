library(eurostat)
library(tidyverse)
library(magrittr)
library(plotly)
namesFunction <- function(nms) {
  janitor::make_clean_names(nms, case = "upper_camel")}

lm_summary_table_function <- function(summary_table){
  
  co_effs <- summary_table$coefficients %>% 
    as_tibble(.name_repair = namesFunction) %>% 
    mutate(Estimate = round(Estimate,
                            10),
           StdError = round(StdError,
                            10))
  
  if (str_detect(as.character(summary_table$call$formula)[[3]],
                 "\\+")==T) {
    
    model_variable_names <- as.character(summary_table$call$formula) %>% 
      tibble() %>%
      rename(variable = 1) %>% 
      filter(!variable %in% c("~",
                              "values")) %>% 
      mutate(variable = str_replace_all(variable,
                                        "\\+",
                                        "&")) %>%
      separate_wider_delim(variable,
                           names = c("f","s","t","fo","fi","si","se","ei","ni","te"),
                           delim = "&",
                           too_few = "align_start") %>% 
      pivot_longer(1:ncol(.)) %>% 
      filter(!is.na(value)) %>% 
      select(variable = value,
             -name) %>% 
      mutate(variable = str_squish(variable))
    
    variable_names <- tibble(variable = "intercept") %>% 
      bind_rows(model_variable_names)
    
  } else {
    
    variable_names <- tibble(variable = "intercept") %>% 
      bind_rows(as.character(summary_table$call$formula) %>% 
                  tibble() %>%
                  rename(variable = 1) %>% 
                  filter(!variable %in% c("~",
                                          "values")))
    
  }
  
  output <- variable_names %>% 
    bind_cols(co_effs) %>% 
    mutate(signif_level = if_else(PrT < 0.001,
                                  "***",
                                  if_else(PrT < 0.01,
                                          "**",
                                          if_else(PrT < 0.05,
                                                  "*",
                                                  if_else(PrT < 0.1,
                                                          ".",
                                                          " "))))) %>% 
    mutate(Estimate = paste(Estimate,
                            signif_level,
                            sep = " ")) %>% 
    mutate(StdError = paste0("(",StdError,")")) %>% 
    select(-c(TValue,
              PrT,
              signif_level)) %>% 
    pivot_longer(2:3) %>% 
    select(-name) %>% 
    bind_rows(tibble("variable" = "Adjust R Squared",
                     "value" = as.character(summary_table$adj.r.squared)))
  
  
  return(output)
  
}
