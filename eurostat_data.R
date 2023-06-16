library(eurostat)

check_access_to_data()

tables <- get_eurostat_toc()

id <- search_eurostat("Modal split of passenger transport",
                      type = "table"
)$code[1]
print(id)

prc_hicp <- get_eurostat("ei_cphi_m")
