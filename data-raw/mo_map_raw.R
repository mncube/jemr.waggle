## code to prepare `mo_map_raw` dataset goes here
library(readr)
mo_map_raw <- readr::read_csv("C:/Users/Administrator/Desktop/jemr.waggle/rawdatafiles/JEMR Waggle MO Efficacy 202402_202502.csv")

usethis::use_data(mo_map_raw, overwrite = TRUE)
