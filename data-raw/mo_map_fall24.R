## code to prepare `mo_map_fall24` dataset goes here
library(readr)
mo_map_fall24 <- readr::read_csv("C:/Users/Administrator/Desktop/jemr.waggle/rawdatafiles/JEMR Waggle MO MAP K-8 Data for Efficacy Study_Fall.csv")

usethis::use_data(mo_map_fall24, overwrite = TRUE)
