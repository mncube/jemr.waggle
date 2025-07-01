## code to prepare `matdat_mixed` dataset goes here
devtools::load_all()
library(dplyr)

matdat_mixed <- matdat_loc |>
  dplyr::filter(GRADE != 2) |>
  dplyr::bind_rows(matdat_exact_loc |> dplyr::filter(GRADE == 2))

usethis::use_data(matdat_mixed, overwrite = TRUE)
