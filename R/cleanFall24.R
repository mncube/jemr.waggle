cleanFall24 <- function(){
  dat <- mo_map_fall24 |>
    dplyr::mutate(treat = ifelse(DISTRICT_BUSINESS_IDENTIFIER == treatment_district, 1, 0))
}
