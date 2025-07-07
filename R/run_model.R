get_analytic_sample <- function(dat,
                                .analysis = c("growth", "badj"),
                                .group = NULL, .control
){

  .analysis <- match.arg(.analysis)

  if (.analysis == "growth"){
    dat <- dat |> dplyr::mutate(outcome = ASSESS_SCORE_out - ASSESS_SCORE)
    .assess <- c("outcome")
  } else if (.analysis == "badj") {
    dat <- dat |>
      dplyr::mutate(outcome = ASSESS_SCORE_out) |>
      dplyr::mutate(baseline = ASSESS_SCORE)
    .assess <- c("outcome", "baseline")
  } else {
    stop("must use 'growth' or 'badj' for .analysis")
  }

  # Filter data down to sub-group of interest
  if(!is.null(.group)){
    if(is.character(.group)) {
      # Parse string expression and evaluate it
      dat <- dat |>
        dplyr::filter(!!rlang::parse_expr(.group))
    } else {
      # Fallback for other input types
      dat <- dat |>
        dplyr::filter(!!.group)
    }
  }

  # Base selection - columns that are always needed
  dat_indices <- dat |>
    dplyr::select(dplyr::all_of(.assess), treat, GRADE, NCES_PUBLIC_PCTFRLUNCH_ROUNDED, GENDER_MALE, GENDER_FEMALE,ETHNICITY_White, ETHNICITY_Hispanic, ETHNICITY_Black,
                  ETHNICITY_Other,URBAN_LOC_21)

  return(dat_indices)
}
