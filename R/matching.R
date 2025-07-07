matchup <- function(df, .form = "treat ~ ASSESS_SCORE + STUDENT_GENDER +
                    NCES_PUBLIC_URBAN_LOC_CODE +
                    ETHNICITY + SPECIAL_PROGRAM_FR_LUNCH_YN + SPECIAL_PROGRAM_ELL_LEP_YN +
                    SPECIAL_PROGRAM_SPECIAL_EDUCATION_YN",
                    method = "nearest",
                    ...){
  #Perform Matching
  match_object <- MatchIt::matchit(formula = stats::as.formula(.form),
                                   data = df, method = method, ...)

  #Get Matched Data
  matched_df <- MatchIt::match.data(match_object)

  #Return Matached Data Frame
  return(matched_df)
}

balance <- function(dat,
                    .b_covs = list("ASSESS_SCORE", "STUDENT_GENDER",
                                   "NCES_PUBLIC_URBAN_LOC_CODE", "ETHNICITY",
                                   "SPECIAL_PROGRAM_FR_LUNCH_YN", "SPECIAL_PROGRAM_ELL_LEP_YN",
                                   "SPECIAL_PROGRAM_SPECIAL_EDUCATION_YN"),
                    .strata = "treat"){
  print(tableone::CreateTableOne(vars = c(unlist(.b_covs)), strata = .strata,
                                 data = dat, test = TRUE),  smd = TRUE)
}

#' Build complete-case and IPCW analysis sets from a matched sample
#'
#' @param out_data            A wide data frame from `pwide()` containing both
#'                            baseline (`*_base`) and outcome (`*_out`) columns.
#' @param orig_matched_data   A data frame of units matched *before* outcomes
#'                            were released (e.g. `matdat_loc`), with the
#'                            treatment indicator `treat`.
#' @param outcome_var         Unquoted name of the *_out column holding the
#'                            outcome (default `ASSESS_SCORE_out`).
#' @param censor_formula      A formula for the IPCW model using **baseline**
#'                            covariates available at matching time.
#' @param match_ratio         Matching ratio used when the set was created
#'                            (2 for 1 : 2 matching).
#' @param trim                Lower & upper trimming factor for IPC weights
#'                            (default 0.01 ⇒ weights capped at 1/0.01 = 100).
#'
#' @export
#'
#' @return A list with two tibbles:
#'   * `$full`  – every originally-matched unit with weights & `obs` flag.
#'   * `$cc`    – the complete-case subset (`obs == TRUE`), ready for analysis.
#'
build_analysis_sets <- function(out_data,
                                orig_matched_data,
                                outcome_var      = ASSESS_SCORE_out,
                                censor_formula   =
                                  obs ~ ASSESS_SCORE + NCES_PUBLIC_PCTFRLUNCH_ROUNDED +
                                  ETHNICITY_White + ETHNICITY_Hispanic +
                                  ETHNICITY_Black + ETHNICITY_Other +
                                  URBAN_LOC_21 + GENDER_MALE,
                                match_ratio      = 2,
                                trim             = 0.01) {

  ## ------------------------------------------------------------------
  ## 1. Merge original match with baseline+outcome info
  ## ------------------------------------------------------------------
  dat <- orig_matched_data |>
    dplyr::left_join(out_data, by = "STUDENT_BUSINESS_IDENTIFIER")

  ## ------------------------------------------------------------------
  ## 2. Flag outcome availability
  ## ------------------------------------------------------------------
  outcome_var <- rlang::ensym(outcome_var)

  dat <- dat |>
    dplyr::mutate(obs = !is.na(!!outcome_var))

  ## ------------------------------------------------------------------
  ## 3. Fit IPCW model on *baseline* covariates
  ## ------------------------------------------------------------------
  ipcw_mod <- stats::glm(censor_formula,
                         data   = dat,
                         family = stats::binomial)

  dat <- dat |>
    dplyr::mutate(pi_hat = stats::predict(ipcw_mod, type = "response"))

  ## ------------------------------------------------------------------
  ## 4. Build weights
  ## ------------------------------------------------------------------
  dat <- dat |>
    dplyr::mutate(
      ## 4a  Matching weight
      w_match = dplyr::if_else(treat == 1L, 1, 1 / match_ratio),

      ## 4b  IPCW (trim pi_hat to avoid extreme weights)
      w_ipcw  = dplyr::if_else(
        obs,
        1 / pmin(pmax(pi_hat, trim), 1 - trim),
        0
      ),

      ## 4c  Final analysis weight
      w_final = w_match * w_ipcw
    )

  ## ------------------------------------------------------------------
  ## 5. Output
  ## ------------------------------------------------------------------
  list(
    full = dat,                # everything, incl. missing outcomes
    cc   = dplyr::filter(dat, obs)  # complete-case analytic set
  )
}


balance_w <- function(dat,
                    .b_covs  = c("ASSESS_SCORE", "STUDENT_GENDER",
                                 "NCES_PUBLIC_URBAN_LOC_CODE", "ETHNICITY",
                                 "SPECIAL_PROGRAM_FR_LUNCH_YN",
                                 "SPECIAL_PROGRAM_ELL_LEP_YN",
                                 "SPECIAL_PROGRAM_SPECIAL_EDUCATION_YN"),
                    .strata  = "treat",
                    .weight  = NULL) {

  # flatten covariates
  vars <- unlist(.b_covs)

  if (is.null(.weight)) {
    # —————————————————————————————
    # Unweighted table
    # —————————————————————————————
    tab <- tableone::CreateTableOne(vars   = vars,
                                    strata = .strata,
                                    data   = dat,
                                    test   = TRUE)
    print(tab, smd = TRUE)
  } else {
    # —————————————————————————————
    # Weighted (survey‐design) table
    # —————————————————————————————
    if (!requireNamespace("survey", quietly = TRUE)) {
      stop("Please install the 'survey' package to do weighted tables.")
    }
    if (!requireNamespace("tableone", quietly = TRUE)) {
      stop("Please install the 'tableone' package (>= v0.13) for svyCreateTableOne().")
    }
    if (!.weight %in% names(dat)) {
      stop("` .weight ` must be a column name in `dat`.")
    }

    # build survey design
    svy <- survey::svydesign(id = ~1,
                             weights = stats::as.formula(paste0("~", .weight)),
                             data = dat)

    # build and print the weighted table
    tabw <- tableone::svyCreateTableOne(vars   = vars,
                                        strata = .strata,
                                        data   = svy,
                                        test   = TRUE)
    print(tabw, smd = TRUE)
  }
}

