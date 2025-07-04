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

