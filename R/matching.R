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
