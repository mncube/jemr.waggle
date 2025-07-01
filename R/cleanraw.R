cleanraw <- function(df){
  dat <- df |>
    dplyr::mutate(treat = ifelse(DISTRICT_BUSINESS_IDENTIFIER == treatment_district, 1, 0)) |>
    # Filter to the correct assessment
    dplyr::filter(!(GRADE %in% c(1,2) & MPG_TEST_YN == 0)) |>
    # Additional Filter to Valid Assessments
    dplyr::filter((ASSESS_STD_ERR >= 1.5 & ASSESS_STD_ERR <= 5.5) | ASSESS_SCORE > 240) |>
    # Standardize fall scores
    dplyr::group_by(GRADE) |>
    dplyr::mutate(ASSESS_SCORE_fmean = mean(ASSESS_SCORE, na.rm = TRUE),
                  ASSESS_SCORE_fsd = stats::sd(ASSESS_SCORE, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::mutate(ASSESS_SCORE_fstd = (ASSESS_SCORE - ASSESS_SCORE_fmean)/ASSESS_SCORE_fsd) |>
    # dplyr::select(dplyr::starts_with("ASSESS"), dplyr::everything()) |>
    # Clean up ethnicity
    dplyr::mutate(ETHNICITY = dplyr::case_match(
      NWEA_ETHNIC_GROUP_NAME,
      "Hispanic or Latino" ~ "Hispanic",
      "Black or African American" ~ "Black",
      "Not Specified or Other" ~ "Unspecifed",
      "Multi-ethnic" ~ "Multi",
      "American Indian or Alaska Native" ~ "AIAN",
      "Native Hawaiian or Other Pacific Islander" ~ "NHOPI",
      NA_character_ ~ "Missing",
      .default = NWEA_ETHNIC_GROUP_NAME
    )) |>
    # Create Binary Variables Instead of Categoricals
    dplyr::mutate(GENDER_MALE = ifelse(STUDENT_GENDER == "MALE", 1, 0),
                  GENDER_FEMALE = ifelse(STUDENT_GENDER == "FEMALE", 1, 0),
                  GENDER_X = ifelse(STUDENT_GENDER == "X", 1, 0),
                  ETHNICITY_White = ifelse(ETHNICITY == "White", 1, 0),
                  ETHNICITY_Hispanic = ifelse(ETHNICITY == "Hispanic", 1, 0),
                  ETHNICITY_Black = ifelse(ETHNICITY == "Black", 1, 0),
                  ETHNICITY_Unspecifed = ifelse(ETHNICITY == "Unspecifed", 1, 0),
                  ETHNICITY_Multi = ifelse(ETHNICITY == "Multi", 1, 0),
                  ETHNICITY_Asian = ifelse(ETHNICITY == "Asian", 1, 0),
                  ETHNICITY_AIAN = ifelse(ETHNICITY == "AIAN", 1, 0),
                  ETHNICITY_NHOPI = ifelse(ETHNICITY == "NHOPI", 1, 0),
                  ETHNICITY_Missing = ifelse(ETHNICITY == "Missing", 1, 0)) |>
    dplyr::mutate(ETHNICITY_Other = ifelse(ETHNICITY_White == 0 &
                                             ETHNICITY_Black == 0 &
                                             ETHNICITY_Hispanic == 0, 1, 0)) |>
    dplyr::mutate(GRADE = factor(GRADE, levels = c(1,2,3,4,5,6,7,8,13)))

  # Copy Columns
  copy_cols <- function(df){
    # First, identify columns that need backup copies
    type1_cols <- grep("^NCES_PUBLIC_", names(df), value = TRUE)
    type2_cols <- grep("^NCES_DISTRICT_", names(df), value = TRUE)
    type3_cols <- grep("^SPECIAL_PROGRAM_", names(df), value = TRUE)
    type4_cols <- grep("^SPANISH_TEST_", names(df), value = TRUE)

    # Create backup columns with _orig suffix
    for (col in c(type1_cols, type2_cols, type3_cols, type4_cols)) {
      df[[paste0(col, "_orig")]] <- df[[col]]
    }

    return(df)
  }

  dat <- dat |> copy_cols()

  # Convert specified columns to numeric and replace NA with 0
  # Find all columns that start with the specified prefixes but don't end with "_orig"
  cols_to_convert <- grep("^(NCES_PUBLIC_|NCES_DISTRICT_|SPECIAL_PROGRAM_|SPANISH_TEST_)(?!.*_orig$)",
                          names(dat),
                          value = TRUE,
                          perl = TRUE)

  # Convert to numeric and replace NA with 0
  dat <- dat |>
    dplyr::mutate(dplyr::across(dplyr::all_of(cols_to_convert),
                                ~as.numeric(.x))) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(cols_to_convert),
                                ~ifelse(is.na(.x), 0, .x)))

  # Reformat Urban Locale as factor
  dat <- dat |>
    dplyr::mutate(URBAN_LOC_0 = ifelse(NCES_PUBLIC_URBAN_LOC_CODE == 0, 1, 0),
                  URBAN_LOC_21 = ifelse(NCES_PUBLIC_URBAN_LOC_CODE == 21, 1, 0),
                  URBAN_LOC_41 = ifelse(NCES_PUBLIC_URBAN_LOC_CODE == 41, 1, 0)) |>
    dplyr::mutate(NCES_PUBLIC_URBAN_LOC_CODE = as.factor(NCES_PUBLIC_URBAN_LOC_CODE)) |>
    dplyr::mutate(LOC = ifelse(URBAN_LOC_41 == 1, 41,
                               ifelse(URBAN_LOC_21 == 1, 21, 0))) |>
    dplyr::mutate(LOC = factor(LOC, levels = c(0, 21,41)))
}
