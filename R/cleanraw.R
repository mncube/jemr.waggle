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

# pwide <- function(df){
#
#   # Split data by year
#   base <- df |> dplyr::filter(TERM_NUMBER == 202404)
#   out <- df |> dplyr::filter(TERM_NUMBER == 202502) |>
#     dplyr::rename_with(~paste0(.x, "_out"),
#                        !dplyr::all_of("STUDENT_BUSINESS_IDENTIFIER"))
#
#   # Join data
#   dat <- out |> dplyr::left_join(base, by = "STUDENT_BUSINESS_IDENTIFIER")
#
#   return(list(base = base,
#               out = out,
#               dat = dat))
# }

pwide <- function(df, handle_dups = NULL){

  # Validate handle_dups parameter if provided
  if (!is.null(handle_dups)) {
    handle_dups <- match.arg(handle_dups, choices = c("high", "low"))
  }

  # Helper function to handle duplicates
  deduplicate <- function(data, method) {
    if (is.null(method)) {
      return(data)
    }

    if (method == "high") {
      data |>
        dplyr::group_by(STUDENT_BUSINESS_IDENTIFIER) |>
        dplyr::arrange(dplyr::desc(ASSESS_SCORE)) |>
        dplyr::slice(1) |>
        dplyr::ungroup()
    } else {
      data |>
        dplyr::group_by(STUDENT_BUSINESS_IDENTIFIER) |>
        dplyr::arrange(ASSESS_SCORE) |>
        dplyr::slice(1) |>
        dplyr::ungroup()
    }
  }

  # Split data by year and handle duplicates
  base <- df |>
    dplyr::filter(TERM_NUMBER == 202404) |>
    deduplicate(handle_dups)

  out <- df |>
    dplyr::filter(TERM_NUMBER == 202502) |>
    deduplicate(handle_dups) |>
    dplyr::rename_with(~paste0(.x, "_out"),
                       !dplyr::all_of("STUDENT_BUSINESS_IDENTIFIER"))

  # Join data
  dat <- out |> dplyr::left_join(base, by = "STUDENT_BUSINESS_IDENTIFIER")

  return(list(base = base,
              out = out,
              dat = dat))
}

# pwide <- function(df){
#
#   # Identify the variables that should be pivoted (vary by term)
#   # These are the variables that change between baseline and outcome
#   pivot_vars <- c(
#     "ASSESS_SCORE",
#     "ASSESS_STD_ERR",
#     "NORMS_2020_ACHIEVEMENT_PERCENTILE",
#     "IWEEKS",
#     "TERM_SEASON",
#     "MPG_TEST_YN",
#     "ASSESS_SCORE_fmean",
#     "ASSESS_SCORE_fsd",
#     "ASSESS_SCORE_fstd"
#   )
#
#   # Create the pivoted dataset
#   dat_wide <- df |>
#     # Create a term label for pivoting
#     dplyr::mutate(term_label = dplyr::case_when(
#       TERM_NUMBER == 202404 ~ "base",
#       TERM_NUMBER == 202502 ~ "out",
#       TRUE ~ as.character(TERM_NUMBER)
#     )) |>
#     # Select student ID, term info, and variables to pivot + static variables
#     dplyr::select(STUDENT_BUSINESS_IDENTIFIER, term_label, dplyr::all_of(pivot_vars),
#            # Include static variables that don't change by term
#            SCHOOL_BUSINESS_IDENTIFIER, DISTRICT_BUSINESS_IDENTIFIER,
#            MEASUREMENT_SCALE_BID, STUDENT_GENDER, NWEA_ETHNIC_GROUP_NAME,
#            CUSTOM_ETHNIC_GROUP_NAME, GRADE, dplyr::starts_with("NCES_"),
#            dplyr::starts_with("SPECIAL_PROGRAM_"), SPANISH_TEST_YN,
#            NCES_PUBLIC_CHARTER, SCHOOL_PUBLIC_FLG, treat,
#            dplyr::starts_with("ETHNICITY"), dplyr::starts_with("GENDER_"),
#            dplyr::starts_with("URBAN_LOC_"), LOC) |>
#     # Remove duplicate static variables by keeping first occurrence per student
#     dplyr::group_by(STUDENT_BUSINESS_IDENTIFIER) |>
#     tidyr::fill(dplyr::everything(), .direction = "downup") |>
#     dplyr::ungroup() |>
#     # Pivot wider
#     tidyr::pivot_wider(
#       id_cols = c(STUDENT_BUSINESS_IDENTIFIER, SCHOOL_BUSINESS_IDENTIFIER,
#                   DISTRICT_BUSINESS_IDENTIFIER, MEASUREMENT_SCALE_BID,
#                   STUDENT_GENDER, NWEA_ETHNIC_GROUP_NAME, CUSTOM_ETHNIC_GROUP_NAME,
#                   GRADE, dplyr::starts_with("NCES_"), dplyr::starts_with("SPECIAL_PROGRAM_"),
#                   SPANISH_TEST_YN, NCES_PUBLIC_CHARTER, SCHOOL_PUBLIC_FLG, treat,
#                   dplyr::starts_with("ETHNICITY"), dplyr::starts_with("GENDER_"),
#                   dplyr::starts_with("URBAN_LOC_"), LOC),
#       names_from = term_label,
#       values_from = dplyr::all_of(pivot_vars),
#       names_sep = "_"
#     )
#
#   # # Check the result
#   # str(dat_wide)
#   # head(dat_wide)
#   #
#   # # Verify we have the expected columns
#   # baseline_cols <- grep("_base$", names(dat_wide), value = TRUE)
#   # outcome_cols <- grep("_out$", names(dat_wide), value = TRUE)
#   #
#   # cat("Baseline columns:\n")
#   # print(baseline_cols)
#   # cat("\nOutcome columns:\n")
#   # print(outcome_cols)
#   #
#   # # Check for students with both baseline and outcome data
#   # complete_students <- dat_wide |>
#   #   dplyr::filter(!is.na(ASSESS_SCORE_base) & !is.na(ASSESS_SCORE_out)) |>
#   #   nrow()
#   #
#   # cat(paste("\nNumber of students with both baseline and outcome data:", complete_students))
# }
