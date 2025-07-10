cleanFall24 <- function(){
  dat <- mo_map_fall24 |>
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

filter_by_grade <- function(dat) {
  # Get list of unique grades
  grades <- unique(dat$GRADE)

  # Create empty data frame to store filtered results
  filtered_data <- data.frame()

  # Process each grade separately
  for (g in grades) {
    # Filter data for current grade
    grade_data <- dat |> dplyr::filter(GRADE == g)

    # Split into treatment and control groups
    treat_data <- grade_data |> dplyr::filter(treat == 1)
    control_data <- grade_data |> dplyr::filter(treat == 0)

    # Skip if no treatment schools in this grade
    if (nrow(treat_data) == 0) {
      next
    }

    # Get ranges for treatment schools
    treat_score_range <- range(treat_data$ASSESS_SCORE, na.rm = TRUE)
    treat_lunch_range <- range(treat_data$NCES_PUBLIC_PCTFRLUNCH_ROUNDED, na.rm = TRUE)
    treat_black_range <- range(treat_data$NCES_PUBLIC_PCT_TOTAL_BLACK_ROUNDED, na.rm = TRUE)
    treat_hispanic_range <- range(treat_data$NCES_PUBLIC_PCT_TOTAL_HISPANIC_ROUNDED, na.rm = TRUE)
    treat_white_range <- range(treat_data$NCES_PUBLIC_PCT_TOTAL_WHITE_ROUNDED, na.rm = TRUE)

    # Filter control schools based on criteria
    filtered_control <- control_data |>
      dplyr::filter(
        # # 1) Score within range of treatment schools
        # ASSESS_SCORE >= treat_score_range[1] & ASSESS_SCORE <= treat_score_range[2],
        #
        # # 2) Free lunch percentage within range of treatment schools
        # NCES_PUBLIC_PCTFRLUNCH_ROUNDED >= treat_lunch_range[1] &
        #   NCES_PUBLIC_PCTFRLUNCH_ROUNDED <= treat_lunch_range[2],

        # 3) Demographic percentages within range of treatment schools
        NCES_PUBLIC_PCT_TOTAL_BLACK_ROUNDED >= treat_black_range[1] &
          NCES_PUBLIC_PCT_TOTAL_BLACK_ROUNDED <= treat_black_range[2],

        NCES_PUBLIC_PCT_TOTAL_HISPANIC_ROUNDED >= treat_hispanic_range[1] &
          NCES_PUBLIC_PCT_TOTAL_HISPANIC_ROUNDED <= treat_hispanic_range[2],

        NCES_PUBLIC_PCT_TOTAL_WHITE_ROUNDED >= treat_white_range[1] &
          NCES_PUBLIC_PCT_TOTAL_WHITE_ROUNDED <= treat_white_range[2]
      )

    # Combine filtered control with treatment data for this grade
    grade_filtered <- dplyr::bind_rows(treat_data, filtered_control)

    # Add to overall filtered data
    filtered_data <- dplyr::bind_rows(filtered_data, grade_filtered)
  }

  return(filtered_data)
}

#' Flag bottom‐percentile students (optionally by grade)
#'
#' @param data A data.frame (e.g. your matched dataset) containing at least
#'   the student ID column and, if using by_grade, the grade column.
#' @param percentiles Numeric vector of percentiles to flag
#'   (e.g. c(0.2, 0.3) or c(20, 30)).
#' @param id_var Name of the student‐ID column (default "STUDENT_BUSINESS_IDENTIFIER").
#' @param score_var Name of the score variable in cleanFall24()
#'   (default "ASSESS_SCORE_fstd").
#' @param group_var Optional name of a grouping column (e.g. "GRADE").
#'   If non‐NULL, thresholds are computed separately within each group.
#'
#' @return `data` with new logical columns `bottom_20`, `bottom_30`, etc.,
#'   indicating whether each student falls in that bottom percentile—
#'   either overall or within their grade.
identify_bottom_percentiles <- function(data,
                                        percentiles,
                                        id_var     = "STUDENT_BUSINESS_IDENTIFIER",
                                        score_var  = "ASSESS_SCORE",
                                        group_var  = NULL) {
  # 1. Get cleaned pre‐matched sample
  cleaned <- cleanFall24()

  # 2. Normalize percentiles to 0–1
  p <- as.numeric(percentiles)
  if (any(p > 1)) p <- p / 100

  # 3. Prep output
  out <- data

  # 4. If no grouping, do as before:
  if (is.null(group_var)) {
    thr <- stats::quantile(cleaned[[score_var]], probs = p, na.rm = TRUE)
    names(thr) <- paste0("bottom_", gsub("%", "", names(thr)))

    for (nm in names(thr)) {
      ids <- cleaned[[id_var]][ cleaned[[score_var]] <= thr[nm] ]
      out[[nm]] <- out[[id_var]] %in% ids
    }

  } else {
    # 4a. We'll build a nested list: thresholds[[grade]][percentile]
    grades <- unique(cleaned[[group_var]])
    # initialize flag columns
    pct_names <- paste0("bottom_", round(p * 100))
    for (nm in pct_names) out[[nm]] <- FALSE

    for (g in grades) {
      sub <- cleaned[ cleaned[[group_var]] == g, ]
      if (nrow(sub) == 0) next
      thr_g <- stats::quantile(sub[[score_var]], probs = p, na.rm = TRUE)
      names(thr_g) <- pct_names

      for (nm in pct_names) {
        ids_g <- sub[[id_var]][ sub[[score_var]] <= thr_g[nm] ]
        sel <- out[[group_var]] == g & out[[id_var]] %in% ids_g
        out[[nm]][ sel ] <- TRUE
      }
    }
  }

  return(out)
}

