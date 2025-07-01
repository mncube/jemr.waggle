## code to prepare `matdat_exact_loc_r1` dataset goes here
devtools::load_all()
# library(Matching)
# library(rgenoud)

rawdat <- cleanFall24()

form = "treat ~ ASSESS_SCORE + NCES_PUBLIC_PCTFRLUNCH_ROUNDED +
                ETHNICITY_White + ETHNICITY_Hispanic +
                ETHNICITY_Black + ETHNICITY_Other"

set.seed(3486)

# Split data by a categorical variable (e.g., GRADE)
grades <- levels(rawdat$GRADE)
matched_list <- list()

for (g in grades) {
  # Subset data for this grade
  grade_data <- rawdat |> dplyr::filter(GRADE == g)

  # Clean before matching
  gc()

  # Match within this subset
  matched_grade <- matchup(grade_data, .form = form, ratio = 1,
                           exact = ~ GENDER_MALE + URBAN_LOC_21,
                           method = "nearest", distance = "glm")

  # Store results
  matched_list[[as.character(g)]] <- matched_grade
  rm(matched_grade)
  cat("finished grade:", g, "\n")
}

# Combine results
matdat_exact_loc_r1 <- dplyr::bind_rows(matched_list)

usethis::use_data(matdat_exact_loc_r1, overwrite = TRUE)
