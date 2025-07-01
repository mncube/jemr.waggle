## code to prepare `matdat` dataset goes here
devtools::load_all()
library(Matching)
library(rgenoud)
# library(optmatch)


rawdat <- cleanFall24()

form = "treat ~ ASSESS_SCORE + NCES_PUBLIC_PCTFRLUNCH_ROUNDED +
                ETHNICITY_White + ETHNICITY_Hispanic +
                ETHNICITY_Black + ETHNICITY_Other"


# set.seed(4579)
set.seed(387)

# matdat <- matchup(rawdat, .form = form, ratio = 2,
#                   exact = ~ GRADE + GENDER_MALE,
#                   method = "optimal", distance = "glm")

# Split data by a categorical variable (e.g., GRADE)
grades <- levels(rawdat$GRADE)
matched_list <- list()

for (g in grades) {
  # Subset data for this grade
  grade_data <- rawdat |> dplyr::filter(GRADE == g)

  # Clean before matching
  gc()

  # Match within this subset
  matched_grade <- matchup(grade_data, .form = form, ratio = 2,
                           exact = ~ GENDER_MALE,
                           method = "genetic", distance = "glm",
                           m.order = "smallest")

  # Store results
  matched_list[[as.character(g)]] <- matched_grade
  rm(matched_grade)
  cat("finished grade:", g, "\n")
}

# Combine results
matdat <- dplyr::bind_rows(matched_list)

usethis::use_data(matdat, overwrite = TRUE)
