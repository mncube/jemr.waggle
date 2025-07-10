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
  if(!is.null(.group) && .group != "All"){
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
    dplyr::select(dplyr::all_of(.assess), treat, GRADE, NCES_PUBLIC_PCTFRLUNCH_ROUNDED,
                  GENDER_MALE, GENDER_FEMALE,ETHNICITY_White, ETHNICITY_Hispanic,
                  ETHNICITY_Black, ETHNICITY_Other,URBAN_LOC_21) |>
    # dplyr::mutate(
    #   GRADE = factor(
    #     dplyr::if_else(GRADE == 13, "K", as.character(GRADE)),
    #     levels = c("K", as.character(1:8))
    #   )) |>
    dplyr::mutate(GRADE = as.numeric(as.character(GRADE))) |>
    dplyr::mutate(GRADE = dplyr::if_else(GRADE == 13, 0, GRADE)) |>
    # dplyr::mutate(GENDER_MALE = factor(GENDER_MALE, levels = c(0, 1)),
    #               GENDER_FEMALE = factor(GENDER_FEMALE, levels = c(0, 1)),
    #               ETHNICITY_White = factor(ETHNICITY_White, levels = c(0, 1)),
    #               ETHNICITY_Hispanic = factor(ETHNICITY_Hispanic, levels = c(0, 1)),
    #               ETHNICITY_Black = factor(ETHNICITY_Black, levels = c(0, 1)),
    #               ETHNICITY_Other = factor(ETHNICITY_Other, levels = c(0, 1)),
    #               URBAN_LOC_21 = factor(URBAN_LOC_21, levels = c(0, 1)),
    #               treat = factor(treat, levels = c(0, 1), labels = c("Control", "Treatment"))) |>
    dplyr::mutate(treat = factor(treat, levels = c(0, 1), labels = c("Control", "Treatment")))


  return(dat_indices)
}


run_model <- function(.df, modname, group, .control = NULL, .analysis = "growth"){

  # Ensure inputs are of same length or recycle appropriately
  max_length <- max(length(group),
                    length(.control),
                    length(.analysis))

  # Recycle vectors if needed
  if (length(group) < max_length) group <- rep(group, length.out = max_length)
  if (length(.control) < max_length) .control <- rep(.control, length.out = max_length)
  if (length(.analysis) < max_length) .analysis <- rep(.analysis, length.out = max_length)

  results <- list()
  for (i in seq_along(group)) {
    # Get the analytic sample
    .df_mod <- get_analytic_sample(
      .df,
      .analysis[i],
      group[i],
      .control[i]
    )

    # Check if treatment factor has at least two levels
    if (length(levels(.df_mod$treat)) < 2) {
      warning(paste("Skipping model", paste0(group[i], "_",.analysis[i]),
                    "- treatment factor has fewer than 2 levels"))
      next  # Skip to the next iteration
    }

    build_model_formula <- function(a) {
      base_formula <- switch(a,
                             "badj" = "outcome ~ treat + baseline",
                             "growth" = "outcome ~ treat",
                             "Invalid group specified")

      if (base_formula == "Invalid group specified") {
        stop("Must select a supported group ('badj' or 'growth').")
      }

      # Initialize formula
      .formula <- base_formula

      # Add control variables if specified and valid
      if (!is.null(.control[i]) && length(.control[i]) > 0 && !is.na(.control[i])) {
        .formula <- paste0(.formula, " + ", .control[i])
      }
      return(.formula)
    }

    .mod <- build_model_formula(.analysis[i])

    tryCatch({
      results[[paste0(modname[i], "_", .analysis[i])]] <-
        stats::lm(stats::as.formula(.mod), data = .df_mod)
    }, error = function(e) {
      warning(paste("Error in model:", group[i], .analysis[i], "-", e$message))
      return(NULL)
    })
  }

  # Output results
  return(results)
}

plot_model_results <- function(run_model_results, terms = "treat"){
  # Check if input is a valid list of models
  if (!is.list(run_model_results) || length(run_model_results) == 0) {
    stop("Input must be a non-empty list of model objects")
  }

  # Check if any models are NULL or invalid
  valid_models <- !sapply(run_model_results, is.null)
  if (sum(valid_models) == 0) {
    stop("No valid models found in input")
  }

  # Only process valid models
  models_to_process <- run_model_results[valid_models]
  model_names <- names(models_to_process)

  plot_treatment_effects <- function(model, model_name) {
    # Try-catch to handle potential errors in ggpredict
    tryCatch({
      # Generate predicted effects for the specified terms
      predictions <- ggeffects::ggpredict(model, terms = terms)

      # Determine y-axis label based on model name suffix
      if (endsWith(model_name, "_badj")){
        .y_lab <- "Percent Proficient"
      } else if (endsWith(model_name, "growth")) {
        .y_lab <- "Assessment Score Growth"
      } else {
        .y_lab <- "Outcome"  # Default label if neither BA nor G
      }

      # Parse model name to create a more readable title
      name_parts <- strsplit(model_name, "_")[[1]]
      if (length(name_parts) >= 3) {
        group <- name_parts[1]
        state <- sub("^S", "", name_parts[2])  # Remove "S" prefix
        grade <- sub("^G", "", name_parts[3])  # Remove "G" prefix

        title <- paste0(
          "Treatment Effects: ",
          group, " Group",
          ifelse(state != "All", paste0(", State: ", state), ""),
          ifelse(grade != "All", paste0(", Grade: ", grade), "")
        )
      } else {
        title <- paste("Treatment Effects:", gsub("_", " ", model_name))
      }

      # Create plot with enhanced styling
      p <- plot(predictions) +
        ggplot2::ggtitle(title) +
        ggplot2::xlab("Group") +
        ggplot2::ylab(.y_lab) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold"),
          axis.title = ggplot2::element_text(face = "bold"),
          legend.position = "bottom"
        )

      return(p)
    }, error = function(e) {
      warning(paste("Error creating plot for model", model_name, ":", e$message))
      return(NULL)
    })
  }

  # Create list of plots and remove any NULL results from errors
  plots <- lapply(seq_along(models_to_process), function(i) {
    plot_treatment_effects(models_to_process[[i]], model_names[i])
  })

  # Filter out NULL plots from errors
  plots <- plots[!sapply(plots, is.null)]

  # Name the plots based on model names
  names(plots) <- model_names[valid_models][!sapply(plots, is.null)]

  return(plots)
}


model_workflow <- function(.df,
                           modname,
                           group,
                           .control = NULL,
                           .analysis = c("growth", "badj")) {

  # Run the models with error handling
  run_model_results <- run_model(.df, modname, group, .control, .analysis)

  # Check if any models were successfully created
  if (length(run_model_results) == 0) {
    cat("\nNo valid models could be created. Please check your input parameters.\n")
    return(invisible(NULL))
  }

  # Process results for each group
  for (name in names(run_model_results)) {
    cat(paste("\nResults for Group:", name, "\n-------------------------\n"))

    # Skip NULL models (which might occur if there was an error)
    if (is.null(run_model_results[[name]])) {
      cat("Model could not be created.\n")
      next
    }

    # a) Descriptives
    if (inherits(run_model_results[[name]], "lm")){
      desc_data <- run_model_results[[name]]$model

      # Check if treatment has enough levels for stratification
      if (length(unique(desc_data$treat)) < 2) {
        cat("Cannot create stratified tables: treatment has fewer than 2 levels.\n")
      } else {
        tryCatch({
          print(tableone::CreateTableOne(vars = all.vars(stats::formula(run_model_results[[name]])),
                                         strata = "treat",
                                         data = desc_data,
                                         test = FALSE,
                                         addOverall = TRUE),
                smd = FALSE)

          print(tableone::CreateTableOne(vars = all.vars(stats::formula(run_model_results[[name]])),
                                         strata = "treat",
                                         data = desc_data,
                                         test = FALSE,
                                         addOverall = TRUE),
                smd = FALSE,
                nonnormal = TRUE)
        }, error = function(e) {
          cat("Error creating table one:", e$message, "\n")
        })
      }
    }

    # b) Summary
    tryCatch({
      model_summary <- summary(run_model_results[[name]])
      print(model_summary)
    }, error = function(e) {
      cat("Error creating model summary:", e$message, "\n")
    })

    # c) Confidence Intervals
    tryCatch({
      broom_out <- broom::tidy(run_model_results[[name]], conf.int = TRUE)
      print(broom_out)
    }, error = function(e) {
      cat("Error creating confidence intervals:", e$message, "\n")
    })

    # d) Standardized Effect Size
    tryCatch({
      spmod <- parameters::standardize_parameters(run_model_results[[name]],
                                                  method = "posthoc")
      print(spmod)
    }, error = function(e) {
      cat("Error calculating standardized effect size:", e$message, "\n")
    })

    # d.2) Hedges G Effect Size
    tryCatch({
      # Check if we have both treatment and control groups
      if (all(c("Treatment", "Control") %in% levels(run_model_results[[name]]$model$treat))) {
        sdT <- stats::sd(run_model_results[[name]]$model[1][run_model_results[[name]]$model$treat == "Treatment",])
        sdC <- stats::sd(run_model_results[[name]]$model[1][run_model_results[[name]]$model$treat == "Control",])
        nT <- length(run_model_results[[name]]$model[1][run_model_results[[name]]$model$treat == "Treatment",])
        nC <- length(run_model_results[[name]]$model[1][run_model_results[[name]]$model$treat == "Control",])
        sdp <- sqrt((((nT-1)*(sdT^2)) + ((nC-1)*(sdC^2)))/(nT + nC - 2))
        degf <- nT + nC - 2
        omega <- 1 - 3/(4*degf - 1)

        # Determine which coefficient to use
        if (any(names(stats::coefficients(run_model_results[[name]])) == "treatTreatment:years_used")){
          b <- broom::tidy(run_model_results[[name]], conf.int = TRUE)[["estimate"]][[length(broom::tidy(run_model_results[[name]], conf.int = TRUE)[["estimate"]])]]
          confL <- broom::tidy(run_model_results[[name]], conf.int = TRUE)[["conf.low"]][[length(broom::tidy(run_model_results[[name]], conf.int = TRUE)[["conf.low"]])]]
          confH <- broom::tidy(run_model_results[[name]], conf.int = TRUE)[["conf.high"]][[length(broom::tidy(run_model_results[[name]], conf.int = TRUE)[["conf.high"]])]]
        } else {
          # Check if treatment coefficient exists
          coef_index <- which(names(stats::coefficients(run_model_results[[name]])) == "treatTreatment")
          if (length(coef_index) > 0) {
            b <- broom::tidy(run_model_results[[name]], conf.int = TRUE)[["estimate"]][[coef_index]]
            confL <- broom::tidy(run_model_results[[name]], conf.int = TRUE)[["conf.low"]][[coef_index]]
            confH <- broom::tidy(run_model_results[[name]], conf.int = TRUE)[["conf.high"]][[coef_index]]
          } else {
            cat("Error: Could not find treatment coefficient\n")
            next
          }
        }

        g <- (omega*b)/sdp
        lCI <- (omega*confL)/sdp
        uCI <- (omega*confH)/sdp

        cat(paste("\nHedges g:", g, "[",lCI, ",", uCI, "]\n"))
      } else {
        cat("Cannot calculate Hedges g: missing either Treatment or Control group\n")
      }
    }, error = function(e) {
      cat("Error calculating Hedges g:", e$message, "\n")
    })

    # e) Diagnostic Plots
    tryCatch({
      plot(run_model_results[[name]])
    }, error = function(e) {
      cat("Error creating diagnostic plots:", e$message, "\n")
    })

    # f) Treatment Effects Plot
    tryCatch({
      if (any(names(stats::coefficients(run_model_results[[name]])) == "treatTreatment:years_used")){
        treatment_effects_plot <- plot_model_results(run_model_results[name], terms = c("treat", "years_used"))
      } else{
        treatment_effects_plot <- plot_model_results(run_model_results[name])
      }
      print(treatment_effects_plot)
    }, error = function(e) {
      cat("Error creating treatment effects plot:", e$message, "\n")
    })

    # g) Estimated Marginal Means
    tryCatch({
      if (any(names(stats::coefficients(run_model_results[[name]])) == "treatTreatment:years_used")){
        print(emmeans::emmeans(run_model_results[[name]], c("treat", "years_used")))
      } else{
        print(emmeans::emmeans(run_model_results[[name]], "treat"))
      }
    }, error = function(e) {
      cat("Error calculating estimated marginal means:", e$message, "\n")
    })

    # h) Predicted Means
    tryCatch({
      run_model_results[[name]]$model[["predicted"]] <- stats::predict(run_model_results[[name]])

      # Check if both treatment and control exist
      if ("Treatment" %in% levels(run_model_results[[name]]$model$treat)) {
        tp_mean <- mean(run_model_results[[name]]$model["predicted"][run_model_results[[name]]$model$treat == "Treatment",])
        cat(paste("\nPredicted Mean Treatment:", tp_mean, "\n"))
      }

      if ("Control" %in% levels(run_model_results[[name]]$model$treat)) {
        cp_mean <- mean(run_model_results[[name]]$model["predicted"][run_model_results[[name]]$model$treat == "Control",])
        cat(paste("\nPredicted Mean Control:", cp_mean, "\n"))
      }
    }, error = function(e) {
      cat("Error calculating predicted means:", e$message, "\n")
    })
  }

  # Return the model results invisibly
  return(invisible(run_model_results))
}
