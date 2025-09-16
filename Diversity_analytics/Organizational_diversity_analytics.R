# ============================================================================
# SETUP AND PACKAGE INSTALLATION
# ============================================================================

# Function to safely install and load packages
install_and_load <- function(packages) {
  for(pkg in packages) {
    if(!require(pkg, character.only = TRUE, quietly = TRUE)) {
      cat("Installing", pkg, "...\n")
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }
}

# Install required packages
required_packages <- c(
  "tidyverse", "broom", "psych", "effectsize", 
  "VIM", "mice", "car", "ggplot2", "plotly",
  "corrplot", "knitr", "DT", "lmtest"
)

install_and_load(required_packages)

# ============================================================================
# DATA LOADING WITH ERROR HANDLING
# ============================================================================

load_diversity_data <- function(file_path) {
  tryCatch({
    # Try different separators
    if(file.exists(file_path)) {
      # First try tab-separated
      df <- read.csv(file_path, sep="\t", stringsAsFactors = FALSE, header = TRUE)
      
      # If only one column, try different separator
      if(ncol(df) == 1) {
        df <- read.csv(file_path, sep=",", stringsAsFactors = FALSE, header = TRUE)
      }
      
      cat("Data loaded successfully:", nrow(df), "rows,", ncol(df), "columns\n")
      cat("Columns:", paste(colnames(df)[1:min(5, ncol(df))], collapse=", "),
          ifelse(ncol(df) > 5, "...", ""), "\n")
      return(df)
    } else {
      cat("Error: File", file_path, "not found\n")
      cat("Current working directory:", getwd(), "\n")
      cat("Files in directory:", paste(list.files(), collapse=", "), "\n")
      return(NULL)
    }
  }, error = function(e) {
    cat("Error loading data:", e$message, "\n")
    return(NULL)
  })
}

# ============================================================================
# DATA CLEANING AND STANDARDIZATION
# ============================================================================

clean_and_standardize <- function(df) {
  
  cat("Cleaning and standardizing data...\n")
  
  # Check for required columns with flexible naming
  column_mapping <- list(
    ug = c("UG", "ug", "UnderRepresented", "underrepresented"),
    percent_male = c("PercentMale", "percentmale", "PercentMale", "Male_Pct"),
    function_col = c("Function", "function", "Func", "Department_Type"),
    london = c("LondonorNot", "londonornot", "London", "Location_Binary"),
    group_size = c("GroupSize", "groupsize", "TeamSize", "Size"),
    team_leads = c("NumberTeamLeads", "numberteamleads", "TeamLeads", "Leaders"),
    female_leads = c("NumberFeMaleTeamLeads", "NumberFemaleTeamLeads",
                     "numberfemalelands", "Female_Leaders", "FemaleLeads"),
    dept_group = c("DepartmentGroupNumber", "departmentgroupnumber",
                   "Group_ID", "Team_ID", "ID"),
    engagement = c("EMPsurvEngagement", "Engagement", "engagement"),
    org_integrity = c("EmpSurvOrgIntegrity", "OrgIntegrity", "Integrity"),
    supervisor = c("EmpSurvSupervisor", "Supervisor", "supervisor")
  )
  
  # Function to find column name
  find_column <- function(possible_names, df_cols) {
    for(name in possible_names) {
      if(name %in% df_cols) return(name)
    }
    return(NULL)
  }
  
  # Map columns
  df_cols <- colnames(df)
  mapped_cols <- list()
  
  for(var in names(column_mapping)) {
    found_col <- find_column(column_mapping[[var]], df_cols)
    if(!is.null(found_col)) {
      mapped_cols[[var]] <- found_col
    }
  }
  
  # Print mapping results
  cat("Column mapping results:\n")
  for(var in names(mapped_cols)) {
    cat("-", var, "->", mapped_cols[[var]], "\n")
  }
  
  # Standardize column names
  if(!is.null(mapped_cols$ug)) df$UG <- df[[mapped_cols$ug]]
  if(!is.null(mapped_cols$percent_male)) df$PercentMale <- df[[mapped_cols$percent_male]]
  if(!is.null(mapped_cols$function_col)) df$Function <- df[[mapped_cols$function_col]]
  if(!is.null(mapped_cols$london)) df$LondonorNot <- df[[mapped_cols$london]]
  if(!is.null(mapped_cols$group_size)) df$GroupSize <- df[[mapped_cols$group_size]]
  if(!is.null(mapped_cols$team_leads)) df$NumberTeamLeads <- df[[mapped_cols$team_leads]]
  if(!is.null(mapped_cols$female_leads)) df$NumberFemaleTeamLeads <- df[[mapped_cols$female_leads]]
  if(!is.null(mapped_cols$dept_group)) df$DepartmentGroupNumber <- df[[mapped_cols$dept_group]]
  if(!is.null(mapped_cols$engagement)) df$EMPsurvEngagement <- df[[mapped_cols$engagement]]
  if(!is.null(mapped_cols$org_integrity)) df$EmpSurvOrgIntegrity <- df[[mapped_cols$org_integrity]]
  if(!is.null(mapped_cols$supervisor)) df$EmpSurvSupervisor <- df[[mapped_cols$supervisor]]
  
  # Create missing columns with defaults if needed
  if(!"NumberFemaleTeamLeads" %in% colnames(df)) {
    cat("Warning: Female team leads column not found. Creating placeholder.\n")
    df$NumberFemaleTeamLeads <- 0
  }
  if(!"NumberTeamLeads" %in% colnames(df)) {
    cat("Warning: Team leads column not found. Creating placeholder.\n")
    df$NumberTeamLeads <- 1  # Assume at least one leader per team
  }
  if(!"DepartmentGroupNumber" %in% colnames(df)) {
    df$DepartmentGroupNumber <- 1:nrow(df)
  }
  
  # Convert to numeric where needed
  numeric_cols <- c("UG", "PercentMale", "GroupSize", "NumberTeamLeads",
                    "NumberFemaleTeamLeads", "EMPsurvEngagement",
                    "EmpSurvOrgIntegrity", "EmpSurvSupervisor")
  
  for(col in numeric_cols) {
    if(col %in% colnames(df)) {
      # Convert UG to percentage if it appears to be in decimal form
      if(col == "UG") {
        temp_val <- suppressWarnings(as.numeric(as.character(df[[col]])))
        if(max(temp_val, na.rm = TRUE) <= 1) {
          df[[col]] <- temp_val * 100  # Convert to percentage
        } else {
          df[[col]] <- temp_val
        }
      } else {
        df[[col]] <- suppressWarnings(as.numeric(as.character(df[[col]])))
      }
    }
  }
  
  cat("Data standardization complete.\n")
  cat("Final dataset:", nrow(df), "rows with standardized columns\n")
  
  return(df)
}

# ============================================================================
# FOUNDATIONAL ANALYSIS WITH REGRESSION TECHNIQUES
# ============================================================================

foundational_analysis <- function(df) {
  
  cat("=== PHASE 1: FOUNDATIONAL DIVERSITY ANALYSIS ===\n\n")
  
  # Validate required columns
  required_cols <- c("UG", "PercentMale", "Function")
  missing_cols <- setdiff(required_cols, colnames(df))
  
  if(length(missing_cols) > 0) {
    cat("Error: Missing required columns:", paste(missing_cols, collapse=", "), "\n")
    cat("Available columns:", paste(colnames(df), collapse=", "), "\n")
    return(NULL)
  }
  
  # 1.1 Descriptive Statistics
  cat("1.1 DESCRIPTIVE STATISTICS\n")
  
  desc_vars <- intersect(c("UG", "PercentMale", "GroupSize", "EMPsurvEngagement"),
                         colnames(df))
  
  if(length(desc_vars) > 0) {
    descriptive_stats <- df %>%
      select(all_of(desc_vars)) %>%
      psych::describe() %>%
      as.data.frame() %>%
      rownames_to_column("Variable") %>%
      mutate(
        missing_rate = round((nrow(df) - n)/nrow(df) * 100, 1),
        cv = round(sd/mean, 3)
      ) %>%
      select(Variable, n, mean, sd, min, max, missing_rate, cv)
    
    print(knitr::kable(descriptive_stats, digits = 2,
                       caption = "Descriptive Statistics"))
  }
  
  # 1.2 Missing Data Analysis
  cat("\n1.2 MISSING DATA ANALYSIS\n")
  
  missing_summary <- df %>%
    summarise(
      total_teams = n(),
      ug_missing = sum(is.na(UG)),
      ug_missing_rate = round(mean(is.na(UG)) * 100, 1),
      male_missing = sum(is.na(PercentMale)),
      male_missing_rate = round(mean(is.na(PercentMale)) * 100, 1)
    )
  
  cat("Missing Data Summary:\n")
  cat("- Total teams:", missing_summary$total_teams, "\n")
  cat("- UG data missing:", missing_summary$ug_missing, "teams (",
      missing_summary$ug_missing_rate, "%)\n")
  cat("- Gender data missing:", missing_summary$male_missing, "teams (",
      missing_summary$male_missing_rate, "%)\n")
  
  # 1.3 Statistical Comparisons
  cat("\n1.3 STATISTICAL COMPARISONS\n")
  
  # Filter complete cases for analysis
  df_complete <- df %>%
    filter(!is.na(UG), !is.na(PercentMale), !is.na(Function))
  
  cat("Complete cases for analysis:", nrow(df_complete), "teams\n")
  
  if(nrow(df_complete) < 10) {
    cat("Error: Insufficient complete data for analysis\n")
    return(NULL)
  }
  
  # Initialize effect size variables
  ug_effect <- NULL
  gender_effect <- NULL
  
  # T-tests with effect sizes
  if(length(unique(df_complete$Function)) >= 2) {
    df_complete$Function <- as.factor(df_complete$Function)
    
    tryCatch({
      ug_test <- t.test(UG ~ Function, data=df_complete, var.equal=FALSE)
      ug_effect <- effectsize::cohens_d(UG ~ Function, data=df_complete)
      
      gender_test <- t.test(PercentMale ~ Function, data=df_complete, var.equal=FALSE)
      gender_effect <- effectsize::cohens_d(PercentMale ~ Function, data=df_complete)
      
      cat("\nUG Representation Analysis:\n")
      cat("- Group 1 mean:", round(ug_test$estimate[1], 2), "%\n")
      cat("- Group 2 mean:", round(ug_test$estimate[2], 2), "%\n")
      cat("- Difference:", round(ug_test$estimate[2] - ug_test$estimate[1], 2),
          "percentage points\n")
      cat("- Effect size (Cohen's d):", round(ug_effect$Cohens_d, 3), "\n")
      cat("- Statistical significance: p =", format.pval(ug_test$p.value), "\n")
      
      cat("\nGender Composition Analysis:\n")
      cat("- Group 1 male %:", round(gender_test$estimate[1], 2), "%\n")
      cat("- Group 2 male %:", round(gender_test$estimate[2], 2), "%\n")
      cat("- Difference:", round(gender_test$estimate[2] - gender_test$estimate[1], 2),
          "percentage points\n")
      cat("- Effect size (Cohen's d):", round(gender_effect$Cohens_d, 3), "\n")
      
    }, error = function(e) {
      cat("Error in statistical tests:", e$message, "\n")
    })
    
  } else {
    cat("Warning: Function variable does not have enough groups for comparison\n")
    cat("Unique values:", unique(df_complete$Function), "\n")
  }
  
  # =========================================================================
  # 1.4 REGRESSION ANALYSIS 
  # =========================================================================
  
  cat("\n1.4 REGRESSION ANALYSIS\n")
  cat("Following systematic model development approach from econometric best practices\n\n")
  
  # Prepare data for regression analysis
  available_predictors <- c()
  
  # Check and prepare variables for regression
  if("LondonorNot" %in% colnames(df_complete) &&
     length(unique(df_complete$LondonorNot)) > 1) {
    df_complete$LondonorNot <- as.factor(df_complete$LondonorNot)
    available_predictors <- c(available_predictors, "LondonorNot")
  }
  
  if("Function" %in% colnames(df_complete) &&
     length(unique(df_complete$Function)) > 1) {
    df_complete$Function <- as.factor(df_complete$Function)
    available_predictors <- c(available_predictors, "Function")
  }
  
  # Add numeric predictors
  numeric_predictors <- c("GroupSize", "NumberFemaleTeamLeads", "PercentMale",
                          "EMPsurvEngagement", "EmpSurvOrgIntegrity", "EmpSurvSupervisor")
  for(var in numeric_predictors) {
    if(var %in% colnames(df_complete) &&
       sum(!is.na(df_complete[[var]])) > 10) {
      df_complete[[var]] <- as.numeric(df_complete[[var]])
      available_predictors <- c(available_predictors, var)
    }
  }
  
  if(length(available_predictors) == 0) {
    cat("No suitable predictors available for regression\n")
    return(list(
      complete_data = df_complete,
      original_data = df,
      models = NULL,
      effect_sizes = list(ug = ug_effect, gender = gender_effect),
      sample_size = nrow(df_complete)
    ))
  }
  
  # Create model data with complete cases only
  vars_needed <- c("UG", available_predictors)
  model_data <- df_complete %>%
    select(all_of(vars_needed)) %>%
    na.omit()
  
  cat("Using", nrow(model_data), "rows for regression after removing NAs.\n")
  
  if(nrow(model_data) < 20) {
    cat("Warning: Insufficient data for reliable regression analysis\n")
    return(list(
      complete_data = df_complete,
      original_data = df,
      models = NULL,
      effect_sizes = list(ug = ug_effect, gender = gender_effect),
      sample_size = nrow(df_complete)
    ))
  }
  # Test transformations for UG
  shapiro.test(df_complete$UG)  # Original
  shapiro.test(log(df_complete$UG + 0.1))  # Log (add small constant for zeros)
  shapiro.test(sqrt(df_complete$UG))  # Square root
  # Ensure UG is numeric
  model_data$UG <- as.numeric(model_data$UG)
  
  # =========================================================================
  # MODEL DEVELOPMENT SEQUENCE
  # =========================================================================
  
  models <- list()
  model_summaries <- list()
  
  # Debug: Check data structure
  cat("\n--- DEBUGGING DATA STRUCTURE ---\n")
  cat("Available predictors:", paste(available_predictors, collapse=", "), "\n")
  cat("Model data dimensions:", nrow(model_data), "x", ncol(model_data), "\n")
  cat("UG variable summary:\n")
  print(summary(model_data$UG))
  
  # Model A: Base model with most correlated variables
  cat("\n--- MODEL A: Base Model ---\n")
  
  # Calculate correlations to identify key predictors
  numeric_vars <- model_data %>% select_if(is.numeric) %>% colnames()
  numeric_vars <- setdiff(numeric_vars, "UG")  # Remove dependent variable
  
  cat("Numeric variables found:", paste(numeric_vars, collapse=", "), "\n")
  
  if(length(numeric_vars) > 0) {
    # Calculate correlations
    correlations <- cor(model_data$UG, model_data[numeric_vars], use="complete.obs")
    cat("Correlations with UG:\n")
    print(round(correlations, 3))
    
    # Lower threshold if no high correlations found
    threshold <- if(max(abs(correlations), na.rm=TRUE) > 0.3) 0.3 else 0.1
    high_corr_vars <- names(correlations[1, abs(correlations[1, ]) > threshold])
    
    cat("Variables above threshold", threshold, ":", paste(high_corr_vars, collapse=", "), "\n")
    
    if(length(high_corr_vars) > 0) {
      # Select top 2-3 most correlated variables for base model
      base_vars <- head(high_corr_vars[order(abs(correlations[1, high_corr_vars]), decreasing = TRUE)], 3)
      
      # Add categorical variables if available
      cat_vars <- available_predictors[!available_predictors %in% numeric_vars]
      if(length(cat_vars) > 0) {
        base_vars <- c(base_vars, cat_vars[1])  # Add first categorical variable
      }
      
      cat("Base model variables:", paste(base_vars, collapse=", "), "\n")
      formula_a <- as.formula(paste("UG ~", paste(base_vars, collapse=" + ")))
      
      tryCatch({
        models$model_a <- lm(formula_a, data=model_data)
        model_summaries$model_a <- summary(models$model_a)
        
        cat("Model A Formula:", deparse(formula_a), "\n")
        cat("R-squared:", round(model_summaries$model_a$r.squared, 4), "\n")
        cat("Adjusted R-squared:", round(model_summaries$model_a$adj.r.squared, 4), "\n")
        cat("F-statistic:", round(model_summaries$model_a$fstatistic[1], 2), "\n")
        cat("p-value:", format.pval(pf(model_summaries$model_a$fstatistic[1],
                                       model_summaries$model_a$fstatistic[2],
                                       model_summaries$model_a$fstatistic[3],
                                       lower.tail=FALSE)), "\n")
        
        # VIF Analysis
        if(length(base_vars) > 1) {
          tryCatch({
            vif_vals <- car::vif(models$model_a)
            if(is.matrix(vif_vals)) vif_vals <- vif_vals[,1]
            cat("VIF values:\n")
            print(round(vif_vals, 2))
          }, error = function(e) {
            cat("VIF calculation failed:", e$message, "\n")
          })
        }
        
      }, error = function(e) {
        cat("Model A failed:", e$message, "\n")
        models$model_a <- NULL
      })
    } else {
      cat("No variables meet correlation threshold - trying simpler approach\n")
      
      # Fallback: Try first available predictor
      if(length(available_predictors) > 0) {
        simple_var <- available_predictors[1]
        formula_simple <- as.formula(paste("UG ~", simple_var))
        
        tryCatch({
          models$model_a <- lm(formula_simple, data=model_data)
          model_summaries$model_a <- summary(models$model_a)
          
          cat("Simple Model Formula:", deparse(formula_simple), "\n")
          cat("R-squared:", round(model_summaries$model_a$r.squared, 4), "\n")
          cat("Adjusted R-squared:", round(model_summaries$model_a$adj.r.squared, 4), "\n")
          
          base_vars <- simple_var
          
        }, error = function(e) {
          cat("Simple model also failed:", e$message, "\n")
          models$model_a <- NULL
        })
      }
    }
  } else {
    cat("No numeric variables found - trying categorical only\n")
    
    # Try with categorical variables only
    cat_vars <- available_predictors[!available_predictors %in% numeric_vars]
    if(length(cat_vars) > 0) {
      simple_var <- cat_vars[1]
      formula_simple <- as.formula(paste("UG ~", simple_var))
      
      tryCatch({
        models$model_a <- lm(formula_simple, data=model_data)
        model_summaries$model_a <- summary(models$model_a)
        
        cat("Categorical Model Formula:", deparse(formula_simple), "\n")
        cat("R-squared:", round(model_summaries$model_a$r.squared, 4), "\n")
        cat("Adjusted R-squared:", round(model_summaries$model_a$adj.r.squared, 4), "\n")
        
        base_vars <- simple_var
        
      }, error = function(e) {
        cat("Categorical model failed:", e$message, "\n")
        models$model_a <- NULL
      })
    }
  }
  
  # Model B: Extended model with additional variables
  cat("\n--- MODEL B: Extended Model ---\n")
  
  if(!is.null(models$model_a) && length(available_predictors) > length(base_vars)) {
    # Add 1-2 more variables to base model
    remaining_vars <- setdiff(available_predictors, base_vars)
    additional_vars <- head(remaining_vars, 2)
    
    extended_vars <- c(base_vars, additional_vars)
    formula_b <- as.formula(paste("UG ~", paste(extended_vars, collapse=" + ")))
    
    tryCatch({
      models$model_b <- lm(formula_b, data=model_data)
      model_summaries$model_b <- summary(models$model_b)
      
      cat("Model B Formula:", deparse(formula_b), "\n")
      cat("R-squared:", round(model_summaries$model_b$r.squared, 4), "\n")
      cat("Adjusted R-squared:", round(model_summaries$model_b$adj.r.squared, 4), "\n")
      cat("F-statistic:", round(model_summaries$model_b$fstatistic[1], 2), "\n")
      
      # Compare with Model A
      if(!is.null(models$model_a)) {
        cat("Improvement over Model A:\n")
        cat("- R-sq change:", round(model_summaries$model_b$adj.r.squared -
                                      model_summaries$model_a$adj.r.squared, 4), "\n")
      }
      
    }, error = function(e) {
      cat("Model B failed:", e$message, "\n")
      models$model_b <- NULL
    })
  }
  
  # Model C: Interaction model (if applicable)
  cat("\n--- MODEL C: Interaction Model ---\n")
  
  if(!is.null(models$model_b)) {
    # Try multiple interaction strategies
    interaction_created <- FALSE
    
    # Strategy 1: Interaction between first two variables (numeric-categorical allowed)
    if(length(base_vars) >= 2 && !interaction_created) {
      var1 <- base_vars[1]
      var2 <- base_vars[2]
      
      cat("Attempting interaction between", var1, "(", class(model_data[[var1]]), ") and",
          var2, "(", class(model_data[[var2]]), ")\n")
      
      if(exists("extended_vars") && length(extended_vars) > 0) {
        formula_c_str <- paste("UG ~", paste(extended_vars, collapse=" + "), "+",
                               var1, ":", var2)
      } else {
        formula_c_str <- paste("UG ~", paste(base_vars, collapse=" + "), "+",
                               var1, ":", var2)
      }
      formula_c <- as.formula(formula_c_str)
      
      tryCatch({
        models$model_c <- lm(formula_c, data=model_data)
        model_summaries$model_c <- summary(models$model_c)
        
        cat("Model C Formula:", deparse(formula_c), "\n")
        cat("R-squared:", round(model_summaries$model_c$r.squared, 4), "\n")
        cat("Adjusted R-squared:", round(model_summaries$model_c$adj.r.squared, 4), "\n")
        
        # Test significance of interaction
        interaction_pval <- tail(model_summaries$model_c$coefficients[,4], 1)
        cat("Interaction p-value:", format.pval(interaction_pval), "\n")
        
        interaction_created <- TRUE
        
      }, error = function(e) {
        cat("Strategy 1 failed:", e$message, "\n")
      })
    }
    
    # Strategy 2: Find two numeric variables for interaction
    if(!interaction_created && length(numeric_vars) >= 2) {
      var1 <- numeric_vars[1]
      var2 <- numeric_vars[2]
      
      cat("Attempting numeric-numeric interaction between", var1, "and", var2, "\n")
      
      # Use existing model variables plus interaction
      current_vars <- if(exists("extended_vars") && length(extended_vars) > 0) {
        extended_vars
      } else {
        base_vars
      }
      
      formula_c_str <- paste("UG ~", paste(current_vars, collapse=" + "), "+",
                             var1, ":", var2)
      formula_c <- as.formula(formula_c_str)
      
      tryCatch({
        models$model_c <- lm(formula_c, data=model_data)
        model_summaries$model_c <- summary(models$model_c)
        
        cat("Model C Formula:", deparse(formula_c), "\n")
        cat("R-squared:", round(model_summaries$model_c$r.squared, 4), "\n")
        cat("Adjusted R-squared:", round(model_summaries$model_c$adj.r.squared, 4), "\n")
        
        # Test significance of interaction
        interaction_pval <- tail(model_summaries$model_c$coefficients[,4], 1)
        cat("Interaction p-value:", format.pval(interaction_pval), "\n")
        
        interaction_created <- TRUE
        
      }, error = function(e) {
        cat("Strategy 2 failed:", e$message, "\n")
      })
    }
    
    # Strategy 3: Quadratic term if no interactions work
    if(!interaction_created && length(numeric_vars) >= 1) {
      var1 <- numeric_vars[1]
      
      cat("Attempting quadratic term for", var1, "\n")
      
      # Create quadratic term
      quad_var_name <- paste0(var1, "_sq")
      model_data[[quad_var_name]] <- model_data[[var1]]^2
      
      current_vars <- if(exists("extended_vars") && length(extended_vars) > 0) {
        extended_vars
      } else {
        base_vars
      }
      
      formula_c_str <- paste("UG ~", paste(current_vars, collapse=" + "), "+", quad_var_name)
      formula_c <- as.formula(formula_c_str)
      
      tryCatch({
        models$model_c <- lm(formula_c, data=model_data)
        model_summaries$model_c <- summary(models$model_c)
        
        cat("Model C Formula:", deparse(formula_c), "\n")
        cat("R-squared:", round(model_summaries$model_c$r.squared, 4), "\n")
        cat("Adjusted R-squared:", round(model_summaries$model_c$adj.r.squared, 4), "\n")
        
        # Test significance of quadratic term
        quad_pval <- tail(model_summaries$model_c$coefficients[,4], 1)
        cat("Quadratic term p-value:", format.pval(quad_pval), "\n")
        
        interaction_created <- TRUE
        
      }, error = function(e) {
        cat("Strategy 3 failed:", e$message, "\n")
      })
    }
    
    if(!interaction_created) {
      cat("No interaction or polynomial terms could be created\n")
      models$model_c <- NULL
    }
    
  } else {
    cat("Model B not available for interaction modeling\n")
  }
  
  # ================================================================
  # Model D: Log-transformed UG as dependent variable
  # ================================================================
  cat("\n--- MODEL D: Log-Transformed UG ---\n")
  if(all(model_data$UG > 0)) {  # Ensure no zeros/negatives
    model_data$logUG <- log(model_data$UG)
    
    formula_d <- as.formula(
      paste("logUG ~", paste(available_predictors, collapse=" + "))
    )
    
    tryCatch({
      models$model_d <- lm(formula_d, data=model_data)
      model_summaries$model_d <- summary(models$model_d)
      
      cat("Model D Formula:", deparse(formula_d), "\n")
      cat("Adjusted R²:", round(model_summaries$model_d$adj.r.squared, 4), "\n")
    }, error=function(e) {
      cat("Model D failed:", e$message, "\n")
      models$model_d <- NULL
    })
  }
  
  # ================================================================
  # Model E: Quadratic Terms
  # ================================================================
  cat("\n--- MODEL E: Quadratic Terms ---\n")
  quad_vars <- intersect(c("GroupSize", "PercentMale", "EMPsurvEngagement"),
                         colnames(model_data))
  
  if(length(quad_vars) > 0) {
    for(v in quad_vars) {
      model_data[[paste0(v, "_sq")]] <- model_data[[v]]^2
    }
    
    formula_e <- as.formula(
      paste("UG ~", paste(c(available_predictors, paste0(quad_vars, "_sq")),
                          collapse=" + "))
    )
    
    tryCatch({
      models$model_e <- lm(formula_e, data=model_data)
      model_summaries$model_e <- summary(models$model_e)
      
      cat("Model E Formula:", deparse(formula_e), "\n")
      cat("Adjusted R²:", round(model_summaries$model_e$adj.r.squared, 4), "\n")
    }, error=function(e) {
      cat("Model E failed:", e$message, "\n")
      models$model_e <- NULL
    })
  }
  
  # ================================================================
  # Model F: Interaction between Engagement and Female Leadership
  # ================================================================
  cat("\n--- MODEL F: Engagement × Female Leadership ---\n")
  if(all(c("EMPsurvEngagement", "NumberFemaleTeamLeads") %in% colnames(model_data))) {
    formula_f <- UG ~ EMPsurvEngagement * NumberFemaleTeamLeads
    
    tryCatch({
      models$model_f <- lm(formula_f, data=model_data)
      model_summaries$model_f <- summary(models$model_f)
      
      cat("Model F Formula:", deparse(formula_f), "\n")
      cat("Adjusted R²:", round(model_summaries$model_f$adj.r.squared, 4), "\n")
    }, error=function(e) {
      cat("Model F failed:", e$message, "\n")
      models$model_f <- NULL
    })
  }
  
  # ================================================================
  # Model G: Stepwise selection (AIC-based)
  # ================================================================
  cat("\n--- MODEL G: Stepwise Regression ---\n")
  full_formula <- as.formula(
    paste("UG ~", paste(available_predictors, collapse=" + "))
  )
  tryCatch({
    full_model <- lm(full_formula, data=model_data)
    models$model_g <- step(full_model, direction="both", trace=0)
    model_summaries$model_g <- summary(models$model_g)
    
    cat("Model G (Stepwise) Formula:",
        deparse(formula(models$model_g)), "\n")
    cat("Adjusted R²:", round(model_summaries$model_g$adj.r.squared, 4), "\n")
  }, error=function(e) {
    cat("Model G failed:", e$message, "\n")
    models$model_g <- NULL
  })
  # =========================================================================
  # MODEL COMPARISON AND SELECTION
  # =========================================================================
  
  cat("\n--- MODEL COMPARISON ---\n")
  
  valid_models <- models[!sapply(models, is.null)]
  
  if(length(valid_models) > 1) {
    # Create comparison table
    comparison_df <- data.frame(
      Model = names(valid_models),
      R_squared = sapply(valid_models, function(m) summary(m)$r.squared),
      Adj_R_squared = sapply(valid_models, function(m) summary(m)$adj.r.squared),
      AIC = sapply(valid_models, AIC),
      BIC = sapply(valid_models, BIC),
      F_statistic = sapply(valid_models, function(m) summary(m)$fstatistic[1]),
      p_value = sapply(valid_models, function(m) {
        fs <- summary(m)$fstatistic
        pf(fs[1], fs[2], fs[3], lower.tail=FALSE)
      }),
      stringsAsFactors = FALSE
    )
    
    comparison_df$R_squared <- round(comparison_df$R_squared, 4)
    comparison_df$Adj_R_squared <- round(comparison_df$Adj_R_squared, 4)
    comparison_df$AIC <- round(comparison_df$AIC, 2)
    comparison_df$BIC <- round(comparison_df$BIC, 2)
    comparison_df$F_statistic <- round(comparison_df$F_statistic, 2)
    comparison_df$p_value <- format.pval(comparison_df$p_value)
    
    print(knitr::kable(comparison_df, caption = "Model Comparison"))
    
    # Select best model based on adjusted R-squared
    best_model_name <- comparison_df$Model[which.max(comparison_df$Adj_R_squared)]
    best_model <- valid_models[[best_model_name]]
    
    cat("\nBest Model:", best_model_name, "\n")
    
    # F-test for nested models (if applicable)
    if(length(valid_models) >= 2) {
      cat("\n--- NESTED MODEL F-TESTS ---\n")
      model_names <- names(valid_models)
      
      for(i in 1:(length(valid_models)-1)) {
        for(j in (i+1):length(valid_models)) {
          tryCatch({
            f_test <- anova(valid_models[[i]], valid_models[[j]])
            cat("F-test:", model_names[i], "vs", model_names[j], "\n")
            cat("F-statistic:", round(f_test$F[2], 3), "\n")
            cat("p-value:", format.pval(f_test$`Pr(>F)`[2]), "\n\n")
          }, error = function(e) {
            cat("F-test failed for", model_names[i], "vs", model_names[j], "\n")
          })
        }
      }
    }
    
  } else if(length(valid_models) == 1) {
    best_model <- valid_models[[1]]
    best_model_name <- names(valid_models)[1]
  } else {
    cat("No valid models created\n")
    best_model <- NULL
    best_model_name <- NULL
  }
  
# =========================================================================
# CORRELATION ANALYSIS
# =========================================================================
  
correlation_analysis <- function(df) {
  
  cat("\n=== COMPREHENSIVE CORRELATION ANALYSIS ===\n\n")
  
  # 1. Prepare data for correlation analysis
  cat("1. PREPARING CORRELATION DATA\n")
  
  # Select numeric variables for correlation
  numeric_vars <- df %>%
    select_if(is.numeric) %>%
    colnames()
  
  cat("Numeric variables found:", paste(numeric_vars, collapse=", "), "\n")
  
  if(length(numeric_vars) < 2) {
    cat("Error: Need at least 2 numeric variables for correlation analysis\n")
    return(NULL)
  }
  
  # Create correlation matrix with complete cases
  cor_data <- df %>%
    select(all_of(numeric_vars)) %>%
    na.omit()
  
  cat("Using", nrow(cor_data), "complete cases for correlation analysis\n")
  
  if(nrow(cor_data) < 10) {
    cat("Warning: Very few complete cases for reliable correlation analysis\n")
  }
  
  # Calculate correlation matrix
  cor_matrix <- cor(cor_data, use = "complete.obs")
  
  # 2. Statistical significance testing
  cat("\n2. CORRELATION SIGNIFICANCE TESTING\n")
  
  # Function to calculate correlation p-values
  cor_test_matrix <- function(data) {
    n_vars <- ncol(data)
    var_names <- colnames(data)
    
    # Initialize matrices
    p_matrix <- matrix(NA, n_vars, n_vars)
    colnames(p_matrix) <- var_names
    rownames(p_matrix) <- var_names
    
    # Calculate p-values for each pair
    for(i in 1:n_vars) {
      for(j in 1:n_vars) {
        if(i != j) {
          test_result <- cor.test(data[,i], data[,j])
          p_matrix[i,j] <- test_result$p.value
        } else {
          p_matrix[i,j] <- 0  # Diagonal is always 1, p-value is 0
        }
      }
    }
    return(p_matrix)
  }
  
  p_matrix <- cor_test_matrix(cor_data)
  
  # 3. Create significance stars matrix
  sig_matrix <- matrix("", nrow = nrow(cor_matrix), ncol = ncol(cor_matrix))
  sig_matrix[p_matrix < 0.001] <- "***"
  sig_matrix[p_matrix >= 0.001 & p_matrix < 0.01] <- "**"
  sig_matrix[p_matrix >= 0.01 & p_matrix < 0.05] <- "*"
  sig_matrix[p_matrix >= 0.05] <- ""
  diag(sig_matrix) <- ""  # Remove stars from diagonal
  
  # 4. Print correlation summary
  cat("Correlation Matrix with Significance:\n")
  cat("* p < 0.05, ** p < 0.01, *** p < 0.001\n\n")
  
  # Create formatted correlation table
  cor_display <- round(cor_matrix, 3)
  for(i in 1:nrow(cor_display)) {
    for(j in 1:ncol(cor_display)) {
      if(i != j) {
        cor_display[i,j] <- paste0(cor_display[i,j], sig_matrix[i,j])
      }
    }
  }
  
  print(cor_display)
  
  # 5. Identify strong correlations
  cat("\n3. STRONG CORRELATIONS IDENTIFIED\n")
  
  # Find correlations above threshold (excluding diagonal)
  strong_cor_threshold <- 0.5
  moderate_cor_threshold <- 0.3
  
  strong_correlations <- which(abs(cor_matrix) > strong_cor_threshold &
                                 cor_matrix != 1, arr.ind = TRUE)
  moderate_correlations <- which(abs(cor_matrix) > moderate_cor_threshold &
                                   abs(cor_matrix) <= strong_cor_threshold &
                                   cor_matrix != 1, arr.ind = TRUE)
  
  if(nrow(strong_correlations) > 0) {
    cat("Strong correlations (|r| > 0.5):\n")
    for(i in 1:nrow(strong_correlations)) {
      row_idx <- strong_correlations[i,1]
      col_idx <- strong_correlations[i,2]
      if(row_idx < col_idx) {  # Avoid duplicates
        var1 <- rownames(cor_matrix)[row_idx]
        var2 <- colnames(cor_matrix)[col_idx]
        cor_val <- cor_matrix[row_idx, col_idx]
        p_val <- p_matrix[row_idx, col_idx]
        cat("- ", var1, " <-> ", var2, ": r = ", round(cor_val, 3),
            " (p = ", format.pval(p_val), ")\n", sep="")
      }
    }
  } else {
    cat("No strong correlations found\n")
  }
  
  if(nrow(moderate_correlations) > 0) {
    cat("\nModerate correlations (0.3 < |r| ≤ 0.5):\n")
    printed_pairs <- character(0)
    for(i in 1:nrow(moderate_correlations)) {
      row_idx <- moderate_correlations[i,1]
      col_idx <- moderate_correlations[i,2]
      if(row_idx < col_idx) {  # Avoid duplicates
        var1 <- rownames(cor_matrix)[row_idx]
        var2 <- colnames(cor_matrix)[col_idx]
        pair_id <- paste(min(var1, var2), max(var1, var2), sep="-")
        
        if(!pair_id %in% printed_pairs) {
          cor_val <- cor_matrix[row_idx, col_idx]
          p_val <- p_matrix[row_idx, col_idx]
          cat("- ", var1, " <-> ", var2, ": r = ", round(cor_val, 3),
              " (p = ", format.pval(p_val), ")\n", sep="")
          printed_pairs <- c(printed_pairs, pair_id)
        }
      }
    }
  }
  
  # 6. Focus on UG correlations
  if("UG" %in% colnames(cor_matrix)) {
    cat("\n4. UG REPRESENTATION CORRELATIONS\n")
    
    ug_correlations <- cor_matrix["UG", ]
    ug_correlations <- ug_correlations[names(ug_correlations) != "UG"]
    ug_p_values <- p_matrix["UG", names(ug_correlations)]
    
    # Sort by absolute correlation strength
    ug_cor_sorted <- sort(abs(ug_correlations), decreasing = TRUE)
    
    cat("Variables correlated with UG representation (sorted by strength):\n")
    for(var in names(ug_cor_sorted)) {
      cor_val <- ug_correlations[var]
      p_val <- ug_p_values[var]
      sig_level <- if(p_val < 0.001) "***" else if(p_val < 0.01) "**" else if(p_val < 0.05) "*" else ""
      
      interpretation <- if(abs(cor_val) > 0.5) "Strong" else if(abs(cor_val) > 0.3) "Moderate" else "Weak"
      direction <- if(cor_val > 0) "positive" else "negative"
      
      cat("- ", var, ": r = ", round(cor_val, 3), sig_level,
          " (", interpretation, " ", direction, " correlation)\n", sep="")
    }
  }
  
  return(list(
    correlation_matrix = cor_matrix,
    p_values = p_matrix,
    significance_stars = sig_matrix,
    correlation_data = cor_data,
    strong_correlations = strong_correlations,
    moderate_correlations = moderate_correlations
  ))
}

# ============================================================================
# CORRELATION VISUALIZATION FUNCTIONS
# ============================================================================

create_clean_correlation_plots <- function(cor_results) {
  
  if(is.null(cor_results)) {
    cat("No correlation results to plot\n")
    return(NULL)
  }
  
  cat("\n=== CREATING CORRELATION VISUALIZATIONS ===\n")
  
  cor_matrix <- cor_results$correlation_matrix
  p_matrix <- cor_results$p_values
  plots <- list()
  
  # =========================================================================
  # 1. CLEAN VARIABLE NAMES FUNCTION
  # =========================================================================
  
  clean_variable_names <- function(var_names) {
    clean_names <- var_names %>%
      # Shorten common patterns
      str_replace("EMPsurvEngagement", "Engagement") %>%
      str_replace("EmpSurvOrgIntegrity", "OrgIntegrity") %>%
      str_replace("EmpSurvSupervisor", "Supervisor") %>%
      str_replace("NumberFemaleTeamLeads", "FemaleLeads") %>%
      str_replace("NumberTeamLeads", "TeamLeads") %>%
      str_replace("DepartmentGroupNumber", "DeptGroup") %>%
      str_replace("PercentMale", "Male%") %>%
      str_replace("GroupSize", "Size") %>%
      str_replace("LondonorNot", "London") %>%
      # Remove numbered suffixes for cleaner look
      str_replace("_[0-9]+$", "") %>%
      str_replace("[0-9]+$", "") %>%
      # Truncate very long names
      str_trunc(12, "right", "...")
    
    return(clean_names)
  }
  
  # =========================================================================
  # 2. FOCUSED UG CORRELATION CHART
  # =========================================================================
  
  if("UG" %in% rownames(cor_matrix)) {
    cat("Creating UG correlation chart...\n")
    
    tryCatch({
      # Extract UG correlations
      ug_cors <- cor_matrix["UG", ]
      ug_cors <- ug_cors[names(ug_cors) != "UG"]
      ug_p_vals <- p_matrix["UG", names(ug_cors)]
      
      # Filter to significant correlations only
      significant_vars <- names(ug_cors)[ug_p_vals < 0.05]
      
      if(length(significant_vars) == 0) {
        cat("No significant correlations with UG found\n")
      } else {
        # Create clean data frame
        ug_cor_df <- data.frame(
          variable = significant_vars,
          correlation = as.numeric(ug_cors[significant_vars]),
          p_value = as.numeric(ug_p_vals[significant_vars]),
          stringsAsFactors = FALSE
        ) %>%
          mutate(
            clean_name = clean_variable_names(variable),
            significance = case_when(
              p_value < 0.001 ~ "***",
              p_value < 0.01 ~ "**", 
              p_value < 0.05 ~ "*",
              TRUE ~ ""
            ),
            abs_correlation = abs(correlation),
            direction = ifelse(correlation > 0, "Positive", "Negative"),
            color = ifelse(correlation > 0, "#2E8B57", "#DC143C")  # Sea green / Crimson
          ) %>%
          arrange(desc(abs_correlation)) %>%
          slice_head(n = 10)  # Top 10 only
        
        # Create clean plot
        plots$ug_correlations_clean <- ug_cor_df %>%
          ggplot(aes(x = reorder(clean_name, abs_correlation), y = correlation)) +
          geom_col(aes(fill = direction), width = 0.6, alpha = 0.8) +
          geom_text(aes(label = paste0(round(correlation, 2), significance)),
                    hjust = ifelse(ug_cor_df$correlation > 0, -0.1, 1.1),
                    size = 4, fontweight = "bold", color = "gray20") +
          scale_fill_manual(values = c("Positive" = "#2E8B57", "Negative" = "#DC143C"),
                            name = "") +
          scale_y_continuous(
            limits = c(min(ug_cor_df$correlation) * 1.2, max(ug_cor_df$correlation) * 1.2),
            labels = function(x) paste0(round(x, 2))
          ) +
          coord_flip() +
          labs(
            title = "Key Variables Correlated with UG Representation",
            subtitle = "Significant correlations only (* p<0.05, ** p<0.01, *** p<0.001)",
            x = "", y = "Correlation Coefficient",
            caption = "Only variables with p < 0.05 shown | Positive = increases with UG representation"
          ) +
          theme_minimal() +
          theme(
            plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray50"),
            plot.caption = element_text(size = 9, color = "gray50"),
            axis.text.y = element_text(size = 11, color = "gray20"),
            axis.text.x = element_text(size = 10, color = "gray40"),
            axis.title.x = element_text(size = 11, face = "bold"),
            legend.position = "bottom",
            legend.text = element_text(size = 10),
            panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_line(color = "gray90", size = 0.3),
            plot.background = element_rect(fill = "white", color = NA),
            plot.margin = margin(20, 20, 20, 20)
          )
        
        print(plots$ug_correlations_clean)
        cat("UG correlation chart created\n")
      }
      
    }, error = function(e) {
      cat("UG correlation chart failed:", e$message, "\n")
    })
  }
  
  # =========================================================================
  # 3. SIMPLIFIED CORRELATION HEATMAP (CORE VARIABLES ONLY)
  # =========================================================================
  
  cat("Creating simplified correlation heatmap...\n")
  
  tryCatch({
    # Select core variables only
    core_vars <- c("UG", "PercentMale", "GroupSize", "NumberTeamLeads",
                   "NumberFemaleTeamLeads", "EMPsurvEngagement")
    
    # Find which core variables exist in our data
    available_core <- intersect(core_vars, colnames(cor_matrix))
    
    if(length(available_core) < 3) {
      # Fallback: select variables with highest variance in correlations
      var_importance <- apply(cor_matrix, 2, function(x) var(x, na.rm = TRUE))
      available_core <- names(sort(var_importance, decreasing = TRUE))[1:min(6, ncol(cor_matrix))]
    }
    
    cat("Using core variables:", paste(available_core, collapse = ", "), "\n")
    
    # Subset matrices
    core_cor_matrix <- cor_matrix[available_core, available_core]
    core_p_matrix <- p_matrix[available_core, available_core]
    
    # Clean variable names for display
    clean_names <- clean_variable_names(available_core)
    rownames(core_cor_matrix) <- clean_names
    colnames(core_cor_matrix) <- clean_names
    
    # Prepare data for ggplot
    core_cor_long <- core_cor_matrix %>%
      as.data.frame() %>%
      rownames_to_column("var1") %>%
      pivot_longer(cols = -var1, names_to = "var2", values_to = "correlation") %>%
      mutate(
        correlation = round(correlation, 2),
        # Add significance for off-diagonal elements
        significance = case_when(
          var1 == var2 ~ "",
          abs(correlation) < 0.3 ~ "",  # Don't show weak correlations
          TRUE ~ if_else(abs(correlation) >= 0.5, "●", "○")  # Strong vs moderate
        ),
        label = if_else(var1 == var2, "",
                        paste0(correlation, "\n", significance)),
        text_color = if_else(abs(correlation) > 0.5, "white", "black")
      )
    
    # Create clean heatmap
    plots$correlation_heatmap_clean <- core_cor_long %>%
      ggplot(aes(x = var1, y = fct_rev(var2), fill = correlation)) +
      geom_tile(color = "white", size = 1) +
      geom_text(aes(label = label, color = text_color),
                size = 3.5, fontface = "bold", lineheight = 0.8) +
      scale_fill_gradient2(
        low = "#DC143C", mid = "#F8F8FF", high = "#2E8B57",
        midpoint = 0, limits = c(-1, 1),
        name = "Correlation"
      ) +
      scale_color_identity() +
      labs(
        title = "Correlation Matrix: Core Diversity Variables",
        subtitle = "● Strong correlation (|r| ≥ 0.5)  ○ Moderate correlation (0.3 ≤ |r| < 0.5)",
        x = "", y = "",
        caption = "Only correlations ≥ 0.3 marked with symbols"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray50"),
        plot.caption = element_text(size = 9, color = "gray50"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "right",
        legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(size = 9),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(20, 20, 20, 20)
      ) +
      coord_fixed()
    
    print(plots$correlation_heatmap_clean)
    cat("Correlation heatmap created\n")
    
  }, error = function(e) {
    cat("Heatmap failed:", e$message, "\n")
  })
  
  # =========================================================================
  # 4. SIMPLE CORRPLOT (PROFESSIONAL VERSION)
  # =========================================================================
  
  cat("Creating professional corrplot...\n")
  
  tryCatch({
    # Use the same core variables as heatmap
    core_vars <- c("UG", "PercentMale", "GroupSize", "NumberTeamLeads",
                   "NumberFemaleTeamLeads", "EMPsurvEngagement")
    available_core <- intersect(core_vars, colnames(cor_matrix))
    
    if(length(available_core) < 3) {
      var_importance <- apply(cor_matrix, 2, function(x) var(x, na.rm = TRUE))
      available_core <- names(sort(var_importance, decreasing = TRUE))[1:min(6, ncol(cor_matrix))]
    }
    
    core_cor_matrix <- cor_matrix[available_core, available_core]
    
    # Clean names for corrplot
    clean_names <- clean_variable_names(available_core)
    rownames(core_cor_matrix) <- clean_names
    colnames(core_cor_matrix) <- clean_names
    
    # Set up clean plotting
    par(mar = c(2, 2, 3, 2))
    
    # Create professional corrplot
    corrplot::corrplot(
      core_cor_matrix,
      method = "color",
      type = "upper",
      order = "original",  # Keep original order
      tl.cex = 1,         # Larger text
      tl.col = "black",
      tl.srt = 45,
      col = colorRampPalette(c("#DC143C", "#F8F8FF", "#2E8B57"))(100),
      addCoef.col = "black",
      number.cex = 0.8,
      diag = FALSE,
      title = "Professional Correlation Matrix",
      mar = c(0, 0, 2, 0),
      cl.cex = 0.8
    )
    
    # Reset margins
    par(mar = c(5, 4, 4, 2) + 0.1)
    
    cat("Professional corrplot created\n")
    
  }, error = function(e) {
    cat("Professional corrplot failed:", e$message, "\n")
    par(mar = c(5, 4, 4, 2) + 0.1)  # Reset margins even on error
  })
  
  cat("Correlation visualization suite complete!\n")
  return(plots)
}

cor_results <- correlation_analysis(df_complete)
clean_plots <- create_clean_correlation_plots(cor_results)

# =========================================================================
# DIAGNOSTIC ANALYSIS FOR ALL MODELS
# =========================================================================

if(length(valid_models) > 0) {
  cat("\n--- COMPREHENSIVE DIAGNOSTIC ANALYSIS ---\n")
  
  # Loop through ALL models for diagnostics
  for(model_name in names(valid_models)) {
    current_model <- valid_models[[model_name]]
    
    cat("\n=== DIAGNOSTICS FOR", toupper(model_name), "===\n")
    
    # Statistical tests
    tryCatch({
      dw <- lmtest::dwtest(current_model)
      cat("Durbin-Watson Statistic:", round(dw$statistic, 4),
          " (p-value:", format.pval(dw$p.value), ")\n")
      
      bp <- lmtest::bptest(current_model)
      cat("Breusch-Pagan Test (Heteroscedasticity):", format.pval(bp$p.value), "\n")
      
      if(length(residuals(current_model)) >= 3) {
        residuals_norm <- shapiro.test(residuals(current_model))
        cat("Shapiro-Wilk Normality Test (residuals):", format.pval(residuals_norm$p.value), "\n")
      }
      
    }, error = function(e) {
      cat("Some diagnostic tests failed for", model_name, ":", e$message, "\n")
    })
    
    # DIAGNOSTIC PLOT SECTION
    tryCatch({
      par(
        mfrow = c(2, 2),
        mar = c(4.2, 4.2, 3.2, 1.5),
        oma = c(2.5, 0.5, 4.5, 0.5),     # more space for outer titles
        mgp = c(2.5, 1, 0),
        cex.main = 1.2,
        cex.lab = 1.1,
        cex.axis = 0.9
      )
      
      # Plot without adding 'main' to avoid title duplication
      plot(current_model, which = 1)
      plot(current_model, which = 2)
      plot(current_model, which = 3)
      plot(current_model, which = 5)
      
      # Clean single-line formula
      model_formula <- paste(deparse(formula(current_model)), collapse = " ")
      
      # Add outer main title
      mtext(paste("Diagnostic Plots for Model:", toupper(model_name)),
            outer = TRUE, line = 3.5, cex = 1.5, font = 2)
      
      # Add model formula as subtitle
      mtext(model_formula,
            outer = TRUE, line = 2, cex = 1.1, font = 3)
      
      # Reset par
      par(mfrow = c(1,1))
      par(mar = c(5, 4, 4, 2) + 0.1)
      
      cat("Diagnostic plots created for", model_name, "\n")
      
    }, error = function(e) {
      cat("Diagnostic plots failed for", model_name, ":", e$message, "\n")
      par(mfrow = c(1,1))
      par(mar = c(5, 4, 4, 2) + 0.1)
    })
    
    cat("\n--- INFLUENTIAL OBSERVATIONS FOR", toupper(model_name), "---\n")
    
    tryCatch({
      cooks_d <- cooks.distance(current_model)
      threshold_cooks <- 4/length(cooks_d)
      influential_obs <- which(cooks_d > threshold_cooks)
      
      if(length(influential_obs) > 0) {
        cat("Influential observations (Cook's D >", round(threshold_cooks, 4), "):\n")
        cat("Observation numbers:", influential_obs, "\n")
        cat("Cook's distances:", round(cooks_d[influential_obs], 4), "\n")
      } else {
        cat("No highly influential observations detected\n")
      }
      
      leverage <- hatvalues(current_model)
      threshold_leverage <- 2 * length(coef(current_model)) / nrow(model_data)
      high_leverage <- which(leverage > threshold_leverage)
      
      if(length(high_leverage) > 0) {
        cat("High leverage observations (h >", round(threshold_leverage, 4), "):\n")
        cat("Observation numbers:", high_leverage, "\n")
        cat("Leverage values:", round(leverage[high_leverage], 4), "\n")
      } else {
        cat("No high leverage observations detected\n")
      }
      
      std_residuals <- rstandard(current_model)
      outliers <- which(abs(std_residuals) > 2.5)
      
      if(length(outliers) > 0) {
        cat("Potential outliers (|standardized residual| > 2.5):\n")
        cat("Observation numbers:", outliers, "\n")
        cat("Standardized residuals:", round(std_residuals[outliers], 3), "\n")
      } else {
        cat("No extreme outliers detected\n")
      }
      
    }, error = function(e) {
      cat("Outlier detection failed for", model_name, ":", e$message, "\n")
    })
    
    cat("\n" , rep("=", 60), "\n")
  }
  
  if(!is.null(best_model)) {
    cat("\n--- BEST MODEL SUMMARY:", toupper(best_model_name), "---\n")
    
    coef_summary <- summary(best_model)$coefficients
    cat("Coefficient Summary:\n")
    print(round(coef_summary, 4))
    
    cat("\nModel Fit Statistics:\n")
    cat("R-squared:", round(summary(best_model)$r.squared, 4), "\n")
    cat("Adjusted R-squared:", round(summary(best_model)$adj.r.squared, 4), "\n")
    cat("Residual standard error:", round(summary(best_model)$sigma, 4), "\n")
    cat("F-statistic:", round(summary(best_model)$fstatistic[1], 2), "\n")
    cat("p-value:", format.pval(pf(summary(best_model)$fstatistic[1],
                                   summary(best_model)$fstatistic[2],
                                   summary(best_model)$fstatistic[3],
                                   lower.tail=FALSE)), "\n")
  }
}

# =========================================================================
# FUNCTION TO CREATE DIAGNOSTIC REPORT
# =========================================================================

create_diagnostic_report <- function(valid_models, model_data) {
  
  diagnostic_summary <- data.frame(
    Model = character(),
    DW_Statistic = numeric(),
    DW_pvalue = numeric(),
    BP_pvalue = numeric(),
    SW_pvalue = numeric(),
    Cook_Outliers = integer(),
    High_Leverage = integer(),
    Std_Outliers = integer(),
    stringsAsFactors = FALSE
  )
  
  for(model_name in names(valid_models)) {
    current_model <- valid_models[[model_name]]
    
    # Initialize row
    row_data <- list(Model = model_name)
    
    # Durbin-Watson test
    tryCatch({
      dw <- lmtest::dwtest(current_model)
      row_data$DW_Statistic <- round(dw$statistic, 4)
      row_data$DW_pvalue <- round(dw$p.value, 4)
    }, error = function(e) {
      row_data$DW_Statistic <- NA
      row_data$DW_pvalue <- NA
    })
    
    # Breusch-Pagan test
    tryCatch({
      bp <- lmtest::bptest(current_model)
      row_data$BP_pvalue <- round(bp$p.value, 4)
    }, error = function(e) {
      row_data$BP_pvalue <- NA
    })
    
    # Shapiro-Wilk test
    tryCatch({
      if(length(residuals(current_model)) >= 3) {
        sw <- shapiro.test(residuals(current_model))
        row_data$SW_pvalue <- round(sw$p.value, 4)
      } else {
        row_data$SW_pvalue <- NA
      }
    }, error = function(e) {
      row_data$SW_pvalue <- NA
    })
    
    # Outlier counts
    tryCatch({
      # Cook's distance outliers
      cooks_d <- cooks.distance(current_model)
      row_data$Cook_Outliers <- sum(cooks_d > 4/length(cooks_d), na.rm = TRUE)
      
      # High leverage points
      leverage <- hatvalues(current_model)
      threshold_leverage <- 2 * length(coef(current_model)) / nrow(model_data)
      row_data$High_Leverage <- sum(leverage > threshold_leverage, na.rm = TRUE)
      
      # Standardized residual outliers
      std_residuals <- rstandard(current_model)
      row_data$Std_Outliers <- sum(abs(std_residuals) > 2.5, na.rm = TRUE)
      
    }, error = function(e) {
      row_data$Cook_Outliers <- NA
      row_data$High_Leverage <- NA
      row_data$Std_Outliers <- NA
    })
    
    # Add row to summary
    diagnostic_summary <- rbind(diagnostic_summary, as.data.frame(row_data))
  }
  
  cat("\n--- DIAGNOSTIC SUMMARY TABLE ---\n")
  print(knitr::kable(diagnostic_summary,
                     caption = "Comprehensive Diagnostic Summary for All Models",
                     col.names = c("Model", "DW Stat", "DW p-val", "BP p-val",
                                   "SW p-val", "Cook's", "Hi-Lev", "Std Out")))
  
  return(diagnostic_summary)
}

# Create diagnostic summary table
diagnostic_summary <- create_diagnostic_report(valid_models, model_data)

return(list(
  complete_data = df_complete,
  original_data = df,
  models = valid_models,
  model = best_model,  # Add this for backward compatibility
  best_model = best_model,
  correlation_results = cor_results,
  correlation_plots = clean_plots,
  best_model_name = best_model_name,
  model_summaries = model_summaries,
  diagnostic_summary = diagnostic_summary,
  effect_sizes = list(ug = ug_effect, gender = gender_effect),
  sample_size = nrow(df_complete),
  diagnostic_warnings = list(
    autocorrelation = if(!is.null(best_model)) {
      tryCatch({
        dw <- lmtest::dwtest(best_model)
        if(dw$statistic < 1.5 || dw$statistic > 2.5) "WARNING: Autocorrelation detected" else "OK"
      }, error = function(e) "Could not test")
    } else "No model",
    heteroscedasticity = if(!is.null(best_model)) {
      tryCatch({
        bp <- lmtest::bptest(best_model)
        if(bp$p.value < 0.05) "WARNING: Heteroscedasticity detected" else "OK"
      }, error = function(e) "Could not test")
    } else "No model",
    normality = if(!is.null(best_model)) {
      tryCatch({
        sw <- shapiro.test(residuals(best_model))
        if(sw$p.value < 0.05) "WARNING: Non-normal residuals" else "OK"
      }, error = function(e) "Could not test")
    } else "No model"
  )
))
}

# ============================================================================
# ADVANCED ANALYSIS
# ============================================================================

advanced_analysis <- function(results) {
  
  cat("\n\n=== PHASE 2: ADVANCED INSIGHTS ===\n\n")
  
  if(is.null(results) || is.null(results$complete_data)) {
    cat("Error: No data available for advanced analysis\n")
    return(NULL)
  }
  
  df <- results$complete_data
  
  # 2.1 Risk Scoring
  cat("2.1 FAIR RISK ASSESSMENT (DATA-DRIVEN)\n")
  
  df <- df %>%
    # Compute function-specific and org-wide medians
    group_by(Function) %>%
    mutate(
      function_ug_median = median(UG, na.rm = TRUE),
      engagement_median = median(EMPsurvEngagement, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    
    # Calculate female leadership ratio safely
    mutate(
      female_leadership_ratio = case_when(
        "NumberTeamLeads" %in% colnames(.) & "NumberFemaleTeamLeads" %in% colnames(.) ~
          NumberFemaleTeamLeads / pmax(NumberTeamLeads, 1, na.rm = TRUE),
        TRUE ~ 0
      ),
      
      # Function-relative gap penalty (capped at 2)
      function_gap_penalty = pmax(0, (function_ug_median - UG) / 1.5),
      function_gap_penalty = pmin(function_gap_penalty, 2),
      
      # Benchmark risk based on org-wide quartiles
      benchmark_risk = case_when(
        UG < quantile(UG, 0.25, na.rm = TRUE) ~ 2,
        UG < quantile(UG, 0.5, na.rm = TRUE) ~ 1,
        TRUE ~ 0
      ),
      
      # Leadership risk
      leadership_risk = case_when(
        female_leadership_ratio == 0 ~ 2,
        female_leadership_ratio < 0.25 ~ 1,
        TRUE ~ 0
      ),
      
      # Engagement buffer (bonus for high engagement)
      engagement_buffer = ifelse(
        EMPsurvEngagement > engagement_median, -0.4, 0
      )
    )
  
  # Stagnation risk using function-level UG quartiles
  assign_stagnation_risk <- function(df_group) {
    df_group <- df_group %>%
      arrange(UG) %>%
      mutate(rank = row_number(),
             quartile = ntile(rank, 4),
             stagnation_risk = case_when(
               quartile == 1 ~ 2,
               quartile == 2 ~ 1,
               TRUE ~ 0
             ))
    return(df_group)
  }
  
  df <- df %>%
    group_by(Function) %>%
    group_modify(~ assign_stagnation_risk(.x)) %>%
    ungroup() %>%
    
    # Final weighted risk score and categories
    mutate(
      risk_score = round(
        function_gap_penalty * 0.3 +
          benchmark_risk * 0.25 +
          stagnation_risk * 0.2 +
          leadership_risk * 0.25 +
          engagement_buffer, 2
      ),
      risk_category = case_when(
        risk_score >= 1.9 ~ "Critical Risk",
        risk_score >= 1.2 ~ "High Risk",
        risk_score >= 0.5 ~ "Medium Risk",
        TRUE ~ "Low Risk"
      )
    )
  
  # Risk summary
  risk_summary <- df %>%
    group_by(risk_category) %>%
    summarise(
      team_count = n(),
      avg_ug = round(mean(UG, na.rm = TRUE), 1),
      low_diversity_rate = round(mean(UG < 10, na.rm = TRUE) * 100, 1),
      pct_sales_teams = round(mean(Function == 1, na.rm = TRUE) * 100, 1),
      .groups = 'drop'
    ) %>%
    arrange(desc(team_count))
  
  cat("Fair Risk Assessment Summary:\n")
  print(knitr::kable(risk_summary))
  
  # 2.2 Performance Analysis
  cat("\n2.2 DIVERSITY-PERFORMANCE RELATIONSHIP\n")
  
  if("EMPsurvEngagement" %in% colnames(df)) {
    perf_correlation <- cor(df$UG, df$EMPsurvEngagement, use="complete.obs")
    cat("UG-Engagement Correlation:", round(perf_correlation, 3), "\n")
    
    # Diversity quintiles
    valid_ug <- df$UG[!is.na(df$UG)]
    if(length(valid_ug) > 5) {
      df$ug_quintile <- ntile(df$UG, 5)
      
      quintile_analysis <- df %>%
        filter(!is.na(ug_quintile)) %>%
        group_by(ug_quintile) %>%
        summarise(
          team_count = n(),
          avg_ug = round(mean(UG, na.rm=T), 1),
          avg_engagement = round(mean(EMPsurvEngagement, na.rm=T), 1),
          .groups = 'drop'
        )
      
      cat("Performance by Diversity Quintiles:\n")
      print(knitr::kable(quintile_analysis))
    }
  }
  
  # 2.3 Intervention Simulation
  cat("\n2.3 INTERVENTION IMPACT PROJECTION\n")
  
  if("Function" %in% colnames(df)) {
    # Convert Function to character for safer comparison
    df$Function_char <- as.character(df$Function)
    
    current_sales_ug <- mean(df$UG[df$Function_char == "1"], na.rm=T)
    current_prof_ug <- mean(df$UG[df$Function_char == "2"], na.rm=T)
    gap <- current_prof_ug - current_sales_ug
    
    scenarios <- data.frame(
      scenario = c("Current State", "5% UG Increase", "Close 50% Gap"),
      sales_ug = c(current_sales_ug,
                   current_sales_ug + 5,
                   current_sales_ug + gap/2),
      gap_reduction = c(0,
                        pmin(5, gap),
                        gap/2),
      teams_affected = c(0,
                         sum(df$Function_char == "1", na.rm=T),
                         sum(df$Function_char == "1", na.rm=T))
    )
    
    cat("Intervention Scenarios:\n")
    print(knitr::kable(scenarios, digits = 1))
  }
  
  return(list(
    enhanced_data = df,
    risk_summary = risk_summary,
    scenarios = if(exists("scenarios")) scenarios else NULL
  ))
  
  
  
  
}

# ============================================================================
# VISUALIZATION SUITE
# ============================================================================

create_visualizations <- function(results, advanced_results) {
  
  if(is.null(results) || is.null(advanced_results)) {
    cat("Cannot create visualizations - missing data\n")
    return(NULL)
  }
  
  df <- advanced_results$enhanced_data
  plots <- list()
  
  cat("Creating professional visualizations...\n")
  
  # 1. EXECUTIVE SUMMARY: The Gap That Matters
  if("Function" %in% colnames(df) && length(unique(df$Function)) >= 2) {
    
    # Convert Function to character for consistent handling
    df$Function_char <- as.character(df$Function)
    
    summary_stats <- df %>%
      group_by(Function_char) %>%
      summarise(
        mean_ug = mean(UG, na.rm=T),
        se_ug = sd(UG, na.rm=T)/sqrt(sum(!is.na(UG))),
        n = sum(!is.na(UG)),
        .groups = 'drop'
      ) %>%
      mutate(
        Function_Label = case_when(
          Function_char == "1" ~ "Sales Teams",
          Function_char == "2" ~ "Professional Service Teams",
          TRUE ~ paste("Function", Function_char)
        )
      )
    
    gap_value <- round(summary_stats$mean_ug[summary_stats$Function_char == "2"] -
                         summary_stats$mean_ug[summary_stats$Function_char == "1"], 1)
    
    plots$executive_gap <- summary_stats %>%
      ggplot(aes(x=reorder(Function_Label, -mean_ug), y=mean_ug, fill=Function_Label)) +
      geom_col(width=0.7, alpha=0.9) +
      geom_errorbar(aes(ymin=pmax(0, mean_ug-se_ug), ymax=mean_ug+se_ug),
                    width=0.2, linewidth=1, color="gray30") +
      geom_text(aes(label=paste0(round(mean_ug, 1), "%")),
                vjust=-1.5, fontface="bold", size=6, color="gray20") +
      geom_text(aes(label=paste0("n=", n)),
                vjust=3, color="white", fontface="bold", size=4) +
      scale_fill_manual(values=c("Sales Teams"="#C0392B",
                                 "Professional Service Teams"="#2980B9")) +
      scale_y_continuous(labels = function(x) paste0(x, "%"),
                         expand = expansion(mult = c(0, 0.15))) +
      labs(
        title=paste0("DIVERSITY GAP: Sales Teams Underperform by ", abs(gap_value), " Percentage Points"),
        subtitle=paste0("UG representation gap of ", gap_value, "% represents missed talent opportunities"),
        x="", y="Average UG Representation (%)",
        caption="Error bars show standard error of the mean"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size=16, face="bold", hjust=0.5, color="gray20"),
        plot.subtitle = element_text(size=13, hjust=0.5, color="gray40", margin=margin(b=20)),
        axis.text.x = element_text(size=12, face="bold"),
        axis.text.y = element_text(size=11),
        axis.title.y = element_text(size=12, face="bold"),
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill="white", color=NA),
        plot.margin = margin(20,20,20,20)
      )
    
    print(plots$executive_gap)
    cat("Created executive gap visualization\n")
  }
  
  # 2. Business Case with Engagement
  if("EMPsurvEngagement" %in% colnames(df) && "UG" %in% colnames(df)) {
    
    # Create diversity quartiles for cleaner visualization
    valid_indices <- !is.na(df$UG) & !is.na(df$EMPsurvEngagement)
    
    if(sum(valid_indices) > 20) {  # Ensure enough data
      df_valid <- df[valid_indices, ]
      df_valid$diversity_quartile <- ntile(df_valid$UG, 4)
      quartile_labels <- c("Bottom 25%\n(Low Diversity)", "25-50%", "50-75%", "Top 25%\n(High Diversity)")
      
      performance_summary <- df_valid %>%
        group_by(diversity_quartile) %>%
        summarise(
          avg_engagement = mean(EMPsurvEngagement, na.rm=T),
          n_teams = n(),
          diversity_range = paste0(round(min(UG, na.rm=T), 1), "-", round(max(UG, na.rm=T), 1), "%"),
          .groups = 'drop'
        ) %>%
        mutate(
          quartile_label = quartile_labels[diversity_quartile],
          engagement_gain = avg_engagement - min(avg_engagement)
        )
      
      max_gain <- round(max(performance_summary$engagement_gain), 1)
      
      plots$business_case <- performance_summary %>%
        ggplot(aes(x=diversity_quartile, y=avg_engagement)) +
        geom_line(linewidth=2, color="#2980B9", alpha=0.8) +
        geom_point(size=6, color="#2980B9", alpha=0.9) +
        geom_text(aes(label=paste0(round(avg_engagement, 1), " pts")),
                  vjust=-1.5, fontface="bold", size=4) +
        geom_text(aes(label=paste0("n=", n_teams)),
                  vjust=2.5, color="gray50", size=3) +
        scale_x_continuous(breaks=1:4, labels=quartile_labels) +
        scale_y_continuous(limits=c(min(performance_summary$avg_engagement)-2,
                                    max(performance_summary$avg_engagement)+3)) +
        labs(
          title=paste0("BUSINESS IMPACT: High-Diversity Teams Score ", max_gain, " Points Higher on Engagement"),
          subtitle="Teams in top diversity quartile consistently outperform on key performance metrics",
          x="Team Diversity Level", y="Average Engagement Score",
          caption="Higher engagement correlates with productivity, retention, and customer satisfaction"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size=16, face="bold", hjust=0.5, color="gray20"),
          plot.subtitle = element_text(size=12, hjust=0.5, color="gray40", margin=margin(b=20)),
          axis.text.x = element_text(size=10, angle=0, hjust=0.5),
          axis.text.y = element_text(size=11),
          axis.title = element_text(size=12, face="bold"),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill="white", color=NA)
        )
      
      print(plots$business_case)
      cat("Created business case visualization\n")
    }
  }
  
  # 3. Risk Landscape (Fair, Function-Aware)
  if("risk_category" %in% colnames(df) && "Function" %in% colnames(df)) {
    
    # Label functions
    df <- df %>%
      mutate(Function_Label = case_when(
        as.character(Function) == "1" ~ "Sales",
        as.character(Function) == "2" ~ "Prof Service",
        TRUE ~ paste("Function", Function)
      ))
    
    # Prepare data for stacked bar chart
    risk_data <- df %>%
      count(Function_Label, risk_category) %>%
      group_by(Function_Label) %>%
      mutate(
        percentage = round(n / sum(n) * 100, 1),
        total_teams = sum(n)
      ) %>%
      ungroup() %>%
      filter(risk_category %in% c("Critical Risk", "High Risk", "Medium Risk"))
    
    # Compute total at-risk teams per function
    total_at_risk <- risk_data %>%
      group_by(Function_Label) %>%
      summarise(at_risk_teams = sum(n), .groups = "drop")
    
    # (Optional) Average risk score annotation
    avg_scores <- df %>%
      group_by(Function_Label) %>%
      summarise(avg_score = round(mean(risk_score, na.rm = TRUE), 2), .groups = "drop")
    
    # Final plot
    plots$risk_landscape <- risk_data %>%
      ggplot(aes(x = Function_Label, y = n, fill = risk_category)) +
      geom_col(position = "stack", width = 0.6, alpha = 0.9) +
      geom_text(data = total_at_risk,
                aes(x = Function_Label, y = at_risk_teams,
                    label = paste0(at_risk_teams, "\nteams\nat risk")),
                vjust = -0.1, fontface = "bold", size = 3, inherit.aes = FALSE) +
      geom_text(data = avg_scores,
                aes(x = Function_Label, y = 0,
                    label = paste0("Avg Score: ", avg_score)),
                vjust = -2.2, fontface = "italic", size = 3.5, inherit.aes = FALSE) +
      scale_fill_manual(
        values = c("Critical Risk" = "#E74C3C", "High Risk" = "#F39C12", "Medium Risk" = "#F7DC6F"),
        name = "Risk Level"
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(
        title = "RISK LANDSCAPE: Fair, Function-Aware Risk Distribution",
        subtitle = "Teams face different risk drivers based on internal benchmarks — not just function labels",
        x = "", y = "Number of Teams",
        caption = "Risk assessment is based on diversity, leadership, stagnation, and engagement data"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.1, color = "gray20"),
        plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40", margin = margin(b = 20)),
        axis.text = element_text(size = 11),
        axis.title.y = element_text(size = 12, face = "bold"),
        legend.position = "bottom",
        legend.title = element_text(face = "bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", color = NA)
      )
    
    print(plots$risk_landscape)
    cat("Created fair, function-aware risk landscape visualization\n")
  }
  

  
  # 4. Size Analysis
  if("GroupSize" %in% colnames(df) && "Function" %in% colnames(df)) {
    
    # Create size categories with proper handling
    df$size_category <- cut(df$GroupSize,
                            breaks=c(0, 20, 40, 60, Inf),
                            labels=c("Small\n(≤20)", "Medium\n(21-40)",
                                     "Large\n(41-60)", "Very Large\n(>60)"),
                            include.lowest = TRUE)
    
    size_analysis <- df %>%
      filter(!is.na(size_category), !is.na(Function), !is.na(UG)) %>%
      mutate(Function_char = as.character(Function)) %>%
      group_by(size_category, Function_char) %>%
      summarise(
        avg_ug = mean(UG, na.rm=T),
        team_count = n(),
        .groups = 'drop'
      ) %>%
      mutate(
        Function_Label = case_when(
          Function_char == "1" ~ "Sales",
          Function_char == "2" ~ "Professional Service",
          TRUE ~ paste("Function", Function_char)
        )
      )
    
    if(nrow(size_analysis) > 0) {
      plots$action_plan <- size_analysis %>%
        ggplot(aes(x=size_category, y=avg_ug, fill=Function_Label)) +
        geom_col(position="dodge", width=0.7, alpha=0.9) +
        geom_text(aes(label=paste0(round(avg_ug, 1), "%")),
                  position=position_dodge(width=0.7), vjust=-0.5,
                  fontface="bold", size=3.5) +
        scale_fill_manual(values=c("Sales"="#C0392B", "Professional Service"="#2980B9")) +
        scale_y_continuous(labels = function(x) paste0(x, "%"),
                           expand = expansion(mult = c(0, 0.12))) +
        labs(
          title="SCALING CHALLENGE: Diversity Gaps Persist Across All Team Sizes",
          subtitle="Interventions needed for teams of every size, with focus on larger Sales teams",
          x="Team Size Category", y="Average UG Representation (%)",
          fill="Function"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size=16, face="bold", hjust=0.5, color="gray20"),
          plot.subtitle = element_text(size=12, hjust=0.5, color="gray40", margin=margin(b=20)),
          axis.text = element_text(size=11),
          axis.title = element_text(size=12, face="bold"),
          legend.position = "bottom",
          legend.title = element_text(face="bold"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill="white", color=NA)
        )
      
      print(plots$action_plan)
      cat("Created action plan visualization\n")
    }
  }
  
  cat("Professional visualization suite complete! Created", length(plots), "strategic plots.\n")
  
  return(plots)
}

# ============================================================================
# SAVE PLOTS FUNCTION
# ============================================================================

save_plots <- function(plots, output_dir = "diversity_plots") {
  
  if(is.null(plots) || length(plots) == 0) {
    cat("No plots to save\n")
    return(NULL)
  }
  
  # Create output directory
  if(!dir.exists(output_dir)) {
    dir.create(output_dir)
    cat("Created directory:", output_dir, "\n")
  }
  
  # Save each plot
  for(plot_name in names(plots)) {
    filename <- file.path(output_dir, paste0(plot_name, ".png"))
    
    tryCatch({
      ggsave(filename, plots[[plot_name]],
             width=12, height=8, dpi=300, bg="white")
      cat("Saved:", filename, "\n")
    }, error = function(e) {
      cat("Failed to save", plot_name, ":", e$message, "\n")
    })
  }
  
  cat("Plot saving complete. Check", output_dir, "directory.\n")
}

# ============================================================================
# EXECUTIVE SUMMARY
# ============================================================================

generate_executive_summary <- function(results, advanced_results) {
  
  cat("\n\n=== EXECUTIVE SUMMARY ===\n\n")
  
  if(is.null(results)) {
    cat("Analysis failed - check data quality\n")
    return(NULL)
  }
  
  df <- results$complete_data
  
  # Key metrics
  total_teams <- nrow(df)
  
  ug_gap <- if("Function" %in% colnames(df)) {
    df_func <- df %>% mutate(Function_char = as.character(Function))
    sales_ug <- mean(df_func$UG[df_func$Function_char == "1"], na.rm=T)
    prof_ug <- mean(df_func$UG[df_func$Function_char == "2"], na.rm=T)
    if(!is.na(sales_ug) && !is.na(prof_ug)) {
      prof_ug - sales_ug
    } else { NA }
  } else { NA }
  
  effect_size <- if(!is.null(results$effect_sizes$ug) &&
                    !is.null(results$effect_sizes$ug$Cohens_d)) {
    results$effect_sizes$ug$Cohens_d
  } else { NA }
  
  critical_teams <- if(!is.null(advanced_results) &&
                       "risk_category" %in% colnames(advanced_results$enhanced_data)) {
    sum(advanced_results$enhanced_data$risk_category == "Critical Risk", na.rm=T)
  } else { 0 }
  
  cat("KEY FINDINGS:\n")
  cat("- Teams analyzed:", total_teams, "\n")
  
  if(!is.na(ug_gap)) {
    cat("- UG representation gap:", round(ug_gap, 2), "percentage points\n")
  } else {
    cat("- UG representation gap: Unable to calculate\n")
  }
  
  if(!is.na(effect_size)) {
    effect_magnitude <- case_when(
      abs(effect_size) < 0.2 ~ "Small",
      abs(effect_size) < 0.5 ~ "Small-Medium", 
      abs(effect_size) < 0.8 ~ "Medium-Large",
      TRUE ~ "Large"
    )
    cat("- Effect size:", round(effect_size, 3),
        "(", effect_magnitude, ")\n")
  } else {
    cat("- Effect size: Unable to calculate\n")
  }
  
  cat("- Critical risk teams:", critical_teams, "\n")
  
  cat("\nRECOMMENDATIONS:\n")
  cat("1. Address critical risk teams immediately\n")
  cat("2. Implement targeted diversity initiatives\n") 
  cat("3. Establish regular monitoring\n")
  
  return(list(
    total_teams = total_teams,
    ug_gap = ug_gap,
    effect_size = effect_size,
    critical_teams = critical_teams
  ))
}

# ============================================================================
# MAIN EXECUTION FUNCTION
# ============================================================================

run_diversity_analysis <- function(file_path = "DiversityGroup_new.txt") {
  
  cat("DIVERSITY ANALYTICS FRAMEWORK\n")
  cat("=============================\n\n")
  
  # Step 1: Load data
  cat("Step 1: Loading data...\n")
  df <- load_diversity_data(file_path)
  
  if(is.null(df)) {
    cat("Failed to load data. Please check:\n")
    cat("1. File exists in working directory:", getwd(), "\n")
    cat("2. File name is correct\n")
    cat("3. File format is readable\n")
    return(NULL)
  }
  
  # Step 2: Clean and standardize
  cat("\nStep 2: Cleaning and standardizing data...\n")
  df_clean <- clean_and_standardize(df)
  
  if(is.null(df_clean) || nrow(df_clean) == 0) {
    cat("Data cleaning failed\n")
    return(NULL)
  }
  
  # Step 3: Foundational analysis
  cat("\nStep 3: Running foundational analysis...\n")
  foundational_results <- foundational_analysis(df_clean)
  
  if(is.null(foundational_results)) {
    cat("Foundational analysis failed\n")
    return(NULL)
  }
  
  # Step 4: Advanced analysis
  cat("\nStep 4: Running advanced analysis...\n")
  advanced_results <- advanced_analysis(foundational_results)
  
  # Step 5: Visualizations
  cat("\nStep 5: Creating visualizations...\n")
  plots <- create_visualizations(foundational_results, advanced_results)
  
  # Step 6: Executive summary
  cat("\nStep 6: Generating executive summary...\n")
  executive_summary <- generate_executive_summary(foundational_results, advanced_results)
  
  # Step 7: Save plots (optional)
  if(!is.null(plots) && length(plots) > 0) {
    cat("\nStep 7: Saving plots...\n")
    save_plots(plots)
  }
  
  cat("\nANALYSIS COMPLETE\n")
  
  return(list(
    data = foundational_results$complete_data,
    foundational = foundational_results,
    advanced = advanced_results,
    plots = plots,
    summary = executive_summary
  ))
}

# ============================================================================
# EXECUTE ANALYSIS
# ============================================================================

# Run the complete analysis
cat("Starting Diversity Analytics...\n")
cat("Make sure 'DiversityGroup_new.txt' is in your working directory\n")
cat("Current directory:", getwd(), "\n\n")

# Execute with error handling
tryCatch({
  results <- run_diversity_analysis()
  
  # Quick check
  if(!is.null(results)) {
    cat("\n=== QUICK RESULTS SUMMARY ===\n")
    print(results$summary)
    
    # Additional diagnostic info
    if(!is.null(results$foundational$models) && length(results$foundational$models) > 0) {
      cat("\nRegression models successfully fitted:", length(results$foundational$models), "models\n")
      cat("Best model:", results$foundational$best_model_name, "\n")
      cat("Best model R-squared:", round(summary(results$foundational$best_model)$adj.r.squared, 3), "\n")
    } else {
      cat("\nRegression model failed - check data quality\n")
    }
    
    if(!is.null(results$plots) && length(results$plots) > 0) {
      cat("Created", length(results$plots), "visualizations\n")
      cat("Plots saved to 'diversity_plots' directory\n")
    }
    
  } else {
    cat("\n=== ANALYSIS FAILED ===\n")
    cat("Check the troubleshooting steps below.\n")
  }
  
}, error = function(e) {
  cat("\n=== ERROR IN MAIN EXECUTION ===\n")
  cat("Error message:", e$message, "\n")
  cat("Check data file and column names.\n")
})

# Troubleshooting section
cat("\n=== TROUBLESHOOTING GUIDE ===\n")
cat("If analysis failed:\n")
cat("1. Check file location: getwd()\n")
cat("2. List files: list.files()\n") 
cat("3. Try: run_diversity_analysis('your_actual_filename.txt')\n")
cat("4. Check data format: head(read.csv('your_file.txt', sep='\\t'))\n")

