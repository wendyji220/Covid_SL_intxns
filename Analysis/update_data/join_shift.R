source("Analysis/update_data/util.R")


################ Define Global Variables ##################

r_libdir <- Sys.getenv("R_LIBDIR")
.libPaths(r_libdir)

SCALE <- FALSE

data_original <- read.csv(PROCESSED_DATA_PATH("cleaned_covid_data_final_Oct_22_21.csv"), check.names = FALSE)
data_original <- data_original[,-1] # remove the column with empty name
ML_pipeline_results <- readRDS(PROCESSED_DATA_PATH("ML_pipeline_6_outcomes_noscale_Dec9_sm.RDS"))

## read in data dictionary for identifying subgroups of top variables to isolate the different control conditions
Data_Dictionary <- read_excel(PROCESSED_DATA_PATH("Data_Dictionary.xlsx"))
Data_Dictionary_Used <- Data_Dictionary %>%
  filter(Keep == "Yes") %>%
  select(`Variable Name`, `Sub-Category`, `Nice Label`)

## remove from the list covariates that had too many NAs and were then dropped before analysis, FIPS, and the outcome data:
vars_rmv_na <- read.csv(PROCESSED_DATA_PATH("vars_removed_na_thresh_combined.csv"))
vars_rmv_na <- as.vector(vars_rmv_na$x)

# 6 types of outcomes
initial_outcomes <- c(
  "CountyRelativeDay100Cases",
  "TotalCasesUpToDate",
  "CountyRelativeDay100Deaths",
  "TotalDeathsUpToDate",
  "Deathsat1year",
  "Casesat1year"
)

removing <- c(
  # vars_rmv_na,
  initial_outcomes
)

Data_Dictionary_Used <- Data_Dictionary_Used[-match(removing, Data_Dictionary_Used$`Variable Name`), ]
variable_list <- Data_Dictionary_Used$`Variable Name`
subcategory_list <- Data_Dictionary_Used$`Sub-Category`


## create the per-capita outcomes
data_original<- data_original %>%
  mutate(across(contains("RelativeDay") | contains("UpToDate") | contains("at1year"), 
                .fns = list(PopScale = function(x) x/data_original$Population),
                .names = "{col}_{fn}" ))

# test perc reduced
percents <- seq(0, 0.9, by = 0.1)

pop_scales <- names(data_original)[grepl("PopScale", names(data_original))]

all_outcomes <- c(
  initial_outcomes,
  pop_scales
)


target_outcomes <- c("FirstCaseDay", pop_scales)

## set up covar features
covars <- colnames(data_original)[-which(names(data_original) %in% c(
  all_outcomes,
  "X1",
  "FIPS",
  "FIPS.1",
  "county_names",
  "countyFIPS"
))]

## doing this outside the map function so it doesn't scale everytime, just need to do it once
# # TODO: data_original_scaled not used in the following
# if (SCALE) {
#   data_original_scaled <- data_original %>%
#     mutate_at(covars, ~ (scale(.)))
# }


get_top_variables <- function(ML_results) {
  varimp_data <- ML_results$var_imp
  highest_rr_idx <- which.max(varimp_data$joint_results$diff) # TODO: use diff from joint_results
  top_var <- varimp_data$joint_results$`Variable Combo`[highest_rr_idx]
  return(top_var)
}

top_vars <- get_top_variables(ML_pipeline_results)


###############################################
##### TOP VARS & CAT FOR EACH OUTCOME #########
###############################################

## get the top variables, their subcategories, and accompanying variables in same category for marginal predictions
top_vars <- strsplit(top_vars, " & ")

# convert them from nice label to variable label
top_vars <- variable_list[match(top_vars[[1]], Data_Dictionary_Used$`Nice Label`)]
top_var_subgroups <- subcategory_list[match(top_vars, variable_list)]
top_var_subcat_vars <- purrr::map(.x = top_var_subgroups, ~ variable_list[subcategory_list %in% .x])


# ################# DAY FIRST ###################
# top_vars_dayfirst <- c("ALWAYS", "co", "pm25", "so2")
# ## get the top variables, their subcategories, and accompanying variables in same category for marginal predictions
# top_var_subgroups_dayfirst <- subcategory_list[match(top_vars_dayfirst, variable_list)]
# top_var_subcat_vars_dayfirst <- purrr::map(.x = top_var_subgroups_dayfirst, ~ variable_list[subcategory_list %in% .x])
# 
# ################# CASES 25 ###################
# top_vars_cases25 <- c("NWWI", "so2", "EPL_LIMENG", "prev_2017_all_ages_Hypertension")
# ## get the top variables, their subcategories, and accompanying variables in same category for marginal predictions
# top_var_subgroups_cases25 <- subcategory_list[match(top_vars_cases25, variable_list)]
# top_var_subcat_vars_cases25 <- purrr::map(.x = top_var_subgroups_cases25, ~ variable_list[subcategory_list %in% .x])
# 
# ################# CASES TOTAL ###################
# top_vars_casestotal <- c("NWWI", "pm10", "EPL_LIMENG", "EPL_AGE65")
# ## get the top variables, their subcategories, and accompanying variables in same category for marginal predictions
# top_var_subgroups_totalcases <- subcategory_list[match(top_vars_casestotal, variable_list)]
# top_var_subcat_vars_totalcases <- purrr::map(.x = top_var_subgroups_totalcases, ~ variable_list[subcategory_list %in% .x])
# 
# ################# DEATH 100 ###################
# top_vars_death100 <- c("ALWAYS", "no2", "pct_black_only_2018", "Income.inequality.raw.value")
# ## get the top variables, their subcategories, and accompanying variables in same category for marginal predictions
# top_var_subgroups_death100 <- subcategory_list[match(top_vars_death100, variable_list)]
# top_var_subcat_vars_death100 <- purrr::map(.x = top_var_subgroups_death100, ~ variable_list[subcategory_list %in% .x])
# 
# ################# TOTAL DEATH ################### 
# top_vars_totaldeaths <- c("NWWI", "pct_black_only_2018", "pct_female_2018", "prev_2017_all_ages_Hypertension")
# ## get the top variables, their subcategories, and accompanying variables in same category for marginal predictions
# top_var_subgroups_totaldeaths <- subcategory_list[match(top_vars_totaldeaths, variable_list)]
# top_var_subcat_vars_totaldeaths <- purrr::map(.x = top_var_subgroups_totaldeaths, ~ variable_list[subcategory_list %in% .x])
# 

make_boot_dfs <- function(top_var, percents) {
  df <- as.data.frame(matrix(nrow = length(percents), ncol = 4))
  colnames(df) <- c(
    top_var,
    "Boot Pred",
    "Boot Low",
    "Boot High"
  )
  return(df)
}

boot_varnames <- paste0("boot_dfs_", c("sl_full", "sl_no_subcat", "univar_gam"))

for(i in seq_along(boot_varnames)) {
  assign(boot_varnames[i], purrr::map(
    .x = top_vars[[1]],
    .f = make_boot_dfs, percents = percents))
}


## set up bootstrap CI function
bootstrapCI <- function(target_variable,
                        data_original,
                        ML_pipeline_result,
                        covars,
                        outcome,
                        perc,
                        sub_cat_vars,
                        target_num) {
  
  sl <- ML_pipeline_result$fit
  
  nr <- nrow(data_original)
  data_tmp <- data_original
  resampled_data <- data_tmp[sample(1:nr, size = nr, replace = TRUE), ]
  
  # two tasks made for two covariates
  covariates <- list(covars, c(covars[covars %notin% sub_cat_vars], target_variable))
  tasks <- lapply(covariates, make_sl3_Task, data = resampled_data, 
                                      outcome = outcome, 
                                      folds = origami::make_folds(resampled_data, fold_fun = folds_vfold, V = 2)
                                      )
  resampled_data_task <- tasks[1]
  resampled_data_task_no_subcat_covars <- tasks[2]
  
  
  ## train a univariate gam on the resampled data
  if (target_num == 1) {
    var_in_formula <- paste("s(" , target_variable,")", sep = "")
  }else{
    var_in_formula <- paste("s(`" ,  paste(target_variable, collapse = "`+`"),"`)", sep = "")
  }
  univar_gam_model <- gam(as.formula(paste(outcome, var_in_formula, sep = "~")), data = resampled_data)
  
  # TODO: Could not find function "delayed_learner_process_formula"
  # TODO: sl --- Error in default_metalearner(self$training_outcome_type) : 
  # could not find function "default_metalearner"
  
  sl_fit_full_resampled <- sl$train(resampled_data_task)
  sl_fit_nosubcat_resampled <- sl$train(resampled_data_task_no_subcat_covars)
  
  
  ## get the original data and reduce the target variable by perc
  data_resampled_reduced <- data_original
  
  for (t_var in target_variable){
    if (t_var == "ALWAYS"){
      other_mask_wearing <- c("NEVER", "RARELY", "SOMETIMES", "FREQUENTLY")
      t_max <- max(data_resampled_reduced[, t_var])
      for(i in 1:nrow(data_resampled_reduced)){
        cur_data <- data_resampled_reduced[[i, t_var]]
        
        if(t_max > cur_data){
          data_resampled_reduced[[i, t_var]] <- cur_data + (cur_data * perc)
          remainder <- (1 - data_resampled_reduced[[i, t_var]])/4
        }else{
          data_resampled_reduced[[i, t_var]] <- t_max
          remainder <- (1 - t_max)/4
        }
        data_resampled_reduced[i, other_mask_wearing] <- data_resampled_reduced[i, other_mask_wearing] - (data_resampled_reduced[i, other_mask_wearing] * remainder)
      }
    }else{
      t_min <- min(data_resampled_reduced[, t_var])
      
      for(i in 1:nrow(data_resampled_reduced)) {
        cur_data <- data_resampled_reduced[[i, t_var]]
        if(t_min < cur_data)
          data_resampled_reduced[[i, t_var]] <- cur_data - (cur_data * perc)
        else {
          data_resampled_reduced[[i, t_var]] <- t_min
        }
      }
    }
  }
  
  
  reduced_tasks <- lapply(covariates, make_sl3_Task, data = data_resampled_reduced,
                          outcome = outcome, 
                          folds = origami::make_folds(resampled_data, fold_fun = folds_vfold, V = 2))
  
  resampled_data_reduced_task <- reduced_tasks[1]
  resampled_data_reduced_task_no_subcat_covars <- reduced_tasks[2]
  
  
  ## predict through superlearner for reduced data on resampled models
  sl_preds_reduced_full <- sl_fit_full_resampled$predict(resampled_data_reduced_task)
  # predict from no subcategories
  sl_preds_reduced_no_subcat <- sl_fit_nosubcat_resampled$predict(resampled_data_reduced_task_no_subcat_covars)
  # predict from univariate gam
  univariate_gam_predictions <- stats::predict(object = univar_gam_model, newdata = data_resampled_reduced)
  
  results <- data.frame(sl_preds_reduced_full, sl_preds_reduced_no_subcat, as.vector(univariate_gam_predictions))
  colnames(results) <- c("SL_full_model", "SL_no_tgt_subcat_vars", "univariate_gam")
  
  return(results)
}


############### ex
bootstrapCI(
  target_variable = top_vars,
  data_original = data_original,
  ML_pipeline_result = ML_pipeline_results,
  covars = covars,
  outcome = target_outcomes[[4]],
  perc = percents[1],
  sub_cat_vars = unique(unlist(top_vars)), 
  target_num = length(top_vars))


##################

## run marginal predictions for each decrease in target variable from variable importance calculations


bootstrap_marginal_predictions <- function(target_variable,
                                           ML_pipeline_result,
                                           outcome,
                                           boot_df_SL_full,
                                           boot_df_SL_no_subcat,
                                           boot_df_SL_univar_gam,
                                           sub_cat_vars,
                                           data_original = data_original,
                                           covars = covars,
                                           percents = percents,
                                           pop = data_original$Population,
                                           boot_num, 
                                           target_num){
  
  initialize_data <- rep(NaN, dim(data_original)[1] * boot_num * length(percents))
  
  
  m <- as.data.frame(matrix(0, ncol = length(percents), nrow = boot_num))
  
  c(boot_data_array_SL_full, boot_data_array_SL_no_subcat, boot_data_array_SL_univar_gam) %<-% replicate(3, m, simplify=FALSE)
  
  
  for (i in 1:length(percents)) {
    
    perc <- percents[i]
    
    # bootstrap for boot_num number of times
    boot_updates <- foreach(this_iter = seq_len(boot_num),
                            .options.multicore = list(preschedule = FALSE),
                            .errorhandling = "remove") %dopar%  { 
                              bootstrapCI(
                                target_variable = target_variable,
                                data_original = data_original,
                                ML_pipeline_result = ML_pipeline_result,
                                covars = covars,
                                outcome = outcome,
                                perc = perc,
                                sub_cat_vars = sub_cat_vars, 
                                target_num)
                            }
    
    
    if (outcome == "FirstCaseDay"){
      pop = 1
    }
    total_totals <- as.data.frame(t(colMeans(bind_rows(boot_updates) * pop)))
    # total_counts_SL_full <- total_totals$SL_full_model
    # total_counts_SL_no_subcat <- total_totals$SL_no_tgt_subcat_vars
    # total_counts_univariate_gam <- total_totals$univariate_gam
    # 
    # boot_updates_SL_full <- colSums(do.call(cbind, sapply(boot_updates, "[", 1)) * pop)
    # boot_updates_SL_nosubcat <- colSums(do.call(cbind, sapply(boot_updates, "[", 2)) * pop)
    # boot_updates_SL_univar_gam <- colSums(do.call(cbind, sapply(boot_updates, "[", 3)) * pop)
    # 
    
    # boot_data_array_full_sl[, i] <- as.vector(boot_updates_SL_full)
    # boot_data_array_full_nosubcat[, i] <- as.vector(boot_updates_SL_nosubcat)
    # boot_data_array_full_univar_gam[, i] <- as.vector(boot_updates_SL_univar_gam)
    # 
    model_names <- c("SL_full_model", "SL_no_tgt_subcat_vars", "univariate_gam")
    model_names_short <- c("SL_full", "SL_no_subcat", "SL_univar_gam")
    
    for(j in seq_along(model_names)){
      m_name <- model_names[j]
      m_name_short <- model_names_short[j]
      assign(paste0("total_counts_", m_name_short), total_totals[m_name]) # total counts
      assign(paste0("boot_updates_", m_name_short), colSums(do.call(cbind, sapply(boot_updates, "[", j)) * pop)) # boot updates
      assign(paste0("boot_data_array_", m_name_short, "[, i]"), as.vector(eval(paste0("boot_updates_", m_name_short)))) # boot data array

      probs <- c(0.025, 0.50, 0.975)
      CI_name <- paste0("CI_boot_", m_name_short)
      assign(CI_name, quantile(eval(paste0("boot_updates_", m_name_short)), probs = probs, na.rm = TRUE))
      
      # update sl df
      boot_df_name <- paste0("boot_df_", m_name_short)
      assign(paste0(boot_df_name, "[i, 1]"), perc)
      assign(paste0(boot_df_name, "[i, 2]"), eval(CI_name)[[2]])
      assign(paste0(boot_df_name, "[i, 3]"), eval(CI_name)[[1]])
      assign(paste0(boot_df_name, "[i, 4]"), eval(CI_name)[[3]])
    }
    
    # 
    # CI_boot_full_sl <- quantile(boot_updates_SL_full, probs = probs, na.rm = TRUE)
    # CI_boot_full_nosubcat <- quantile(boot_updates_SL_nosubcat, probs = probs, na.rm = TRUE)
    # CI_boot_full_univar_gam <- quantile(boot_updates_SL_univar_gam, probs = probs, na.rm = TRUE)
    
    # ## update full sl df
    # boot_df_sf_full[i, 1] <- perc
    # boot_df_sf_full[i, 2] <- CI_boot_full_sl[[2]]
    # boot_df_sf_full[i, 3] <- CI_boot_full_sl[[1]]
    # boot_df_sf_full[i, 4] <- CI_boot_full_sl[[3]]
    # 
    # ## update subcat sl df
    # boot_df_sf_no_subcat[i, 1] <- perc
    # boot_df_sf_no_subcat[i, 2] <- CI_boot_full_nosubcat[[2]]
    # boot_df_sf_no_subcat[i, 3] <- CI_boot_full_nosubcat[[1]]
    # boot_df_sf_no_subcat[i, 4] <- CI_boot_full_nosubcat[[3]]
    # 
    # # ## update univar gam df
    # boot_df_univar_gam[i, 1] <- perc
    # boot_df_univar_gam[i, 2] <- CI_boot_full_univar_gam[[2]]
    # boot_df_univar_gam[i, 3] <- CI_boot_full_univar_gam[[1]]
    # boot_df_univar_gam[i, 4] <- CI_boot_full_univar_gam[[3]]
  }
  
  model_names_short <- c("SL_full", "SL_no_subcat", "SL_univar_gam")
  
  return(list(
    "full_sl_results" = list(
      "boot_CI_df_sl_full" = boot_df_SL_full,
      "boot_sl_full_array" = boot_data_array_SL_full
    ),
    "no_subcat_sl_results" = list(
      "boot_CI_df_sl_nosubcat" = boot_df_SL_no_subcat,
      "boot_sl_nosubcat_array" = boot_data_array_SL_no_subcat
    ),
    "univar_gam_results" = list(
      "boot_CI_univar_gam" = boot_df_SL_univar_gam,
      "boot_sl_univar_gam_array" = boot_data_array_SL_univar_gam
    )
  ))
}


#####################
### 25 CASES #####
#####################
joint_impact_day_25cases <- bootstrap_marginal_predictions(target_variable = top_vars_cases25, 
                                                           ML_pipeline_result = ML_pipeline_results[[2]],
                                                           outcome = target_outcomes[[2]],
                                                           boot_df_sf_no_subcat = boot_dfs_sl_no_subcat[[2]],
                                                           sub_cat_vars = unique(unlist(top_var_subcat_vars_cases25)),
                                                           boot_df_univar_gam = boot_dfs_univar_gam[[2]],
                                                           boot_df_sf_full = boot_dfs_sl_full[[2]],
                                                           boot_num = 10,
                                                           target_num = 4)


# nwwi_impact_day_25cases <- bootstrap_marginal_predictions(target_variable = c("NWWI"), 
#                                                           ML_pipeline_result = ML_pipeline_results[[2]],
#                                                           outcome = target_outcomes[[2]],
#                                                           boot_df_sf_no_subcat = boot_dfs_sl_no_subcat[[2]],
#                                                           sub_cat_vars = unique(unlist(top_var_subcat_vars_cases25[1])),
#                                                           boot_df_univar_gam = boot_dfs_univar_gam[[2]],
#                                                           boot_df_sf_full = boot_dfs_sl_full[[2]],
#                                                           boot_num = 10,
#                                                           target_num = 1)
# 
# 
# so2_impact_day_25cases <- bootstrap_marginal_predictions(target_variable = c("so2"), 
#                                                          ML_pipeline_result = ML_pipeline_results[[2]],
#                                                          outcome = target_outcomes[[2]],
#                                                          boot_df_sf_no_subcat = boot_dfs_sl_no_subcat[[2]],
#                                                          sub_cat_vars = unique(unlist(top_var_subcat_vars_cases25[2])),
#                                                          boot_df_univar_gam = boot_dfs_univar_gam[[2]],
#                                                          boot_df_sf_full = boot_dfs_sl_full[[2]],
#                                                          boot_num = 10,
#                                                          target_num = 1)
# 
# 
# epl_min_impact_day_25cases <- bootstrap_marginal_predictions(target_variable = c("EPL_LIMENG"), 
#                                                              ML_pipeline_result = ML_pipeline_results[[2]],
#                                                              outcome = target_outcomes[[2]],
#                                                              boot_df_sf_no_subcat = boot_dfs_sl_no_subcat[[2]],
#                                                              sub_cat_vars = unique(unlist(top_var_subcat_vars_cases25[3])),
#                                                              boot_df_univar_gam = boot_dfs_univar_gam[[2]],
#                                                              boot_df_sf_full = boot_dfs_sl_full[[2]],
#                                                              boot_num = 10,
#                                                              target_num = 1)
# 
# hypert_impact_day_25cases <- bootstrap_marginal_predictions(target_variable = c("prev_2017_all_ages_Hypertension"), 
#                                                             ML_pipeline_result = ML_pipeline_results[[2]],
#                                                             outcome = target_outcomes[[2]],
#                                                             boot_df_sf_no_subcat = boot_dfs_sl_no_subcat[[2]],
#                                                             sub_cat_vars = unique(unlist(top_var_subcat_vars_cases25[4])),
#                                                             boot_df_univar_gam = boot_dfs_univar_gam[[2]],
#                                                             boot_df_sf_full = boot_dfs_sl_full[[2]],
#                                                             boot_num = 10,
                                                            # target_num = 1)

#####################
### Day 100 Deaths ##
#####################

joint_impact_day100_deaths <- bootstrap_marginal_predictions(target_variable = top_vars_death100, 
                                                             ML_pipeline_result = ML_pipeline_results[[4]],
                                                             outcome = target_outcomes[[4]],
                                                             boot_df_sf_no_subcat = boot_dfs_sl_no_subcat[[4]],
                                                             sub_cat_vars = unique(unlist(top_var_subcat_vars_death100)),
                                                             boot_df_univar_gam = boot_dfs_univar_gam[[4]],
                                                             boot_df_sf_full = boot_dfs_sl_full[[4]],
                                                             data_original = data_original,
                                                             covars = covars,
                                                             percents = percents,
                                                             pop = data_original$Population,
                                                             boot_num = 10,
                                                             target_num = 4)

#saveRDS(joint_impact_day100_deaths, here("Analysis/update_data/data/processed/joint_impact_day100_deaths.RDS"))


no2_impact_day100_deaths <- bootstrap_marginal_predictions(target_variable = c("no2"), 
                                                           ML_pipeline_result = ML_pipeline_results[[4]],
                                                           outcome = target_outcomes[[4]],
                                                           boot_df_sf_no_subcat = boot_dfs_sl_no_subcat[[4]],
                                                           boot_df_univar_gam = boot_dfs_univar_gam[[4]],
                                                           boot_df_sf_full = boot_dfs_sl_full[[4]],
                                                           sub_cat_vars = unique(unlist(top_var_subcat_vars_death100[2])),
                                                           boot_num = 10, 
                                                           target_num = 1)

#saveRDS(no2_impact_day100_deaths, here("Analysis/update_data/data/processed/no2_impact_day100_deaths.RDS"))

mask_impact_day100_deaths <- bootstrap_marginal_predictions(target_variable = c("ALWAYS"), 
                                                            ML_pipeline_result = ML_pipeline_results[[4]],
                                                            outcome = target_outcomes[[4]],
                                                            boot_df_sf_no_subcat = boot_dfs_sl_no_subcat[[4]],
                                                            boot_df_univar_gam = boot_dfs_univar_gam[[4]],
                                                            boot_df_sf_full = boot_dfs_sl_full[[4]],
                                                            sub_cat_vars = unique(unlist(top_var_subcat_vars_death100[1])),
                                                            boot_num = 4,
                                                            target_num = 1)

#saveRDS(mask_impact_day100_deaths, here("Analysis/update_data/data/processed/mask_impact_day100_deaths.RDS"))

prop_black_impact_day100_deaths <- bootstrap_marginal_predictions(target_variable = c("pct_black_only_2018"), 
                                                                  ML_pipeline_result = ML_pipeline_results[[4]],
                                                                  outcome = target_outcomes[[4]],
                                                                  boot_df_sf_no_subcat = boot_dfs_sl_no_subcat[[4]],
                                                                  boot_df_sf_full = boot_dfs_sl_full[[4]],
                                                                  boot_df_univar_gam = boot_dfs_univar_gam[[4]],
                                                                  sub_cat_vars = unique(unlist(top_var_subcat_vars_death100[3])),
                                                                  boot_num = 5,
                                                                  target_num = 1)

#saveRDS(prop_black_impact_day100_deaths, here("Analysis/update_data/data/processed/prop_black_impact_day100_deaths.RDS"))


income_impact_day100_deaths <- bootstrap_marginal_predictions(target_variable = c("Income.inequality.raw.value"), 
                                                              ML_pipeline_result = ML_pipeline_results[[4]],
                                                              outcome = target_outcomes[[4]],
                                                              boot_df_sf_no_subcat = boot_dfs_sl_no_subcat[[4]],
                                                              boot_df_sf_full = boot_dfs_sl_full[[4]],
                                                              boot_df_univar_gam = boot_dfs_univar_gam[[4]],
                                                              sub_cat_vars = unique(unlist(top_var_subcat_vars_death100[4])),
                                                              boot_num = 5,
                                                              target_num = 1)

#saveRDS(pm25_impact_day100_deaths, here("Analysis/update_data/data/processed/pm25_impact_day100_deaths.RDS"))

# ######################
# ### Total Cases ######
# ######################

joint_impact_day_total_cases <- bootstrap_marginal_predictions(target_variable = top_vars_casestotal, 
                                                               ML_pipeline_result = ML_pipeline_results[[3]],
                                                               outcome = target_outcomes[[3]],
                                                               boot_df_sf_no_subcat = boot_dfs_sl_no_subcat[[3]],
                                                               sub_cat_vars = unique(unlist(top_var_subcat_vars_totalcases)),
                                                               boot_df_univar_gam = boot_dfs_univar_gam[[3]],
                                                               boot_df_sf_full = boot_dfs_sl_full[[3]],
                                                               boot_num = 5,
                                                               target_num = 4)

NWWI_impact_day_total_cases <- bootstrap_marginal_predictions(target_variable = c("NWWI"), 
                                                              ML_pipeline_result = ML_pipeline_results[[3]],
                                                              outcome = target_outcomes[[3]],
                                                              boot_df_sf_no_subcat = boot_dfs_sl_no_subcat[[3]],
                                                              sub_cat_vars = unique(unlist(top_var_subcat_vars_totalcases[1])),
                                                              boot_df_univar_gam = boot_dfs_univar_gam[[3]],
                                                              boot_df_sf_full = boot_dfs_sl_full[[3]],
                                                              boot_num = 5,
                                                              target_num = 1)

pm10_impact_day_total_cases <- bootstrap_marginal_predictions(target_variable = c("pm10"), 
                                                              ML_pipeline_result = ML_pipeline_results[[3]],
                                                              outcome = target_outcomes[[3]],
                                                              boot_df_sf_no_subcat = boot_dfs_sl_no_subcat[[3]],
                                                              sub_cat_vars = unique(unlist(top_var_subcat_vars_totalcases[2])),
                                                              boot_df_univar_gam = boot_dfs_univar_gam[[3]],
                                                              boot_df_sf_full = boot_dfs_sl_full[[3]],
                                                              boot_num = 5,
                                                              target_num = 1)

epl_limeng_impact_day_total_cases <- bootstrap_marginal_predictions(target_variable = c("EPL_LIMENG"), 
                                                                    ML_pipeline_result = ML_pipeline_results[[3]],
                                                                    outcome = target_outcomes[[3]],
                                                                    boot_df_sf_no_subcat = boot_dfs_sl_no_subcat[[3]],
                                                                    sub_cat_vars = unique(unlist(top_var_subcat_vars_totalcases[3])),
                                                                    boot_df_univar_gam = boot_dfs_univar_gam[[3]],
                                                                    boot_df_sf_full = boot_dfs_sl_full[[3]],
                                                                    boot_num = 5,
                                                                    target_num = 1)

epl_65_impact_day_total_cases <- bootstrap_marginal_predictions(target_variable = c("EPL_AGE65"), 
                                                                ML_pipeline_result = ML_pipeline_results[[3]],
                                                                outcome = target_outcomes[[3]],
                                                                boot_df_sf_no_subcat = boot_dfs_sl_no_subcat[[3]],
                                                                sub_cat_vars = unique(unlist(top_var_subcat_vars_totalcases[4])),
                                                                boot_df_univar_gam = boot_dfs_univar_gam[[3]],
                                                                boot_df_sf_full = boot_dfs_sl_full[[3]],
                                                                boot_num = 5,
                                                                target_num = 1)

# ######################
# ### Total to date ####
# ######################


joint_impact_day_all_deaths <- bootstrap_marginal_predictions(target_variable = top_vars_totaldeaths,
                                                              ML_pipeline_result = ML_pipeline_results[[5]],
                                                              outcome = target_outcomes[[5]],
                                                              boot_df_sf_no_subcat = boot_dfs_sl_no_subcat[[5]],
                                                              sub_cat_vars = unique(unlist(top_var_subcat_vars_totaldeaths)),
                                                              boot_df_univar_gam = boot_dfs_univar_gam[[5]],
                                                              boot_df_sf_full = boot_dfs_sl_full[[5]],
                                                              boot_num = 5,
                                                              target_num = 4)



nwwi_impact_all_deaths <- bootstrap_marginal_predictions(target_variable = "NWWI",
                                                         ML_pipeline_result = ML_pipeline_results[[5]],
                                                         outcome = target_outcomes[[5]],
                                                         boot_df_sf_no_subcat = boot_dfs_sl_no_subcat[[5]],
                                                         boot_df_univar_gam = boot_dfs_univar_gam[[5]],
                                                         boot_df_sf_full = boot_dfs_sl_full[[5]],
                                                         sub_cat_vars = unique(unlist(top_var_subcat_vars_totaldeaths[1])),
                                                         boot_num = 5,
                                                         target_num = 1)


black_impact_all_deaths <- bootstrap_marginal_predictions(target_variable = c("pct_black_only_2018"),
                                                          ML_pipeline_result = ML_pipeline_results[[5]],
                                                          outcome = target_outcomes[[5]],
                                                          boot_df_sf_no_subcat = boot_dfs_sl_no_subcat[[5]],
                                                          boot_df_univar_gam = boot_dfs_univar_gam[[5]],
                                                          boot_df_sf_full = boot_dfs_sl_full[[5]],
                                                          sub_cat_vars = unique(unlist(top_var_subcat_vars_totaldeaths[2])),
                                                          boot_num = 4,
                                                          target_num = 1)


sex_all_deaths <- bootstrap_marginal_predictions(target_variable = c("pct_female_2018"),
                                                 ML_pipeline_result = ML_pipeline_results[[5]],
                                                 outcome = target_outcomes[[5]],
                                                 boot_df_sf_no_subcat = boot_dfs_sl_no_subcat[[5]],
                                                 boot_df_sf_full = boot_dfs_sl_full[[5]],
                                                 boot_df_univar_gam = boot_dfs_univar_gam[[5]],
                                                 sub_cat_vars = unique(unlist(top_var_subcat_vars_totaldeaths[3])),
                                                 boot_num = 4,
                                                 target_num = 1)


hypertension_all_deaths <- bootstrap_marginal_predictions(target_variable = c("prev_2017_all_ages_Hypertension"),
                                                          ML_pipeline_result = ML_pipeline_results[[5]],
                                                          outcome = target_outcomes[[5]],
                                                          boot_df_sf_no_subcat = boot_dfs_sl_no_subcat[[5]],
                                                          boot_df_sf_full = boot_dfs_sl_full[[5]],
                                                          boot_df_univar_gam = boot_dfs_univar_gam[[5]],
                                                          sub_cat_vars = unique(unlist(top_var_subcat_vars_totaldeaths[4])),
                                                          boot_num = 4,
                                                          target_num = 1)

masks_all_deaths <- bootstrap_marginal_predictions(target_variable = c("ALWAYS"),
                                                   ML_pipeline_result = ML_pipeline_results[[5]],
                                                   outcome = target_outcomes[[5]],
                                                   boot_df_sf_no_subcat = boot_dfs_sl_no_subcat[[5]],
                                                   boot_df_sf_full = boot_dfs_sl_full[[5]],
                                                   boot_df_univar_gam = boot_dfs_univar_gam[[5]],
                                                   sub_cat_vars = unique(unlist(top_var_subcat_vars_death100[1])),
                                                   boot_num = 4,
                                                   target_num = 1)

