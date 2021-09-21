library(here)
library(readxl)
library(dplyr)
library(purrr)
library(sl3)
library(tidyr)
library(data.table)
library(gbm)
library(ggplot2)
library(svMisc)

## changed the varimp function to do subcategories

calculate_model_risk_rsq <- function(model, fold_number = "validation"){
  
  fit <- model$fit
  task <- fit$training_task
  dat <- task$data
  X <- task$nodes$covariates
  Y <- task$Y
  preds <- fit$predict_fold(task, fold_number = fold_number)
  risk <- mean(loss(preds, Y))
  rmse <- sqrt(risk)
  risk_pop <- risk  * sum(dat$Population)
  rmse_pop <- sqrt(risk_pop)
  rss <- sum((Y-preds)^2)  ## residual sum of squares
  tss <- sum((Y-mean(Y))^2) 
  
  rsq <- 1 - rss/tss
  
  return(list("rmse" = rmse, "rmse_pop" = rmse_pop, "rsq" = rsq))
}




super_varimp <- function (.x, 
                           loss, 
                           variable_list, 
                           subcategory_list,
                           fold_number = "validation", 
                           type = c("ratio", "difference"), 
                           intxn_list = intxn.vars,
                           intxn_size = 5) 
{

  fit <- .x$fit
  type <- match.arg(type)
  task <- fit$training_task
  dat <- task$data
  X <- task$nodes$covariates
  Y <- task$Y
  subcategories <- unique(subcategory_list)
  preds <- fit$predict_fold(task, fold_number = fold_number)
  risk <- mean(loss(preds, Y))
  
  
  ##################################################
  ### Variable Importance by Individual Variable ###
  ##################################################
  
  individual_var_imp <- lapply(X, function(i) {
    scrambled_col <- data.table(sample(unlist(dat[, i, with = FALSE]), 
                                       nrow(dat)))
    names(scrambled_col) <- i
    scrambled_col_names <- task$add_columns(scrambled_col)
    scrambled_col_task <- task$next_in_chain(column_names = scrambled_col_names)
    scrambled_sl_preds <- fit$predict_fold(scrambled_col_task, 
                                           fold_number)
    risk_scrambled <- mean(loss(scrambled_sl_preds, Y))
    
    if (type == "ratio") {
      varimp_metric <- risk_scrambled/risk
    }
    else if (type == "difference") {
      varimp_metric <- risk_scrambled - risk
    }
    return(varimp_metric)
  })
  
  names(individual_var_imp) <- X
  
  if (type == "ratio") {
    indiv_results <- data.table(Variable = names(individual_var_imp), Risk_ratio = unlist(individual_var_imp))
    indiv_results <- indiv_results[order(-indiv_results$Risk_ratio)]
  } else if (type == "difference") {
    indiv_results <- data.table(Variable = names(individual_var_imp), Risk_Difference = unlist(individual_var_imp))
    indiv_results <- indiv_results[order(-indiv_results$Risk_Difference)]
  }
  
  #######################################
  ### Variable Importance by Category ###
  #######################################
  
  var_cat_importance <- lapply(subcategories, function(i) {
    subcat_vars <- variable_list[which(subcategory_list %in% i)]
    scrambled_rows <- dat[sample(nrow(dat)), ]
    scrambled_rows_selection <- scrambled_rows %>% dplyr::select(!!subcat_vars)
    scrambled_col_names <- task$add_columns(scrambled_rows_selection)
    scrambled_col_task <- task$next_in_chain(column_names = scrambled_col_names)
    scrambled_sl_preds <- fit$predict_fold(scrambled_col_task, 
                                           fold_number)

    risk_scrambled <- mean(loss(scrambled_sl_preds, Y))
    
    if (type == "ratio") {
      varimp_metric <- risk_scrambled/risk
    }
    else if (type == "difference") {
      varimp_metric <- risk_scrambled - risk
    }
    return(varimp_metric)
  })
  
  
  names(var_cat_importance) <- subcategories
  
  if (type == "ratio") {
    subcat_results <- data.table(Subcategory = names(var_cat_importance), Risk_ratio = unlist(var_cat_importance))
    subcat_results <- subcat_results[order(-subcat_results$Risk_ratio)]
  } else if (type == "difference") {
    subcat_results <- data.table(Subcategory = names(var_cat_importance), Risk_Difference = unlist(var_cat_importance))
    subcat_results <- subcat_results[order(-subcat_results$Risk_Difference)]
  }
  
  ##########################################
  ### Variable Importance by Permutation ###
  ##########################################
  
  ### Create list with all intxn_size interactions for the intxn_list variable set of interest:
  variable_combinations <- combn(intxn_list, m = intxn_size)
  variable_combinations <- as.data.frame(variable_combinations)
  ### Run the additive vs. joint error calculation for each set of possible interactions of selected size:
  
  pb <- txtProgressBar(min = 0, max = dim(variable_combinations)[2], style = 3)
  
  permuted_importance <- lapply(seq(variable_combinations), function(i) {
    setTxtProgressBar(pb, i)
    target_vars <- variable_combinations[,i]
    ## compute the additive risk for this set of variables
    additives <- individual_var_imp[target_vars]
    additive_risk <- sum(unlist(additives) - 1) + 1 
    
    ## calculate the permuted risk for this set of variables
    scrambled_rows <- dat[sample(nrow(dat)), ]
    scrambled_rows_selection <- scrambled_rows %>% dplyr::select(!!target_vars)
    scrambled_col_names <- task$add_columns(scrambled_rows_selection)
    scrambled_col_task <- task$next_in_chain(column_names = scrambled_col_names)
    scrambled_sl_preds <- fit$predict_fold(scrambled_col_task, 
                                           fold_number)
    
    risk_scrambled <- mean(loss(scrambled_sl_preds, Y))
    if (type == "ratio") {
      varimp_metric <- risk_scrambled/risk
    } else if (type == "difference") {
      varimp_metric <- risk_scrambled - risk
    }
    return(list("permuted_set_risk" = varimp_metric, "additive_set_risk" = additive_risk))
  }
  )
  
  variables_concat <- lapply(seq(variable_combinations), function(i){
    vars <- variable_combinations[,i]
    vars_concat <- paste(vars, collapse = " & ")
    return(vars_concat)
    
  }
  )
  
  names(permuted_importance) <- variables_concat
  
  permutation_comp_results <- data.table(Variables = names(permuted_importance), 
                        additive_risk = unlist(purrr::map(permuted_importance, "additive_set_risk")), 
                        permuted_risk = unlist(purrr::map(permuted_importance, "permuted_set_risk")))
  
  permutation_comp_results$perm_additive_diff <- permutation_comp_results$permuted_risk - permutation_comp_results$additive_risk
  
  return(list("indv_var_results" = indiv_results, 
              "subcat_results" = subcat_results, 
              "permutation_results" = permutation_comp_results))
  
  }

## plotting results
plot_variable_importance_for_cat <- function(input_df, plot_label, save_label, type = "ind"){
  var_importance <- input_df
  colnames(var_importance) <- c("County_Features", "Risk_Ratio")
  
  var_importance$County_Features <- as.factor(var_importance$County_Features)
  var_importance$County_Features <- factor(var_importance$County_Features, levels = var_importance$County_Features[order(var_importance$Risk_Ratio)])
  
  pdf(paste(here("Visulizations/var_imp/"),save_label , sep = ""))
  
  if (type == "cat") {
    limit <- 1.01
  }else{limit <- 1.009}
  
  var_imp_plot <- var_importance %>%
    filter(Risk_Ratio > limit) %>%
    ggplot(aes(x = County_Features, y = Risk_Ratio)) +
    geom_dotplot(binaxis = "y", dotsize = 0.4) +
    labs(x = "County Level Feature", y = "Risk Ratio", 
         title = plot_label) + 
    coord_flip() +
    theme(axis.text.x  = element_text(size=20), axis.text.y  = element_text(size=15))
  
  print(var_imp_plot)
  
  dev.off()
  
}
ML_pipeline_results <- readRDS(here("Analysis/update_data/data/processed/ML_pipeline_6_outcomes_noscale_Dec9_sm.RDS"))
Data_Dictionary <- read_excel(here("Analysis/update_data/data/processed/Data_Dictionary.xlsx"))
Data_Dictionary_Used <- Data_Dictionary %>% filter(Keep == "Yes") %>% select(`Variable Name`, `Sub-Category`)

vars_rmv_na <- read.csv(here("Analysis/update_data/data/processed/vars_removed_na_thresh.csv"))
##remove from the list covariates that had too many NAs and were then dropped before analysis, FIPS, and the outcome data:
removing <- c(vars_rmv_na$x,
                "countyFIPS",
                "occ_all_private",
                "CountyRelativeDay25Cases",
                "TotalCasesUpToDate",
                "USRelativeDay100Deaths", 
                "TotalDeathsUpToDate",
                "FirstCaseDay")

Data_Dictionary_Used <- Data_Dictionary_Used[-match(removing , Data_Dictionary_Used$`Variable Name`),]
variable_list <-  Data_Dictionary_Used$`Variable Name`
subcategory_list <- Data_Dictionary_Used$`Sub-Category`


intxn_vars <- c("NWWI", "ALWAYS", "co", "no2",  "o3", "pm10", "pm25", "so2", 
                "pct_black_only_2018", "EPL_LIMENG", "EPL_AGE65", 
                "pct_female_2018","MALATHION", "2,4-D",
                "prev_2017_all_ages_Diabetes", "prev_2017_all_ages_Hypertension", 
                "Income.inequality.raw.value")

lapply(ML_pipeline_results, calculate_model_risk_rsq)

var_imps <- purrr::map(.x = ML_pipeline_results, 
    .f = super_varimp, 
    loss = loss_squared_error, 
    variable_list = variable_list,
    subcategory_list =subcategory_list,
    fold_number = "validation", 
    intxn_list = intxn_vars,
    intxn_size = 4,
    type = "ratio")

saveRDS(var_imps, here("Analysis/update_data/data/processed/var_imps_rac_seg.RDS"))




plot_variable_importance_for_cat(input_df = var_imps[[1]]$subcat_results, plot_label = "Day First Case Outcome by Cat", save_label = "day_first_case_x_cat.pdf", type = "cat")
plot_variable_importance_for_cat(input_df = var_imps[[2]]$subcat_results, plot_label = "Cases Rate at Day 25 Outcome by Cat", save_label = "day_25_cases_x_cat.pdf", type = "cat")
plot_variable_importance_for_cat(input_df = var_imps[[3]]$subcat_results, plot_label = "Cases Rate Total To Date Outcome by Cat", save_label = "tota_cases_2date_x_cat.pdf", type = "cat")
plot_variable_importance_for_cat(input_df = var_imps[[4]]$subcat_results, plot_label = "Mortality at Day 100 Rate Outcome by Cat", save_label = "day_100_mortality_x_cat.pdf", type = "cat")
plot_variable_importance_for_cat(input_df = var_imps[[5]]$subcat_results, plot_label = "Mortality Total Rate Outcome by Cat", save_label = "total_deaths_2date_x_cat.pdf", type = "cat")

plot_variable_importance_for_cat(input_df = var_imps[[1]]$indv_var_results, plot_label = "Day First Case Outcome by Var", save_label = "day_first_case_x_ind.pdf", type = "ind")
plot_variable_importance_for_cat(input_df = var_imps[[2]]$indv_var_results, plot_label = "Cases Rate at Day 25 Outcome by Var", save_label = "day_25_cases_x_ind.pdf", type = "ind")
plot_variable_importance_for_cat(input_df = var_imps[[3]]$indv_var_results, plot_label = "Cases Rate Total To Date Outcome by Var", save_label = "tota_cases_2date_x_ind.pdf", type = "ind")
plot_variable_importance_for_cat(input_df = var_imps[[4]]$indv_var_results, plot_label = "Mortality at Day 100 Rate Outcome by Var", save_label = "day_100_mortality_x_ind.pdf", type = "ind")
plot_variable_importance_for_cat(input_df = var_imps[[5]]$indv_var_results, plot_label = "Mortality Total Rate Outcome by Var", save_label = "total_deaths_2date_x_ind.pdf", type = "ind")


