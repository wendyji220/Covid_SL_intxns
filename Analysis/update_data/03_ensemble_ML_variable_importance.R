# switch up library path for the slurm monster
r_libdir <- Sys.getenv("R_LIBDIR")

# set user-specific package library
if (grepl("savio2", Sys.info()["nodename"])) {
  .libPaths(r_libdir)
  Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")
}


library(sl3)
library(origami)
library(dplyr)
library(knitr)
library(ggplot2)
library(here)
library(R6)
library(tidyverse)
library(readxl)
library(here)
library(data.table)
library(readr)
library(cowplot)
library(future)
library(parallel)
library(doParallel)
# library(doSNOW)

# machines=rep(strsplit(Sys.getenv("SLURM_NODELIST"), ",")[[1]],
#              each = as.numeric(Sys.getenv("SLURM_CPUS_ON_NODE")) )
# 
# cl = makeCluster(machines)
# 
# registerDoSNOW(cl)


plan(multisession)

## load data
covid_data_processed <- read_csv(here("Analysis/update_data/data/processed/cleaned_covid_data_final_sept_24_21.csv"))
covid_data_processed <- covid_data_processed[,-1]

Data_Dictionary <- read_excel(here("Analysis/update_data/data/processed/Data_Dictionary.xlsx"))
Data_Dictionary <- Data_Dictionary[Data_Dictionary$`Variable Name` %in% colnames(covid_data_processed),]

# names(covid_data_processed) <- Data_Dictionary$`Nice Label`[match(names(covid_data_processed), Data_Dictionary$`Variable Name`)]

## source the custom learners built for poisson outcomes
sapply(list.files(path = here("Analysis/poisson_learners"),
                  full.names = TRUE), 
                  source)


covid_data_processed$CountyRelativeDay100Cases <- covid_data_processed$CountyRelativeDay100Cases / covid_data_processed$Population 
covid_data_processed$TotalCasesUpToDate <- covid_data_processed$TotalCasesUpToDate / covid_data_processed$Population 
covid_data_processed$CountyRelativeDay100Deaths <- covid_data_processed$CountyRelativeDay100Deaths / covid_data_processed$Population 
covid_data_processed$TotalDeathsUpToDate <- covid_data_processed$TotalDeathsUpToDate / covid_data_processed$Population
covid_data_processed$Deathsat1year <- covid_data_processed$Deathsat1year / covid_data_processed$Population
covid_data_processed$Casesat1year <- covid_data_processed$Casesat1year / covid_data_processed$Population


outcomes <- c("CountyRelativeDay100Cases", 
              "TotalCasesUpToDate", 
              "CountyRelativeDay100Deaths" , 
              "TotalDeathsUpToDate", 
              "Deathsat1year",
              "Casesat1year")


covars <- colnames(covid_data_processed)[-which(names(covid_data_processed) %in% c(
  outcomes,
  "FIPS",
  "county_names"
))]

varimp_server <- function(fit,
                          loss, 
                          covars,
                          outcome,
                          data = covid_data_processed,
                          data_dictionary = Data_Dictionary, 
                          label = label) 
{
  
    # best_estimator <- fit$learner_fits[[which(fit$coefficients == 1)]]
    task <- fit$training_task
    dat <- task$data
    X <- task$nodes$covariates
    Y <- task$Y
    preds <- fit$predict_fold(task, fold_number = "validation")
    risk <- mean(loss(preds, Y))
    
    nworkers <- 18 #as.numeric(Sys.getenv('SLURM_CPUS_ON_NODE'))
    nworkers
    doParallel::registerDoParallel(nworkers)
    
    risk_importance <- foreach(i = X, .combine = 'c', .errorhandling = "pass") %dopar% {
      scrambled_col <- data.table(sample(unlist(dat[, i, with = FALSE]), nrow(dat)))
      names(scrambled_col) <- i
      scrambled_col_names <- task$add_columns(scrambled_col)
      scrambled_col_task <- task$next_in_chain(column_names = scrambled_col_names)
      scrambled_sl_preds <- fit$predict_fold(scrambled_col_task, fold_number = "validation")
    #   i_removed_learner <- fit$reparameterize(list(covariates = setdiff(X, i)))
    #   i_removed_fit <- i_removed_learner$train(task)
    #   i_removed_pred <- i_removed_fit$predict_fold(task, fold_number = "validation")
      no_i_risk <- mean(loss(scrambled_sl_preds, Y))
      varimp_metric <- no_i_risk/risk
    #   
      return(varimp_metric)
    }
    
    quantile_importance <- foreach(i = X, .combine = 'c') %dopar% {
      dat <- fit$training_task$data

      dat_x75 <- dat_x25 <- dat
      quantile_25 <- quantile(dat[[i]])[2]
      quantile_75 <- quantile(dat[[i]])[4]
      
      dat_x25[[i]] <-  quantile_25
      dat_x75[[i]] <-  quantile_75
      
      task_25 <- make_sl3_Task(
        data = dat_x25,
        covariates = covars,
        outcome = outcome)
      
      task_75 <- make_sl3_Task(
        data = dat_x75,
        covariates = covars,
        outcome = outcome)
      
      x_25_predictions <- fit$predict_fold(task = task_25, fold_number = "validation")
      x_75_predictions <- fit$predict_fold(task = task_75, fold_number = "validation")
      
      varimp_metric <- mean(x_75_predictions - x_25_predictions)
      return(varimp_metric)
    }
      
    names(risk_importance) <- X
    names(quantile_importance) <- X
    
    risk_results <- data.table(X = names(risk_importance), risk_ratio = unlist(risk_importance))
    risk_results_ordered <- risk_results[order(-risk_results$risk_ratio)]
   
    quantile_results <- data.table(X = names(quantile_importance), risk_difference = unlist(quantile_importance))
    quantile_results_ordered <- quantile_results[order(-quantile_results$risk_difference)]
    
    merged_results <- merge(risk_results_ordered, quantile_results_ordered, by= "X")
    merged_results$X<- data_dictionary$`Nice Label`[match(merged_results$X, data_dictionary$`Variable Name`)]
    
    risk_plot <- merged_results %>%
      arrange(risk_ratio) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
      filter(risk_ratio > 1.01)  %>%
      mutate(name=factor(X, levels=X)) %>%   # This trick update the factor levels
      ggplot( aes(x=name, y=risk_ratio)) +
      geom_segment( aes(xend=name, yend=1)) +
      geom_point( size=4, color="orange") +
      coord_flip() +
      theme_bw() +
      ylab("Model Risk Ratio") +
      xlab("County Feature")
    
    quantile_plot <- merged_results %>%
      arrange(risk_ratio) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
      filter(risk_ratio > 1.01)  %>%
      mutate(name=factor(X, levels=X)) %>%   # This trick update the factor levels
      ggplot( aes(x=name, y=risk_difference)) +
      geom_segment( aes(xend=name, yend=0)) +
      geom_point( size=4, color="blue") +
      coord_flip() +
      theme_bw() +
      ylab("Difference between 75th and 25th Quantile") + 
      xlab("")
    
    p <- plot_grid(risk_plot, quantile_plot, labels = label)
    ggsave(here(paste("Visulizations/New_Varimp/", "varimp_", label, ".png", sep = "")), p)

    
  return(merged_results)
}


## use a function to apply task over multiple outcomes 
run_sl3_poisson_lrns <- function(outcome, 
                                 data, 
                                 covars,
                                 data_dictionary, 
                                 label) {
  
  
  task <- make_sl3_Task(
  data = data,
  covariates = covars,
  outcome = outcome,
  folds = origami::make_folds(data, fold_fun = folds_vfold, V = 10))
  
  # if (outcome_type == "Poisson") {
    
    ## set up the custom learners and some standard ones as well
    ## set up baseline mean to make sure our other learners are working better than mean
  mean_lrnr <- Lrnr_mean$new()
  ## standard poisson GLM I wrote
  Lrnr_david_pois <- make_learner(Lrnr_david_pois)
  
  ## custom xgboost for poisson outcome with varying parameters (should try grid as well)
  Lrnr_david_xgboost_pois_850 <- make_learner(Lrnr_david_xgboost_pois, max_depth = 8,  nrounds = 50)
  Lrnr_david_xgboost_pois_5100 <- make_learner(Lrnr_david_xgboost_pois, max_depth = 5,  nrounds = 100)
  Lrnr_david_xgboost_pois_10200 <- make_learner(Lrnr_david_xgboost_pois, max_depth = 10,  nrounds = 200)
  
  ## custom ridge and lass from glmnet poisson
  ridge_lrnr_pois <- make_learner(Lrnr_david_glmnet_pois,alpha = 0, nfolds = 10)
  lasso_lrnr_pois <- make_learner(Lrnr_david_glmnet_pois,alpha = 1, nfolds = 10)
  
  ## custom gbm and glmnet for poisson with varying parameters
  Lrnr_david_gbm_pois <- make_learner(Lrnr_david_gbm_pois)
  Lrnr_david_glmnet_pois_25 <- make_learner(Lrnr_david_glmnet_pois, alpha = 0.25, nfolds = 3)
  Lrnr_david_glmnet_pois_50 <- make_learner(Lrnr_david_glmnet_pois, alpha = 0.50, nfolds = 3)
  Lrnr_david_glmnet_pois_75 <- make_learner(Lrnr_david_glmnet_pois, alpha = 0.75, nfolds = 3)
  
  ## custom HAL
  # Lrnr_david_hal9001_pois <- make_learner(Lrnr_david_hal9001_pois)
  ## custom earth
  #Lrnr_david_earth_pois <- make_learner(Lrnr_david_earth_pois)
  
  lrnr_ranger100 <- make_learner(Lrnr_ranger, num.trees = 100)
  lrnr_gam <- make_learner(Lrnr_gam)
  lrnr_polspline <- make_learner(Lrnr_polspline)
    
    ## create the stack of learners 
    
    ## custom xgboost for poisson outcome with varying parameters (should try grid as well)
  Lrnr_david_xgboost_pois_850 <- make_learner(Lrnr_david_xgboost_pois, max_depth = 8,  nrounds = 50)
  Lrnr_david_xgboost_pois_5100 <- make_learner(Lrnr_david_xgboost_pois, max_depth = 5,  nrounds = 100)
  Lrnr_david_xgboost_pois_10200 <- make_learner(Lrnr_david_xgboost_pois, max_depth = 10,  nrounds = 200)
  
  # choose base learners
  lrnr_glm <- make_learner(Lrnr_glm)

  lrnr_ranger10 <- make_learner(Lrnr_ranger, num.trees = 10)
  lrnr_ranger50 <- make_learner(Lrnr_ranger, num.trees = 50)
  lrnr_hal_simple <- make_learner(Lrnr_hal9001, max_degree = 2, n_folds = 2)
  lrnr_lasso <- make_learner(Lrnr_glmnet) # alpha default is 1
  lrnr_ridge <- make_learner(Lrnr_glmnet, alpha = 0)
  lrnr_elasticnet <- make_learner(Lrnr_glmnet, alpha = .5)
  
  # choose base learners
  lrnr_ranger100 <- make_learner(Lrnr_ranger, num.trees = 100)
  lrnr_ranger200 <- make_learner(Lrnr_ranger, num.trees = 200)
  lrnr_ranger300 <- make_learner(Lrnr_ranger, num.trees = 300)
  lrnr_ranger400 <- make_learner(Lrnr_ranger, num.trees = 400)
  lrnr_ranger500 <- make_learner(Lrnr_ranger, num.trees = 500)
  

  #lrnr_bayesglm <- Lrnr_pkg_SuperLearner$new("SL.bayesglm")
  
  # I like to crock pot my super learners
  grid_params <- list(max_depth = c(2, 4, 6, 8, 10, 12),
                      eta = c(0.001, 0.01, 0.1, 0.2, 0.3),
                      nrounds = c(20, 50))
  grid <- expand.grid(grid_params, KEEP.OUT.ATTRS = FALSE)
  params_default <- list(nthread = getOption("sl.cores.learners", 1))
  xgb_learners <- apply(grid, MARGIN = 1, function(params_tune) {
    do.call(Lrnr_xgboost$new, c(params_default, as.list(params_tune)))})
  
  full_lrn_earth_1 <- Lrnr_earth$new(degree = 1)
  full_lrn_earth_2 <- Lrnr_earth$new(degree = 2)
  full_lrn_earth_3 <- Lrnr_earth$new(degree = 3)
  full_lrn_earth_4 <- Lrnr_earth$new(degree = 4)
  
  full_lrn_poly_1 <- Lrnr_polspline$new(knots = 1)
  full_lrn_poly_2 <- Lrnr_polspline$new(knots = 2)
  full_lrn_poly_3 <- Lrnr_polspline$new(knots = 3)
  full_lrn_poly_4 <- Lrnr_polspline$new(knots = 4)
  full_lrn_poly_5 <- Lrnr_polspline$new(knots = 5)
  full_lrn_poly_6 <- Lrnr_polspline$new(knots = 6)
  full_lrn_poly_7 <- Lrnr_polspline$new(knots = 7)
  
  
  
  stack <- make_learner(
    Stack,
    mean_lrnr,
    Lrnr_david_pois,
    Lrnr_david_xgboost_pois_850,
    Lrnr_david_xgboost_pois_5100,
    Lrnr_david_xgboost_pois_10200,
    ridge_lrnr_pois,
    lasso_lrnr_pois,
    Lrnr_david_gbm_pois,
    Lrnr_david_glmnet_pois_25,
    Lrnr_david_glmnet_pois_50,
    Lrnr_david_glmnet_pois_75,
    lrnr_gam,
    lrnr_polspline,
    Lrnr_david_xgboost_pois_850,
    Lrnr_david_xgboost_pois_5100,
    Lrnr_david_xgboost_pois_10200,
    lrnr_glm,
    lrnr_ranger10,
    lrnr_ranger50,
    lrnr_ranger100,
    lrnr_ranger200,
    lrnr_ranger300,
    lrnr_ranger400,
    lrnr_ranger500,
    lrnr_lasso,
    lrnr_ridge,
    lrnr_elasticnet,
    lrnr_ranger100,
    xgb_learners[[31]], 
    xgb_learners[[32]], 
    xgb_learners[[33]],
    xgb_learners[[34]],
    xgb_learners[[40]],
    lrnr_ranger10, 
    xgb_learners[[50]], 
    xgb_learners[[60]], 
    full_lrn_earth_1,
    full_lrn_earth_2,
    full_lrn_earth_3,
    full_lrn_earth_4,
    full_lrn_poly_1,
    full_lrn_poly_2,
    full_lrn_poly_3,
    full_lrn_poly_4,
    full_lrn_poly_5,
    full_lrn_poly_6,
    full_lrn_poly_7
  )
  
  
  discrete_sl_metalrn <- Lrnr_cv_selector$new()
  
  discrete_sl <- Lrnr_sl$new(
    learners = stack,
    metalearner = discrete_sl_metalrn
  )
  ## fit the sl3 object
  sl_fit <- discrete_sl$train(task)
  
  # best_model <- sl_fit$fit_object$full_fit$learner_fits$Stack$learner_fits$Lrnr_ranger_200_TRUE_none_1$fit_object
  
  ## cross validate across the learners in sl3 object to see how learners perform 
  # CVsl <- CV_lrnr_sl(sl_fit, task, loss_squared_error)
  
  ## get variable importance from the sl3 object
  var_importance <- varimp_server(fit = sl_fit, 
                                  loss = loss_squared_error, 
                                  covars = covars, 
                                  outcome = outcome,
                                  data = data,
                                  data_dictionary = data_dictionary,
                                  label = label)
  
  SL_results <- list('fit' = sl_fit, 'var_imp' = var_importance)
  
  saveRDS(SL_results, here(paste("Analysis/update_data/data/processed/", outcome, ".RDS", sep = "")))
  
  return(list('var_imp' = var_importance))
}

# print(system.time(out <- foreach(i = outcomes[1:length(outcomes)]) %dopar% {
#   outSub <- run_sl3_poisson_lrns(outcome = i, data = covid_data_processed, covars = covars )
#   outSub # variable importancesdd
# }))

Casestodate <- run_sl3_poisson_lrns(outcome = "TotalCasesUpToDate", 
                                            data = covid_data_processed, 
                                            covars = covars,
                                            data_dictionary = Data_Dictionary, 
                                            label = "Total COVID Cases To-Date")

# ptm <- proc.time()
# ML_pipeline_output <- purrr::walk(.x = outcomes[1:length(outcomes)], 
#                           .f = run_sl3_poisson_lrns, 
#                           data = covid_data_processed, 
#                           covars = covars)
# proc.time() - ptm

# TotalCasesUpToDate <- readRDS("~/Documents/PhD/covid/Covid_SL_intxns/Covid_SL_intxns/Analysis/update_data/data/processed/TotalCasesUpToDate.RDS")
# TotalCasesUpToDate_risk <- TotalCasesUpToDate$var_imp$risk_results
# TotalCasesUpToDate_quantile <- TotalCasesUpToDate$var_imp$quantile_results
# 
# 




# saveRDS(ML_pipeline_output, here("Analysis/update_data/data/processed/ML_pipeline_092321.RDS"))

# plot_variable_importance <- function(input_df, plot_label, save_label){
#  
#   var_importance <- as.data.frame(input_df$var_imp)
#   colnames(var_importance) <- c("County_Features", "Risk_Ratio")
#   
#   var_importance$County_Features <- as.factor(var_importance$County_Features)
#   var_importance$County_Features <- factor(var_importance$County_Features, levels = var_importance$County_Features[order(var_importance$Risk_Ratio)])
#   
#   pdf(paste(here("Visulizations/var_imp/"),save_label , sep = ""))
#   
  # var_imp_plot <- var_importance %>%
  #   arrange(mtcars, cyl, disp)
  # 
  #   filter(Risk_Ratio > 1.01)  %>%
  #   ggplot(aes(x = County_Features, y = Risk_Ratio)) +
  #   geom_dotplot(binaxis = "y", dotsize = 0.25) +
  #   labs(x = "County Level Feature", y = "Risk Ratio",
  #        title = plot_label) +
  #   coord_flip() +
  #   theme_bw()
#   
#   print(var_imp_plot)
#   
#   dev.off()
# 
#   }

#plot_variable_importance(input_df = ML_pipeline_output[[1]], plot_label = "Day First Case Outcome", save_label = "day_first_case.pdf")
#plot_variable_importance(input_df = ML_pipeline_output[[2]], plot_label = "Cases Rate at Day 25 Outcome", save_label = "day_25_cases.pdf")
#plot_variable_importance(input_df = ML_pipeline_output[[3]], plot_label = "Cases Rate Total To Date Outcome", save_label = "tota_cases_2date.pdf")
#plot_variable_importance(input_df = ML_pipeline_output[[4]], plot_label = "Mortality at Day 100 Rate Outcome", save_label = "day_100_mortality.pdf")
#plot_variable_importance(input_df = ML_pipeline_output[[5]], plot_label = "Mortality Total Rate Outcome", save_label = "total_deaths_2date.pdf")

  