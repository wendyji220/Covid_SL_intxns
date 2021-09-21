actual_outcomes <- c(
  mean(data_original$FirstCaseDay),
  sum(data_original$CountyRelativeDay25Cases),
  sum(data_original$TotalCasesUpToDate),
  sum(data_original$USRelativeDay100Deaths),
  sum(data_original$TotalDeathsUpToDate)
)

############################################
##### Day 25 Cases ##########
############################################

joint_day25 <- joint_impact_day_25cases$no_subcat_sl_results$boot_CI_df_sl_nosubcat
colnames(joint_day25)[1] <- "Perc"
joint_day25$var <- "Joint Imp"

nwwi_day25 <- nwwi_impact_day_25cases$no_subcat_sl_results$boot_CI_df_sl_nosubcat
colnames(nwwi_day25)[1] <- "Perc"
nwwi_day25$var <- "nwwi"

so2_day25 <- so2_impact_day_25cases$no_subcat_sl_results$boot_CI_df_sl_nosubcat
colnames(so2_day25)[1] <- "Perc"
so2_day25$var <- "so2"

epl_day25 <- epl_min_impact_day_25cases$no_subcat_sl_results$boot_CI_df_sl_nosubcat
colnames(epl_day25)[1] <- "Perc"
epl_day25$var <- "EPL Minority"

hyper_day25 <- hypert_impact_day_25cases$no_subcat_sl_results$boot_CI_df_sl_nosubcat
colnames(hyper_day25)[1] <- "Perc"
hyper_day25$var <- "Hypertension"


joint_boot_df_day25 <- rbind(joint_day25, nwwi_day25, so2_day25, epl_day25, hyper_day25)


############################################
##### bring together total cases ##########
############################################

joint_totalcases <- joint_impact_day_total_cases$no_subcat_sl_results$boot_CI_df_sl_nosubcat
colnames(joint_totalcases)[1] <- "Perc"
joint_totalcases$var <- "Joint Imp"

nwwi_totalcases <- NWWI_impact_day_total_cases$no_subcat_sl_results$boot_CI_df_sl_nosubcat
colnames(nwwi_totalcases)[1] <- "Perc"
nwwi_totalcases$var <- "NWWI"

pm10_totalcases <- pm10_impact_day_total_cases$no_subcat_sl_results$boot_CI_df_sl_nosubcat
colnames(pm10_totalcases)[1] <- "Perc"
pm10_totalcases$var <- "pm10"

epm_limeng_totalcases <- epl_limeng_impact_day_total_cases$no_subcat_sl_results$boot_CI_df_sl_nosubcat
colnames(epm_limeng_totalcases)[1] <- "Perc"
epm_limeng_totalcases$var <- "Limited English"

epm_65_totalcases <- epl_65_impact_day_total_cases$no_subcat_sl_results$boot_CI_df_sl_nosubcat
colnames(epm_65_totalcases)[1] <- "Perc"
epm_65_totalcases$var <- "Proportion 65 or Older"


joint_boot_totalcases <- rbind(joint_totalcases, nwwi_totalcases, pm10_totalcases, epm_limeng_totalcases, epm_65_totalcases)


############################################
##### bring together day 100 data ##########
############################################

joint_100 <- joint_impact_day100_deaths$no_subcat_sl_results$boot_CI_df_sl_nosubcat
colnames(joint_100)[1] <- "Perc"
joint_100$var <- "Joint Imp"

no2_100 <- no2_impact_day100_deaths$no_subcat_sl_results$boot_CI_df_sl_nosubcat
colnames(no2_100)[1] <- "Perc"
no2_100$var <- "no2"

mask_100 <- mask_impact_day100_deaths$no_subcat_sl_results$boot_CI_df_sl_nosubcat
colnames(mask_100)[1] <- "Perc"
mask_100$var <- "mask"

black_100 <- prop_black_impact_day100_deaths$no_subcat_sl_results$boot_CI_df_sl_nosubcat
colnames(black_100)[1] <- "Perc"
black_100$var <- "Proportion Black"

income_100 <- income_impact_day100_deaths$no_subcat_sl_results$boot_CI_df_sl_nosubcat
colnames(income_100)[1] <- "Perc"
income_100$var <- "Income Inequality"


joint_boot_df_100 <- rbind(joint_100, no2_100, mask_100, black_100, income_100)

####################################################
##### bring together day total death data ##########
####################################################

joint_total <- joint_impact_day_all_deaths$no_subcat_sl_results$boot_CI_df_sl_nosubcat
colnames(joint_total)[1] <- "Perc"
joint_total$var <- "Joint Imp"

nwwi_total <- nwwi_impact_all_deaths$no_subcat_sl_results$boot_CI_df_sl_nosubcat
colnames(nwwi_total)[1] <- "Perc"
nwwi_total$var <- "NWWI"

black_total <- black_impact_all_deaths$no_subcat_sl_results$boot_CI_df_sl_nosubcat
colnames(black_total)[1] <- "Perc"
black_total$var <- "Proportion Black"

sex_total <- sex_all_deaths$no_subcat_sl_results$boot_CI_df_sl_nosubcat
colnames(sex_total)[1] <- "Perc"
sex_total$var <- "Sex"

hypert_total <- hypertension_all_deaths$no_subcat_sl_results$boot_CI_df_sl_nosubcat
colnames(hypert_total)[1] <- "Perc"
hypert_total$var <- "Hypertension"

mask_total <- masks_all_deaths$no_subcat_sl_results$boot_CI_df_sl_nosubcat
colnames(mask_total)[1] <- "Perc"
mask_total$var <- "mask"

joint_boot_df_total <- rbind(joint_total, nwwi_total, black_total, sex_total, hypert_total,mask_total)



plot_bootstrap_results <- function(boot_res,
                                   desc_outcome,
                                   desc_var,
                                   actual_outcome) {
  
  title <- as.character(desc_var)
  ylabel <- as.character(desc_outcome)
  
  boot_res$var <- as.factor(boot_res$var)
  file_name <- paste(desc_outcome, "_marginal_predictions_", title, ".png", sep = "")
  
  
  plot <- ggplot(boot_res, aes(x = boot_res[, "Perc"], y = `Boot Pred`, colour = var)) +
    geom_line(aes(linetype = `var`, colour = `var`)) +
    geom_point(aes(y = `Boot Pred`, colour = `var`)) +
    geom_errorbar(
      aes(ymin = `Boot Low`, ymax = `Boot High`, group = `var`),
      width = 0.05
    ) +
    xlab("Percent Reduced") +
    ylab("Toots") +
    labs(title = title) +
    geom_hline(yintercept = actual_outcome)

  
  ggsave(
    filename = file_name,
    plot = plot,
    device = NULL,
    path = here("/Visulizations/marginal_predictions"),
    scale = 1,
    width = NA,
    height = NA,
    units = c("in", "cm", "mm"),
    dpi = 300,
    limitsize = TRUE
  )
}

plot_bootstrap_results(boot_res = joint_boot_df_100, 
                       desc_outcome = "Deaths Day 100",
                       desc_var = "Aggregate",
                       actual_outcome = actual_outcomes[4])

plot_bootstrap_results(boot_res = joint_boot_df_day25, 
                       desc_outcome = "Day 25 Cases",
                       desc_var = "Aggregate",
                       actual_outcome = actual_outcomes[2])

plot_bootstrap_results(boot_res = joint_boot_totalcases, 
                       desc_outcome = "Total Cases",
                       desc_var = "Aggregate",
                       actual_outcome = actual_outcomes[3])


plot_bootstrap_results(boot_res = joint_boot_df_total, 
                       desc_outcome = "Total Deaths",
                       desc_var = "Aggregate",
                       actual_outcome = actual_outcomes[5])

##############################################################################################
## run linear models through one axis of the 3d arrays of the bootstrap in order to derive coefficients for predictions that look linear:

coef_list_of_lists <- list()
pval_list_of_lists <- list()

boot_data <- total_cases_by_minority_boot$no_subcat_sl_results$boot_sl_nosubcat_array


coef_list <- list()
pval_list <- list()
names(boot_data) <- percents

for (j in 1:dim(boot_data)[1]) {
  
  data <- boot_data[j, ]
  data <- t(data)
  data <- as.data.frame(data)
  data$perc <- as.numeric(rownames(data))
  colnames(data) <- c("Outcome", "Percent Changed")
  coef <- coef(lm(Outcome ~ `Percent Changed`, data = data))[[2]]
  pval <- summary(lm(Outcome ~ `Percent Changed`, data = data))[4][[1]][8]
  coef <- coef / 10
  coef_list[[j]] <- coef
  pval_list[[j]] <- pval
}

coef_list_of_lists[[i]] <- coef_list
pval_list_of_lists[[i]] <- pval_list

quantile(unlist(coef_list), probs = c(0.025, 0.50, 0.975))
quantile(unlist(pval_list), probs = c(0.025, 0.50, 0.975))

lm_boot_results <- map(.x = coef_list_of_lists, ~quantile(unlist(.x),  probs = c(0.025, 0.50, 0.975)))
lm_pval_boot_results <- map(.x = pval_list_of_lists, ~quantile(unlist(.x),  probs = c(0.025, 0.50, 0.975)))

lm_day_first_case <- lm_boot_results[1]
lm_day_25_cases <- lm_boot_results[2]
lm_total_to_date_cases <- lm_boot_results[3]
lm_day_100_deaths <- lm_boot_results[4]
lm_total_to_date_deaths <- lm_boot_results[5]

