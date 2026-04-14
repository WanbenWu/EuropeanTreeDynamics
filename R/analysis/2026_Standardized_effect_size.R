############################################################
# 0. Load required packages
############################################################
library(dplyr)
library(ggplot2)
library(caret)
library(car)
library(broom)
library(boot)
library(readr)
library(patchwork)

############################################################
# 1. Load and prepare data
############################################################
setwd('O:/Nat_Ecoinformatics/C_Write/_User/WanbenWu_au749682/Forest structure EU/2025/Fig4_BRT Model/2025_mean_trend_condition/')
data <- read_csv("TStrend_Variables_updatedWind.csv")

# Select response variables and predictors
datatrain <- data[c('FCI_trend_mean','FC_trend_mean','FHI_trend_mean',
                    'AMT','ATP','DEM','Slope','DroughtIntensity',
                    'WindstormIntensity','WildfireIntensity','FAarea','PA','FMI',
                    'Accessibility2City','DePOPfraction')]

# Rename variables for clarity
names(datatrain) <- c('FCI_trend_mean','FC_trend_mean','FHI_trend_mean',
                      'AMT','ATP','DEM','Slope','DroughtIntensity',
                      'WindstormIntensity','WildfireIntensity','FormalCroplandFraction',
                      'ProtectionAreaFraction','ForestManagementIntensity',
                      'Accessibility2City','DePOPfraction')

# Remove missing values
datatrain <- na.omit(datatrain)

############################################################
# 2. Define predictors and display names
############################################################
predictors <- c('AMT','ATP','DEM','Slope','DroughtIntensity',
                'WildfireIntensity','FormalCroplandFraction',
                'ProtectionAreaFraction','ForestManagementIntensity',
                'Accessibility2City','DePOPfraction')

# Human-readable names for plotting
predictors_named <- c('AMT','ATP','Elevation','Slope','Drought intensity',
                      'Wildfire intensity','Formal cropland fraction',
                      'Protection area fraction','Forest management intensity',
                      'Accessibility to City','Depopulation fraction')

name_map <- setNames(predictors_named, predictors)

############################################################
# 3. Main modeling function
############################################################
run_effect_model <- function(response_var, datatrain, predictors, show_y = TRUE){
  
  cat("\n==============================\n")
  cat("Running model for:", response_var, "\n")
  cat("==============================\n")
  
  ##########################################################
  # Step 1: Subset data
  ##########################################################
  df <- datatrain[, c(response_var, predictors)] %>%
    na.omit()
  
  ##########################################################
  # Step 2: Remove highly correlated predictors
  ##########################################################
  cor_mat <- cor(df[, predictors])
  high_cor <- findCorrelation(cor_mat, cutoff = 0.7, names = TRUE)
  predictors_filtered <- setdiff(predictors, high_cor)
  
  cat("Removed due to high correlation:\n")
  print(high_cor)
  
  ##########################################################
  # Step 3: Variance Inflation Factor (VIF) filtering
  ##########################################################
  vif_threshold <- 5
  
  repeat {
    formula_str <- paste(response_var, "~", paste(predictors_filtered, collapse = "+"))
    model <- lm(as.formula(formula_str), data = df)
    
    vif_values <- vif(model)
    if (max(vif_values) < vif_threshold) break
    
    remove_var <- names(which.max(vif_values))
    cat("Removing:", remove_var, "VIF =", max(vif_values), "\n")
    
    predictors_filtered <- setdiff(predictors_filtered, remove_var)
  }
  
  cat("Final predictors:\n")
  print(predictors_filtered)
  
  ##########################################################
  # Step 4: Standardize variables
  ##########################################################
  df_std <- df[, c(response_var, predictors_filtered)] %>%
    mutate(across(everything(), scale))
  
  ##########################################################
  # Step 5: Fit linear model
  ##########################################################
  formula_str <- paste(response_var, "~", paste(predictors_filtered, collapse = "+"))
  model_std <- lm(as.formula(formula_str), data = df_std)
  
  model_summary <- summary(model_std)
  adj_r2 <- model_summary$adj.r.squared
  
  ##########################################################
  # Step 6: Bootstrap estimation of coefficients
  ##########################################################
  boot_fun <- function(data, indices) {
    d <- data[indices, ]
    fit <- lm(as.formula(formula_str), data = d)
    return(coef(fit)[-1])
  }
  
  set.seed(123)
  boot_res <- boot(df_std, boot_fun, R = 500)
  
  effect_mean <- colMeans(boot_res$t)
  effect_ci <- apply(boot_res$t, 2, function(x) {
    quantile(x, c(0.025, 0.975))
  })
  
  ##########################################################
  # Step 7: Extract p-values
  ##########################################################
  coef_p <- tidy(model_std) %>%
    dplyr::filter(term != "(Intercept)") %>%
    dplyr::select(term, p.value)
  
  ##########################################################
  # Step 8: Merge results and create labels
  ##########################################################
  full_df <- data.frame(term = predictors)
  
  coef_df <- full_df %>%
    left_join(
      data.frame(
        term = predictors_filtered,
        effect = effect_mean,
        lower = effect_ci[1, ],
        upper = effect_ci[2, ]
      ),
      by = "term"
    ) %>%
    left_join(coef_p, by = "term") %>%
    mutate(
      term_named = name_map[term],
      term_named = factor(term_named, levels = rev(predictors_named)),
      signif = case_when(
        is.na(p.value) ~ "",
        p.value < 0.001 ~ "***",
        p.value < 0.01  ~ "**",
        p.value < 0.05  ~ "*",
        TRUE ~ ""
      ),
      # Label for standardized effect size with significance
      effect_label = ifelse(
        is.na(effect),
        "",
        paste0(sprintf("%.2f", effect), signif)
      )
    )
  
  ##########################################################
  # Step 9: Visualization
  ##########################################################
  p <- ggplot(coef_df, aes(x = effect, y = term_named, color = effect > 0)) +
    
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    
    geom_errorbarh(aes(xmin = lower, xmax = upper),
                   height = 0.2, size = 1, na.rm = TRUE) +
    
    geom_point(size = 3, na.rm = TRUE) +
    
    # Placeholder for excluded predictors
    geom_point(
      data = coef_df %>% filter(is.na(effect)),
      aes(x = 0, y = term_named),
      color = "grey85",
      size = 2
    ) +
    
    # Add effect size labels
    geom_text(aes(label = effect_label),
              hjust = ifelse(coef_df$effect > 0, -0.2, 1.2),
              size = 4,
              fontface = "bold",
              na.rm = TRUE) +
    
    scale_color_manual(values = c("#3B6FB6", "#B54A4A")) +
    
    scale_x_continuous(
      limits = c(-0.7, 0.7),
      labels = function(x) ifelse(abs(x) < 1e-8, "0", sprintf("%.1f", x))
    ) +
    
    labs(
      x = "Standardized effect size (β)",
      y = NULL
    ) +
    
    annotate("text",
             x = -0.65,
             y = length(predictors_named)-3,
             label = paste0("Adj. R² = ", sprintf("%.2f", adj_r2), "\n", "p < 0.001"),
             hjust = 0,
             size = 4) +
    
    theme_classic(base_size = 16) +
    theme(
      legend.position = "none",
      axis.text.y = if (show_y) element_text(size = 14) else element_blank(),
      axis.ticks.y = if (show_y) element_line() else element_blank()
    )
  
  return(list(plot = p))
}

############################################################
# 4. Run models for each response variable
############################################################
res_FC  <- run_effect_model("FC_trend_mean",  datatrain, predictors, TRUE)
res_FCI <- run_effect_model("FCI_trend_mean", datatrain, predictors, FALSE)
res_FHI <- run_effect_model("FHI_trend_mean", datatrain, predictors, FALSE)

############################################################
# 5. Combine plots into a single figure
############################################################
combined_plot <- (res_FC$plot | res_FCI$plot | res_FHI$plot) +
  plot_annotation(tag_levels = "a")

combined_plot

############################################################
# 6. Save figure
############################################################
ggsave(
  filename = "Combined_Forest_Trends.png",
  plot = combined_plot,
  width = 12,
  height = 6,
  dpi = 300
)