# install.packages("ggtext")

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
library(ggtext)

############################################################
# 1. Load and prepare data -- unchanged
############################################################

data <- read_csv(file.path("Data", "TStrend_Variables.csv"))

datatrain <- data[c(
  "FCI_trend_mean", "FC_trend_mean", "FHI_trend_mean",
  "AMT", "ATP", "DEM", "Slope", "DroughtIntensity",
  "WindstormIntensity", "WildfireIntensity", "FAarea", "PA", "FMI",
  "Accessibility2City", "DePOPfraction"
)]

names(datatrain) <- c(
  "FCI_trend_mean", "FC_trend_mean", "FHI_trend_mean",
  "AMT", "ATP", "DEM", "Slope", "DroughtIntensity",
  "WindstormIntensity", "WildfireIntensity", "FormalCroplandFraction",
  "ProtectionAreaFraction", "ForestManagementIntensity",
  "Accessibility2City", "DePOPfraction"
)

datatrain <- na.omit(datatrain)

############################################################
# 2. Predictors and plotting order
############################################################

predictors <- c(
  "AMT", "ATP", "DEM", "Slope", "DroughtIntensity",
  "WildfireIntensity", "FormalCroplandFraction",
  "ProtectionAreaFraction", "ForestManagementIntensity",
  "Accessibility2City", "DePOPfraction"
)

predictors_named <- c(
  "AMT", "ATP", "Elevation", "Slope", "Drought intensity",
  "Wildfire intensity", "Former cropland fraction",
  "Protection area fraction", "Forest management intensity",
  "Accessibility to City", "Depopulation fraction"
)

name_map <- setNames(predictors_named, predictors)

# Topography -> Climate -> Disturbance ->
# Land management -> Depopulation -> Accessibility
plot_terms <- c(
  "DEM",
  "Slope",
  "AMT",
  "ATP",
  "DroughtIntensity",
  "WildfireIntensity",
  "ProtectionAreaFraction",
  "ForestManagementIntensity",
  "FormalCroplandFraction",
  "DePOPfraction",
  "Accessibility2City"
)

plot_names <- unname(name_map[plot_terms])

plot_groups <- c(
  "Topography", "Topography",
  "Climate", "Climate",
  "Disturbance", "Disturbance",
  "Land management", "Land management", "Land management",
  "Socioeconomic", "Socioeconomic"
)

group_colors <- c(
  "Topography"      = "#404040",
  "Climate"         = "#C04F15",
  "Disturbance"     = "#A02B93",
  "Land management" = "#3B7D23",
  "Socioeconomic"   = "#28637f"
)

y_labels <- setNames(
  sprintf(
    "<span style='color:%s;'>%s</span>",
    unname(group_colors[plot_groups]),
    plot_names
  ),
  plot_names
)

n_terms <- length(plot_terms)
group_ends <- head(cumsum(rle(plot_groups)$lengths), -1)
group_separators <- n_terms - group_ends + 0.5

effect_colors <- c(
  "Negative" = "#3B6FB6",
  "Positive" = "#B54A4A"
)

############################################################
# 3. Format actual model P values
############################################################

format_log_p <- function(log_p) {
  
  if (is.na(log_p)) return("NA")
  
  if (log_p == -Inf) {
    return("0 (numerical underflow)")
  }
  
  if (log_p >= log(0.001)) {
    return(sprintf("%.3g", exp(log_p)))
  }
  
  exponent <- floor(log_p / log(10))
  mantissa <- exp(log_p - exponent * log(10))
  mantissa <- round(mantissa, 2)
  
  if (mantissa >= 10) {
    mantissa <- mantissa / 10
    exponent <- exponent + 1
  }
  
  sprintf("%.2fe%+.0f", mantissa, exponent)
}

############################################################
# 4. Main modelling function
############################################################

run_effect_model <- function(
    response_var,
    datatrain,
    predictors,
    show_y = TRUE
) {
  
  cat("\n==============================\n")
  cat("Running model for:", response_var, "\n")
  cat("==============================\n")
  
  ##########################################################
  # Step 1: Subset data -- unchanged
  ##########################################################
  df <- datatrain[, c(response_var, predictors)] %>%
    na.omit()
  
  ##########################################################
  # Step 2: Correlation filtering -- unchanged
  ##########################################################
  cor_mat <- cor(df[, predictors])
  
  high_cor <- findCorrelation(
    cor_mat,
    cutoff = 0.7,
    names = TRUE
  )
  
  predictors_filtered <- setdiff(predictors, high_cor)
  
  cat("Removed due to high correlation:\n")
  print(high_cor)
  
  ##########################################################
  # Step 3: VIF filtering -- unchanged
  ##########################################################
  vif_threshold <- 5
  
  repeat {
    
    formula_str <- paste(
      response_var,
      "~",
      paste(predictors_filtered, collapse = "+")
    )
    
    model <- lm(as.formula(formula_str), data = df)
    
    vif_values <- vif(model)
    
    if (max(vif_values) < vif_threshold) break
    
    remove_var <- names(which.max(vif_values))
    
    cat(
      "Removing:", remove_var,
      "VIF =", max(vif_values), "\n"
    )
    
    predictors_filtered <- setdiff(
      predictors_filtered,
      remove_var
    )
  }
  
  cat("Final predictors:\n")
  print(predictors_filtered)
  
  ##########################################################
  # Step 4: Standardize variables -- unchanged
  ##########################################################
  df_std <- df[, c(response_var, predictors_filtered)] %>%
    mutate(across(everything(), scale))
  
  ##########################################################
  # Step 5: Fit linear model -- unchanged
  ##########################################################
  formula_str <- paste(
    response_var,
    "~",
    paste(predictors_filtered, collapse = "+")
  )
  
  model_std <- lm(
    as.formula(formula_str),
    data = df_std
  )
  
  model_summary <- summary(model_std)
  adj_r2 <- model_summary$adj.r.squared
  
  f_stat <- model_summary$fstatistic
  
  model_log_p <- pf(
    unname(f_stat["value"]),
    df1 = unname(f_stat["numdf"]),
    df2 = unname(f_stat["dendf"]),
    lower.tail = FALSE,
    log.p = TRUE
  )
  
  ##########################################################
  # Step 6: Bootstrap coefficients -- unchanged
  ##########################################################
  boot_fun <- function(data, indices) {
    
    d <- data[indices, ]
    
    fit <- lm(
      as.formula(formula_str),
      data = d
    )
    
    coef(fit)[-1]
  }
  
  set.seed(123)
  
  boot_res <- boot(
    df_std,
    boot_fun,
    R = 500
  )
  
  effect_mean <- colMeans(boot_res$t)
  
  effect_ci <- apply(
    boot_res$t,
    2,
    function(x) {
      quantile(x, c(0.025, 0.975))
    }
  )
  
  ##########################################################
  # Step 7: Coefficient P values -- unchanged
  ##########################################################
  coef_p <- tidy(model_std) %>%
    dplyr::filter(term != "(Intercept)") %>%
    dplyr::select(term, p.value)
  
  ##########################################################
  # Step 8: Merge results and create labels
  ##########################################################
  coef_df <- data.frame(term = predictors) %>%
    left_join(
      data.frame(
        term = predictors_filtered,
        effect = effect_mean,
        lower = effect_ci[1, ],
        upper = effect_ci[2, ]
      ),
      by = "term"
    ) %>%
    left_join(
      coef_p,
      by = "term"
    ) %>%
    mutate(
      term_named = factor(
        name_map[term],
        levels = rev(plot_names)
      ),
      
      signif = case_when(
        is.na(p.value) ~ "",
        p.value < 0.001 ~ "***",
        p.value < 0.01  ~ "**",
        p.value < 0.05  ~ "*",
        TRUE ~ ""
      ),
      
      effect_label = ifelse(
        is.na(effect),
        "",
        paste0(sprintf("%.2f", effect), signif)
      ),
      
      direction = ifelse(
        effect > 0,
        "Positive",
        "Negative"
      )
    )
  
  cat(
    "Adj. R2 =", sprintf("%.4f", adj_r2),
    "; model P =", format_log_p(model_log_p),
    "\n"
  )
  
  return(list(
    coef_df = coef_df,
    adj_r2 = adj_r2,
    model_log_p = model_log_p,
    show_y = show_y
  ))
}

############################################################
# 5. Calculate a common x-axis range
############################################################

get_axis_settings <- function(results) {
  
  all_coef <- bind_rows(
    lapply(results, function(x) x$coef_df)
  )
  
  bounds <- c(
    0,
    all_coef$effect,
    all_coef$lower,
    all_coef$upper
  )
  
  bounds <- bounds[is.finite(bounds)]
  
  data_range <- range(bounds)
  data_span <- max(diff(data_range), 0.2)
  
  max_chars <- max(nchar(all_coef$effect_label))
  
  label_offset <- 0.025 * data_span
  
  label_padding <- max(
    0.30,
    0.045 * max_chars
  ) * data_span
  
  return(list(
    limits = data_range +
      c(-1, 1) * (label_offset + label_padding),
    
    label_offset = label_offset
  ))
}

############################################################
# 6. Visualization
############################################################

make_effect_plot <- function(
    result,
    axis_settings,
    panel_tag = NULL,
    show_groups = FALSE
)  {
  
  coef_df <- result$coef_df %>%
    mutate(
      label_x = ifelse(
        effect > 0,
        pmax(effect, upper) + axis_settings$label_offset,
        pmin(effect, lower) - axis_settings$label_offset
      ),
      
      label_hjust = ifelse(
        effect > 0,
        0,
        1
      )
    )
  
  stat_label <- paste0(
    "Adj. R\u00b2 = ",
    sprintf("%.2f", result$adj_r2),
    "\nP = ",
    format_log_p(result$model_log_p)
  )
  
  x_limits <- axis_settings$limits
  
  p <- ggplot(
    coef_df,
    aes(
      x = effect,
      y = term_named,
      color = direction
    )
  ) +
    
    geom_hline(
      yintercept = group_separators,
      linetype = "dashed",
      color = "grey75",
      linewidth = 0.45
    ) +
    
    geom_vline(
      xintercept = 0,
      linetype = "dashed",
      color = "grey65",
      linewidth = 0.55
    ) +
    
    geom_errorbar(
      aes(
        xmin = lower,
        xmax = upper
      ),
      orientation = "y",
      width = 0.18,
      linewidth = 0.8,
      na.rm = TRUE
    ) +
    
    geom_point(
      size = 3,
      na.rm = TRUE
    ) +
    
    geom_point(
      data = coef_df %>%
        dplyr::filter(is.na(effect)),
      
      aes(
        x = 0,
        y = term_named
      ),
      
      inherit.aes = FALSE,
      color = "grey85",
      size = 2
    ) +
    
    geom_text(
      aes(
        x = label_x,
        label = effect_label,
        hjust = label_hjust
      ),
      size = 4,
      fontface = "bold",
      na.rm = TRUE
    ) +
    
    scale_color_manual(
      values = effect_colors
    ) +
    
    scale_y_discrete(
      limits = rev(plot_names),
      labels = y_labels,
      drop = FALSE,
      
      expand = expansion(
        add = c(0.6, 2.1)
      )
    ) +
    
    scale_x_continuous(
      breaks = function(x) pretty(x, n = 4),
      
      labels = function(x) {
        ifelse(
          abs(x) < 1e-10,
          "0",
          format(
            x,
            trim = TRUE,
            scientific = FALSE
          )
        )
      },
      
      expand = expansion(mult = 0)
    ) +
    
    coord_cartesian(
      xlim = x_limits,
      clip = "off"
    ) +
    
    labs(
      x = "Standardized effect size (\u03b2)",
      y = NULL
    ) +
    
    annotate(
      "text",
      
      x = x_limits[1] + 0.16 * diff(x_limits),
      
      y = n_terms + 1.2,
      label = stat_label,
      
      hjust = 0,
      vjust = 0.5,
      size = 4,
      lineheight = 1.15
    ) +
    
    theme_classic(
      base_size = 16
    ) +
    
    theme(
      legend.position = "none",
      
      panel.border = element_rect(
        color = "grey45",
        fill = NA,
        linewidth = 0.65
      ),
      
      axis.line = element_blank(),
      
      axis.text.x = element_text(
        size = 12,
        color = "grey25"
      ),
      
      axis.title.x = element_text(
        size = 14,
        margin = margin(t = 10)
      ),
      
      axis.text.y = if (result$show_y) {
        ggtext::element_markdown(
          size = 14,
          margin = margin(r = 7)
        )
      } else {
        element_blank()
      },
      
      axis.ticks.y = if (result$show_y) {
        element_line(
          color = "grey50",
          linewidth = 0.4
        )
      } else {
        element_blank()
      },
      
      plot.margin = margin(
        t = 12,
        r = 12,
        b = 10,
        l = 8
      )
    )
  
  if (!is.null(panel_tag)) {
    
    p <- p +
      annotate(
        "text",
        x = x_limits[1] + 0.035 * diff(x_limits),
        y = Inf,
        label = panel_tag,
        hjust = 0,
        vjust = 1.25,
        size = 5.2,
        fontface = "bold",
        color = "black"
      )
  }
  
  if (show_groups) {
    
    group_names <- unique(plot_groups)
    
    group_centres <- vapply(
      group_names,
      function(g) {
        mean(n_terms - which(plot_groups == g) + 1)
      },
      numeric(1)
    )
    
    group_labels <- ifelse(
      group_names == "Socioeconomic",
      "Human activity",
      group_names
    )
    
    p <- p +
      annotate(
        "text",
        x = x_limits[2] + 0.065 * diff(x_limits),
        y = group_centres,
        label = group_labels,
        color = unname(group_colors[group_names]),
        angle = 270,
        size = 3.8,
        fontface = "bold",
        hjust = 0.5,
        vjust = 0.5
      ) +
      theme(
        plot.margin = margin(
          t = 12,
          r = 55,
          b = 10,
          l = 8
        )
      )
  }
  
  return(p)
}

############################################################
# 7. Run models -- unchanged
############################################################

res_FC <- run_effect_model(
  "FC_trend_mean",
  datatrain,
  predictors,
  TRUE
)

res_FCI <- run_effect_model(
  "FCI_trend_mean",
  datatrain,
  predictors,
  FALSE
)

res_FHI <- run_effect_model(
  "FHI_trend_mean",
  datatrain,
  predictors,
  FALSE
)

common_axis <- get_axis_settings(
  list(res_FC, res_FCI, res_FHI)
)

res_FC$plot <- make_effect_plot(
  res_FC,
  common_axis,
  panel_tag = "a"
)

res_FCI$plot <- make_effect_plot(
  res_FCI,
  common_axis,
  panel_tag = "b"
)

res_FHI$plot <- make_effect_plot(
  res_FHI,
  common_axis,
  panel_tag = "c",
  show_groups = TRUE
)

combined_plot <- (
  res_FC$plot |
    res_FCI$plot |
    res_FHI$plot
)

print(combined_plot)

############################################################
# 9. Save figure
############################################################

ggsave(
  filename = file.path("outputs", "Figure4.png"),
  plot = combined_plot,
  
  width = 14,
  height = 6.5,
  
  dpi = 300,
  bg = "white"
)




