#Code creates function that can make OR/IRR tables from models. Change the family to "binomial", or "logit" to switch between 
makeIRRTable <- function(mod, predictors=NULL, ref_levels = NULL, dp = 3, categorical_variables = NULL) {
  mod_exp <- (mod$family$family == "poisson")
  
  if (class(mod)[1] == "gam") {
    tab <- as.data.frame(summary.gam(mod)$p.table)
    tab$Lower = tab$Estimate - 1.96 * tab$`Std. Error`
    tab$Upper = tab$Estimate + 1.96 * tab$`Std. Error`
    tab <- tab %>% dplyr::select(Estimate, Lower, Upper, `Pr(>|z|)`)
    tab$Level <- rownames(tab)
    tab <- tab %>% dplyr::select(Level, everything())
    colnames(tab) <- c("Level", "OR", "Lower", "Upper", "P_value")
    
    if (!is.null(categorical_variables)) {
      for (i in seq_along(categorical_variables)) {
        ref_level_name <- ifelse(is.null(ref_levels), 
                                 paste0(categorical_variables[i], " [reference]"),
                                 paste0(ref_levels[i], " [reference]"))
        tab <- rbind(tab, c(ref_level_name, 1, NA_real_, NA_real_, NA_real_))
      }
    }
    if (!is.null(predictors)) {
      # Repeat predictor names for each category
      num_categories <- length(unique(mod$Level))
      predictors <- rep(predictors, each = num_categories)
    }
    
    tab[, 2] <- round(exp(as.numeric(tab[, 2])), dp)
    tab[, 3] <- round(exp(as.numeric(tab[, 3])), dp)
    tab[, 4] <- round(exp(as.numeric(tab[, 4])), dp)
    tab[, 5] <- round(as.numeric(tab[, 5]), 5)
  } else {
    tab <- jtools::summ(mod, exp = mod_exp, ORs = mod_exp)
    
    if (mod_exp) {
      tab <- tab$coeftable %>% as.data.frame() %>% dplyr::select(1, 2, 3, 5)
    } else {
      tab <- tab$coeftable %>% as.data.frame() %>% dplyr::select(1, 2, 4)
      tab$Lower = tab$Est. - qnorm(p = 0.975, mean = 0, sd = 1) * tab$S.E.
      tab$Upper = tab$Est. + qnorm(p = 0.975, mean = 0, sd = 1) * tab$S.E.
      tab <- tab[, c(1, 4, 5, 3)]
    }
    
    tab$Level <- rownames(tab)
    tab <- tab %>% dplyr::select(Level, everything())
    colnames(tab) <- c("Level", "OR", "Lower", "Upper", "P_value")
    
    if (!is.null(categorical_variables)) {
      for (i in seq_along(categorical_variables)) {
        ref_level_name <- ifelse(is.null(ref_levels), 
                                 paste0(categorical_variables[i], " [reference]"),
                                 paste0(ref_levels[i], " [reference]"))
        tab <- rbind(tab, c(ref_level_name, 1, NA_real_, NA_real_, NA_real_))
      }
    }
    
    # Add a column for predictors
    if (!is.null(predictors)) {
      tab$Predictor <- predictors
    }
    tab[, 2] <- round(as.numeric(tab[, 2]), dp)
    tab[, 3] <- round(as.numeric(tab[, 3]), dp)
    tab[, 4] <- round(as.numeric(tab[, 4]), dp)
    tab[, 5] <- round(as.numeric(tab[, 5]), 5)
  }
  
  rownames(tab) <- 1:nrow(tab)
  return(tab)
}

test2 <- makeORTable2(poisson_model,predictors=predictors,
                      ref_levels=ref_levels, categorical_variables=categorical_variables)
