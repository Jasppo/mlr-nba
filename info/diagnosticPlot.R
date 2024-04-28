plotChecks <- function(myModel, myData, myY) {
  
  
  stdTheme <- theme_gray() +
    theme(
      plot.title = element_text(size = 12, face = "bold", color = "darkblue"), 
      plot.subtitle = element_text(size = 10),
      axis.title = element_text(size = 10, face = "bold"),
      axis.text = element_text(size = 8),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8),
      strip.text = element_text(size = 10, face = "bold")
    )
  
  # Slight curvature
  aLinear <- tibble(x = myModel$fitted.values, y = myY) %>% 
    ggplot(aes(x = x, y = y)) +
    geom_point() +
    stat_smooth(method = "loess") +
    labs(x = "Fitted Values", y = "Observed Values", 
         title = "Fitted versus Observed Values") +
    stdTheme
  
  aVar <- tibble(y = rstandard(myModel), x = myY) %>% 
    ggplot(aes(x = x, y = y)) +
    geom_point() +
    geom_hline(yintercept = c(-4, 0, 4), color = "red", linetype = "dashed") +
    stat_smooth(method = "loess", se = T) +
    labs(x = "Fitted Values", y = "Standardized Residuals", 
         title = "Standardized Residuals versus Fitted Values") +
    stdTheme
  
  aNormality <- tibble(x = rstandard(myModel)) %>% 
    ggplot(aes(sample = x)) +
    geom_qq() +
    geom_qq_line(color = "blue") +
    labs(x = "Theoretical Quantiles", y = "Sample Quantiles", 
         title = "Normal Q-Q Plot") +
    stdTheme
  
  
  aLev <- tibble(y = rstandard(myModel), x = hatvalues(myModel)) %>% 
    ggplot(aes(x = x, y = y)) +
    geom_point() +
    geom_hline(yintercept = c(-4, 0, 4), color = "red", linetype = "dashed") +
    geom_vline(xintercept = 2*myModel$rank/nrow(myData), color = "red", linetype = "dashed") +
    labs(x = "Leverage Points", y = "Standardized Residuals", 
         title = "Standardized Residuals versus Leverage Points") +
    stdTheme
  
  
  cowplot::plot_grid(aLinear, aVar, aNormality, aLev, ncol = 2, nrow = 2)
  
}