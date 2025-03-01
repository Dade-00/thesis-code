news_impact_curve = function(beta, model) {
  
  lagVaR = 1.645  
  y = seq(-10, 10, by = 0.05)  
  
  if (model == "SAV") {
    impact_curve = beta[1] + beta[2] * lagVaR + beta[3] * abs(y)
    
  } else if (model == "AS") {
    impact_curve = beta[1] + beta[2] * lagVaR + beta[3] * abs(y) * (y > 0) + beta[4] * abs(y) * (y < 0)
    
  } else if (model == "GARCH") {
    impact_curve = sqrt(beta[1] + beta[2] * lagVaR^2 + beta[3] * y^2)
    
  } else if (model == "Adaptive") {
    indicator = exp(10 * (y + lagVaR))
    THETA = 0.05
    impact_curve = lagVaR + beta * (1 / (1 + indicator) - THETA)
    
  } else {
    stop("Unknown model")
  }
  
  df = data.frame(y = y, impact_curve = impact_curve, Model = model)
  return(df)
}

# Estimated parameters as in caviar models.R
beta_sav = par_SAV
beta_as = par_AS
beta_garch = par_inv
beta_adaptive = par_adaptive


df_sav = news_impact_curve(beta_sav, "SAV")
df_as = news_impact_curve(beta_as, "AS")
df_garch = news_impact_curve(beta_garch, "GARCH")
df_adaptive = news_impact_curve(beta_adaptive, "Adaptive")

df_all = rbind(df_sav, df_as, df_garch, df_adaptive)


# Figure 2.7
y_min = min(df_sav$impact_curve, df_as$impact_curve, df_garch$impact_curve, df_adaptive$impact_curve)
y_max = max(df_sav$impact_curve, df_as$impact_curve, df_garch$impact_curve, df_adaptive$impact_curve)

par(mfrow = c(2, 2))
plot(df_sav$y, df_sav$impact_curve, type = "l", col = "black", lwd = 2,
     main = "SAV Model", xlab = "Lagged Return", ylab = "Current VaR",
     ylim = c(y_min, y_max))

plot(df_as$y, df_as$impact_curve, type = "l", col = "black", lwd = 2,
     main = "AS Model", xlab = "Lagged Return", ylab = "Current VaR",
     ylim = c(y_min, y_max))

plot(df_garch$y, df_garch$impact_curve, type = "l", col = "black", lwd = 2,
     main = "GARCH Model", xlab = "Lagged Return", ylab = "Current VaR",
     ylim = c(y_min, y_max))

plot(df_adaptive$y, df_adaptive$impact_curve, type = "l", col = "black", lwd = 2,
     main = "Adaptive Model", xlab = "Lagged Return", ylab = "Current VaR",
     ylim=c(y_min,6),xlim=c(-5,5))

par(mfrow = c(1,1))