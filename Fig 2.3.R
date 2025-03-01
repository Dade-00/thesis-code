library(car)
data("Salaries")

library(quantreg)
library(ggplot2)
library(dplyr)

Salaries <- Salaries %>%
  filter(is.finite(log(yrs.service)), is.finite(log(salary))) %>%
  mutate(
    log_exp = log(yrs.service),
    experience_group = cut(yrs.service, breaks=c(0,4,8,13,18,23,29,36,65), include.lowest=TRUE)
    )

# log linear regression quantile models
tau_values <- c(0.25, 0.5, 0.75)
models <- lapply(tau_values, function(tau) {
  rq(log(salary) ~ log_exp, data = Salaries, tau = tau)
})

# Predictions
log_exp_seq <- seq(min(Salaries$log_exp, na.rm = TRUE), max(Salaries$log_exp, na.rm = TRUE), length.out = 100)
predictions <- do.call(rbind, lapply(1:length(models), function(i) {
  data.frame(
    log_exp = log_exp_seq,
    salary_pred = exp(predict(models[[i]], newdata = data.frame(log_exp = log_exp_seq))),
    tau = tau_values[i]
  )
}))

# Plot
ggplot(Salaries, aes(x=yrs.service, y = salary)) +
  geom_boxplot(aes(group = experience_group),
               alpha = 0.7, fill = "gray80",outlier.shape = NA) +  
  geom_line(data = predictions, aes(x = exp(log_exp), y = salary_pred, color = factor(tau)), size = 1) +
  labs(
    title = "Salary Distribution with Log-Linear Quantile Regression",
    x = "Years of Service",
    y = "Salary ($)",
    color = "Quantile"
  ) +
  scale_color_manual(values = c("red", "green", "blue")) +
  theme_minimal()
