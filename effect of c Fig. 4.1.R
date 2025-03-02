library(ggplot2)
library(dplyr)
library(tidyr)

Loss <- function(dY, dQ, dTau, c = NA) {
  if (is.na(c)) {
    if (dY < dQ) return((dTau - 1) * (dY - dQ))
    if (dY > dQ) return(dTau * (dY - dQ))
    return(0)
  } else {
    return((dTau - 1 / (1 + exp((dY - dQ) / c))) * (dY - dQ))
  }
}

x_vals <- seq(-1, 1, length.out = 1000)
c_values <- c(NA, 0.1, 0.5, 1, 2, 5, 10)


df <- expand.grid(x = x_vals, c = c_values) %>%
  mutate(c = ifelse(is.na(c), "smooth=FALSE", as.character(c)),  
         loss = mapply(Loss, x, 0, 0.3, ifelse(c == "smooth=FALSE", NA, as.numeric(c)))) 

df$c <- factor(df$c, levels = c("smooth=FALSE", as.character(c_values[-1])))


ggplot(df, aes(x = x, y = loss, color = c, linetype = c)) +
  geom_line(size = 1) +
  scale_color_manual(values = gray.colors(length(c_values), start = 0.1, end = 0.7)) + 
  scale_linetype_manual(values = 1:length(c_values)) + 
  labs(title = "Effect of c on the Loss Function ",
       x = "Error (dY - dQ)",
       y = "Loss",
       color = "c values", linetype = "c values") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),  
        legend.position = "right") 