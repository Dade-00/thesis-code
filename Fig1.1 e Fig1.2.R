library(ggplot2)

# Figure 1.1
loss <- function(x, tau) {
  x * (tau - ifelse(x < 0, 1, 0))
}

x_values <- seq(-2, 2, by = 0.001)  
tau <- 0.3                        
loss_values <- loss(x_values, tau) 

data <- data.frame(x = x_values, loss = loss_values)

ggplot(data, aes(x = x, y = loss)) +
  geom_line(color = "black", linewidth = 1.2) +         
  geom_vline(xintercept = 0, color = "black") +
  geom_hline(yintercept = 0, color = "black") + 
  labs(
       x = expression(italic(u)), 
       y = expression(Loss(italic(u), tau))) +    
  theme_minimal(base_size = 14) +                
  theme(plot.title = element_text(hjust = 0.5))  


# Figure 1.2
library(quantreg)

dgp=function(b_0=1,b_1=1.5,x,eps){
  y=b_0+b_1*x+eps
}
x=seq(from=0,to=5,length.out=70)

set.seed(10)
eps=rnorm(70)
y=dgp(x=x,eps=eps)
data=data.frame(y,x)
lm_reg=lm(y~x) # LM
quantiles=c(0.05, 0.25, 0.50, 0.75, 0.95)

par(mfrow=c(2,2))  

# Plot 1: Quantile Regression τ = 0.25
plot(x, y, ylim=c(0,10), main="Quantile Regression: τ = 0.25")
for (i in quantiles){
  qt = dgp(x=x, eps=qnorm(i))
  lines(x, qt, col="grey", lty=2, lw=2)  
}
abline(q_0.25, col="black", lw=2, lty=1)  

# Plot 2: Quantile Regression τ = 0.75
plot(x, y, ylim=c(0,10), main="Quantile Regression: τ = 0.75")
for (i in quantiles){
  qt = dgp(x=x, eps=qnorm(i))
  lines(x, qt, col="grey", lty=2, lw=2)
}
abline(q_0.75, col="black", lw=2, lty=1)

y[69]=40

# Plot 3: OLS vs Quantile Regression (Highlighting Outlier)
plot(x, y, ylim=c(0,10), main="OLS")
abline(lm(y~x), col="red", lw=2, lty=1)  
abline(lm_reg, col="red", lty=2, lw=2)  

# Plot 4: Quantile Regression τ = 0.95
plot(x, y, ylim=c(0,10), main="Quantile Regression: τ = 0.95")
abline(rq(y~x, tau=0.95), col="black", lw=2, lty=1)
abline(q_0.95, col="black", lw=2, lty=2)


q_0.25=rq(y~x,tau=0.25,data=data)
q_0.50=rq(y~x,tau=0.5,data=data)
q_0.75=rq(y~x,tau=0.75,data=data)
q_0.95=rq(y~x,tau=0.95,data=data)

