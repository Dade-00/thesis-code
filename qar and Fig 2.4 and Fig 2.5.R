# Load the data
data=read.table("LFPR dataset.csv",header=T,sep=",")
names(data)=c("Date","Rate")
attach(data)
Tempo=as.Date(x=Date,format="%Y-%m-%d")
start=as.numeric(c(format(Tempo[1],"%Y"),format(Tempo[1],"%m")))
y=ts( data = data$Rate, start = start, frequency = 12)

## seasonally adjust
library(forecast)
stl_decomp <- stl(y, s.window = "periodic")
y_adj_stl <- seasadj(stl_decomp)


library(quantreg)
y_lag <- stats::lag(y_adj_stl, +1)  
df <- data.frame(y_lag = y_lag[-length(y_lag)],y_adj_stl = y_adj_stl[-1])

taus=c(0.01,seq(0.05,0.95,0.05),0.99)

# Figure 2.4
par(mfrow=c(1,2))
plot(x=Tempo,y=y_adj_stl, type = "l", col = "red", main = "Labor Force Participation Rate", ylab = "% Rate",xlab="Time")
plot(df,type="n",main="QAR(1)", xlab="(Lagged) LFPR",ylab="LFPR")
points(df,cex=0.5,col="blue")
for (i in 1:length(taus)){
  abline(rq(y_adj_stl ~ y_lag,tau=taus[i],data = df),col="gray")
}

# Figure 2.5
fit1=summary(rq(y_adj_stl ~ y_lag, tau = 2:98/100, data = df))
plot(fit1,mfrow=c(1,2))
