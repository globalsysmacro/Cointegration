library('quantmod')
library("tseries")
#Obtain prices
getSymbols('TLT',from='2010-01-01')
getSymbols('IEF',from='2010-01-01')
tltAdj=unclass(TLT$TLT.Adjusted)
iefAdj=unclass(IEF$IEF.Adjusted)


## Plot the ETF  closing prices
plot(tltAdj, type="l", xlab="Jan 1st 2010 to Feb 2nd 2018", ylab="ETF  Price in USD", col="blue")
par(new=T)
plot(iefAdj, type="l",axes=F, xlab="", ylab="", col="red")
par(new=F)

## Plot a scatter graph of the ETF adjusted prices
plot(tltAdj, iefAdj, xlab="TLT Backward-Adjusted Prices", ylab="IEF Backward-Adjusted Prices")


model1=lm(tltAdj~iefAdj)
model2=lm(iefAdj~tltAdj)

# Plot the residuals of the first linear combination
plot(model1$residuals, type="l", xlab="Jan 1st 2010 to Feb 2nd 2018", ylab="Residuals of TLT and IEF regression")

# Observe betas for model1 and model2 and notice how they differ
model1
model2

#Perform the ADF test on the residuals, or "spread" of each model, using a single lag order
adf.test(model1$residuals, k=1)
adf.test(model2$residuals, k=1) 

# Total least squares regression
model3 <- prcomp( ~ tltAdj + iefAdj )
slope <- model3$rotation[2, 1]  /  model3$rotation[1, 1]
intercept <- model3$center[2] - slope * model3$center[1]

res_model3<-iefAdj-intercept-slope*tltAdj
plot(res_model3, type="l", xlab="Jan 1st 2010 to Feb 2nd 2018", ylab="Residuals of TLT and IEF regression")

adf.test(res_model3, k=1)

#scatter plot price level with OLS and show the first principle component on the plot

plot(iefAdj, tltAdj, main = "Scatter plot of prices. TLT vs. IEFL",
     cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
abline(lm(tltAdj ~ iefAdj))
abline(lm(iefAdj ~ tltAdj), lty = 2)
grid()
abline(a = intercept, b = slope, lty = 3)
