# Package
library(survival)
library(readxl)
library(survminer)

library(rms)
library(Hmisc)
library(plyr)

# Impor Data
data = read_excel("D:/dataETS.xlsx")

# Memberi Label Data Kategorik Grup Paisen
data$Grup = factor(data$Grup, levels = c("1", "2"), labels = c("IN", "IP"))

# Menampilkan Data Survival
objk_surv = Surv(time = data$Waktu, event = data$Status)
objk_surv

# Perhitungan Kaplan Meier
fit = survfit(objk_surv ~ Grup, data = data)
summary(fit)
uji = survdiff(objk_surv ~ Grup, data = data)
uji

# Kurva KM
ggsurvplot(fit, data = data, pval = TRUE)

# Model Eksponensial
exp = survreg(formula = objk_surv ~ data$Grup, data = data, dist = "exponential")
summary(exp)

# Model Weibull
weibull = survreg(formula = objk_surv ~ Grup, data = data, 
                  dist = "weibull")
summary(weibull)

# Model Log Logistik
loglog = survreg(formula = objk_surv ~ Grup, data = data, 
                  dist = c("weibull","exponential","gaussian",
                           "logistic","lognormal","loglogistic")[6])
summary(loglog)

# Model Terbaik AIC
library(flexsurv)
aexp <- flexsurvreg(Surv(Waktu, Status) ~ Grup, data = data, dist = "exponential")
awei <- flexsurvreg(Surv(Waktu, Status) ~ Grup, data = data, dist = "weibull")
alolog <- flexsurvreg(Surv(Waktu, Status) ~ Grup, data = data, dist = "llogis")
fs3 <- flexsurvreg(Surv(Waktu, Status) ~ Grup, data = data, anc = list(sigma = ~ Grup), dist = "gengamma")

# Model Terbaik Plot
sWei <- survreg(s ~ as.factor(sex),dist='weibull',data=lung)

lines(predict(exp, newdata=list(Grup = 0),type="quantile",p=seq(.01,.99,by=.01)),seq(.99,.01,by=-.01),col="red")
lines(predict(exp, newdata=list(Grup = 1),type="quantile",p=seq(.01,.99,by=.01)),seq(.99,.01,by=-.01),col="red")

ggsurvplot(fit, data = data,
         xlab = "", ylab = "Survival",
         label.curves = TRUE,         
         time.inc = 1,
         n.risk   = TRUE)
res.cox <- coxph(formula = objk_surv ~ Grup, data = data)
summary(res.cox)

lines(survfit(res.cox, newdata = data.frame(Grup = 0:1)), col = "green", lty = 1:2, mark.time = FALSE)

## Define a function to plot survreg prediction by gender
survreg.curves <- function(model, col = "black", values = c(0, 1),
                           seq.quantiles = seq(from = 0.00, to = 1.00, by = 0.01)) {
  
  l_ply(values, function(X) {
    lines(x = predict(model,            
                      newdata = data.frame(Grup = X), # Dataset to perform prediction for
                      type = "quantile",                # Predict survival time (X-axis values) given event quantile
                      p = seq.quantiles),               # Vector of quantiles (Y-axis values)
          
          y = (1 - seq.quantiles),              # Change to survival quantile (proportion remaining)
          
          col = col, lty = X + 1)               # COLor and Line TYpe
  })
}

## Plot exponential model prediction
survreg.curves(exp, "red")

## Plot Weibull model prediction
survreg.curves(weibull, "blue")

## Add legends
legend(x = "topright",
       legend = c("Kaplan-Meier", "Exponential", "Weibull", "Log-Logistic"),
       lwd = 2, bty = "n",
       col = c("black", "green", "red", "blue"))