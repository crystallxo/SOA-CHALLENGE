library(dplyr)
library(fitdistrplus)
library(actuar)
library(MASS)
library(nortest)
library(VGAM)
library(goftest)


data = read.csv("C:/Users/raffa/Desktop/My files/UPH/8) Semester 6/Kapita Selekta/soa study case/dam-clean.csv")

missing_values <- colSums(is.na(data))
print(missing_values)


data <- na.omit(data)  # Remove all rows with NA

data$Total_Loss <- data$Loss_gf_Prop + data$Loss_gf_Liab + data$Loss_gf_BI

sum(is.na(data$Total_Loss))  # Check for missing values
summary(data$Total_Loss)  # Check data summary to confirm its range and structure
sum(data$Total_Loss <= 0)  # Check if there are any zero or negative values


# Fit Pareto distribution
fit_pareto <- fitdist(data$Total_Loss, "pareto", start = list(shape = 1, scale = min(data$Total_Loss)))
summary(fit_pareto)

par(mar = c(4, 4, 2, 2))  # Default is often c(5, 4, 4, 2)
plot(fit_pareto)
cdfcomp(fit_pareto)
shape <- fit_pareto$estimate["shape"]
scale <- fit_pareto$estimate["scale"]

ks_test <- ks.test(data$Total_Loss, "ppareto", shape = shape, scale = scale)
print(ks_test)


# Fit Lognormal distribution
fit_lognormal <- fitdist(data$Total_Loss, "lnorm")
meanlog <- fit_lognormal$estimate["meanlog"]
sdlog <- fit_lognormal$estimate["sdlog"]

plot(fit_lognormal)
cdfcomp(fit_lognormal)

ks_test_lognormal <- ks.test(data$Total_Loss, "plnorm", meanlog = meanlog, sdlog = sdlog)
print(ks_test_lognormal)



# Fit Weibull distribution
fit_weibull <- fitdist(data$Total_Loss, "weibull")
shape_w <- fit_weibull$estimate["shape"]
scale_w <- fit_weibull$estimate["scale"]

plot(fit_weibull)
cdfcomp(fit_weibull)

ks_test_weibull <- ks.test(data$Total_Loss, "pweibull", shape = shape_w, scale = scale_w)
print(ks_test_weibull)

ad_test_weibull <- ad.test(data$Total_Loss, "pweibull", shape = shape_w, scale = scale_w)
print("Anderson-Darling Test for Weibull Distribution:")
print(ad_test_weibull)


# Fit Generalized Pareto Distribution
library(ismev)
gpd_fit <- gpd.fit(data$Total_Loss, threshold = min(data$Total_Loss))
xi <- gpd_fit$mle[1]   # Shape parameter
beta <- gpd_fit$mle[2] # Scale parameter

library(evd)
ks_test_gpd <- ks.test(data$Total_Loss, "pgpd", loc = 0, scale = beta, shape = xi)
print(ks_test_gpd)



# Fit Log Total Loss to Normal
data$log_total <- log1p(data$Total_Loss)
mu <- mean(data$log_total)
sigma <- sd(data$log_total)

cat("Mean:", mu, "\nStandard Deviation:", sigma, "\n")
# Histogram of the data
hist(data$log_total, probability = TRUE, col = "lightblue", main = "Histogram with Normal Fit", xlab = "Data values")

curve(dnorm(x, mean = mu, sd = sigma), add = TRUE, col = "red", lwd = 2)

