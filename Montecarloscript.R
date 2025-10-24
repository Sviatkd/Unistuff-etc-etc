library(readr)
library(fitdistrplus)
library(ggplot2)
wind_dataset <- read_csv("C:/Users/alexa/Downloads/wind_dataset.csv")
windspeeds <- wind_dataset$WIND
windspeedsclean <- windspeeds + 0.001
fittweibul <- fitdist(windspeedsclean,"weibull")
coefs <- fittweibul$estimate
print(coefs)
plot1 <- hist(windspeedsclean,breaks=40,probability = TRUE
)
curve(dweibull(x,shape=2.075,scale=11.073),col="purple",add=TRUE)
set.seed(333)
airdens <- 1.23
windturbA <- 1500
hrs <- 24
n <- 365*hrs
Cp <- 0.40
windspeed <- rweibull(n,2.074749,11.073441)
meanwindpowerh <- (windturbA*airdens*windspeed^3)/2
meanwindhe <- meanwindpowerh*Cp
kw <- meanwindhe/1000
powerchange <- diff(kw)

onepersurg <- quantile(powerchange,0.99)
fivepersurge <- quantile(powerchange,0.95)
fivekwhigh <- quantile(kw,0.95)
onekwhigh <- quantile(kw,0.99)
oneperdrop <- quantile(powerchange,1-0.99)
fiveperdrop <- quantile(powerchange,1-0.95)
fivekwlow <- quantile(kw,1-0.95)
onekwlow <- quantile(kw,1-0.99)
tenpersurg <- quantile(powerchange,0.90)
tenkwhigh <- quantile(kw,0.90)
tenperdrop <- quantile(powerchange,1-0.90)
tenkwlow <- quantile(kw,1-0.90)
simsummary_kw <- summary(kw)
simsummary_powerchange <- summary(powerchange)
monte_carlo_summary <- list(
  Power_Quantiles = data.frame(
    Percentile = c("99%", "95%", "90%", "1%", "5%", "10%"),
    Direction = c("High", "High", "High", "Low", "Low", "Low"),
    Power_Change = c(
      onepersurg,
      fivepersurge,
      tenpersurg,
      oneperdrop,
      fiveperdrop,
      tenperdrop
    )
  ),
  
  kW_Quantiles = data.frame(
    Percentile = c("95%", "99%", "5%", "1%", "90%", "10%"),
    Direction = c("High", "High", "Low", "Low", "High", "Low"),
    kW_Value = c(
      fivekwhigh,
      onekwhigh,
      fivekwlow,
      onekwlow,
      tenkwhigh,
      tenkwlow
    )
  ),
  
  Summary_Statistics = list(
    kW = simsummary_kw,
    Power_Change = simsummary_powerchange
  )
)

# Print formatted summary
cat("MONTE CARLO SIMULATION SUMMARY\n")
cat("==============================\n\n")

cat("Power Change Quantiles (Extreme Events):\n")
print(monte_carlo_summary$Power_Quantiles, row.names = FALSE)
cat("\n")

cat("kW Output Quantiles (Performance Thresholds):\n")
print(monte_carlo_summary$kW_Quantiles, row.names = FALSE)
cat("\n")

cat("Distribution Summary - kW Output:\n")
print(monte_carlo_summary$Summary_Statistics$kW)
cat("\n")

cat("Distribution Summary - Power Change:\n")
print(monte_carlo_summary$Summary_Statistics$Power_Change)
# Set up 3x2 plotting grid with reduced margins
par(mfrow = c(3, 2), mar = c(3, 3, 2, 1), oma = c(2, 2, 2, 0))

# Plot 1: Wind speed distribution with Weibull fit
hist(windspeedsclean, breaks = 40, probability = TRUE,
     main = "Wind Speed Distribution (Weibull Fit)",
     xlab = "", ylab = "",
     col = "lightblue", border = "white")
curve(dweibull(x, shape = 2.075, scale = 11.073), 
      col = "purple", lwd = 2, add = TRUE)
legend("topright", legend = bquote(Weibull(shape==2.075, scale==11.073)),
       col = "purple", lty = 1, lwd = 1, cex = 0.71)

# Plot 2: kW histogram (default breaks)
hist(kw, 
     main = "kW Output Distribution", 
     xlab = "", ylab = "",
     col = "lightblue", border = "white")

# Plot 3: Power change histogram (50 breaks)
hist(powerchange, breaks = 50,
     main = "Power Change Distribution", 
     xlab = "", ylab = "",
     col = "lightblue", border = "white")

# Plot 4: kW time series
plot.ts(kw, 
        main = "kW Output Time Series", 
        ylab = "",
        col = "darkgreen")

# Plot 5: Power change time series
plot.ts(powerchange, 
        main = "Power Change Time Series", 
        ylab = "",
        col = "darkgreen")

# Plot 6: kW zoomed histogram (0-1000 range)
hist(kw, breaks = 200, xlim = c(0, 1000),
     main = "kW Output (0-1000 Range)", 
     xlab = "", ylab = "",
     col = "lightblue", border = "white")

# Add global labels
mtext("Value", side = 1, outer = TRUE, line = 0.5)
mtext("Density/Frequency", side = 2, outer = TRUE, line = 0.5)
mtext("Wind Turbine Performance Monte Carlo", side = 3, outer = TRUE, 
      cex = 1.2, font = 2, line = 0.2)

