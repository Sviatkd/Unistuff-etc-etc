set.seed(333)
exporttar <- runif(n,0.01,0.27) #runif(n,0.01,0.27) by default
tarstar <- 0.021
exporttar=tarstar
cost <- (kw^2-kw)/100000
avcost <- ((kw^2-kw)/100000)/kw
revn <- exporttar*kw
prof <- revn-cost
totalprof <- cumsum(prof)
summary(prof)
summary(totalprof)
plot31 <- plot.ts(totalprof)
plot42 <- plot.ts(prof)
library(dplyr)
library(tidyr)
avcostts <- as.ts(avcost)
avrevts <- as.ts(exporttar)
# Create plottable data frame
timedf_long1 <- data.frame(
  Time = time(avcostts),
  Total_Cost = as.numeric(cost),
  Export_Revenue = as.numeric(revn)
) %>%
  pivot_longer(cols = -Time,
               names_to = "Series", 
               values_to = "Value")

# Create plot
plot1 <- ggplot(timedf_long1, aes(x = Time, y = Value, color = Series)) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = c("Total_Cost" = "#E41A1C", 
                                "Export_Revenue" = "#377EB8")) +
  labs(title = "Time Series Comparison",
       x = "Time Period",
       y = "Value",
       color = "Metric") +
  theme_minimal() +
  theme(legend.position = "top")
timedf_long2 <- data.frame(
  Time = time(avcostts),
  Average_Cost = as.numeric(avcostts),
  Export_Revenue = as.numeric(avrevts)
) %>%
  pivot_longer(cols = -Time, 
               names_to = "Series", 
               values_to = "Value")

# Create plot
plot2 <- ggplot(timedf_long2, aes(x = Time, y = Value, color = Series)) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = c("Average_Cost" = "#E41A1C", 
                                "Export_Revenue" = "#377EB8")) +
  labs(title = "Time Series Comparison",
       x = "Time Period",
       y = "Value",
       color = "Metric") +
  theme_minimal() +
  theme(legend.position = "top")
 plot(exporttar,prof)
 plot(exporttar,prof,xlim=c(0.01,0.05))
