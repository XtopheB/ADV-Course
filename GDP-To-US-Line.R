library(ggplot2)
library(quantmod)
library(dplyr)
library(lubridate)

# Fetch GBP/USD data from Yahoo Finance (GBPUSD=X)
# For 2017 data
getSymbols("GBPUSD=X", src = "yahoo", from = "2017-01-01", to = "2017-12-31", auto.assign = TRUE)
gbp_2017 <- data.frame(
  Date = index(`GBPUSD=X`),
  Rate = as.numeric(`GBPUSD=X`$`GBPUSD=X.Close`)
)

# Create a complete date sequence and interpolate missing values
date_seq_2017 <- data.frame(Date = seq(as.Date("2017-01-01"), as.Date("2017-12-11"), by = "day"))
gbp_2017 <- left_join(date_seq_2017, gbp_2017, by = "Date")

# Linear interpolation for missing values
gbp_2017$Rate <- approx(x = gbp_2017$Date[!is.na(gbp_2017$Rate)], 
                        y = gbp_2017$Rate[!is.na(gbp_2017$Rate)], 
                        xout = gbp_2017$Date, 
                        method = "linear")$y

# For 2015-2020 data
getSymbols("GBPUSD=X", src = "yahoo", from = "2015-01-01", to = "2020-01-31", auto.assign = TRUE)
gbp_2015_2020 <- data.frame(
  Date = index(`GBPUSD=X`),
  Rate = as.numeric(`GBPUSD=X`$`GBPUSD=X.Close`)
)

# Create a complete date sequence and interpolate missing values
date_seq_2015_2020 <- data.frame(Date = seq(as.Date("2015-01-01"), as.Date("2020-01-31"), by = "day"))
gbp_2015_2020 <- left_join(date_seq_2015_2020, gbp_2015_2020, by = "Date")

# Linear interpolation for missing values
gbp_2015_2020$Rate <- approx(x = gbp_2015_2020$Date[!is.na(gbp_2015_2020$Rate)], 
                             y = gbp_2015_2020$Rate[!is.na(gbp_2015_2020$Rate)], 
                             xout = gbp_2015_2020$Date, 
                             method = "linear")$y

# Graphic 1: January 1, 2017 - December 11, 2017
# Filter to exact date range
gbp_2017_filtered <- gbp_2017 %>%
  filter(Date >= as.Date("2017-01-01") & Date <= as.Date("2017-12-11"))

# Graphic 1  

plot1 <- ggplot(gbp_2017_filtered, aes(x = Date, y = Rate)) +
  geom_ribbon(aes(ymin = 1.15, ymax = Rate), fill = "#ADD8E6", alpha = 0.5) +
  geom_line(color = "#4682B4", size = 1) +
  scale_y_continuous(
    limits = c(1.15, 1.40),
    breaks = seq(1.15, 1.40, by = 0.05),
    labels = sprintf("%.2f", seq(1.15, 1.40, by = 0.05))
  ) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %Y"
  ) +
  labs(
    title = "GBP/USD Exchange Rate",
    subtitle = "January 1 - December 11, 2017",
    x = NULL,
    y = "Exchange Rate (USD per GBP)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#4682B4"),
    plot.subtitle = element_text(color = "#4682B4", size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey90", size = 0.3),
    panel.grid.major.y = element_line(color = "grey90", size = 0.3),
    axis.text.x = element_text(angle = 45, hjust = 1, color = "#4682B4"),
    axis.text.y = element_text(color = "#4682B4"),
    axis.title.y = element_text(margin = margin(r = 10), color = "#4682B4")
  )

plot1

# Graphic 2: January 1, 2015 - January 31, 2020
gbp_2015_2020_filtered <- gbp_2015_2020 %>%
  filter(Date >= as.Date("2015-01-01") & Date <= as.Date("2020-01-31"))

plot2 <- ggplot(gbp_2015_2020_filtered, aes(x = Date, y = Rate)) +
  # Add red shaded rectangle to highlight period of plot 1
  # annotate("rect", 
  #          xmin = as.Date("2017-01-01"), 
  #          xmax = as.Date("2017-12-11"),
  #          ymin = 1.15, 
  #          ymax = 1.60,
  #          alpha = 0.2, 
  #          fill = "red") +
  # Add red vertical lines at the boundaries
  geom_vline(xintercept = as.Date("2017-01-01"), 
             color = "darkred", 
             linetype = "solid", 
             size = 0.8) +
  geom_vline(xintercept = as.Date("2017-12-11"), 
             color = "darkred", 
             linetype = "solid", 
             size = 0.8) +
  geom_ribbon(aes(ymin = 1.15, ymax = Rate), fill = "#ADD8E6", alpha = 0.5) +
  geom_line(color = "#4682B4", size = 0.5) +
  scale_y_continuous(
    limits = c(1.15, 1.60),
    breaks = seq(1.15, 1.60, by = 0.05),
    labels = sprintf("%.2f", seq(1.15, 1.60, by = 0.05))
  ) +
  scale_x_date(
    date_breaks = "4 months",
    date_labels = "%b %Y"
  ) +
  labs(
    title = "GBP/USD Exchange Rate",
    subtitle = "January 2015 - January 2020\n (Plot 1 range highlighted)",
    x = NULL,
    y = "Exchange Rate (USD per GBP)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#4682B4"),
    plot.subtitle = element_text(color = "#4682B4", size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey90", size = 0.3),
    panel.grid.major.y = element_line(color = "grey90", size = 0.3),
    axis.text.x = element_text(angle = 45, hjust = 1, color = "#4682B4"),
    axis.text.y = element_text(color = "#4682B4"),
    axis.title.y = element_text(margin = margin(r = 10), color = "#4682B4")
  )

# Display plots
print(plot1)
print(plot2)