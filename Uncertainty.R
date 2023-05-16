library(tidyverse)
library(patchwork)

set.seed(123)


# Create an empty data.frame
df <- data.frame()

# Loop over years from 2003 to 2022
for(year in 2003:2022) {
  # Create a data.frame with 100 observations for group A, with mean increasing over time
  a <- data.frame(year = year, group = "A", value = rnorm(100, mean = 10 + (year - 2003)*0.4, sd = 1 + (year - 2003)*0.22))
  # Create a data.frame with 100 observations for group B, with mean increasing over time
  b <- data.frame(year = year, group = "B", value = rnorm(100, mean = 20 - (year - 2003)*0.15, sd = 4 - (year - 2003)*0.15))
  # Create a data.frame with 100 observations for group C, with constant mean
  # c <- data.frame(year = year, group = "C", value = rnorm(100, mean = 15 + (year - 2003)*0.05,  sd = 3 - (year - 2003)*0.1))
  # Bind the three data.frames together
  df <- rbind(df, a, b)
}


# Create the first plot (mean over time)
p1 <-  ggplot(df, aes(x = year, y = value, color = group)) +
   stat_summary(fun = mean, geom = "line", size = 2 ) +
  scale_y_continuous(limits = c(0, 30)) +
  labs(title = "Index over Time (Mean)", x = "Year", y = "SDG Indicator") + 
   theme_minimal()

# Create the second plot (mean and variance over time)
p2 <-  ggplot(df, aes(x = year, y = value, color = group, fill = group)) +
  stat_summary(fun = mean, geom = "line", size = 2) +
  stat_summary(fun.data = "mean_sdl", fun.args = list(mult = 1), geom = "ribbon", alpha = 0.2) +
  scale_y_continuous(limits = c(0, 30)) +
  labs(title = "Index over Time (with uncertainty)", x = "Year", y = "SDG Indicator") +  
   theme_minimal()

p3 <-  ggplot(df, aes(x = year, y = value, color = group)) +
  stat_summary(fun = mean, geom = "line", size = 2) +
  #stat_summary(fun.data = "mean_sdl", fun.args = list(mult = 1), geom = "ribbon", alpha = 0.2) +
  geom_jitter(alpha = 0.2, size = 1) +
  scale_y_continuous(limits = c(0, 30)) +
  labs(title = "Index over Time (with observations)", x = "Year", y = "SDG Indicator") +  
  theme_minimal()

# Display the plots
(p1 / p2/p3)
p1
p2
p3

