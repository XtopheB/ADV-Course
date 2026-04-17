library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

# Data (approximated from your figure)
df <- tibble(
  year = rep(c(1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010), 4),
  cause = rep(c("Mental", "Cancer", "Circulatory", "Musculoskeletal"), each = 8),
  value = c(
    # Mental
    11, 11, 18, 23, 22, 23, 26, 23,
    # Cancer
    10, 15, 15, 14, 16, 16, 14, 14,
    # Circulatory
    32, 26, 19, 16, 14, 13, 11, 11,
    # Musculoskeletal
    17, 18, 13, 16, 12, 18, 20, 26
  )
)

p1 <- ggplot(df, aes(year, value, color = cause, shape = cause)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  ) +
  labs(x = NULL, y = NULL)

p2 <- ggplot(df, aes(year, value, group = cause)) +
  
  # background lines (all causes in grey)
  geom_line(color = "grey70", size = 0.8) +
  
  # highlight current facet
  geom_line(
    data = df,
    aes(color = cause),
    size = 1.3
  ) +
  
  facet_wrap(~cause, ncol = 2) +
  
  scale_color_manual(values = c(
    "Mental" = "#1b3a57",
    "Cancer" = "#1b3a57",
    "Circulatory" = "#1b3a57",
    "Musculoskeletal" = "#1b3a57"
  )) +
  
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold"),
    axis.title = element_blank()
  )


p1 + p2
labs(title = "Initial DI Worker Awards by Major Cause of Disability")

ggsave("figure.png", p1 + p2, width = 14, height = 5, dpi = 300)
