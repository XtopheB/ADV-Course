library(ggplot2)
library(dplyr)
library(tidyr)

# --- data (same as before) ---
set.seed(1)

df <- tibble(
  x = rep(1:8, 6),
  panel = rep(1:6, each = 8),
  A = c(
    c(1,2,4,3,5,6,7,6),
    c(4,4.1,4.2,4.2,4.3,4.4,4.3,4.3),
    c(2,2.5,3,3.5,3.2,3.8,4,3.9),
    c(1,2,4,3,5,6,7,6),
    c(1,2,4,3,5,6,7,6),
    c(4,4.05,4.1,4.1,4.15,4.2,4.2,4.2)
  ),
  B = c(
    c(5,5.1,5.2,5.1,5.2,5.3,5.1,5.1),
    c(1,1.5,6,4,6,5,7,4),
    c(2.2,2.4,6,4,6,5,7,4),
    c(1,1.2,2,1.5,2.2,1.8,2.3,1.5),
    c(1,1.5,3.5,2,4,3,4.5,2.5),
    c(3.5,3.6,3.7,3.6,3.7,3.7,3.8,3.7)
  )
)

df_long <- df %>%
  pivot_longer(cols = c(A, B),
               names_to = "series",
               values_to = "value")

labels <- c(
  "B steady,\nA massively increasing.",
  "A steady,\nBincreasing.",
  "Initially at the same level,\nbut B increased more than A.",
  "Initially at the same level,\nbut A increased more than B.",
  "Same increase,\nthen A raced to the top.",
  "Both steady."
)

df_long$panel_label <- factor(df_long$panel, labels = labels)

# --- get last point for labeling ---
df_labels <- df_long %>%
  group_by(panel, series) %>%
  filter(x == max(x))

# --- plot ---
ggplot(df_long, aes(x = x, y = value, color = series, group = series)) +
  geom_line(size = 1.2) +
 # geom_point(size = 2) +
  
  # Add A/B labels at end of lines
  geom_text(data = df_labels,
            aes(label = series),
            hjust = -1,
            size = 4,
            show.legend = FALSE) +
  
  facet_wrap(~panel_label, ncol = 3) +
  
  scale_color_manual(values = c("A" = "#2C7F99", "B" = "#F8766D")) +
  
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 9),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  
  # Add space on the right for labels
  expand_limits(x = max(df_long$x) + 1)


#  scale_color_manual(values = c("blue" = "#28A197" , "pink" ="#EA738D")) +
