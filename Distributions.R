library(ggplot2)
set.seed(123) # for reproducibility
SIAP.color <- "#0385a8"



# create data set with bimodal variable
df <- data.frame(x = c(rnorm(50, -2, 1), rnorm(100, 2, 1.5)))

df.bounds <- c(-8,8)

# create box plot
ggplot(df, aes(x="", y=x)) +
  geom_boxplot(color = SIAP.color, fill = SIAP.color, alpha = 0.2, width = 0.5) +
  labs(title = "Box plot", x = "", y = "x") +
  scale_y_continuous(limits = df.bounds) +
  coord_flip() +
  theme_minimal()


# create box plot with points
ggplot(df, aes(x="", y=x)) +
  geom_boxplot(color = SIAP.color, fill = SIAP.color, alpha = 0.2, width = 0.5) +
  geom_jitter(colour = "pink", size = 2, width = 0.10, alpha = 0.6) +
  labs(title = "Box plot with points (jittered)", x = "", y = "x") +
  scale_y_continuous(limits = df.bounds) +
  coord_flip() +
  theme_minimal()


# create jittered
ggplot(df, aes(x="", y=x)) +
  # geom_boxplot(color = SIAP.color, fill = SIAP.color, alpha = 0.2, width = 0.5) +
  geom_jitter(colour = "pink", size = 3, width = 0.10, alpha = 0.6) +
  labs(title = "Jittered points", x = "", y = "x") +
  scale_y_continuous(limits = df.bounds) +
  coord_flip() +
  theme_minimal()


# Histogram 
ggplot(df, aes(x = x)) +
  geom_histogram( fill = SIAP.color, alpha = 0.4) +
  labs(title = "Histogram", x = "x")+
  scale_y_continuous(limits = c(-20,20)) +
  scale_x_continuous(limits = df.bounds) +
  theme_minimal()


# density 
ggplot(df, aes(x = x)) +
  geom_density(fill = SIAP.color, alpha = 0.4) +
  labs(title = "Density (estimated)", x = "x")+
  scale_x_continuous(limits = df.bounds) +
  scale_y_continuous(limits = c(-0.4, 0.4)) +
  theme_minimal()



# density 
ggplot(df, aes(x = x)) +
  geom_density(fill = SIAP.color, alpha = 0.4) +
  labs(title = "Density (estimated)", x = "x")+
  geom_rug(colour = "pink") +
  scale_x_continuous(limits = df.bounds) +
  scale_y_continuous(limits = c(-0.4, 0.4)) +
  theme_minimal()

# create violin plot
ggplot(df, aes(x="", y=x)) +
  geom_violin(color = SIAP.color, fill = SIAP.color, alpha = 0.2, width = 0.50) +
  labs(title = "Violin plot", x = "", y = "x")+
  scale_y_continuous(limits = df.bounds) +
  coord_flip() +
  theme_minimal()


# # create rug plot
# ggplot(df, aes(x="", y=x)) +
#   labs(title = "rug plot", x = "", y = "x") +
#   geom_rug(colour = "pink") +
#  # scale_x_discrete(limits = df.bounds) +
#   coord_flip() +
#   theme_minimal() 

# create stripes 
ggplot(df, aes(x="", y=x, fill = x)) +
 # geom_hline(yintercept = 0, size = 1) +
  geom_tile(color = "pink", width = 0.3) +
  #scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Stripes ", x = "", y = "x", fill = "") +
  scale_y_continuous(limits = df.bounds) +
  coord_flip() +
  theme_minimal() +  
  theme(legend.position = "none")

# create color band plot
ggplot(df, aes(x = as.factor(x), y = 1, fill = x )) +
  geom_tile() +
  labs(title = "Color band plot", x ="") +
  scale_fill_gradientn(colors = rev(col_strip)) +
  scale_x_discrete() +
  guides(fill = guide_colorbar(barwidth = 1)) +
  theme_minimal() 
 
# 2 dimensions

# Strip 
library(RColorBrewer)
col_strip <- brewer.pal(11, "RdBu")
# col_strip <- c("black" , "grey", "lightgrey" )

# --data-- 
# Create a vector of dates from January 203 to December 2022
dates <- seq(as.Date("203-01-01"), as.Date("2022-12-01"), by = "month")
# Create a vector of random temperature values between 15 and 16
temp <- runif(length(dates), 15, 16)
# Combine the two vectors into a data frame
df2 <- data.frame(date = dates, temp = temp)

ggplot(df2,  aes(x = date, y = 1, fill = temp))+
  geom_tile() +
  scale_fill_gradientn(colors = rev(col_strip)) +
  guides(fill = guide_colorbar(barwidth = 1)) +
  theme_void() +
  theme(legend.position = "none")


