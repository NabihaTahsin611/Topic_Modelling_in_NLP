install.packages(c("moments", "ggplot2")) 

library(moments)
library(ggplot2)

data <- read.csv("C:/Users/NABIHA TAHSIN/Documents/Data Science/diamonds_data.csv")

str(data)

Null_Value <- colSums(is.na(data))


ggplot(data, aes(x = price)) +
  geom_histogram(binwidth = 500, fill = "yellow", color = "black", alpha = 0.7) +
  
  geom_freqpoly(binwidth = 500, color = "red") +
  labs(title = "Histogram and Frequency Polygon of Diamond Prices",
       x = "Price",
       y = "Frequency") +
  theme_minimal() 


skewness_value <- skewness(data$price)
cat("Skewness:", skewness_value, "\n")

if (skewness_value > 0) {
  cat("The data is positively skewed (right-skewed).\n")
} else if (skewness_value < 0) {
  cat("The data is negatively skewed (left-skewed).\n")
} else {
  cat("The data is symmetric.\n")
}

ggplot(data, aes(x = price)) +
geom_density(fill = "yellow", color = "red", alpha = 0.7) +
  labs(
    title = paste("Density Curve of price (Skewness: ", round(skewness_value, 2), ")", sep = ""), 
    x = "price", 
    y = "Density"
  ) +
  geom_vline(aes(xintercept = median(price, na.rm = TRUE)), color = "black", linetype = "dashed") +
  theme_minimal() 



ggplot(data, aes(x = "", y = price)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(
    title = "Boxplot of Diamond Price",
    x = "Diamond Prices",
    y = "Price"
  ) +
  theme_minimal()



p <- plot(x = data$carat, y = data$price,
     xlab = "carat",
     ylab = "price",
     xlim = c(0.1, 1.0),
     ylim = c(500, 3000),        
     main = "carat vs price")




ggplot(data, aes(x = price, y = depth)) +
  geom_line(color = "blue") +
  labs(title = "Price VS Depth ",
       x = "Price",
       y = "Depth") +
  theme_minimal()



ggplot(data = diamonds, aes(x = clarity, y = price, fill = clarity)) +
  geom_violin(trim = FALSE) + # Violin plot
  stat_summary(fun = function(y) quantile(y, 0.25), geom = "point", 
               color = "blue", size = 2, shape = 16) + 
  stat_summary(fun = function(y) quantile(y, 0.5), geom = "point", 
               color = "red", size = 2, shape = 16) + 
  stat_summary(fun = function(y) quantile(y, 0.75), geom = "point", 
               color = "green", size = 2, shape = 16) + 
  labs(title = "Distribution of Diamond Prices by Clarity",
       x = "Clarity",
       y = "Price",
       fill = "Clarity") +
  theme_minimal() +
  theme(legend.position = "none")
