# Load necessary libraries
library(ggplot2)
library(fitdistrplus)
library(actuar)

# Read data from the CSV file
file_sizes_data <- read.csv("filesizes (1).csv")
file_sizes <- file_sizes_data$x

# 1. Plot a histogram
ggplot(data.frame(x = file_sizes), aes(x = x)) +
  geom_histogram(binwidth = 100, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "File Sizes Histogram",
       x = "File Size (kB)",
       y = "Frequency")

# 2. Calculate numerical summaries
summary_stats <- summary(file_sizes)
mean_value <- mean(file_sizes)
sd_value <- sd(file_sizes)
median_value <- median(file_sizes)
quantiles_values <- quantile(file_sizes, c(0.25, 0.75))

# Print the results
cat("Summary Statistics:\n")
cat("Mean:", mean_value, "\n")
cat("Standard Deviation:", sd_value, "\n")
cat("Median:", median_value, "\n")
cat("1st Quartile:", quantiles_values[1], "\n")
cat("3rd Quartile:", quantiles_values[2], "\n")

# 3. Fit Pareto distribution using the fitdistrplus package with better starting values
alpha_estimate <- (length(file_sizes) - 1) / sum(log(file_sizes / min(file_sizes)))

pareto_fit <- fitdistrplus::fitdist(
  file_sizes,
  "pareto",
  method = "mle",
  start = list(shape = alpha_estimate, scale = min(file_sizes))
)

# Print the estimated parameters
cat("Pareto Fit Parameters:\n")
print(pareto_fit$estimate)

# 4. Visualize the fitted Pareto distribution
# Generate x values for the plot
x_values <- seq(min(file_sizes), max(file_sizes), length.out = 1000)

# Calculate corresponding probabilities using the estimated parameters
y_values <- actuar::dpareto(x_values, shape = pareto_fit$estimate[1], scale = pareto_fit$estimate[2])

# Plot the fitted Pareto distribution
ggplot() +
  geom_histogram(data = data.frame(x = file_sizes), aes(x = x), binwidth = 100, fill = "blue", color = "black", alpha = 0.7) +
  geom_line(data = data.frame(x = x_values, y = y_values), aes(x = x, y = y), color = "red", linewidth = 2) +
  labs(title = "Fitted Pareto Distribution",
       x = "File Size (kB)",
       y = "Probability Density")
