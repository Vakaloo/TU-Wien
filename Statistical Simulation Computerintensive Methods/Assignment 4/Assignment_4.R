# Task 1
data.points <- c(4.94, 5.06, 4.53, 5.07, 4.99, 
                 5.16, 4.38, 4.43, 4.93, 4.72, 
                 4.92, 4.96)

# Task 1.1

# The possible bootstrap samples are n^n, n is equal to 12 (length(data.points))

# Task 1.2

mean.orig <- mean(data.points)
median.orig <- median(data.points)

# Task 1.3
# bootstrap.samples <- matrix(NA, nrow = 2000, ncol = 12)
# mean.samples <- c()
# for (i in 1:2000){
  
  # Create the bootstrap samples
  # bootstrap.samples[i,] <- sample(data.points, replace = TRUE)
  
  # Calculate the mean of each sample
  # mean.samples <- c(mean.samples, mean(bootstrap.samples[i,]))

# }
set.seed(12223236)
mean.samples <- replicate(2000, mean(sample(data.points, replace=TRUE)))
print(mean.samples)

# Task 1.3.1

mean.sample.20 <- mean(mean.samples[1:20])

# Task 1.3.2

mean.sample.200 <- mean(mean.samples[1:200])

# Task 1.3.3

mean.sample.2000 <- mean(mean.samples[1:2000])


# Task 1.3.4
library(ggplot2)

ggplot(data.frame(Means = mean.samples), aes(x = Means)) +
  geom_histogram(binwidth = 0.01, fill = "blue", color = "black") +
  geom_vline(xintercept = mean.orig, color = "red", linetype = "dashed", linewidth = 1) +
  labs(title = "Distribution of Bootstrap Sample Means vs. Sample Mean",
       x = "Sample Means",
       y = "Frequency") +
  theme_minimal()

# The CLT kicks in and that can be proven from the values of the 2 means
# Sample mean
print(mean.orig)
# Mean of the means
print(mean.sample.2000)


# Task 1.3.5

mean_sample_list <- list(mean.samples[1:20],
                         mean.samples[1:200],
                         mean.samples[1:2000])
names(mean_sample_list) <- c("sample_20","sample_200","sample_2000")

quantiles_list <- sapply(mean_sample_list, function(x) quantile(x, c(0.025, 0.975)))

true_ci <- t.test(data.points)$conf.int



# Task 1.4

set.seed(12223236)
median.samples <- replicate(2000, median(sample(data.points, replace=TRUE)))
print(median.samples)


# Task 1.4.1

median.sample.20 <- median(median.samples[1:20])

# Task 1.4.2

median.sample.200 <- median(median.samples[1:200])

# Task 1.4.3

median.sample.2000 <- median(median.samples[1:2000])


# Task 1.4.4
library(ggplot2)

ggplot(data.frame(Medians = median.samples), aes(x = Medians)) +
  geom_histogram(binwidth = 0.01, fill = "blue", color = "black") +
  geom_vline(xintercept = median.orig, color = "red", linetype = "dashed", linewidth = 1) +
  labs(title = "Distribution of Bootstrap Sample Medians vs. Sample Median",
       x = "Sample Medians",
       y = "Frequency") +
  theme_minimal()

# Task 1.4.5

median_sample_list <- list(median.samples[1:20],
                         median.samples[1:200],
                         median.samples[1:2000])
names(median_sample_list) <- c("20","200","2000")

quantiles_list <- lapply(median_sample_list, function(x) quantile(x, c(0.025, 0.975)))


# Task 2.1
set.seed(1234)
x.clean <- rnorm(1960)
x.cont <- runif(40,4,5)
x <- c(x.clean,x.cont)
set.seed(12223236)


# Task 2.2

result <- data.frame(
  row.names = c("x","x_clean"),
  Median = c(median(x),median(x.clean)),
  Mean = c(mean(x), mean(x.clean)),
  Trimmed_Mean = c(mean(x, trim = 0.05), mean(x.clean, trim = 0.05))
)

# Task 2.3

# https://bookdown.org/compfinezbook/introcompfinr/The-Nonparametric-Bootstrap.html

# x data set
x.median <- replicate(10000, median(sample(x, replace=TRUE)))
x.mean <- replicate(10000, mean(sample(x, replace=TRUE)))
x.trimmed_mean <- replicate(10000, mean(sample(x, replace=TRUE),trim = 0.05))

sd_values <- sapply(list(x.median, x.mean, x.trimmed_mean), sd)
quantile_values <- sapply(list(x.median, x.mean, x.trimmed_mean), function(x) quantile(x, c(0.025, 0.975)))

result_x <- data.frame(
  row.names = c("x_Median", "x_Mean","x_Trimmed_Mean"),
  Standard_Error = sd_values,
  Quantile_0.025 = quantile_values[1, ],
  Quantile_0.975 = quantile_values[2, ]
)

# x.clean data set
x_clean.median <- replicate(10000, median(sample(x.clean, replace=TRUE)))
x_clean.mean <- replicate(10000, mean(sample(x.clean, replace=TRUE)))
x_clean.trimmed_mean <- replicate(10000, mean(sample(x.clean, replace=TRUE),trim = 0.05))

sd_values <- sapply(list(x_clean.median, x_clean.mean, x_clean.trimmed_mean), sd)
quantile_values <- sapply(list(x_clean.median, x_clean.mean, x_clean.trimmed_mean), 
                          function(x) quantile(x, c(0.025, 0.975)))

result_x_clean <- data.frame(
  row.names = c("x_clean_Median", "x_clean_Mean","x_clean_Trimmed_Mean"),
  Standard_Error = sd_values,
  Quantile_0.025 = quantile_values[1, ],
  Quantile_0.975 = quantile_values[2, ]
)

# Task 2.4

parametric_bootstrap <- function(data, n_bootstraps, alpha = 0.05) {
  
  boot_means <- replicate(n_bootstraps, mean(rnorm(data)))
  boot_trimmed_means <- replicate(n_bootstraps, mean(rnorm(data), trim = 0.05))
  
  # bias
  bias_mean <- mean(boot_means) - mean(data)
  bias_trimmed_mean <- mean(boot_trimmed_means) - mean(data)
  
  # standard error
  se_mean <- sd(boot_means)
  se_trimmed_mean <- sd(boot_trimmed_means)
  
  # 95 percentile CI
  ci_mean <- quantile(boot_means, c(alpha / 2, 1 - alpha / 2))
  ci_trimmed_mean <- quantile(boot_trimmed_means, c(alpha / 2, 1 - alpha / 2))
  
  # bias-corrected estimate
  corrected_mean <- mean(data) - bias_mean
  corrected_trimmed_mean <- mean(data) - bias_trimmed_mean
  
  return(list(
    bias_mean = bias_mean,
    se_mean = se_mean,
    ci_mean = ci_mean,
    corrected_mean = corrected_mean,
    bias_trimmed_mean = bias_trimmed_mean,
    se_trimmed_mean = se_trimmed_mean,
    ci_trimmed_mean = ci_trimmed_mean,
    corrected_trimmed_mean = corrected_trimmed_mean
  ))
}

set.seed(12223236)
n_bootstraps <- 10000

result_x2 <- parametric_bootstrap(x, n_bootstraps, alpha = 0.05)
result_x_clean2 <- parametric_bootstrap(x.clean, n_bootstraps, alpha = 0.05)

result_x2_df <- data.frame(
  row.names = c("x_Mean","x_Trimmed_Mean"),
  Bias = c(result_x2$bias_mean,result_x2$bias_trimmed_mean),
  Standard_Error = c(result_x2$se_mean,result_x2$se_trimmed_mean) ,
  Quantile_0.025 = c(result_x2$ci_mean[1], result_x2$ci_trimmed_mean[1]),
  Quantile_0.975 = c(result_x2$ci_mean[2], result_x2$ci_trimmed_mean[2]),
  Corrected_Bias = c(result_x2$corrected_mean,result_x2$corrected_trimmed_mean)
)

result_x_clean2_df <- data.frame(
  row.names = c("x_Mean","x_Trimmed_Mean"),
  Bias = c(result_x_clean2$bias_mean,result_x_clean2$bias_trimmed_mean),
  Standard_Error = c(result_x_clean2$se_mean,result_x_clean2$se_trimmed_mean) ,
  Quantile_0.025 = c(result_x_clean2$ci_mean[1], result_x_clean2$ci_trimmed_mean[1]),
  Quantile_0.975 = c(result_x_clean2$ci_mean[2], result_x_clean2$ci_trimmed_mean[2]),
  Corrected_Bias = c(result_x_clean2$corrected_mean,result_x_clean2$corrected_trimmed_mean)
)

