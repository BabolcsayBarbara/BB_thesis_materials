#shapley_synthetic_data_r

library(iml)
library(fastshap)
library(ggplot2)
library(dplyr)

set.seed(123)

# Generate synthetic data
n <- 500
p <- 5
X <- as.data.frame(matrix(runif(n * p), nrow = n, ncol = p))
colnames(X) <- paste0("X", 1:p)

# Define true coefficients
true_coefs <- c(1, 2, -1, 0, 0)
y <- as.matrix(X) %*% true_coefs + rnorm(n, sd = 0.1)
y <- as.vector(y)

model_lm <- lm(y ~ ., data = cbind(X, y))

x_interest <- X[1, ]

# Compute theoretical shapley values
shap_theoretical <- function(x, coefs, X_train) {
  means <- rep(0.5,5)
  shap_vals <- (x - means) * coefs
  names(shap_vals) <- names(x)
  return(shap_vals)
}

# Compute exhaustive shapley values
shap_exhaustive <- function(x, coefs, X_train) {
  means <- colMeans(X_train)
  shap_vals <- (x - means) * coefs
  names(shap_vals) <- names(x)
  return(shap_vals)
}

shap_theory <- shap_theoretical(x_interest, true_coefs, X)
#shap_theory

shap_exhaust <- shap_exhaustive(x_interest, true_coefs, X)
#shap_exhaust

predictor <- Predictor$new(model_lm, data = X, y = y)

shap_iml <- Shapley$new(predictor, x_interest)
shap_vals_iml <- shap_iml$results
#print(shap_vals_iml)

pred_fun <- function(object, newdata) {
  predict(model_lm, newdata = newdata)
}

# Call the default method directly
shap_fast <- fastshap:::explain.default(
  object = NULL,           # still required, can be NULL
  X = X,                   # background data
  pred_wrapper = pred_fun, # prediction function
  newdata = x_interest,    # row to explain
  nsim = 100               # number of permutations
)


df_results <- data.frame(
  Features = names(shap_theory),
  Theoretical = t(shap_theory),
  Exhaustive = t(shap_exhaust),
  IML_Sampling = shap_vals_iml$phi,
  Fastshap_Kernel = as.numeric(shap_fast)
)

names(df_results) = c('Features','Theoretical','Exhaustive','Sampling','Kernel_SHAP')

# Reshape for plotting
df_long <- reshape2::melt(df_results, id.vars = "Features")

# Plot
ggplot(df_long, aes(x = Features, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Shapley Value Comparison (Linear Regression)",
       y = "Shapley Value", fill = "Method")

