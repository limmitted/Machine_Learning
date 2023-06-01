#Machine learning 
# based on https://daviddalpiaz.github.io/r4sl/biasvariance-tradeoff.html
# bias variance trade - off 

# 1. plot
  f = function(x) {
    x ^ 2
  }

get_sim_data = function(f, sample_size = 100) {
  x = runif(n = sample_size, min = 0, max = 1)
  y = rnorm(n = sample_size, mean = f(x), sd = 0.3)
  data.frame(x, y)
}

set.seed(1)
sim_data = get_sim_data(f)

fit_0 = lm(y ~ 1,                   data = sim_data)
fit_1 = lm(y ~ poly(x, degree = 1), data = sim_data)
fit_2 = lm(y ~ poly(x, degree = 2), data = sim_data)
fit_4 = lm(y ~ poly(x, degree = 4), data = sim_data)
fit_6 = lm(y ~ poly(x, degree = 6), data = sim_data)
fit_20 = lm(y ~ poly(x, degree = 9), data = sim_data)

set.seed(1)
sim_data = get_sim_data(f)
plot(y ~ x, data = sim_data, col = "grey", pch = 20,
     main = "Six Polynomial Models fit to a Simulated Dataset")
grid()
grid = seq(from = 0, to = 2, by = 0.01)
lines(grid, f(grid), col = "black", lwd = 3)
lines(grid, predict(fit_0, newdata = data.frame(x = grid)), col = "dodgerblue",  lwd = 2, lty = 2)
lines(grid, predict(fit_1, newdata = data.frame(x = grid)), col = "firebrick",   lwd = 2, lty = 3)
lines(grid, predict(fit_2, newdata = data.frame(x = grid)), col = "springgreen", lwd = 2, lty = 4)
lines(grid, predict(fit_4, newdata = data.frame(x = grid)), col = "purple", lwd = 2, lty = 5)
lines(grid, predict(fit_6, newdata = data.frame(x = grid)), col = "brown", lwd = 2, lty = 6)
lines(grid, predict(fit_9, newdata = data.frame(x = grid)), col = "darkorange",  lwd = 2, lty = 7)

legend("topleft", 
       c("y ~ 1", "y ~ poly(x, 1)", "y ~ poly(x, 2)", "y ~ poly(x, 4)", "y ~ poly(x, 6)",  "y ~ poly(x, 9)", "truth"), 
       col = c("dodgerblue", "firebrick", "springgreen", "purple", "brown", "darkorange", "black"), lty = c(2, 3, 4, 5,6,7, 1), lwd = 2)



# 2. predictions
set.seed(1)
n_sims = 250
n_models = 6
x = data.frame(x = 0.90) # fixed point at which we make predictions
predictions = matrix(0, nrow = n_sims, ncol = n_models)
for (sim in 1:n_sims) {
  
  # simulate new, random, training data
  # this is the only random portion of the bias, var, and mse calculations
  # this allows us to calculate the expectation over D
  sim_data = get_sim_data(f)
  
  # fit models
  fit_0 = lm(y ~ 1,                   data = sim_data)
  fit_1 = lm(y ~ poly(x, degree = 1), data = sim_data)
  fit_2 = lm(y ~ poly(x, degree = 2), data = sim_data)
  fit_4 = lm(y ~ poly(x, degree = 4), data = sim_data)
  fit_6 = lm(y ~ poly(x, degree = 6), data = sim_data)
  fit_9 = lm(y ~ poly(x, degree = 9), data = sim_data)
  
  # get predictions
  predictions[sim, 1] = predict(fit_0, x)
  predictions[sim, 2] = predict(fit_1, x)
  predictions[sim, 3] = predict(fit_2, x)
  predictions[sim, 4] = predict(fit_4, x)
  predictions[sim, 5] = predict(fit_6, x)
  predictions[sim, 6] = predict(fit_9, x)
}

get_mse = function(truth, estimate) {
  mean((estimate - truth) ^ 2)
}

get_bias = function(estimate, truth) {
  mean(estimate) - truth
}

get_var = function(estimate) {
  mean((estimate - mean(estimate)) ^ 2)
}

bias = apply(predictions, 2, get_bias, truth = f(x = 0.90))
variance = apply(predictions, 2, get_var)
mse = apply(predictions, 2, get_mse, truth = f(x = 0.90))
results = data.frame(
  poly_degree = c(0, 1, 2,4,6,9),
  round(mse, 5),
  round(bias ^ 2, 5),
  round(variance, 5)
)
colnames(results) = c("Degree", "Mean Squared Error", "Bias Squared", "Variance")
rownames(results) = NULL
knitr::kable(results, booktabs = TRUE, escape = TRUE, align = "c")
