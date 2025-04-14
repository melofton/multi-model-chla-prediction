library("mgcv")
library("gratia")
library("dplyr")

df <- data.frame(x = seq(0, 1, length = 100))
bs <- basis(s(x, bs = "bs", k = 10), data = df)

# let's weight the basis functions (simulating model coefs)
set.seed(1)
betas <- data.frame(bf = factor(1:10), beta = rnorm(10))

# we need to merge the weights for each basis function with the basis object
bs <- bs |>
  left_join(betas, by = join_by(".bf" == "bf")) |>
  mutate(value_w = .value * beta)

# now we want to sum the weighted basis functions for each value of `x`
spl <- bs |>
  group_by(x) |>
  summarise(spline = sum(value_w))

# now plot
bs |> 
  ggplot(aes(x = x, y = value_w, colour = .bf, group = .bf)) +
  geom_line(show.legend = FALSE) +
  geom_line(aes(x = x, y = spline), data = spl, linewidth = 1.5,
            inherit.aes = FALSE) +
  labs(y = expression(f(x)), x = "x")

dat <- data_sim("eg1", seed = 4)
m <- gam(y ~ s(x0) + s(x1) + s(x2, bs = "bs") + s(x3),
         data = dat, method = "REML")

# data to evaluate the basis at
# using the CRAN version of {gratia}, we need `m`
ds <- data_slice(m, x2 = evenly(x2, n = 200))
# from 0.9.0 (or current GitHub version) you can do
# ds <- data_slice(dat, x2 = evenly(x2, n = 200))

# generate a tidy representation of the fitted basis functions
x2_bs <- basis(m, term = "s(x2)", data = ds)

# compute values of the spline by summing basis functions at each x2
x2_spl <- x2_bs |>
  group_by(x2) |>
  summarise(spline = sum(.value))

# now plot
x2_bs |> 
  ggplot(aes(x = x2, y = .value, colour = .bf, group = .bf)) +
  geom_line(show.legend = FALSE) +
  geom_line(aes(x = x2, y = spline), data = x2_spl, linewidth = 1.5,
            inherit.aes = FALSE) +
  labs(y = expression(f(x2)), x = "x2")

# evaluate the spline at the same values as we evaluated the basis functions
x2_sm <- smooth_estimates(m, "s(x2)", data = ds) |>
  add_confint()

# now plot
x2_bs |> 
  ggplot(aes(x = x2, y = .value, colour = .bf, group = .bf)) +
  geom_line(show.legend = FALSE) +
  geom_ribbon(aes(x = x2, ymin = .lower_ci, ymax = .upper_ci),
              data = x2_sm, # <---- new !
              inherit.aes = FALSE, alpha = 0.2) +
  geom_line(aes(x = x2, y = .estimate), data = x2_sm, # <---- new !
            linewidth = 1.5, inherit.aes = FALSE) +
  labs(y = expression(f(x)), x = "x") +
  theme_bw()
