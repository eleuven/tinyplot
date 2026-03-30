source("helpers.R")
using("tinysnapshot")

weighted_lm_data = data.frame(
  x = c(1, 2, 3, 4, 5, 6, 7),
  y = c(1, 2, 3, 4, 100, NA, 7),
  w = c(1, 1, 1, 1, 0, 1, NA)
)

# internal type_lm consumer check
lm_settings = new.env(parent = emptyenv())
lm_settings$datapoints = data.frame(
  x = weighted_lm_data$x[1:5],
  y = weighted_lm_data$y[1:5],
  weights = weighted_lm_data$w[1:5],
  by = "",
  facet = ""
)
lm_settings$legend_args = list()
type_lm(se = FALSE)$data(lm_settings)

weighted_fit = lm(y ~ x, data = weighted_lm_data[1:5, ], weights = w)
unweighted_fit = lm(y ~ x, data = weighted_lm_data[1:5, ])
rendered_fit = lm(y ~ x, data = lm_settings$datapoints)

expect_true(abs(unname(coef(weighted_fit)[["x"]]) - unname(coef(unweighted_fit)[["x"]])) > 1)
expect_equal(unname(coef(rendered_fit)), unname(coef(weighted_fit)))

# public formula path: weights should flow through model.frame
f = function() {
  plt(
    y ~ x,
    data = weighted_lm_data,
    weights = ~ w,
    type = type_lm(se = FALSE),
    xlim = c(1, 7),
    ylim = c(0, 105)
  )
  tinyplot_add(type = "p", pch = 16)
}
expect_snapshot_plot(f, label = "model_lm_weights")
