source("helpers.R")
using("tinysnapshot")

mod = lm(mpg ~ wt, mtcars)
pred = predict(mod, interval = "confidence")

mtcars2 = cbind(mtcars, pred)


f = function() {
  with(
    mtcars2,
    tinyplot(
      x = wt, y = fit,
      ymin = lwr, ymax = upr,
      type = "ribbon"
    )
  )
}
expect_snapshot_plot(f, label = "ribbon")

f = function() {
  with(
    mtcars2,
    tinyplot(
      x = wt,
      ymin = lwr, ymax = upr,
      type = "ribbon"
    )
  )
}
expect_snapshot_plot(f, label = "ribbon_no_y")

f = function() {
  with(
    mtcars2,
    tinyplot(
      x = wt, y = fit,
      type = "ribbon"
    )
  )
}
expect_snapshot_plot(f, label = "ribbon_no_yminymax")


f = function() {
  with(
    mtcars2,
    tinyplot(
      x = wt, y = fit,
      ymin = lwr, ymax = upr,
      type = type_ribbon(alpha = .5)
    )
  )
}
expect_snapshot_plot(f, label = "ribbon_alpha50")
