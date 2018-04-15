model_sim <- function(n, f1l1_f2l1, f1l1_f2l2, f1l2_f2l1, f1l2_f2l2) {
  f1 <- factor(sample(c("l1", "l2"), n, TRUE))
  f2 <- factor(sample(c("l1", "l2"), n, TRUE))
  dv <- rnorm(
    n,
    ifelse(
      f1 == "l1" & f2 == "l1", 
      f1l1_f2l1, 
      ifelse(
        f1 == "l1" & f2 == "l2",
        f1l1_f2l2,
        ifelse(
          f1 == "l2" & f2 == "l1",
          f1l2_f2l1,
          f1l2_f2l2
        )
      )
    )
  )
  return(summary(lm(dv ~ f1 * f2))$coef[4, 4] < .05)
}

get_power <- function(n, f1l1_f2l1, f1l1_f2l2, f1l2_f2l1, f1l2_f2l2, reps) {
  mean(
    sapply(1:reps, function(placeholder) {
      model_sim(n, f1l1_f2l1, f1l1_f2l2, f1l2_f2l1, f1l2_f2l2)
    })
  )
}

power_analysis <- function(f1l1_f2l1, f1l1_f2l2, f1l2_f2l1, 
                           f1l2_f2l2, reps, start, end, by) {
  set.seed(1839)
  out <- lapply(
    seq(start, end, by), 
    get_power, 
    f1l1_f2l1, f1l1_f2l2, f1l2_f2l1, f1l2_f2l2, reps
  )
  out <- as.data.frame(do.call(rbind, out))
  names(out) <- "Interaction Term Power"
  out$`Sample Size` <- seq(start, end, by)
  return(
    out[, c(2, 1)]
  )
}

plot_mean_pattern <- function(f1l1_f2l1, f1l1_f2l2, f1l2_f2l1, f1l2_f2l2) {
  require(ggplot2)
  dat <- data.frame(
    factor_1 = c("Level 1", "Level 1", "Level 2", "Level 2"),
    factor_2 = c("Level 1", "Level 2", "Level 1", "Level 2"),
    Outcome = c(f1l1_f2l1, f1l1_f2l2, f1l2_f2l1, f1l2_f2l2)
  )
  ggplot(dat, aes(x = factor_1, y = Outcome, color = factor_2)) +
    geom_point(position = position_dodge(width = 1), size = 4) +
    theme_light() +
    labs(x = "Factor 1") +
    scale_color_discrete(name = "Factor 2") +
    theme(text = element_text(size = 18))
}
