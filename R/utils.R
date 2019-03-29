

n_days_overlap <- function(x, y) {
  seq_y <- seq(from = y[1], to = y[2], by = "days")
  sum(seq_y >= x[1] & seq_y <= x[2])
}

n_overlaps <- function(w1, w2, s1, s2) {
  x1 <- (as.numeric(w2 - s1) + 1) * (s1 > w1 & s1 < w2 & s2 >= w2)
  x2 <- (as.numeric(s2 - w1) + 1) * (s2 > w1 & s2 < w2 & s1 <= w1)
  x3 <- 7 * (s1 <= w1 & s2 >= w2)
  x4 <- (as.numeric(s2 - s1) + 1) * (s1 > w1 & s2 < w2)
  x1 + x2 + x3 + x4
}
