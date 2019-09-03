init <- function(nrow = 100, ncol = 100, prob = .2) {
  m <- matrix(ifelse(runif(nrow * ncol) <= prob, 1L, 0L), nrow, ncol)
  attr(m, "generation") <- 1
  m
}

step <- function(m) {
  idx_prev_x <- c(ncol(m), 1:(ncol(m)-1))
  idx_this_x <- 1:ncol(m)
  idx_next_x <- c(2:ncol(m), 1)

  idx_prev_y <- c(nrow(m), 1:(nrow(m)-1))
  idx_this_y <- 1:nrow(m)
  idx_next_y <- c(2:nrow(m), 1)

  m2 <- m[idx_prev_y, idx_prev_x] +
    m[idx_prev_y, idx_this_x] +
    m[idx_prev_y, idx_next_x] +
    m[idx_this_y, idx_prev_x] +
    m[idx_this_y, idx_next_x] +
    m[idx_next_y, idx_prev_x] +
    m[idx_next_y, idx_this_x] +
    m[idx_next_y, idx_next_x]

  m2 <- ifelse(m2 == 3, 1, ifelse(m2 == 2, m, 0))
  attr(m2, "generation") <- attr(m, "generation") + 1
  m2
}

show <- function(m) {
  image(m, col = c("white", "red"), main = paste0("Generation ", attr(m, "generation")))
}

set.seed(12345)
m <- init(); repeat { show(m); Sys.sleep(.1); m <- step(m) }
