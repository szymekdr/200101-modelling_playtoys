## Szymek Drobniak
## 20/01/2020
## Pop-genetics models to demonstrate the most important po-gen topics
## Based mostly on S.H.Rice 2018; D.Roff 2014; S.Otto&T.Day 2015


#### single locus bi-allelic FIS ----

## fully additive inheritance

w_11 <- .75
w_12 <- 0.5
w_22 <- .25
p <- seq(0,1,0.01)

w_1_star <- p*w_11 + (1-p)*w_12
w_2_star <- p*w_12 + (1-p)*w_22

# fitness function

w_bar <- p^2*w_11 + 2*p*(1-p)*w_12 + (1-p)^2*w_22

plot(p, w_bar, type = "l")

p_tplus <- (p * w_1_star)/w_bar

plot(p, p_tplus, type = "l")
abline(a=0, b=1, lty = 2)

# build a spiders-web plot

p_start <- 0.2
N <- 50
diagonal_pos <- p_start
p_t0 <- p_start

for (i in 1:N) {
  
  w_bar_curr <- p_t0^2*w_11 + 2*p_t0*(1-p_t0)*w_12 + (1-p_t0)^2*w_22
  w_1_star_curr <- p_t0*w_11 + (1-p_t0)*w_12
  p_t1 <- (p_t0 * w_1_star_curr)/w_bar_curr
  lines(c(p_t0, p_t0), c(diagonal_pos, p_t1), col = "pink")
  
  diagonal_pos <- p_t1
  
  lines(c(p_t0, p_t1), c(p_t1, diagonal_pos), col = "pink")
  
  p_t0 <- p_t1
  
}

points(p_start, p_start, pch = 20, col = "pink", cex = 2)
points(p_t0, (p_t0 * w_1_star_curr)/w_bar_curr, pch = 8, col = "red", cex = 2)


## heterozygote advantage

w_11 <- .25
w_12 <- 1
w_22 <- .25
p <- seq(0,1,0.01)

w_1_star <- p*w_11 + (1-p)*w_12
w_2_star <- p*w_12 + (1-p)*w_22

# fitness function

w_bar <- p^2*w_11 + 2*p*(1-p)*w_12 + (1-p)^2*w_22

plot(p, w_bar, type = "l")

p_tplus <- (p * w_1_star)/w_bar

plot(p, p_tplus, type = "l")
abline(a=0, b=1, lty = 2)

# build a spiders-web plot

p_start <- 0.0001
N <- 50
diagonal_pos <- p_start
p_t0 <- p_start

for (i in 1:N) {
  
  w_bar_curr <- p_t0^2*w_11 + 2*p_t0*(1-p_t0)*w_12 + (1-p_t0)^2*w_22
  w_1_star_curr <- p_t0*w_11 + (1-p_t0)*w_12
  p_t1 <- (p_t0 * w_1_star_curr)/w_bar_curr
  lines(c(p_t0, p_t0), c(diagonal_pos, p_t1), col = "pink")
  
  diagonal_pos <- p_t1
  
  lines(c(p_t0, p_t1), c(p_t1, diagonal_pos), col = "pink")
  
  p_t0 <- p_t1
  
}

points(p_start, p_start, pch = 20, col = "pink", cex = 2)
points(p_t0, (p_t0 * w_1_star_curr)/w_bar_curr, pch = 8, col = "red", cex = 2)


# heterozygote disadvantage

w_11 <- 2
w_12 <- 0.2
w_22 <- 2
p <- seq(0,1,0.01)

w_1_star <- p*w_11 + (1-p)*w_12
w_2_star <- p*w_12 + (1-p)*w_22

# fitness function

w_bar <- p^2*w_11 + 2*p*(1-p)*w_12 + (1-p)^2*w_22

plot(p, w_bar, type = "l")

p_tplus <- (p * w_1_star)/w_bar

plot(p, p_tplus, type = "l")
abline(a=0, b=1, lty = 2)

# build a spiders-web plot

p_start <- 0.50001
N <- 50
diagonal_pos <- p_start
p_t0 <- p_start

for (i in 1:N) {
  
  w_bar_curr <- p_t0^2*w_11 + 2*p_t0*(1-p_t0)*w_12 + (1-p_t0)^2*w_22
  w_1_star_curr <- p_t0*w_11 + (1-p_t0)*w_12
  p_t1 <- (p_t0 * w_1_star_curr)/w_bar_curr
  lines(c(p_t0, p_t0), c(diagonal_pos, p_t1), col = "pink")
  
  diagonal_pos <- p_t1
  
  lines(c(p_t0, p_t1), c(p_t1, diagonal_pos), col = "pink")
  
  p_t0 <- p_t1
  
}

points(p_start, p_start, pch = 20, col = "pink", cex = 2)
points(p_t0, (p_t0 * w_1_star_curr)/w_bar_curr, pch = 8, col = "red", cex = 2)


#### single locus bi-allelic FDS ----

p <- seq(0,1,0.01)

A1A1 <- p^2
A1A2 <- 2*p*(1-p)
A2A2 <- (1-p)^2

s <- 0
  
w_11 <- 1 - 3*A1A2 + 3*A2A2
w_12 <- 1 - s*A1A2
w_22 <- 1 - 3*A1A2 + 3*A1A1

w_1_star <- p*w_11 + (1-p)*w_12
w_2_star <- p*w_12 + (1-p)*w_22

# fitness function

w_bar <- p^2*w_11 + 2*p*(1-p)*w_12 + (1-p)^2*w_22

plot(p, w_bar, type = "l")

p_tplus <- (p * w_1_star)/w_bar

plot(p, p_tplus, type = "l")
abline(a=0, b=1, lty = 2)

# build a spiders-web plot

p_start <- 0.001
N <- 500
diagonal_pos <- p_start
p_t0 <- p_start

for (i in 1:N) {
  A1A1 <- p_t0^2
  A1A2 <- 2*p_t0*(1-p_t0)
  A2A2 <- (1-p_t0)^2
  
  w_11 <- 1 - 3*A1A2 + 3*A2A2
  w_12 <- 1 - s*A1A2
  w_22 <- 1 - 3*A1A2 + 3*A1A1
  
  w_bar_curr <- p_t0^2*w_11 + 2*p_t0*(1-p_t0)*w_12 + (1-p_t0)^2*w_22
  w_1_star_curr <- p_t0*w_11 + (1-p_t0)*w_12
  p_t1 <- (p_t0 * w_1_star_curr)/w_bar_curr
  lines(c(p_t0, p_t0), c(diagonal_pos, p_t1), col = "pink")
  
  diagonal_pos <- p_t1
  
  lines(c(p_t0, p_t1), c(p_t1, diagonal_pos), col = "pink")
  
  p_t0 <- p_t1
  
}

points(p_start, p_start, pch = 20, col = "pink", cex = 2)
points(p_t0, (p_t0 * w_1_star_curr)/w_bar_curr, pch = 8, col = "red", cex = 2)
