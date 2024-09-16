library(Rcpp)
sourceCpp("src/mod_inverse.cpp")

break_lcg <- function(vec) {

  if (length(vec) < 5) {
    stop("Error: the number of consequtive samples must be at least 5.")
  }
  mod <- compute_modulus(vec)

  term1 <- vec[2] - vec[3]
  term2 <- mod_inv(vec[1] - vec[2], mod)
  a <- (term1 * term2) %% mod
  c <- (vec[2] - a * vec[1]) %% mod
  return(new("lcg_parameters", mod = mod, a = a, c = c))
}

setClass(Class = "lcg_parameters",
  representation(
    mod = "numeric",
    a = "numeric",
    c = "numeric"
  )
)

gcd_vector <- function(nums) {
  gcd_two <- function(a, b) if (b == 0) a else Recall(b, a %% b)
  Reduce(gcd_two, nums)
}

compute_modulus <- function(outputs) {
  ts <- c()
  for (i in 1:(length(outputs) - 1)) {
    ts <- c(ts, outputs[i + 1] - outputs[i])
  }

  us <- c()
  for (i in 1:(length(ts) - 2)) {
    us <- c(us, abs(ts[i + 2] * ts[i] - ts[i + 1]^2))
  }

  modulus <- gcd_vector(us)
  modulus
}
