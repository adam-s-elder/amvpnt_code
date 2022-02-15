# Functions used to generate the data used in the second
# data example.

gen_dat_de2 <- function(ss, dim = 10, w_coefs = rep(0, dim),
                        cor = 0.6, mis_probs = FALSE){
  ## This function is based on the data generating function:
  ## logit(pr(Y^(a) = 1 | w)) = bj w_j

  sig_mat <- matrix(cor, nrow = dim, ncol = dim)
  diag(sig_mat) <- 1

  expit <- function(x) exp(x)/(1 + exp(x))

  ws <- MASS::mvrnorm(n = ss, mu = rep(0, dim), Sigma = sig_mat)
  prob_mis <- expit(0.5 + 0.15 * ws[, max(1, ncol(ws) - 1)] -
                   0.275 * ws[, max(2, ncol(ws))])
  obs_data <- data.frame("y" = rbinom(ss, 1, 0.5),
                         "delta" = rbinom(ss, 1, prob_mis),
                         "w" = ws)

  logodds <- as.matrix(obs_data[, -c(1, 2)]) %*%
    matrix(w_coefs, nrow = dim)
  obs_data$y <- rbinom(n = ss, size = 1,
                       prob = expit(logodds[, , drop = TRUE]))
  if (mis_probs) {
    return(prob_mis)
  }else{
    return(obs_data)
  }
}

make_mod_dat_de2 <- function(ss, dim, model, cor = 0.6,
                             mis_probs = FALSE){
  if (model == 1) {
    dat <- gen_dat_de2(ss = ss, dim = dim, w_coefs = rep(0, dim),
                       cor = cor, mis_probs = mis_probs)
  }else if (model == 2) {
    dat <- gen_dat_de2(ss = ss, dim = dim,
                       w_coefs = c(6/10, rep(0, dim - 1)),
                       cor = cor, mis_probs = mis_probs)
  }else if (model == 3) {
    dat <- gen_dat_de2(ss = ss, dim = dim,
                       w_coefs = 0.32 * c(rep(c(-1, 1), times = 5),
                                         rep(0, dim - 10)),
                       cor = cor, mis_probs = mis_probs)
  }else if (model == 4) {
    dat <- gen_dat_de2(ss = ss, dim = dim,
                       w_coefs = 0.4675 * c(rep(c(0.5, 1), times = 5),
                                         rep(0, dim - 10)),
                       cor = cor, mis_probs = mis_probs)
  }else{
    stop("You must provide a model number of 1 (Null model),
         2 (One covariate is directly associated with the outcome,
         3 (10 covariates have a weak direct associations with the outcome) or,
         4 (10 covariates all with a weak positive direct associations with the outcome)")
  }
  return(dat)
}