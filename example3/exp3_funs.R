gen_dat <- function(ss, dim = 10, cor = 0.6, int_terms = rep(0, dim),
                    main_a = 0.1, main_w = rep(0, dim)){

  sig_mat <- matrix(cor, nrow = dim, ncol = dim)
  diag(sig_mat) <- 1
  ## This function is based on the data generating function:
  ## logit(pr(Y^(a) = 1 | w)) = b_0 + b_1 a + b_2j w_j + b_3j aw_r
  obs_data <- data.frame("y" = rbinom(ss, 1, 0.5),
                         "a" = rbinom(ss, 1, 0.5),
                         "w" = MASS::mvrnorm(n = ss, mu = rep(0, dim), Sigma = sig_mat))

  fin_data <- obs_data
  fin_data <- cbind(fin_data, diag(fin_data$a) %*%
                      as.matrix(fin_data[, -c(1, 2)]))

  expit <- function(x) exp(x)/(1 + exp(x))
  coef_val <- c(main_a, main_w, int_terms)

  logodds <- as.matrix(fin_data[, -c(1)]) %*%
    matrix(coef_val, nrow = 2 * dim + 1)
  obs_data$y <- rbinom(n = ss, size = 1, prob = expit(logodds[, , drop = TRUE]))
  return(obs_data)
}

make_mod_dat <- function(ss, dim, model, xcor = 0.6){
  if (model == 1) {
    dat <- gen_dat(ss = ss, cor = xcor, dim = dim, int_terms = rep(0, dim),
                   main_a = 0.2, main_w = c(rep(0.7/dim, floor(dim/2)),
                                            rep(0, ceiling(dim/2))))
  }else if (model == 2) {
    dat <- gen_dat(ss = ss, cor = xcor, dim = dim,
                   int_terms = 0.4 * c(3, rep(0, dim - 1)),
                   main_a = 0.2, main_w = c(rep(0.7/dim, floor(dim/2)),
                                            rep(0, ceiling(dim/2))))
  }else if (model == 3) {
    dat <- gen_dat(ss = ss, cor = xcor, dim = dim,
                   int_terms = 0.8 * c(rep(-1, 5), rep(1, 5),
                                        rep(0, dim - 10)),
                   main_a = 0.2, main_w = c(rep(0.7/dim, floor(dim/2)),
                                            rep(0, ceiling(dim/2))))
  }else if (model == 4) {
    dat <- gen_dat(ss = ss, cor = xcor, dim = dim,
                   int_terms = 0.14 * c(rep(0.5, 5), rep(1, 5),
                                        rep(0, dim - 10)),
                   main_a = 0.2, main_w = c(rep(0.7/dim, floor(dim/2)),
                                            rep(0, ceiling(dim/2))))
  }else{
    stop("You must provide a model number of 1 (Null model),
         2 (One covariate has treatment modification, or
         3 (10 covariates have weak treatment modification) or
         4 (10 covariates with all positive weak direct treatment modification)")
  }
  return(dat)
}

