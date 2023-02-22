log_likelihood_linear = function(design, outcome, coeff, noise_var = 1){

    n = length(outcome)

    - (n/2) * log(2 * pi) - n * log(noise_var^0.5) - (1/(2 * noise_var)) * sum((outcome - design %*% coeff)^2)
}

log_likelihood_linear_gradient = function(design, outcome, coeff, noise_var = 1){

    1/noise_var * t(design) %*% (outcome - design %*% coeff)

}

log_likelihood_linear_gradient_numerical = function(design, outcome, coeff, noise_var = 1,
                                             dx = .Machine$double.eps^(1/3)){

    n_pred = ncol(design)
    grad_numerical = rep(NA, n_pred)

    for (i in 1:n_pred) {
        delta = rep(0, n_pred)
        delta[i] = dx

        grad_numerical[i] = (log_likelihood_linear(design, outcome, coeff + delta, noise_var = 1)
                             -log_likelihood_linear(design, outcome, coeff - delta, noise_var = 1)) / (2 * dx)
    }

    grad_numerical

}
