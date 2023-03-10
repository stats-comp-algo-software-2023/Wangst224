source("help.R")

test_that("linear regression log likelihood gradient", {

    n_obs = 32
    n_pred = 4

    data_list = simulate_data(n_obs, n_pred)

    LL_gradient = log_likelihood_linear_gradient(data_list$design, data_list$outcome, data_list$coef_true)
    LL_gradient_numerical = log_likelihood_linear_gradient(data_list$design, data_list$outcome, data_list$coef_true)


    abs_tol = 1e-6
    rel_tol = 1e-6

    expect_true(are_all_close(
        LL_gradient, LL_gradient_numerical, abs_tol = abs_tol, rel_tol = rel_tol
    ))

})
