#' Bayesian logistic model with Stan
#'
#' @export
#' @param ... Arguments passed to `rstan::sampling` (e.g. iter, chains).
#' @return An object of class `stanfit` returned by `rstan::sampling`
#'
logistic_model_stan <- function(N, t, sg, og, fg_ant, fg_sd, days, ...) {
  standata <- list(N = N, t= t, sg = sg, og = og, fg_ant = fg_ant, fg_sd = fg_sd, days = days)
  out <- rstan::sampling(stanmodels$logistic_model, data = standata, ...)
  return(out)
}
