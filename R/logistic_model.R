#' Bayesian logistic model with Stan
#'
#' @export
#' @param ... Arguments passed to `rstan::sampling` (e.g. iter, chains).
#' @return An object of class `stanfit` returned by `rstan::sampling`
#'
logistic_model_stan <- function(data, pars = c("t", "sg"), og = NULL, fg_ant, fg_sd, days, ...) {

  t  <- unlist(data[, pars[1] ])
  sg <- unlist(data[, pars[2] ])

  if(is.null(og)) og <- max(sg)

  standata <- list(
    N = length(t),
    t = t,
    sg = sg,
    og = og,
    fg_ant = fg_ant,
    fg_sd = fg_sd,
    days = days
  )

  out <- rstan::sampling(stanmodels$logistic_model, data = standata, ...)

  return(out)
  }
