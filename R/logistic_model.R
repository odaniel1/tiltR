#' Bayesian logistic model with Stan
#'
#' @export
#' @param ... Arguments passed to `rstan::sampling` (e.g. iter, chains).
#' @return An object of class `stanfit` returned by `rstan::sampling`
#'
logistic_model_stan <- function(data, cal_data, pars = c("t", "sg"), og = NULL, fg_ant, fg_sd, days, ...) {

  t  <- unlist(data[, pars[1] ])
  sg <- unlist(data[, pars[2] ])

  t_cal <- unlist(cal_data[,pars[1]])
  sg_cal <- unlist(cal_data[,pars[2]])

  if(is.null(og)) og <- max(sg)

  standata <- list(
    N = length(t),
    t = t,
    sg = sg,
    N_cal = length(t_cal),
    t_cal = t_cal,
    sg_cal = sg_cal,
    og = og,
    fg_ant = fg_ant,
    fg_sd = fg_sd,
    days = days
  )

  print(standata)
  out <- rstan::sampling(stanmodels$logistic_model, data = standata, ...)

  return(out)
  }
