#' Posterior credible quantiles for specific gravity
#'
#' @export
sg_posterior <- function(stan_fit, probs = c(0.1,0.5, 0.9)){

  post_df <- summary(stan_fit, pars = "sg_fit", probs = probs)$summary

  post_df <- tibble::as_tibble(post_df, rownames = "variable") %>%
    dplyr::select_at(vars(variable, contains("%")))

  post_df <- post_df %>%
    dplyr::mutate(
      t = stringr::str_match(variable, "\\d+") %>% as.numeric(),
      t = (t-1)/48
    ) %>%
    dplyr::select(-variable)

  post_df <- post_df %>%
    tidyr::gather("quantile", "sg_post", -t,) %>%
    dplyr::mutate(
      quantile_value = stringr::str_extract(quantile, "\\d+") %>% as.numeric,
      range_value = 2 * abs(quantile_value - 50),
      range = dplyr::if_else(range_value == 0, "Median", paste0(range_value, "%"))
    )

  post_df <- post_df %>% dplyr::select(t, quantile, range, sg_post)

  return(post_df)
}
