
#' @export
get_tilt_data <- function(url){

  googlesheets4::sheets_deauth()

  df <- googlesheets4::read_sheet(url,sheet = "Report", "C:H")

  # remove temperature scale (F or C).
  df <- df %>% dplyr::rename_all(~stringr::str_remove_all(., " (.*)"))

  # calculate days; difftime returns seconds, convert to days.
  df <- df %>% dplyr::mutate(
    day = difftime(Timepoint, min(Timepoint)) / (24 * 60^2),
    day = as.character(day) %>% as.numeric()
  )

  return(df)
}

