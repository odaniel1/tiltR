
#' @export
get_tilt_data <- function(url){

  googlesheets4::sheets_deauth()

  df <- googlesheets4::read_sheet(url,sheet = "Report", "C:H")

  # remove temperature scale (F or C).
  df <- df %>% dplyr::rename_all(~stringr::str_remove_all(., " (.*)"))

  df <- df %>% dplyr::mutate(
    sg_points = (SG - 1) * 1000,

    # calculate days; difftime returns seconds, convert to days.
    day = difftime(Timepoint, min(Timepoint)) / (24 * 60^2),
    day = as.character(day) %>% as.numeric()
  )

  return(df)
}


#' @export
calibrate_data <- function(df, calibration_day, calibration_points){

nearest_pt <- df %>% dplyr::mutate(
  abs_day_dist = abs(calibration_day - day),
  sg_point_dist = calibration_points - sg_points
) %>% slice(which.min(abs_day_dist))

df <- df %>%
  mutate(
    sg_points = case_when(
      day >= nearest_pt$day ~ sg_points + nearest_pt$sg_point_dist,
      TRUE ~ sg_points
    )
  )

return(df)
}
