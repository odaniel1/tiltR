## EXAMPLE ONLY - Actual tiltURLs.R is in .gitignore
## code to prepare `tiltURLs` dataset
tiltURLs <- tibble::tribble(
  ~name, ~status, ~url,
  "name1", "status1", "url1"
)

usethis::use_data(tiltURLs, internal = TRUE)
