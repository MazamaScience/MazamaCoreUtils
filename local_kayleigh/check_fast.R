# check fast function which skips vignettes
check_fast <- function() {
  devtools::check(
    build_args = c("--no-build-vignettes"),
    args = c('--ignore-vignettes')
  )

}
