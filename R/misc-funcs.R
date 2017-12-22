format_numbers <- function(x) {
  ans <- x %>%
    str_replace_all(pattern = ',', replacement = '') %>%
    str_replace_all(pattern = '^\\(', replacement = '-') %>%
    str_replace_all(pattern = '\\)$', replacement = '')

  suppressWarnings(as.numeric(ans))
}
