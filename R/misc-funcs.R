format_numbers <- function(x) {
  x %>%
    str_replace_all(pattern = ',', replacement = '') %>%
    str_replace_all(pattern = '^\\(', replacement = '-') %>%
    str_replace_all(pattern = '\\)$', replacement = '') %>%
    as.numeric
}
