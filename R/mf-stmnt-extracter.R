extract_tables_custom <- function(file_location, password = NULL) {
  params_list <- list(file = file_location,
                      guess = FALSE,
                      columns = list(c(70.5, 338.2, 375, 433, 492)))

  if (!is.null(password)) params_list <- c(params_list, list(password = password))

  do.call(what = tabulizer::extract_tables, args = params_list)
}

process_data <- function(extracted) {

  if (extracted %>% sapply(ncol) %>% equals(6) %>% all %>% not) stop('Number of columns doesnt equal six!')

  tmp <- lapply(extracted, function(x) {
    x <- as_data_frame(x)
    x[] <- lapply(x, str_trim)
    x
  })

  tmp <- tmp %>% bind_rows

  which_ones_date <- tmp$V1 %>% as.POSIXct(format = '%d-%b-%Y') %>% is.na %>% not
  which_ones_date_rle <- rle(which_ones_date)
  which_ones_date_rle$end <- cumsum(which_ones_date_rle$lengths)
  which_ones_date_rle$start <- c(1, cumsum(which_ones_date_rle$lengths) + 1)
  which_ones_date_rle$start <- which_ones_date_rle$start[-length(which_ones_date_rle$start)]

  purchases_start <- which_ones_date_rle$start[which_ones_date_rle$values]
  purchases_end <- which_ones_date_rle$end[which_ones_date_rle$values]

  info_start <- which_ones_date_rle$start[!which_ones_date_rle$values]
  info_end <- which_ones_date_rle$end[!which_ones_date_rle$values]

  rm(which_ones_date, which_ones_date_rle)

  purchase_data <- Map(f = function(i,j) {
    tmp %>% slice(i:j)
  }, i = purchases_start, j = purchases_end)

  rm(purchases_start, purchases_end)

  info_data <- Map(f = function(i,j) {
    extracted %>%
      lapply(as_data_frame) %>%
      bind_rows %>%
      slice(i:j) %>%
      apply(MARGIN = 1, paste0, collapse = '')
  }, i = info_start, j = info_end)

  rm(info_start, info_end)

  if (length(info_data) != (length(purchase_data) + 1)) stop('info length should be one more than purchase length!')

  is_folio_present <- info_data[-length(info_data)] %>%
    sapply(. %>%
             paste0(collapse = '\n') %>%
             tolower %>%
             str_detect(pattern = fixed('folio')) %>%
             any)

  is_folio_present <- cumsum(is_folio_present)

  purchase_data <- lapply(is_folio_present %>% unique %>% sort, function(i) {
    purchase_data[i == is_folio_present] %>% bind_rows
  })

  info_data <- lapply(is_folio_present %>% unique %>% sort, function(i) {
    which_ones <- which(i == is_folio_present)

    this_info_1 <- info_data[[min(which_ones)]]
    this_info_1 <- this_info_1[(str_detect(tolower(this_info_1), 'folio') %>% which %>% max %>% subtract(0)):length(this_info_1)]

    this_info_2 <- info_data[[max(which_ones) + 1]]
    this_info_2 <- this_info_2[1:(str_detect(tolower(this_info_2), 'folio') %>% which %>% min %>% subtract(2))]

    this_info <- c(this_info_1, this_info_2)
    this_info
  })

  mf_name <- sapply(info_data, . %>% grep(pattern = 'Registrar', value = TRUE))

  if (length(mf_name) != length(info_data)) stop('Number of MF names is not equal to the length of info data!')

  valuation_text <- lapply(info_data, . %>% grep(pattern = 'Valuation', value = TRUE)) %>%
    unlist(recursive = FALSE, use.names = FALSE)

  if (length(valuation_text) != length(info_data)) stop('Number of MF names is not equal to the length of info data!')

  valuation <- valuation_text %>%
    str_replace_all('.*Valuation.*INR(.*)$', '\\1') %>%
    str_trim %>%
    str_replace_all(',', '') %>%
    as.numeric

  if (valuation %>% is.na %>% any) stop('some valuations are NA!')

  valuation_date <- valuation_text %>%
    str_replace_all('.*Valuation.*on(.*):.*INR.*$', '\\1') %>%
    str_trim %>%
    as.POSIXct(format = '%d-%b-%Y')

  rm(valuation_text)

  format_numbers <- . %>%
    str_replace_all(pattern = ',', replacement = '') %>%
    str_replace_all(pattern = '^\\(', replacement = '-') %>%
    str_replace_all(pattern = '\\)$', replacement = '') %>%
    as.numeric

  purchase_data <- lapply(purchase_data,
                          . %>%
                            setNames(c('Date', 'Transaction', 'Amount', 'Units', 'Price', 'Unit Balance')) %>%
                            mutate(Date = as.POSIXct(Date, format = '%d-%b-%Y')) %>%
                            mutate(Amount         = format_numbers(Amount),
                                   Price          = format_numbers(Price),
                                   Units          = format_numbers(Units),
                                   `Unit Balance` = format_numbers(`Unit Balance`)) %>%
                            filter(!is.na(Amount)))

  ans <- Map(f = function(p, q, r, s, t) {
    list(purchase_data  = p,
         info_data      = q,
         valuation      = r,
         valuation_date = s,
         mf_name        = t)
  },
  p = purchase_data,
  q = info_data,
  r = valuation,
  s = valuation_date,
  t = mf_name)

  names(ans) <- mf_name

  ans
}
