extract_raw_data_from_cams_stmnt <- function(file_location, password = NULL) {

  tmp_file_path <- tempfile()

  args_list <- c('-jar',
                 system.file('tabula-0.9.2-jar-with-dependencies.jar',
                             package = 'personalFinanceR',
                             mustWork = TRUE),
                 '-c', '70.5,333,375,433,492',
                 '--pages', 'all',
                 '-o', tmp_file_path)

  if (!is.null(password)) args_list <- c(args_list, '--password', password)

  args_list <- c(args_list, file_location)

  proc_out_status <- system2(command = 'java',
                             args = args_list,
                             stderr = FALSE)

  if (proc_out_status != 0) stop('return status from tabula is not 0!')

  extracted_data <- fread(tmp_file_path, header = FALSE)

  unlink(tmp_file_path)

  return(extracted_data)
}

format_numbers <- function(x) {
  x %>%
    str_replace_all(pattern = ',', replacement = '') %>%
    str_replace_all(pattern = '^\\(', replacement = '-') %>%
    str_replace_all(pattern = '\\)$', replacement = '') %>%
    as.numeric
}

#' @import data.table
#' @import magrittr
#' @import stringr
#' @import stringi
#' @export
cams_stmnt_extracter <- function(file_location, password = NULL) {

  tmp <- extract_raw_data_from_cams_stmnt(file_location, password = password)

  if (tmp %>% ncol %>% equals(6) %>% not) stop('Number of columns doesnt equal six!')

  which_ones_date <- tmp[[1]] %>% str_trim %>% as.POSIXct(format = '%d-%b-%Y') %>% is.na %>% not
  which_ones_date_rle <- rle(which_ones_date)
  which_ones_date_rle$end <- cumsum(which_ones_date_rle$lengths)
  which_ones_date_rle$start <- c(1, cumsum(which_ones_date_rle$lengths) + 1)
  which_ones_date_rle$start <- which_ones_date_rle$start[-length(which_ones_date_rle$start)]

  purchases_start <- which_ones_date_rle$start[which_ones_date_rle$values]
  purchases_end <- which_ones_date_rle$end[which_ones_date_rle$values]

  info_start <- which_ones_date_rle$start[!which_ones_date_rle$values]
  info_end <- which_ones_date_rle$end[!which_ones_date_rle$values]

  rm(which_ones_date, which_ones_date_rle)

  setnames(tmp, paste0('V', 1:6), c('Date', 'Transaction', 'Amount', 'Units', 'Price', 'CumulativeUnits'))

  purchase_data <- Map(f = function(i,j) {
    tmp[i:j]
  }, i = purchases_start, j = purchases_end)

  rm(purchases_start, purchases_end)

  info_data <- Map(f = function(i,j) {
    apply(X = tmp[i:j], MARGIN = 1, FUN = paste0, collapse = '')
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
    rbindlist(l = purchase_data[i == is_folio_present], use.names = TRUE, fill = FALSE)
  })

  rm(is_folio_present)

  info_data <- unlist(x = info_data, recursive = FALSE, use.names = FALSE)

  mf_names <- grep(x = info_data, pattern = 'Registrar', value = TRUE) %>%
    str_trim %>%
    str_replace_all('Mid-Cap', 'Mid Cap') %>%
    stri_extract_all(regex = '[a-zA-Z ]+', merge = TRUE) %>%
    sapply(function(x) x[which.max(str_length(x))]) %>%
    str_trim %>%
    str_replace_all('([A-Z][a-z]+)([A-Z][a-z]+)', ' \\1 \\2') %>%
    str_trim %>%
    stri_trans_totitle %>%
    str_replace_all('Hdfc', 'HDFC')

  if (length(mf_names) != length(purchase_data)) stop('Number of MF names is not equal to the length of purchase data!')

  valuation_text <- grep(info_data, pattern = 'Valuation', value = TRUE)

  if (length(valuation_text) != length(purchase_data)) stop('Number of MF names is not equal to the length of purchase data!')

  valuation <- valuation_text %>%
    str_replace_all('.*Valuation.*INR(.*)$', '\\1') %>%
    str_trim %>%
    format_numbers

  if (valuation %>% is.na %>% any) stop('some valuations are NA!')

  valuation_date <- valuation_text %>%
    str_replace_all('.*Valuation.*on(.*):.*INR.*$', '\\1') %>%
    str_trim %>%
    as.POSIXct(format = '%d-%b-%Y')

  if (valuation_date %>% is.na %>% any) stop('some valuation dates are NA!')

  closing_unit_balance <- valuation_text %>%
    str_replace_all('.*Balance: ([0-9,\\.]+).*', '\\1') %>%
    format_numbers

  if (closing_unit_balance %>% is.na %>% any) stop('some closing unit balances are NA!')

  nav <- valuation_text %>%
    str_replace_all('.* ([0-9,\\.]+)Valuation.*', '\\1') %>%
    format_numbers

  if (nav %>% is.na %>% any) stop('some NAVs are NA!')

  rm(valuation_text)

  purchase_data <- lapply(purchase_data, function(x) {
    x[, Date := as.POSIXct(Date, format = '%d-%b-%Y')]
    x[, Amount := format_numbers(Amount)]
    x[, Units  := format_numbers(Units)]
    x[, Price  := format_numbers(Price)]
    x[, CumulativeUnits   := format_numbers(CumulativeUnits)]
    x
  })

  metadata <- data.table(Name = mf_names,
                         Valuation = valuation,
                         ValuationDate = valuation_date,
                         NAV = nav,
                         ClosingUnitBalance = closing_unit_balance)

  ans <- lapply(seq_along(purchase_data), function(i) {
    list(
      metadata = metadata[i],
      transactions = purchase_data[[i]]
    )
  })

  names(ans) <- mf_names

  ans
}
