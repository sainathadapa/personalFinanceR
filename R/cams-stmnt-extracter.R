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

parse_each_section <- function(x) {

  purchases <- x[c(-1:-3, -nrow(x)),]
  setnames(purchases, paste0('V', 1:6), c('Date', 'Transaction', 'Amount', 'Units', 'Price', 'CumulativeUnits'))
  purchases[, Date := as.POSIXct(Date, format = '%d-%b-%Y')]
  purchases[, Amount := format_numbers(Amount)]
  purchases[, Units  := format_numbers(Units)]
  purchases[, Price  := format_numbers(Price)]
  purchases[, CumulativeUnits   := format_numbers(CumulativeUnits)]
  purchases <- purchases[!is.na(purchases$Amount),]

  folio_number <- x[1,] %>% as.character %>% paste0(collapse = '') %>%
    str_extract_all('[0-9][[0-9]/ ]+') %>% extract2(1) %>% extract2(1)

  mf_name <- x[2,] %>% as.character %>% paste0(collapse = '') %>%
    str_trim %>%
    str_replace_all('Mid-Cap', 'Mid Cap') %>%
    stri_extract_all(regex = '[a-zA-Z ]+', merge = TRUE) %>%
    sapply(function(x) x[which.max(str_length(x))]) %>%
    str_trim %>%
    str_replace_all('([A-Z][a-z]+)([A-Z][a-z]+)', ' \\1 \\2') %>%
    str_trim %>%
    stri_trans_totitle %>%
    str_replace_all('Hdfc', 'HDFC') %>%
    str_replace_all('Icici', 'ICICI')

  valuation_text <- x[nrow(x)] %>% as.character %>% paste0(collapse = '')

  valuation <- valuation_text %>%
    str_replace_all('.*Valuation.*INR(.*)$', '\\1') %>%
    str_trim %>%
    format_numbers

  valuation_date <- valuation_text %>%
    str_replace_all('.*Valuation.*on(.*):.*INR.*$', '\\1') %>%
    str_trim %>%
    as.POSIXct(format = '%d-%b-%Y')

  closing_unit_balance <- valuation_text %>%
    str_replace_all('.*Balance: ([0-9,\\.]+).*', '\\1') %>%
    format_numbers

  nav <- valuation_text %>%
    str_replace_all('.* ([0-9,\\.]+)Valuation.*', '\\1') %>%
    format_numbers

  metadata <- data.table(Name = mf_name,
                         Valuation = valuation,
                         ValuationDate = valuation_date,
                         NAV = nav,
                         ClosingUnitBalance = closing_unit_balance)

  ans <- list(
    metadata = metadata,
    transactions = purchases
  )

  return(ans)
}

#' @import data.table
#' @import magrittr
#' @import stringr
#' @import stringi
#' @export
cams_stmnt_extracter <- function(file_location, password = NULL) {

  tmp <- extract_raw_data_from_cams_stmnt(file_location, password = password)

  if (tmp %>% ncol %>% equals(6) %>% not) stop('Number of columns doesnt equal six!')

  full_lines      <- apply(tmp, MARGIN = 1, FUN = paste0, collapse='')
  start_locations <- full_lines %>% str_detect('Opening.*Unit.*Balance') %>% which %>% subtract(2)
  end_locations   <- full_lines %>% str_detect('Closing.*Unit.*Balance') %>% which

  if (length(start_locations) != length(end_locations)) stop('Number of start locations does not match with number of end lcoations')

  ans <- Map(function(i,j) parse_each_section(tmp[i:j]), i = start_locations, j = end_locations)

  names(ans) <- sapply(ans, function(x) x$metadata$Name)

  ans
}
