# Modified from original code by Alberto Santini
# Original code is present at https://gist.github.com/albertosantini/3638454
sppv <- function (i, n) {
  return((1 + i/100)^(-n))
}

npv <- function(x, i) {
  npv = c()
  for (k in 1:length(i)) {
    pvs = x * sppv(i[k], 1:length(x))
    npv = c(npv, sum(pvs))
  }
  return(npv)
}

# Internal rate of return for non-periodic cash flow
# Input: cashflow - vector of numeric
#           dates - vector of strings containing dates with this format "%d-%m-%y"
# Output: irr - internal rate of return - range 0,1
#
xirr <- function (cashflow, dates) {
  if (length(cashflow) != length(dates)) {
    stop("length(cashflow) != length(dates)")
  }
  
  cashflow_adj <- c(cashflow[1])
  for (i in 1:(length(cashflow)-1)) {
    d1 <- as.Date(dates[i], "%d-%m-%y")
    d2 <- as.Date(dates[i+1], "%d-%m-%y")
    # There are no checks about the monotone values of dates
    interval <- as.integer(d2 - d1)
    cashflow_adj <- c(cashflow_adj, rep(0, interval-1), cashflow[i+1])
  }
  
  # Bisection method finding the rate to zero npv
  left = -10
  right = 10
  epsilon = 1e-8
  while (abs(right-left) > 2*epsilon) {
    midpoint = (right+left)/2
    if (npv(cashflow_adj, left) * npv(cashflow_adj, midpoint) > 0) {
      left = midpoint
    } else {
      right = midpoint
    }
  }
  
  # Irr for daily cashflow (not in percentage format)
  irr = (right+left) / 2 / 100  
  # Irr for daily cashflow multiplied by 365 to get yearly return
  irr <- irr * 365 
  # Annualized yield (return) reflecting compounding effect of daily returns
  irr <- (1 + irr / 365) ^ 365 - 1
  
  irr
}
