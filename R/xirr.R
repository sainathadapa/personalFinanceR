#~ Title: XIRR Excel function simulation
#~ 
#~ Reference 1: XIRR manual - http://office.microsoft.com/en-gb/excel-help/xirr-HP005209341.aspx
#~ Reference 2: How to calculate IRR manually - http://www.s-anand.net/Calculating_IRR_manually.html
#~ 
#~ Step 1: enter zeroes (0) against dates that do not have any cash outflow or inflows.
#~ Step 1bis: calculate IRR for these cash flow values using normal IRR function.
#~ Step 1tris: or using an iteractive approach as bisection method to find the NPV zeroes.
#~ Step 2: multiply this value of IRR by 365 to get annual IRR (since, these are daily cash flows).
#~ Step 3: refine using the formula =( 1+ R / 365) ^ 365 - 1), where R is the the value obtained in Step2.
#~

# 
sppv <- function (i, n) {
  return((1 + i/100)^(-n))
}

# Net Present Value
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

#~ npv(rep(8792,12), 666.31/12) # 15755.01 - 100000 - 10.0088%

# the correct IRR is 10.06% - source: matlab example
mycashflow <- c(-10000,2500, 2000, 3000,4000)
mydates <- c("12-01-87", "14-02-88", "03-03-88", "14-06-88", "01-12-88")
xirr(mycashflow, mydates)

# the correct IRR is 37.34% - source: microsoft example
mycashflow <- c(-10000,2750,4250,3250,2750)
mydates <- c("1-01-08","1-03-08","30-10-08","15-02-09","1-04-09")
xirr(mycashflow, mydates)
