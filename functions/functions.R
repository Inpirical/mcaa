# Functions for "Re Momentum, Size and Value (MCAA)"

# "CalibStat": data against statistics reported in Asness, Frazzini,
#   Israel, Moskowitz (2014) for a given function.
#
# "HitRatios": percentage of periods with positive returns given window.
# "SharpeAnnual": annualised sharpe ratio given monthly data.
#
# "MixedStrat" : evaluate function over a linear combination of UMD, HML and
#   SMB factors. 
#
# "MovingAvgReturns": moving average log returns from monthly log returns
# "ReturnsMinusMarket": absolute asset returns net of absolute market returns
#
# "MarketAdjReturn": market adjusted return from absolute return and market returns
#   using linear regression to estimate alpha.
#
# "MovingMarketAdjustedReturn": series of adjusted return calculations given
#   a fixed-length trailing window.


CalibStat = function(dat, fun, columns=c("RMRF", "SMB", "HML", "UMD"),
  end.year=2013, start.years=c(1927, 1963, 1991)) {
  # Benchmark our data against statistics reported in Asness, Frazzini,
  # Israel, Moskowitz (2014) for a given function.

  # Arguments:
  # "dat" the historical data on Fama French factors, one column
  #   per factor (xts)
  # "fun" the function used to calculate the reported statistic (function)
  # "columns" the columns of the data set to perform calculations on (character)
  # "end.year" the last year of the data to be inluded (numeric)
  # "start.years" the different start years to use for the benchmarking
  #   (numeric)

  # Returns:
  # Results of the function per start year and per factor (matrix)

  # Subset the data on the columns we want to analyse.
  dat = dat[ , columns]

  # Apply the chosen function to the data subset for the desidered time range
  # determined byt "end.year" and "start.year".
  x = sapply(dat, function(d) {
    sapply(start.years, function(start.year) {
      d = d[paste0(start.year, "::", end.year)]
      fun(d)
    })
  })

  # Format the row names.
  rownames(x) = paste0(start.years, "-", end.year)

  # Return the results.
  x
}


HitRatios = function(dat, months) {
  # Calculate the percentage of periods with positive returns given a moving avarege window length.

  # Arguments:
  # "dat" time series with values per Fama-French factor (xts)
  # "months" number of months in the moving average (numeric)

  # Returns: hit ratio in percent per Fama-French factor (numeric)

  sapply(dat, function(d) {
    x = mean(filter(d, rep(1 / months, months), sides=1) > 0, na.rm=TRUE)
    round(100 * x)   # Convert to perentage.
  })
}


SharpeAnnual = function(x) {
  # Computes annualised sharpe ratio given monthly data.
  # Arguments: "x" time series of monthly log returns (xts)
  # Returns: annualised sharpe ratio (numeric)

  sharpe.monthly = (mean(x, na.rm=TRUE) / sd(x, na.rm=TRUE))
  sharpe.monthly * sqrt(12)   # Annualise ratio.
}


MixedStrat = function(dat, fun, w1, w2) {
  # Computes the result of a function applied to a mix of UMD, HML, and SMB
  # factors with non-negative weights summing to 100%.

  # Arguments:
  # "dat" time series of returns on UMD, HML and SMB factors (xts).
  # "fun" function to be applied to the mixed factor returns (function).
  # "w1" the absolute weight applied to the "UMD" factor (numeric).
  # "w2" the weight of the "HML" factor relative to the "SMB" factor (numeric).

  # Returns:
  # the result of the function applied to the time series of the mixed factor.

  # We essentially compute the weights for each factor, apply the weights
  # (multiply) to each factor and add the weighted reutrns together (since we
  # are dealing with log returns).

  fun(
    (dat$UMD * w1) +
    (dat$HML * (1 - w1) * w2) +
    (dat$SMB * (1 - w1) * (1 - w2))
  )
}


MovingAvgReturns = function(results, ma.periods) {
  # Computes moving average log returns from monthly log returns.

  # Arguments: "results" periodic return data (xts)
  # "ma.months" number of months for computing average (integer)

  # Returns: moving average returns (xts)

  x = filter(results, rep(1 / ma.periods, ma.periods), sides=1)
  na.omit(as.xts(as.numeric(x), order.by=index(results)))
}


ReturnsMinusMarket = function(x, rmrf, rf) {
  # Calculates returns net of market.

  # Arguments:
  # "x" absolute returns time series of an asset (xts).
  # "rmrf" excess returns time series of the market over risk free rate (xts).
  # "rf" time series of the risk free rate (xts).

  # Returns:
  # time series of excess returns of asset over market returns (xts).

  x - rmrf - rf   # Risk free rate deducted to get absolute market returns.
}


MarketAdjReturn = function(x, rmrf, rf){
  # Calculates the market adjusted return from the absolute returns of an asset.

  # Arguments:
  # "x" time series of log returns of the asset (xts)
  # "rmrf" time series of excess returns of the whole market (xts)
  # "rf" time series of risk free rate (xts)

  # Returns:
  # The estimated market adjusted periodic return of the asset (numeric)

  excess.returns = x - rf   # Absolute asset return minus risk-free rate.
 
  # Linearly regress asset excess returns market excess returns and use the
  # intercept as an estimate of the asset market-adjusted returns.
  lm((x -rf) ~ (rmrf))$coefficients[["(Intercept)"]]
}


MovingMarketAdjustedReturn = function(x, rmrf, rf, periods) {
  # Calculates the market adjusted return based on a trailing window of
  # fixed length given a returns time series.

  # Arguments:
  # "x" time series of log returns of the asset (xts)
  # "rmrf" time series of excess returns of the whole market (xts)
  # "rf" time series of risk free rate (xts)
  # "periods" number of months lenght of the trailing window (numeric)

  # Returns:
  # a series of moving market adjusted returns (xts)

  
  sapply((periods + 1):(nrow(x) - periods), function(i) {

    # Calculate the rows corresponding to the period and window size.
    rows = (i - periods + 1):i

    # Calculate the market adjusted return for the relevant rows.
    MarketAdjReturn(x[rows, ], rmrf[rows, ], rf[rows, ])

  })
}
