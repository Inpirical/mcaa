# Functions for use in MCAA. 

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
  dat = dat[ , columns]

  x = sapply(dat, function(d) {
    sapply(start.years, function(start.year) {
      d = d[paste0(start.year, "::", end.year)]
      fun(d)
    })
  })

  rownames(x) = paste0(start.years, "-", end.year)
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
  (mean(x, na.rm=TRUE) / sd(x, na.rm=TRUE)) * sqrt(12)
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

