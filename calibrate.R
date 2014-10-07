# To calibrate the data pulled from Quandl against the reported statistics in the Paper.

require(xts)
load("data/dat.Rdat")
source("functions/functions.R")

# Functions, results of which we want to calibrate.
calib.funs = list(
  r      = function(x) round(mean(x) * 12, 1),
  sr     = function(x) round(sqrt(12) * mean(x) / sd(x), 2),
  hit.1y = function(x) HitRatios(x, months=12),
  hit.5y = function(x) HitRatios(x, months=(12 * 5))
)

# Use CalibStat (see "functions/functions.R") to calibrate results.
CalibResults = function() {
  lapply(calib.funs, CalibStat, dat=dat)
}
