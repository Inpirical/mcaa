require(Quandl)
require(xts)

# Load the Quandl API token you want to use (from a local file).
file.name = "quandl_api.token"

quandl.token = readChar(
  con    = file.name,
  nchars = file.info(file.name)$size - 1
  # Subtract 1 from "nchars" to remove trailing newline.
)

# Load the API token for use with Quandl.
Quandl.auth(quandl.token)

# Quandl data sets we are going to use.
quandl.datasets = c(
  "KFRENCH/FACTORS_M",   # Monthly Fama-French factors.
  "KFRENCH/MOMENTUM_M"   # Monthly momentum factors.
)

# Pull the data sets into one frame.
dat = do.call(Quandl, list(quandl.datasets, type="xts"))

colnames(dat) = c("RMRF", "SMB", "HML", "RF", "UMD")

# Save the data list to the data folder.
save(dat, file="data/dat.Rdat")


# Long vs. Short sides

# In addition we will be doing some calculations of performance attribution to
# long and short sides of the momentum returns. This means that we need to pull
# data down on the consitutuent 4 portfolios in the UMD factor (Big/High,
# Big/Low, Small/High, Small/Low).

ls.dataset = "KFRENCH/6_P_ME_AVWR_M"   # The Quandl code of the long-short data set

# We pull the data as type "raw" rather than "xts", then do our own conversion
# to "xts" to avoid the time index being "yearmon" rather than "Date". Yearmon
# has implementation issues with time indexing..

ls.dat = Quandl(ls.dataset, type="raw")
ls.dat = as.xts(ls.data[ , -1], order.by=ls.data[ , 1])

# Subset the data just on the columns we need for our analysis
ls.dat = ls.dat[ , c(1, 3, 4, 6)]

# Give new, abbreviated column names to the data "sl" for small/slow, "sh" for
# small/high, "bl" for big/low, "bh" for big/high.

colnames(ls.dat) = c("sl", "sh", "bl", "bh")

# Calculate the long side returns of the momentum factor (UMD)
ls.dat$long  = ((ls.dat$sh + ls.dat$bh) / 2)   # Average big and small "high"
ls.dat$short = ((ls.dat$sl + ls.dat$bl) / 2)   # Average big and small "low"

# Calculate the UMD factor
ls.dat$UMD = ls.dat$long - ls.dat$short   # Long side minus short side.

# Save the long-short data to the data folder.
save(ls.dat, file="data/ls_dat.Rdat")
