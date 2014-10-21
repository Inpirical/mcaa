# Downloading Fama-French Factors data from Quandl and saving the data as an
# R xts time series.

# we are dealing with monthly data on the following categories

# RMRF    - return on market minus risk-free rate
# RF      - risk-free rate
# SMB     - "Small minus Big" a.k.a. the "size" factor
# HML     - "High minus Low" a.k.a. the "value" factor
# UMD     - "Up mins Down" a.k.a. the "momentum factor

# Structure

# We store all the monthly data in an xts time frame with column headings comprisi
# ng two parts: a) the category and b) the specific data separated by full-stop ".".
# The factor itself is stored only with the category part of the name (e.g. dat$UMD
# gives the momentum factor (UMD).

# For example **dat$UMD.long** contains the monthly long-side returns of the UMD
# factor.


# Data
# ----

# RMRF	- single column containing the log market excess returns.
# RF	- single column containing the risk-free rate.

# SMB	- "sh", "s", "sl": small high, mid and low returns respectively.
#	- "bh", "b", "bl": big high, mid and low returns respectively.
#	- "long" long side returns.
#	- "short" short side returns.
#	- "" the factor

# HML	- "sh", "sl": small high and low returns respectively.
#	- "bh", "bl": big high and low returns respectively.
#	- "long" long side returns.
#	- "short" short side returns.
#	- "" the factor

# UMD	- "sh", "sl": small high and low returns respectively.
#	- "bh", "bl": big high and low returns respectively.
#	- "long" long side returns.
#	- "short" short side returns.
#	- "" the factor

# Load required packages.
require(Quandl)   # For using the Quandl API.
require(xts)      # FOr using xts time series.

# Load the Quandl API token you want to use (from a local file).
file.name = "quandl_api.token"

quandl.token = readChar(
  con    = file.name,
  nchars = file.info(file.name)$size - 1
  # Subtract 1 from "nchars" to remove trailing newline.
)

# Load the API token for use with Quandl.
Quandl.auth(quandl.token)


# Quandl data sets we are going to use ("_M" signifies monthly).
data.sets = list(
  "FACTORS_M",       # Factors SMB and HML, and also RMRF and RF.
  "MOMENTUM_M",      # Monthly momentum factors.
  "6_P_ME_AVWR_M",   # Data for deriving the "momentum" factor.
  "6_S_BTM_AVWR_M"   # Data for deriving "value", and "size" factors.
)

# Pull all the data into one big list

data.list = lapply(data.sets, function(data.set) {

  x = Quandl(paste0("KFRENCH/", data.set), type="raw")
  # We pull the data as type "raw" rather than "xts", then do our own conversion
  # to "xts" to avoid the time index being "yearmon" rather than "Date". Yearmon
  # has implementation issues with time indexing..
  as.xts(x[ , -1], order.by=x[ , 1])
})

names(data.list) = data.sets


# Extracting the "factors data from the data list".
dat = data.list$FACTORS_M
colnames(dat) = c("RMRF", "SMB", "HML", "RF")
dat$UMD = data.list$MOMENTUM_M


# Pull the data needed to compute "UMD" into the time series frame.
dat = cbind(dat, data.list[["6_P_ME_AVWR_M"]][ , c(1, 3, 4, 6)])
colnames(dat)[6:9] = paste0("UMD.", c("sl", "sh", "bl", "bh"))


# Calculate the long and short side returns of the UMD / Momentum factor.
dat$UMD.long  = ((dat$UMD.sh + dat$UMD.bh) / 2)   # Avg. big and small "high"
dat$UMD.short = ((dat$UMD.sl + dat$UMD.bl) / 2)   # Avg. big and small "low"


# Pull the data needed to compute "SMB" into the time series frame.
dat = cbind(dat, data.list[["6_S_BTM_AVWR_M"]])
colnames(dat)[c(12:17)] = paste0("SMB.", c("sl", "s", "sh", "bl", "b", "bh"))

dat$SMB.long  = (dat$SMB.sl + dat$SMB.s + dat$SMB.sh) / 3
dat$SMB.short = (dat$SMB.bl + dat$SMB.b + dat$SMB.bh) / 3


# Pull the data needed to compute "HML" into the time series frame.
dat = cbind(dat, data.list[["6_S_BTM_AVWR_M"]][ , c(1, 3, 4, 6)])
colnames(dat)[20:23] = paste0("HML.", c("sl", "sh", "bl", "bh"))

dat$HML.long  = (dat$HML.sh + dat$HML.bh) / 2
dat$HML.short = (dat$HML.sl + dat$HML.bl) / 2


# Save the "dat" variable.
save(dat, file="data/dat.Rdat")
