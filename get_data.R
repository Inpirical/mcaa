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
  "KFRENCH/MOMENTUM_M" # Monthly momentum factors.
)

# Pull the data sets into one frame.
dat = do.call(Quandl, list(quandl.datasets, type="xts"))

colnames(dat) = c("RMRF", "SMB", "HML", "RF", "UMD")

# Save the data list to the data folder.
save(dat, file="data/dat.Rdat")
