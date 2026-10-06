library(tidyquant)
library(tidyverse)

unemp_raw <- tq_get(
  c("UNRATE", "ORUR", "CAUR", "TXUR", "NYUR"),
  get = "economic.data",
  from = "1976-01-01"
)
