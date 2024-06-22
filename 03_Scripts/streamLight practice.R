# stream light practice
install.packages("devtools")
install.packages("data.table")

library(devtools)
R_REMOTES_NO_ERRORS_FROM_WARNINGS='false'

library(devtools)
library(data.table)


devtools::install_github("psavoy/StreamLightUtils")
devtools::install_github("psavoy/StreamLight")

library(StreamLightUtils)
