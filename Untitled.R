library(ggplot2)
source("label_trends.R")

# recessions data (from https://fredhelp.stlouisfed.org/fred/data/understanding-the-data/recession-bars/)
# 1857 - present
recessions <- read.table("./raw_data/recessions.tsv",sep="\t",header=TRUE,colClasses=c('Date', 'Date'))
recessions$Peak <- as.Date(recessions$Peak)
recessions$Trough <- as.Date(recessions$Trough)
head(recessions)
tail(recessions)

### Median Sales Price

# look at median sales price instead (closer to real-time indicator)
# https://fred.stlouisfed.org/series/MSPUS
MSPUS <- read.csv("./raw_data/MSPUS.csv", stringsAsFactors = FALSE)
head(MSPUS)

# join with CPI data
