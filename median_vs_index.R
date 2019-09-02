library(ggplot2)
library(lubridate)

MSPUS <- read.csv("./raw_data/MSPUS.csv", stringsAsFactors = FALSE)
head(MSPUS)
tail(MSPUS)
MSPUS$DATE <- as.Date(MSPUS$DATE)

# add CPI data
# https://fred.stlouisfed.org/series/CPIAUCSL
cpi.data <- read.csv("./raw_data/CPIAUCSL.csv", stringsAsFactors = FALSE)
head(cpi.data)
cpi.data$DATE <- as.Date(cpi.data$DATE)

# deflate median home prices

# quarterly all sales version
hp.data <- merge(MSPUS, cpi.data, by="DATE")
hp.data$real.median <- hp.data$MSPUS / hp.data$CPIAUCSL

# US Real Home Prices Data (from http://www.econ.yale.edu/~shiller/data/Fig3-1.xls, originally in Irrational Exuberance)
# formatted in https://docs.google.com/spreadsheets/d/1NqAZQHDQzG-y-7vsj_ixmoYbyaQcj9WbGF2eUrZSecs/edit#gid=0
real.res.prices.schiller <- read.table("./prepared_data/Schiller Real Home Prices Formatting - output.tsv",
                                       sep="\t",header=TRUE)
head(real.res.prices.schiller)
tail(real.res.prices.schiller)

real.res.prices.schiller$Date.Formatted <- as.Date(real.res.prices.schiller$Date.Formatted)
# subtract 2 months to the Schiller data to get comparable dates
real.res.prices.schiller$Date.Comp <- real.res.prices.schiller$Date.Formatted - months(2)

# combine the 2 series
comp.data <- merge(hp.data, real.res.prices.schiller, by.x="DATE", by.y = "Date.Comp")
comp.data$real.median.normalized <- comp.data$real.median / comp.data$real.median[1]
comp.data$real.index.normalized <- comp.data$Real.Home.Price.Index / comp.data$Real.Home.Price.Index[1]
plot(comp.data$DATE,comp.data$real.median.normalized,type="l",col="red")
lines(comp.data$DATE,comp.data$real.index.normalized)

# add recessions
recessions <- read.table("./raw_data/recessions.tsv",sep="\t",header=TRUE,colClasses=c('Date', 'Date'))
recessions$Peak <- as.Date(recessions$Peak)
recessions$Trough <- as.Date(recessions$Trough)
head(recessions)
tail(recessions)

recessions.trimmed <- recessions[recessions$Peak>min(comp.data$DATE) & 
                                   recessions$Peak<max(comp.data$DATE) ,]

# plot both together w/ recessions
# data formatting to get multiple series together
index.data <- data.frame("DATE"=comp.data$DATE,
                         "values"=comp.data$"real.index.normalized",
                         "series"="Case-Schiller Index")
median.data <- data.frame("DATE"=comp.data$DATE,
                         "values"=comp.data$"real.median.normalized",
                         "series"="Median Sales Price")
comp.data.plot <- rbind(index.data,median.data)
ggplot(data=comp.data.plot) + 
  geom_line(aes(x=DATE,y=values,color=series)) + 
  ggtitle("Case-Schiller Index vs Median Sales Price") + 
  xlab("Date") + ylab("Normalized Real Prices") + 
  theme_light() + theme(plot.title = element_text(hjust = 0.5)) + 
  geom_hline(yintercept=0) + 
  scale_color_manual(values=c("blue","red")) + 
  geom_rect(data=recessions.trimmed, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='black', alpha=0.3)
# ggsave("./charts/median_vs_index.png")
# the two series only significantly disagree twice, in the late 60's and 2017-2019
# last time, the median sales prices data was right
