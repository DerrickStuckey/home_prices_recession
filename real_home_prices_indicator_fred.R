library(ggplot2)
source("label_trends.R")

# original FRED data (https://fred.stlouisfed.org/series/QUSR628BIS)
# 1970 - present
real.res.prices.fred <- read.table("./raw_data/QUSR628BIS.csv",sep=",",header=TRUE)
head(real.res.prices.fred)
tail(real.res.prices.fred)

# recessions data (from https://fredhelp.stlouisfed.org/fred/data/understanding-the-data/recession-bars/)
# 1857 - present
recessions <- read.table("./raw_data/recessions.tsv",sep="\t",header=TRUE,colClasses=c('Date', 'Date'))
recessions$Peak <- as.Date(recessions$Peak)
recessions$Trough <- as.Date(recessions$Trough)
head(recessions)
tail(recessions)

### look only at data after 1970
real.res.prices.recent <- real.res.prices.schiller[real.res.prices.schiller$Date.Formatted>=as.Date("1970-01-01"),]
head(real.res.prices.recent)

plot(real.res.prices.recent$Date.Formatted,real.res.prices.recent$Real.Home.Price.Index,type="l")

# trim recessions to period "1970 - present"
recessions.recent <- recessions[recessions$Peak>=min(real.res.prices.recent$Date),]

# Real home prices since 1970 with recessions shaded
ggplot(data=real.res.prices.recent) + 
  geom_line(aes(x=Date.Formatted,y=Real.Home.Price.Index),color="blue") + 
  ggtitle("Real Home Prices Since 1970") + 
  xlab("Date") + ylab("Real Home Prices") + 
  theme_light() + theme(plot.title = element_text(hjust = 0.5)) + 
  geom_hline(yintercept=0) + 
  geom_rect(data=recessions.recent, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='black', alpha=0.3)
# ggsave(filename = "./charts/rhp_recessions_ind_1970_pres_nr.png")

# mark declines of 'critical.change.rate' % or more
decline.indexes <- flag.drops.pct(real.res.prices.recent$Real.Home.Price.Index, 0.03)
real.res.prices.recent[decline.indexes,]

# Real home prices since 1970 with recessions shaded
# and declines of 3% or more from the previous peak flagged
ggplot(data=real.res.prices.recent) + 
  geom_line(aes(x=Date.Formatted,y=Real.Home.Price.Index),color="blue") + 
  ggtitle("Real Home Prices Since 1970\nDeclines of 3% in Red") + 
  xlab("Date") + ylab("Real Home Prices") + 
  theme_light() + theme(plot.title = element_text(hjust = 0.5)) + 
  geom_hline(yintercept=0) + 
  geom_rect(data=recessions.recent, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="black", alpha=0.3) + 
  geom_vline(xintercept=real.res.prices.recent$Date.Formatted[decline.indexes], color="red")
# ggsave(filename = "./charts/rhp_recessions_ind_1970_pres.png")


### Try for period 1953 - 1970 (1953 is the earliest year with monthly home price data)
real.res.prices.midcent <- real.res.prices.schiller[
  real.res.prices.schiller$Date.Formatted>=as.Date("1953-01-01") &
    real.res.prices.schiller$Date.Formatted<as.Date("1970-01-01"),]
head(real.res.prices.midcent)
tail(real.res.prices.midcent)
decline.indexes.midcent <- flag.drops.pct(real.res.prices.midcent$Real.Home.Price.Index, 0.03)
recessions.midcent <- recessions[recessions$Peak>=min(real.res.prices.midcent$Date) &
                                   recessions$Peak<=max(real.res.prices.midcent$Date),]

# Real home prices 1953 - 1970 with recessions shaded
# and declines of 3% or more from the previous peak flagged
ggplot(data=real.res.prices.midcent) + 
  geom_line(aes(x=Date.Formatted,y=Real.Home.Price.Index),color="blue") + 
  ggtitle("Real Home Prices 1953-1970\nDeclines of 3% in Red") + 
  xlab("Date") + ylab("Real Home Prices") + 
  theme_light() + theme(plot.title = element_text(hjust = 0.5)) + 
  geom_hline(yintercept=0) + 
  geom_rect(data=recessions.midcent, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="black", alpha=0.3) + 
  geom_vline(xintercept=real.res.prices.midcent$Date.Formatted[decline.indexes.midcent], color="red")
# ggsave(filename = "./charts/rhp_recessions_ind_1953_1970.png")


### full data after 1953
real.res.prices.modern <- real.res.prices.schiller[real.res.prices.schiller$Date.Formatted>=as.Date("1953-01-01"),]
head(real.res.prices.modern)

# trim recessions to period "1953 - present"
recessions.modern <- recessions[recessions$Peak>=min(real.res.prices.modern$Date),]

# Real home prices since 1953 with recessions shaded
ggplot(data=real.res.prices.modern) + 
  geom_line(aes(x=Date.Formatted,y=Real.Home.Price.Index),color="blue") + 
  ggtitle("Real Home Prices Since 1953") + 
  xlab("Date") + ylab("Real Home Prices") + 
  theme_light() + theme(plot.title = element_text(hjust = 0.5)) + 
  geom_hline(yintercept=0) + 
  geom_rect(data=recessions.modern, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='black', alpha=0.3)

# mark declines of 'critical.change.rate' % or more
decline.indexes <- flag.drops.pct(real.res.prices.modern$Real.Home.Price.Index, 0.03)
real.res.prices.modern[decline.indexes,]

# Real home prices since 1953 with recessions shaded
# and declines of 3% or more from the previous peak flagged
ggplot(data=real.res.prices.modern) + 
  geom_line(aes(x=Date.Formatted,y=Real.Home.Price.Index),color="blue") + 
  ggtitle("Real Home Prices Since 1953\nDeclines of 3% in Red") + 
  xlab("Date") + ylab("Real Home Prices") + 
  theme_light() + theme(plot.title = element_text(hjust = 0.5)) + 
  geom_hline(yintercept=0) + 
  geom_rect(data=recessions.modern, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="black", alpha=0.3) + 
  geom_vline(xintercept=real.res.prices.modern$Date.Formatted[decline.indexes], color="red")
# ggsave(filename = "./charts/rhp_recessions_ind_1953_pres.png")

### Nominal home prices

# mark declines of 3% or more
decline.indexes <- flag.drops.pct(real.res.prices.modern$Nominal.Home.Price.Index, 0.03)
real.res.prices.modern[decline.indexes,]

ggplot(data=real.res.prices.modern) + 
  geom_line(aes(x=Date.Formatted,y=Nominal.Home.Price.Index),color="blue") + 
  ggtitle("Nominal Home Prices\nDeclines of 3% in Red") + 
  xlab("Date") + ylab("Nominal Home Prices (log-scale)") + 
  theme_light() + theme(plot.title = element_text(hjust = 0.5)) + 
  geom_hline(yintercept=0) + 
  geom_rect(data=recessions.modern, aes(xmin=Peak, xmax=Trough, ymin=0, ymax=+Inf), fill="black", alpha=0.3) + 
  geom_vline(xintercept=real.res.prices.modern$Date.Formatted[decline.indexes], color="red") + 
  scale_y_continuous(trans="log10")
# ggsave(filename = "./charts/nominal_hp_declines_3pct.png")

# mark declines of 1% or more
decline.indexes <- flag.drops.pct(real.res.prices.modern$Nominal.Home.Price.Index, 0.01)
real.res.prices.modern[decline.indexes,]

ggplot(data=real.res.prices.modern) + 
  geom_line(aes(x=Date.Formatted,y=Nominal.Home.Price.Index),color="blue") + 
  ggtitle("Nominal Home Prices\nDeclines of 1% in Red") + 
  xlab("Date") + ylab("Nominal Home Prices (log-scale)") + 
  theme_light() + theme(plot.title = element_text(hjust = 0.5)) + 
  geom_hline(yintercept=0) + 
  geom_rect(data=recessions.modern, aes(xmin=Peak, xmax=Trough, ymin=0, ymax=+Inf), fill="black", alpha=0.3) + 
  geom_vline(xintercept=real.res.prices.modern$Date.Formatted[decline.indexes], color="red") + 
  scale_y_continuous(trans="log10")
# ggsave(filename = "./charts/nominal_hp_declines_1pct.png")


### Pre-1953
real.res.prices.old <- real.res.prices.schiller[real.res.prices.schiller$Date.Formatted<as.Date("1953-01-01"),]
head(real.res.prices.old)
tail(real.res.prices.old)

plot(real.res.prices.old$Date.Formatted,real.res.prices.old$Real.Home.Price.Index,type="l")

# trim recessions to period before 1953
recessions.old <- recessions[recessions$Peak>=min(real.res.prices.old$Date) & 
                               recessions$Peak<=max(real.res.prices.old$Date),]
head(recessions.old)
tail(recessions.old)

# mark declines of 'critical.change.rate' % or more
decline.indexes <- flag.drops.pct(real.res.prices.old$Real.Home.Price.Index, 0.03)
real.res.prices.old[decline.indexes,]

# Real home prices with recessions shaded
# and declines of 3% or more from the previous peak flagged
ggplot(data=real.res.prices.old) + 
  geom_line(aes(x=Date.Formatted,y=Real.Home.Price.Index),color="blue") + 
  ggtitle("Real Home Prices Since 1970\nDeclines of 3% in Red") + 
  xlab("Date") + ylab("Real Home Prices") + 
  theme_light() + theme(plot.title = element_text(hjust = 0.5)) + 
  geom_hline(yintercept=0) + 
  geom_rect(data=recessions.old, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="black", alpha=0.3) + 
  geom_vline(xintercept=real.res.prices.old$Date.Formatted[decline.indexes], color="red")
# ggsave(filename = "./charts/rhp_recessions_ind_1890_1953.png")


### Simple Moving Average Method

# flag drops w/ simple moving average, 24-month lookback
start.val <- real.res.prices.schiller$Real.Home.Price.Index[real.res.prices.schiller$Date.Formatted=="1953-01-01"]
decline.indexes <- flag.drops.sma(real.res.prices.modern$Real.Home.Price.Index, 18, start.val)
real.res.prices.modern[decline.indexes,]

# Real home prices since 1953 with recessions shaded
# and declines of 3% or more from the previous peak flagged
ggplot(data=real.res.prices.modern) + 
  geom_line(aes(x=Date.Formatted,y=Real.Home.Price.Index),color="blue") + 
  ggtitle("Real Home Prices Since 1953\nDrops Below SMA in Red") + 
  xlab("Date") + ylab("Real Home Prices") + 
  theme_light() + theme(plot.title = element_text(hjust = 0.5)) + 
  geom_hline(yintercept=0) + 
  geom_rect(data=recessions.modern, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="black", alpha=0.3) + 
  geom_vline(xintercept=real.res.prices.modern$Date.Formatted[decline.indexes], color="red")
# ggsave(filename = "./charts/rhp_recessions_ind_1953_pres_sma.png")



