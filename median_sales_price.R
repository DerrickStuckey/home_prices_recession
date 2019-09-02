library(ggplot2)
source("label_trends.R")

# look at median sales price instead
# https://fred.stlouisfed.org/series/MSPUS
# quarterly, all houses sold
MSPUS <- read.csv("./raw_data/MSPUS.csv", stringsAsFactors = FALSE)
head(MSPUS)
tail(MSPUS)
MSPUS$DATE <- as.Date(MSPUS$DATE)

# https://fred.stlouisfed.org/series/MSPNHSUS
# monthly, new houses only
# MSPNHSUS <- read.csv("./raw_data/MSPNHSUS.csv", stringsAsFactors = FALSE)
# head(MSPNHSUS)
# MSPNHSUS$DATE <- as.Date(MSPNHSUS$DATE)

# add CPI data
# https://fred.stlouisfed.org/series/CPIAUCSL
cpi.data <- read.csv("./raw_data/CPIAUCSL.csv", stringsAsFactors = FALSE)
head(cpi.data)
cpi.data$DATE <- as.Date(cpi.data$DATE)

# add PCE data
# https://fred.stlouisfed.org/series/JCXFE
# pce.data <- read.csv("./raw_data/JCXFE.csv", stringsAsFactors = FALSE)
# head(pce.data)
# pce.data$DATE <- as.Date(pce.data$DATE)

# deflate median home prices

# quarterly all sales version
hp.data <- merge(MSPUS, cpi.data, by="DATE")
hp.data$rmhs <- hp.data$MSPUS / hp.data$CPIAUCSL
# hp.data <- merge(MSPUS, pce.data, by="DATE")
# hp.data$rmhs <- hp.data$MSPUS / hp.data$JCXFE

# monthly new sales only
# hp.data <- merge(MSPNHSUS, cpi.data, by="DATE")
# hp.data$rmhs <- hp.data$MSPNHSUS / hp.data$CPIAUCSL

plot(hp.data$DATE, hp.data$rmhs,type="l")

# recessions data (from https://fredhelp.stlouisfed.org/fred/data/understanding-the-data/recession-bars/)
# 1857 - present
recessions <- read.table("./raw_data/recessions.tsv",sep="\t",header=TRUE,colClasses=c('Date', 'Date'))
recessions$Peak <- as.Date(recessions$Peak)
recessions$Trough <- as.Date(recessions$Trough)
head(recessions)
tail(recessions)

recessions.trimmed <- recessions[recessions$Peak>min(hp.data$DATE) & 
                                   recessions$Peak<max(hp.data$DATE) ,]

# plot deflated median home sales prices with recessions
ggplot(data=hp.data) + 
  geom_line(aes(x=DATE,y=rmhs),color="blue") + 
  ggtitle("Real Median Home Sales Prices") + 
  xlab("Date") + ylab("Deflated Median Home Sales Prices") + 
  theme_light() + theme(plot.title = element_text(hjust = 0.5)) + 
  geom_hline(yintercept=0) + 
  geom_rect(data=recessions.trimmed, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='black', alpha=0.3)
# ggsave("./charts/median_hsp_w_recessions.png")

# mark declines of 'critical.change.rate' % or more
decline.indexes <- flag.drops.pct(hp.data$rmhs, 0.05)
hp.data[decline.indexes,]
head(decline.indexes)
# looks to be seasonal as many big drops are in July

# YoY change and SMA
hp.data$rmhsp.yoy <- NA
hp.data$rmhsp.ma.4 <- NA
hp.data$rmhsp.ma.4.yoy <- NA
for (i in 1:nrow(hp.data)) {
  if (i-4>0) {
    hp.data$rmhsp.yoy[i] <- hp.data$rmhs[i] / hp.data$rmhs[i-4] - 1
    hp.data$rmhsp.ma.4[i] <- (hp.data$rmhs[i-3] + hp.data$rmhs[i-2] + hp.data$rmhs[i-1] + hp.data$rmhs[i])/4
  }
  if (i-8>0) {
    hp.data$rmhsp.ma.4.yoy[i] <- hp.data$rmhsp.ma.4[i] / hp.data$rmhsp.ma.4[i-4] - 1
  }
}

# plot real median home sales YoY change w/ recessions shaded
ggplot(data=hp.data) + 
  geom_line(aes(x=DATE,y=rmhsp.yoy),color="blue") + 
  ggtitle("Real Median Home Sales Prices\nYoY Change") + 
  xlab("Date") + ylab("YoY Prices Change") + 
  theme_light() + theme(plot.title = element_text(hjust = 0.5)) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  geom_hline(yintercept=0) + 
  geom_rect(data=recessions.trimmed, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='black', alpha=0.3)
# ggsave("./charts/median_hsp_yoy.png")
# definitely a relationship, but looks more like a coincident indicator than a leading one

# still pretty noisy, so let's try the 4-quarter MA version
# plot real median home sales 4-quarter MA YoY change w/ recessions shaded
ggplot(data=hp.data) + 
  geom_line(aes(x=DATE,y=rmhsp.ma.4.yoy),color="blue") + 
  ggtitle("Real Median Home Sales Prices\n4-Quarter Moving Average YoY Change") + 
  xlab("Date") + ylab("4-Quarter MA YoY Prices Change") + 
  theme_light() + theme(plot.title = element_text(hjust = 0.5)) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  geom_hline(yintercept=0) + 
  geom_rect(data=recessions.trimmed, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='black', alpha=0.3)
# ggsave("./charts/median_hsp_ma4_yoy.png")

# add red lines when MA drops below 0
# for (i in 1:nrow(hp.data)) {
#   
# }

# mark declines of 'critical.change.rate' % or more in 4-quarter MA
plot(hp.data$DATE,hp.data$rmhsp.ma.4,type="l")
hp.data.ma <- hp.data[!is.na(hp.data$rmhsp.ma.4),]
decline.indexes <- flag.drops.pct(hp.data.ma$rmhsp.ma.4, 0.03)
hp.data.ma[decline.indexes,]
head(decline.indexes)

# plot 4-quarter MA with recessions in grey, declines of 3% or more in red
ggplot(data=hp.data.ma) + 
  geom_line(aes(x=DATE,y=rmhsp.ma.4),color="blue") + 
  ggtitle("Real Median Home Sales Prices 4-Quarter MA\nDeclines of 3% in Red") + 
  xlab("Date") + ylab("Prices 4-Quarter MA") + 
  theme_light() + theme(plot.title = element_text(hjust = 0.5)) + 
  geom_hline(yintercept=0) + 
  geom_rect(data=recessions.trimmed, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="black", alpha=0.3) + 
  geom_vline(xintercept=hp.data.ma$DATE[decline.indexes], color="red")
# ggsave(filename = "./charts/median_hsp_ma4_declines_3pct.png")



