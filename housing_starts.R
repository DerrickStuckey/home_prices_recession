library(ggplot2)

# total housing starts data
houst <- read.csv("./raw_data/HOUST.csv", stringsAsFactors = FALSE)
head(houst)
tail(houst)
houst$DATE <- as.Date(houst$DATE)

# MA and YoY
houst$starts.yoy <- NA
houst$starts.ma.12 <- NA
houst$starts.ma.12.yoy <- NA
for (i in 1:nrow(houst)) {
  if (i-12>0) {
    houst$starts.yoy[i] <- houst$HOUST[i] / houst$HOUST[i-12] - 1
    houst$starts.ma.12[i] <- mean(houst$HOUST[(i-11):i])
  }
  if (i-24>0) {
    houst$starts.ma.12.yoy[i] <- houst$starts.ma.12[i] / houst$starts.ma.12[i-12] - 1
  }
}

plot(houst$starts.ma.12, type='l')

# add recessions data
recessions <- read.table("./raw_data/recessions.tsv",sep="\t",header=TRUE,colClasses=c('Date', 'Date'))
recessions$Peak <- as.Date(recessions$Peak)
recessions$Trough <- as.Date(recessions$Trough)
head(recessions)
tail(recessions)

recessions.trimmed <- recessions[recessions$Peak>min(houst$DATE) & 
                                   recessions$Peak<max(houst$DATE) ,]


# plot 12-month MA of housing starts with recessions shaded
ggplot(data=houst) + 
  geom_line(aes(x=DATE,y=starts.ma.12),color="blue") + 
  ggtitle("Total Housing Starts\n12-Month Moving Average") + 
  xlab("Date") + ylab("Housing Starts") + 
  theme_light() + theme(plot.title = element_text(hjust = 0.5)) + 
  geom_hline(yintercept=0) + 
  geom_rect(data=recessions.trimmed, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='black', alpha=0.3)
# ggsave("./charts/starts_w_recessions_ma12.png")

# 2000 peak: 1664.17 on 1999-09-01
# 2000 trough: 1559.83 on 2001-03-01
# 6.27% drop peak-to-trough

# recent peak: 1268.2500 on 2018-09-01
# recent trough: 1217.2500 on 2019-05-01
# 1217.2500 / 1268.2500
# 4.02% drop peak-to-trough

# plot YoY change in 12-month MA of housing starts with recessions shaded
ggplot(data=houst) + 
  geom_line(aes(x=DATE,y=starts.ma.12.yoy),color="blue") + 
  ggtitle("Total Housing Starts\n12-Month Moving Average YoY Change") + 
  xlab("Date") + ylab("YoY Change") + 
  theme_light() + theme(plot.title = element_text(hjust = 0.5)) + 
  geom_hline(yintercept=0) + 
  geom_rect(data=recessions.trimmed, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='black', alpha=0.3)
# ggsave("./charts/starts_w_recessions_ma12yoy.png")

