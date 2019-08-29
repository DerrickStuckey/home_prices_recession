
# return indexes of a series which mark declines of 'critical.change.rate'
# from previous highs, resetting after each increase of 'critical.change.rate'
flag.drops.pct <- function(series, critical.change.rate) {
  last.peak <- series[1]
  last.trough <- series[1]
  drop.points <- c()
  in.decline <- FALSE
  for (i in 1:length(series)) {
    # for (i in 1:80) {
    current.val <- series[i]
    
    # flag a "new" decline of "critical.change.rate" or more from the last peak
    if (!in.decline & (current.val < last.peak * (1-critical.change.rate))) {
      in.decline <- TRUE
      drop.points <- c(drop.points, i)
      last.trough <- current.val
    }
    
    # end a decline after an increase of "critical.change.rate" or more from the last trough
    if (in.decline & (current.val > last.trough * (1+critical.change.rate))) {
      in.decline <- FALSE
      last.peak <- current.val
    }
    
    #
    if (current.val > last.peak) {
      last.peak <- current.val
    }
    if (current.val < last.trough) {
      last.trough <- current.val
    }
  }
  return(drop.points)
}


# uses simple moving average over 'lookback', flags drops to below SMA
flag.drops.sma <- function(series, lookback, start.val=series[1]) {
  in.uptrend <- TRUE
  sma.val <- start.val
  drop.points <- c()
  for(i in 1:length(series)) {
    current.val <- series[i]
    
    if(in.uptrend & (current.val < sma.val)) {
      in.uptrend <- FALSE
      drop.points <- c(drop.points,i)
    }
    
    if(!in.uptrend & (current.val > sma.val)) {
      in.uptrend <- TRUE
    }
    
    sma.val <- ((lookback-1) * sma.val + current.val)/lookback
  }
  
  return(drop.points)
}
