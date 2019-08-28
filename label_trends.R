
# return indexes of a series which mark declines of 'critical.change.rate'
# from previous highs, resetting after each increase of 'critical.change.rate'
flag.declines <- function(series, critical.change.rate) {
  last.peak <- series[1]
  last.trough <- series[1]
  decline.points <- c()
  in.decline <- FALSE
  for (i in 1:length(series)) {
    # for (i in 1:80) {
    current.val <- series[i]
    
    # flag a "new" decline of "critical.change.rate" or more from the last peak
    if (!in.decline & (current.val < last.peak * (1-critical.change.rate))) {
      in.decline <- TRUE
      decline.points <- c(decline.points, i)
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
  return(decline.points)
}
