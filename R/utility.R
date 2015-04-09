getDateFmt = function(inputString) {
  dF1 = "%Y-%m-%d %H:%M:%S"
  dF2 = "%m/%d/%Y %H:%M:%S"
  if (!is.na(strptime(str_trim(inputString), dF1))) {
    return(dF1)
    }
  if (!is.na(strptime(str_trim(inputString), dF2))) {
    return(dF2)
  }
  return(NULL)
}


alignStart = function(winSize, start) {
  d0 = trunc(start, "days")
  s = as.numeric(difftime(start, d0, units="secs"))
  w = ceiling(s / winSize)
  newStart = as.POSIXlt(d0 + w * 60)
  return(newStart)
}


stratSample = function(labels, nsamp) {
  t = table(labels)
  lmat = data.frame(label=labels)
  lmat$idx = as.numeric(rownames(lmat))
  samples = numeric(0)
  for (i in 1:length(names(t))) {
    s = sample(lmat[lmat$label == names(t)[i], c("idx")], nsamp, replace=TRUE)
    samples = c(samples, s)
  }
  return(samples)
}
