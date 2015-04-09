writePredictions = function(values, timestamps, saveFile) {
  if (length(values) != length(timestamps)) {
    stop("lengths dont match")
  }
  if (file.exists(saveFile)) {
    warning("overwriting file")
    file.remove(saveFile)
  }
  if (!file.exists(dirname(saveFile))) {
    dir.create(dirname(saveFile), recursive=TRUE)
  }
  cat("timestamp,prediction\n", file=saveFile, sep="", append=TRUE)
  for (i in 1:length(values)) {
    cat(paste(timestamps[i], as.character(values[i]), sep = ","), "\n", file=saveFile, sep = "", append=TRUE)
  }
}
